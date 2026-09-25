"""Tests never mutate the invoking checkout (#6239).

`test_guard_lifecycle` and `test_chaos_engine_hook` ran SessionStart with a
session id against the real repository root. The session-worktree gate then
checked the developer's clean task branch out to the default branch, hard-reset
it, and registered a sibling session worktree. These tests pin the three
defenses: protected checkouts, the pytest repo-state guard, and isolation of
the two offending tests.
"""

from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess  # nosec B404 - tests drive local git/python on temp fixtures.
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).resolve().parent))

import repo_state_guard  # noqa: E402
from scripts.agents import session_worktree as sw  # noqa: E402


def git(cwd: Path, *arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git argv on a temp fixture.
        ["git", "-c", "core.longpaths=true", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


class FixtureRepository(unittest.TestCase):
    """A primary checkout on a clean task branch equal to origin/main: the #6239 shape."""

    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.base = Path(self.temporary_directory.name).resolve()
        self.main = self.base / "checkout"
        self.main.mkdir()
        git(self.main, "init", "-q", "-b", "main", ".")
        git(self.main, "config", "user.email", "harness@example.invalid")
        git(self.main, "config", "user.name", "Harness")
        (self.main / "README.md").write_text("# Project\n", encoding="utf-8")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "initial")
        head = git(self.main, "rev-parse", "main").stdout.strip()
        git(self.main, "update-ref", "refs/remotes/origin/main", head)
        git(self.main, "symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main")
        git(self.main, "checkout", "-q", "-b", "task/branch")

    def tearDown(self):
        self.temporary_directory.cleanup()

    def branch(self) -> str:
        return git(self.main, "symbolic-ref", "--short", "HEAD").stdout.strip()


class ProtectedCheckoutTest(FixtureRepository):
    def test_unprotected_fixture_reproduces_the_switch_and_reset(self):
        with patch.dict(os.environ, {sw.PROTECTED_CHECKOUTS_ENV: ""}):
            result = sw.prepare_session(self.main, "repro")
        self.assertEqual("created", result["status"])
        self.assertEqual("main", self.branch())

    def test_protected_checkout_is_never_switched_reset_or_given_a_worktree(self):
        with patch.dict(os.environ, {sw.PROTECTED_CHECKOUTS_ENV: str(self.main)}):
            result = sw.prepare_session(self.main, "guarded")
        self.assertEqual("skipped", result["status"])
        self.assertIn("protected", result["message"])
        self.assertEqual("task/branch", self.branch())
        self.assertFalse(sw.worktree_path_for(self.main, "guarded").exists())
        self.assertEqual(1, len(git(self.main, "worktree", "list").stdout.splitlines()))

    def test_protected_primary_is_not_reset_after_teardown(self):
        git(self.main, "checkout", "-q", "main")
        (self.main / "scratch.txt").write_text("keep me\n", encoding="utf-8")
        with patch.dict(os.environ, {sw.PROTECTED_CHECKOUTS_ENV: str(self.main)}):
            sw._reset_primary_default(self.main)
        self.assertTrue((self.main / "scratch.txt").is_file())

    def test_protection_lists_several_paths(self):
        other = self.base / "elsewhere"
        value = os.pathsep.join([str(other), str(self.main)])
        with patch.dict(os.environ, {sw.PROTECTED_CHECKOUTS_ENV: value}):
            self.assertTrue(sw.is_protected_checkout(self.main))
            self.assertFalse(sw.is_protected_checkout(self.base))


class RepoStateSnapshotTest(FixtureRepository):
    def test_clean_run_reports_no_difference(self):
        before = repo_state_guard.snapshot(self.main)
        self.assertEqual([], repo_state_guard.differences(before, repo_state_guard.snapshot(self.main)))

    def test_branch_switch_head_move_edit_and_worktree_are_each_reported(self):
        before = repo_state_guard.snapshot(self.main)
        git(self.main, "checkout", "-q", "main")
        (self.main / "README.md").write_text("# Changed\n", encoding="utf-8")
        git(self.main, "worktree", "add", "-q", "--detach", str(self.base / "sibling"), "HEAD")
        changed = repo_state_guard.differences(before, repo_state_guard.snapshot(self.main))
        fields = {line.split(":", 1)[0] for line in changed}
        self.assertEqual({"branch", "status", "worktrees"}, fields)
        git(self.main, "commit", "-qam", "move head")
        moved = repo_state_guard.differences(before, repo_state_guard.snapshot(self.main))
        self.assertIn("head", {line.split(":", 1)[0] for line in moved})

    def test_protect_merges_roots_into_the_environment(self):
        with patch.dict(os.environ, {repo_state_guard.PROTECTED_CHECKOUTS_ENV: "/already"}):
            repo_state_guard.protect([self.main])
            value = os.environ[repo_state_guard.PROTECTED_CHECKOUTS_ENV].split(os.pathsep)
        self.assertEqual(["/already", str(self.main)], value)
        self.assertEqual(sw.PROTECTED_CHECKOUTS_ENV, repo_state_guard.PROTECTED_CHECKOUTS_ENV)

    def test_checkout_roots_names_worktree_and_primary(self):
        linked = self.base / "linked"
        git(self.main, "worktree", "add", "-q", "--detach", str(linked), "HEAD")
        self.assertEqual([linked.resolve(), self.main], repo_state_guard.checkout_roots(linked))


@unittest.skipUnless(shutil.which("git"), "git is required")
class PytestRepoStateGuardTest(FixtureRepository):
    """The conftest guard fails a pytest run that mutates the invoking checkout."""

    def setUp(self):
        super().setUp()
        if importlib.util.find_spec("pytest") is None:
            self.skipTest("pytest is not installed")
        suite = self.main / "tests" / "scripts"
        suite.mkdir(parents=True)
        for name in ("conftest.py", "repo_state_guard.py"):
            shutil.copy2(ROOT / "tests" / "scripts" / name, suite / name)
        (self.main / ".gitignore").write_text("__pycache__/\n.pytest_cache/\n", encoding="utf-8")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "suite")

    def run_suite(self, body: str) -> subprocess.CompletedProcess:
        (self.main / "tests" / "scripts" / "test_probe.py").write_text(textwrap.dedent(body), encoding="utf-8")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "probe")
        return subprocess.run(  # nosec B603 - fixed interpreter on a temp fixture.
            [sys.executable, "-m", "pytest", "-q", "-p", "no:cacheprovider", "tests/scripts"],
            cwd=self.main,
            capture_output=True,
            text=True,
            check=False,
            timeout=300,
        )

    def test_mutating_test_fails_the_run_with_the_change_named(self):
        completed = self.run_suite(
            """
            import subprocess
            from pathlib import Path

            def test_switches_branch():
                subprocess.run(["git", "checkout", "-q", "main"], cwd=Path(__file__).parents[2], check=True)
            """
        )
        self.assertNotEqual(0, completed.returncode, completed.stdout)
        self.assertIn("repo-state guard (#6239)", completed.stdout)
        self.assertIn("branch: 'task/branch' -> 'main'", completed.stdout)

    def test_non_mutating_run_passes_and_marks_the_checkout_protected(self):
        completed = self.run_suite(
            """
            import os
            from pathlib import Path

            def test_protected():
                protected = os.environ["CHAOS_ENGINE_PROTECTED_CHECKOUTS"].split(os.pathsep)
                assert str(Path(__file__).resolve().parents[2]) in protected
                assert os.environ["CHAOS_ENGINE_STORE_REFRESH"] == "0"
            """
        )
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertNotIn("repo-state guard", completed.stdout)


class OffendingTestsAreIsolatedTest(unittest.TestCase):
    """SC-2: the two SessionStart tests that reached the real root stay isolated."""

    def test_guard_lifecycle_kernel_reachability_patches_session_worktree(self):
        source = (ROOT / "tests/scripts/test_guard_lifecycle.py").read_text(encoding="utf-8")
        body = source.split("def test_repository_hook_dispatch_reaches_the_portable_kernel", 1)[1]
        body = body.split("\n    def ", 1)[0]
        self.assertIn("_prepare_session_worktree", body)

    def test_hook_companion_parity_runs_against_a_protected_root(self):
        source = (ROOT / "tests/scripts/test_chaos_engine_hook.py").read_text(encoding="utf-8")
        body = source.split("def test_source_and_portable_session_start_share_exact_companion_context", 1)[1]
        body = body.split("\n    def ", 1)[0]
        self.assertIn("isolated_session_environment", body)


if __name__ == "__main__":
    unittest.main()
