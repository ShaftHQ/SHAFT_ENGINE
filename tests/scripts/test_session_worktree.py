"""Session-scoped primary setup and worktree teardown (#5325)."""

from __future__ import annotations

import os
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import tempfile
import unittest
from pathlib import Path

from scripts.agents import session_worktree as sw


def git(cwd: Path, *arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
        ["git", "-c", "core.longpaths=true", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


class SessionWorktreeTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.base = Path(self.temporary_directory.name)
        self.main = self.base / "checkout"
        self.main.mkdir()
        git(self.main, "init", "-q", "-b", "main", ".")
        git(self.main, "config", "user.email", "harness@example.invalid")
        git(self.main, "config", "user.name", "Harness")
        self.write(self.main, "README.md", "# Project\n")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "initial")
        self.publish_main()

    def tearDown(self):
        self.temporary_directory.cleanup()

    def publish_main(self) -> None:
        head = git(self.main, "rev-parse", "main").stdout.strip()
        git(self.main, "update-ref", "refs/remotes/origin/main", head)
        git(self.main, "symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main")

    def write(self, root: Path, relative_path: str, content: str) -> Path:
        path = root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        return path

    def test_dirty_main_resets_and_creates_sibling_worktree(self):
        self.write(self.main, "scratch.txt", "discard me\n")
        result = sw.prepare_session(self.main, "sess-1")
        self.assertEqual("created", result["status"], result)
        worktree = Path(result["worktreePath"])
        self.assertTrue(worktree.is_dir())
        self.assertEqual(self.base / "checkout.session-sess-1", worktree)
        self.assertFalse((self.main / "scratch.txt").exists())
        self.assertEqual(
            git(self.main, "rev-parse", "HEAD").stdout.strip(),
            git(self.main, "rev-parse", "origin/main").stdout.strip(),
        )
        self.assertEqual("HEAD", git(worktree, "rev-parse", "--abbrev-ref", "HEAD").stdout.strip())

    def test_dirty_leftover_task_branch_is_preserved(self):
        git(self.main, "checkout", "-qb", "ChaosEngine/leftover")
        leftover = self.write(self.main, "keep-me.txt", "unique work\n")
        result = sw.prepare_session(self.main, "sess-2")
        self.assertEqual("halted", result["status"], result)
        self.assertTrue(leftover.is_file())
        self.assertEqual("ChaosEngine/leftover", git(self.main, "rev-parse", "--abbrev-ref", "HEAD").stdout.strip())
        self.assertFalse((self.base / "checkout.session-sess-2").exists())

    def test_same_session_id_reuses_one_worktree(self):
        first = sw.prepare_session(self.main, "same")
        second = sw.prepare_session(self.main, "same")
        self.assertEqual("created", first["status"], first)
        self.assertEqual("reused", second["status"], second)
        self.assertEqual(first["worktreePath"], second["worktreePath"])

    def test_unique_commits_on_main_are_not_reset(self):
        self.write(self.main, "ahead.md", "unique\n")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "unique")
        result = sw.prepare_session(self.main, "sess-unique")
        self.assertEqual("halted", result["status"], result)
        self.assertTrue((self.main / "ahead.md").exists())

    def test_teardown_without_merge_leaves_the_worktree(self):
        created = sw.prepare_session(self.main, "sess-keep")
        kept = sw.teardown_session(self.main, "sess-keep")
        self.assertEqual("kept", kept["status"], kept)
        self.assertTrue(Path(created["worktreePath"]).is_dir())

    def test_teardown_after_recorded_merge_removes_only_that_worktree(self):
        created = sw.prepare_session(self.main, "sess-merge")
        worktree = Path(created["worktreePath"])
        git(worktree, "checkout", "-qb", "ChaosEngine/sess-merge")
        sw.record_merge(
            self.main,
            "sess-merge",
            branch="ChaosEngine/sess-merge",
            head=git(worktree, "rev-parse", "HEAD").stdout.strip(),
        )
        removed = sw.teardown_session(self.main, "sess-merge")
        self.assertEqual("removed", removed["status"], removed)
        self.assertFalse(worktree.exists())
        self.assertEqual(
            git(self.main, "rev-parse", "--verify", "ChaosEngine/sess-merge").returncode,
            0,
        )

    def test_isolation_denies_primary_writes_and_allows_session_writes(self):
        created = sw.prepare_session(self.main, "sess-iso")
        worktree = Path(created["worktreePath"])
        denied = sw.isolation_denial(
            cwd=self.main,
            session_id="sess-iso",
            mutation=True,
            workdir=str(self.main),
            targets=("README.md",),
        )
        self.assertIsNotNone(denied)
        allowed = sw.isolation_denial(
            cwd=worktree,
            session_id="sess-iso",
            mutation=True,
            workdir=str(worktree),
            targets=("README.md",),
        )
        self.assertIsNone(allowed)

    def test_second_teardown_after_remove_is_absent(self):
        sw.prepare_session(self.main, "sess-twice")
        sw.record_merge(self.main, "sess-twice")
        self.assertEqual("removed", sw.teardown_session(self.main, "sess-twice")["status"])
        self.assertEqual("absent", sw.teardown_session(self.main, "sess-twice")["status"])


if __name__ == "__main__":
    unittest.main()
