"""Local overlay pre-push budget, pinned clauses, and reachability."""

from __future__ import annotations

import shutil
import subprocess  # nosec B404 - fixed git init/add against a temp repo only.
import tempfile
import unittest
from contextlib import contextmanager
from pathlib import Path

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci import overlay_pre_push as opp

coverage_ratchet_failures = opp.coverage_ratchet_failures
PLAYBOOK = opp.PLAYBOOK
overlay_pre_push_failures = opp.overlay_pre_push_failures
playbook_contract_failures = opp.playbook_contract_failures
release_note_label_failures = opp.release_note_label_failures
touches_overlay_contract = opp.touches_overlay_contract

ROOT = Path(__file__).resolve().parents[2]


def _live_playbook() -> Path:
    """Playbook lives in the harness overlay after #8057; PLAYBOOK stays the logical path."""
    overlay = ROOT / ".chaos-engine" / "references" / "work-github-playbook.md"
    legacy = ROOT / PLAYBOOK
    return overlay if overlay.is_file() else legacy


PINNED = (
    "Before committing any subagent's work",
    "Before reviewing or shipping any nontrivial diff",
    "deferred/out-of-scope/adjacent-finding/follow-up",
)


def _playbook(text: str) -> str:
    return text



@contextmanager
def _park_leaked_pdca(root: Path):
    """Hide suite-local PDCA events for one contract check, then restore them."""
    folder = root / ".chaos-engine-state" / "repeat-failure-pdca"
    parked = folder.with_name(folder.name + "-suite-park")
    moved = False
    if folder.exists():
        if parked.exists():
            shutil.rmtree(parked)
        folder.rename(parked)
        moved = True
    try:
        yield
    finally:
        if moved:
            if folder.exists():
                shutil.rmtree(folder)
            parked.rename(folder)


def _stage_version(root: Path) -> None:
    """Write a stub chaos-engine/VERSION for fixtures that stage chaos-engine/ paths."""
    version = root / ".chaos-engine" / "VERSION"
    version.parent.mkdir(parents=True, exist_ok=True)
    if not version.is_file():
        version.write_text("0.0.0-test\n", encoding="utf-8")

class OverlayPrePushTest(unittest.TestCase):
    def test_over_budget_fixture_fails(self):
        text = ("x" * 16385) + "\n" + "\n".join(PINNED)
        failures = playbook_contract_failures(text)
        self.assertTrue(any("16384" in failure for failure in failures))

    def test_dropped_pinned_clause_fails_under_the_byte_budget(self):
        text = "short playbook\n" + "\n".join(PINNED[1:])
        self.assertLess(len(text.encode("utf-8")), 16384)
        failures = playbook_contract_failures(text)
        self.assertTrue(any("Before committing any subagent's work" in failure for failure in failures))

    def test_live_playbook_has_room_for_another_lesson_without_dropping_promotion(self):
        playbook = _live_playbook().read_text(encoding="utf-8")
        promotion = (ROOT / ".chaos-engine/references/details/work-github-playbook-promotion.md").read_text(
            encoding="utf-8"
        )
        self.assertEqual(playbook_contract_failures(playbook), [])
        self.assertLessEqual(len(playbook.encode("utf-8")) + 1024, 16384)
        self.assertIn("work-github-playbook-promotion.md", playbook)
        self.assertIn("privacy-safe queued learning payload", promotion)
        self.assertIn("caller-supplied participant list", promotion)

    def test_playbook_without_lesson_headroom_fails_before_ci(self):
        # #6846: pre-push used to pass anything up to the cap while CI demanded 1024 B of headroom.
        pinned = "\n".join(PINNED)
        near_cap = ("z" * (16384 - 1000 - len(pinned.encode("utf-8")))) + "\n" + pinned
        self.assertTrue(any("headroom" in failure for failure in playbook_contract_failures(near_cap)))
        roomy = ("z" * 100) + "\n" + pinned
        self.assertEqual([], playbook_contract_failures(roomy))

    def test_pre_push_runs_the_fixture_only_when_the_diff_touches_overlay(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            path = root / PLAYBOOK
            path.parent.mkdir(parents=True)
            path.write_text(("y" * 16385) + "\n" + "\n".join(PINNED), encoding="utf-8")
            touched = overlay_pre_push_failures(root, [PLAYBOOK])
            self.assertTrue(any("16384" in failure for failure in touched))
            self.assertEqual([], overlay_pre_push_failures(root, ["shaft-engine/src/Main.java"]))
            path.write_text("short\n" + "\n".join(PINNED[1:]), encoding="utf-8")
            dropped = overlay_pre_push_failures(root, ["chaos-engine/hooks/guard.py"])
            self.assertTrue(any("Before committing any subagent's work" in failure for failure in dropped))

    def test_touch_matrix_includes_markdown_hooks_and_skill_entrypoints(self):
        self.assertTrue(touches_overlay_contract("chaos-engine/references/work-github-playbook.md"))
        self.assertTrue(touches_overlay_contract("chaos-engine/hooks/guard.py"))
        self.assertTrue(touches_overlay_contract("chaos-engine/skills/chaos-engine/SKILL.md"))
        self.assertFalse(touches_overlay_contract("shaft-engine/src/Main.java"))

    def test_guard_blocks_git_push_when_the_staged_playbook_fails(self):
        from scripts.agents import guard

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            destination = root / "scripts/ci/overlay_pre_push.py"
            destination.parent.mkdir(parents=True)
            shutil.copy(ROOT / "scripts/ci/overlay_pre_push.py", destination)
            playbook = root / PLAYBOOK
            playbook.parent.mkdir(parents=True)
            playbook.write_text(("z" * 16385) + "\n" + "\n".join(PINNED), encoding="utf-8")
            _stage_version(root)
            git = shutil.which("git")
            self.assertIsNotNone(git)
            subprocess.run([git, "init"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "add", "-A"], cwd=root, check=True, capture_output=True)  # nosec B603
            reason = guard._overlay_pre_push_reason(str(root))
            self.assertIsNotNone(reason)
            self.assertIn("16384", reason)
            self.assertIsNone(guard._overlay_pre_push_reason(str(root / "missing")))


    def test_overlay_push_block_uses_git_toplevel_not_process_cwd(self):
        """#6155: portable guard resolves the push worktree via git toplevel."""
        import importlib.util
        import os
        from unittest import mock

        spec = importlib.util.spec_from_file_location(
            "ce_guard_6155", ROOT / ".chaos-engine/hooks/guard.py"
        )
        self.assertIsNotNone(spec and spec.loader)
        ce_guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(ce_guard)
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            git = shutil.which("git")
            self.assertIsNotNone(git)
            subprocess.run([git, "init"], cwd=root, check=True, capture_output=True)  # nosec B603
            destination = root / "scripts/ci/overlay_pre_push.py"
            destination.parent.mkdir(parents=True)
            shutil.copy(ROOT / "scripts/ci/overlay_pre_push.py", destination)
            playbook = root / PLAYBOOK
            playbook.parent.mkdir(parents=True)
            playbook.write_text(("z" * 16385) + "\n" + "\n".join(PINNED), encoding="utf-8")
            _stage_version(root)
            subprocess.run([git, "add", "-A"], cwd=root, check=True, capture_output=True)  # nosec B603
            previous = Path.cwd()
            captured: list[list[str]] = []
            real_run = ce_guard.subprocess.run

            def capture_run(argv, *args, **kwargs):  # noqa: ANN001
                captured.append(list(argv))
                return real_run(argv, *args, **kwargs)

            try:
                os.chdir(root)
                with mock.patch.object(ce_guard.subprocess, "run", side_effect=capture_run):
                    blocked = ce_guard._overlay_push_block(("git push origin HEAD",))
            finally:
                os.chdir(previous)
            self.assertTrue(blocked)
            self.assertIn("16384", blocked)
            self.assertTrue(captured, "expected git rev-parse via subprocess.run")
            self.assertTrue(Path(captured[0][0]).is_absolute(), captured[0][0])
            self.assertEqual(Path(captured[0][0]), Path(git))
            self.assertEqual(captured[0][1:], ["rev-parse", "--show-toplevel"])

    def test_overlay_push_block_reports_a_checker_crash(self):
        """A checker exception is a contract failure, not an invalid-JSON deny."""
        import importlib.util
        import os

        spec = importlib.util.spec_from_file_location(
            "ce_guard_crash", ROOT / ".chaos-engine/hooks/guard.py"
        )
        self.assertIsNotNone(spec and spec.loader)
        ce_guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(ce_guard)
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            git = shutil.which("git")
            self.assertIsNotNone(git)
            subprocess.run([git, "init"], cwd=root, check=True, capture_output=True)  # nosec B603
            script = root / "scripts/ci/overlay_pre_push.py"
            script.parent.mkdir(parents=True)
            script.write_text(
                "def overlay_pre_push_failures(root, paths=None):\n"
                "    raise ModuleNotFoundError(\"No module named 'yaml'\")\n",
                encoding="utf-8",
            )
            previous = Path.cwd()
            try:
                os.chdir(root)
                blocked = ce_guard._overlay_push_block(("git push origin HEAD",))
            finally:
                os.chdir(previous)
        self.assertIn("ModuleNotFoundError", blocked)
        self.assertIn("yaml", blocked)
        self.assertNotIn("invalid JSON", blocked)

    def test_agents_guard_reports_a_checker_crash(self):
        from scripts.agents import guard

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            script = root / "scripts/ci/overlay_pre_push.py"
            script.parent.mkdir(parents=True)
            script.write_text(
                "def overlay_pre_push_failures(root, paths=None):\n"
                "    raise ModuleNotFoundError(\"No module named 'yaml'\")\n",
                encoding="utf-8",
            )
            reason = guard._overlay_pre_push_reason(str(root))
        self.assertIn("ModuleNotFoundError", reason)
        self.assertIn("yaml", reason)
        self.assertNotIn("invalid JSON", reason)

    def test_parked_pdca_hides_leaked_events_and_restores_them(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            folder = root / ".chaos-engine-state" / "repeat-failure-pdca"
            folder.mkdir(parents=True)
            events = folder / "events.jsonl"
            events.write_text("leaked\n", encoding="utf-8")
            with _park_leaked_pdca(root):
                self.assertFalse(folder.exists())
            self.assertEqual("leaked\n", events.read_text(encoding="utf-8"))

    def test_release_note_label_fails_when_open_pr_has_none(self):
        def fake(_root):
            return {
                "number": 6702,
                "author": {"login": "MohabMohie"},
                "labels": [{"name": "ci"}],
            }

        failures = release_note_label_failures(ROOT, pr_fetcher=fake)
        self.assertTrue(any("release-note label" in f and "#6702" in f for f in failures))
        self.assertTrue(any("none" in f for f in failures))

    def test_release_note_label_passes_with_exactly_one_classification(self):
        def fake(_root):
            return {
                "number": 6702,
                "author": {"login": "MohabMohie"},
                "labels": [{"name": "skip-release-notes"}, {"name": "ci"}],
            }

        self.assertEqual([], release_note_label_failures(ROOT, pr_fetcher=fake))

    def test_release_note_label_skips_bots_and_missing_pr(self):
        self.assertEqual(
            [],
            release_note_label_failures(
                ROOT,
                pr_fetcher=lambda _r: {
                    "number": 1,
                    "author": {"login": "dependabot[bot]"},
                    "labels": [],
                },
            ),
        )
        self.assertEqual([], release_note_label_failures(ROOT, pr_fetcher=lambda _r: None))

    def test_overlay_pre_push_includes_release_note_when_live(self):
        """Live mode (paths=None) consults the open-PR fetcher; fixture mode does not."""
        from unittest import mock

        # #8057: CI has no installed .chaos-engine/ overlay; the moved-gate
        # fail-closed path is covered by MovedChaosEngineGateTest.
        with _park_leaked_pdca(ROOT), mock.patch.object(opp, "_ce_moved", return_value=False):
            with mock.patch.object(
                opp,
                "release_note_label_failures",
                return_value=["release-note label: open PR #1 needs exactly one"],
            ) as mocked:
                with mock.patch.object(opp, "code_quality_failures", return_value=[]):
                    with mock.patch.object(opp, "changed_overlay_paths", return_value=[]):
                        with mock.patch.object(opp, "tip_preflight_failures", return_value=[]):
                            live = opp.overlay_pre_push_failures(ROOT)
            fixture = opp.overlay_pre_push_failures(ROOT, ["shaft-engine/src/Main.java"])
        self.assertTrue(any("release-note label" in f for f in live))
        self.assertEqual([], fixture)
        mocked.assert_called_once()


class MovedChaosEngineGateTest(unittest.TestCase):
    """#8057: after the move the safety gates resolve the installed overlay or fail closed."""

    def _moved_root(self, temporary: str) -> Path:
        """Shim only. ``_ce_moved`` is chaos-engine/install.sh without install.py."""
        root = Path(temporary)
        shim = root / "chaos-engine"
        shim.mkdir()
        (shim / "install.sh").write_text("#!/usr/bin/env sh\n", encoding="utf-8")
        return root

    def test_moved_checkout_without_overlay_fails_closed(self):
        from unittest import mock

        with tempfile.TemporaryDirectory() as temporary:
            root = self._moved_root(temporary)
            self.assertIn("client-name scrub unavailable", opp.client_name_scrub_failures(root, environ={})[0])
            self.assertIn("L77-no-token-in-remote unavailable", opp.remote_credential_failures(root)[0])
            # Ignore an overlay installed in this checkout (CI fetches one, e2c):
            # the fixture must model a moved checkout with no overlay anywhere.
            original = opp._ce_module_path
            with mock.patch.object(
                opp, "_ce_module_path",
                side_effect=lambda base, rel: original(base, rel) if Path(base).resolve() == root.resolve() else None,
            ):
                self.assertIn("operation boundary unavailable", opp._operation_disk_failures(root, [])[0])

    def test_installed_overlay_modules_are_used(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = self._moved_root(temporary)
            rules = root / ".chaos-engine" / "rules"
            rules.mkdir(parents=True)
            (rules / "remote_credential_guard.py").write_text(
                "def pre_push_failures(root):\n    return ['overlay-L77-ran']\n", encoding="utf-8"
            )
            (rules / "operation_boundary.py").write_text(
                "def disk_blocks(root, changed):\n    return ['overlay-boundary-ran']\n", encoding="utf-8"
            )
            self.assertEqual(["overlay-L77-ran"], opp.remote_credential_failures(root))
            self.assertEqual(["operation boundary: overlay-boundary-ran"], opp._operation_disk_failures(root, []))


class CoverageRatchetPrePushTest(unittest.TestCase):
    """#6851: the Javadoc/test-reference ratchets run before push when Java sources change."""

    def _root(self, temporary: str, exit_code: int) -> Path:
        root = Path(temporary)
        (root / "scripts/ci").mkdir(parents=True)
        for name in ("validate_javadoc_coverage.py", "validate_test_reference_coverage.py"):
            (root / "scripts/ci" / name).write_text(
                f"print('Public members no test calls')\nraise SystemExit({exit_code})\n", encoding="utf-8")
        return root

    def test_java_change_runs_ratchets_and_reports_regressions(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            failures = coverage_ratchet_failures(self._root(temporary, 1), ["m/src/main/java/A.java"])
        self.assertEqual(2, len(failures))
        self.assertIn("no test calls", failures[1])

    def test_passing_ratchets_and_non_java_changes_are_silent(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = self._root(temporary, 0)
            self.assertEqual([], coverage_ratchet_failures(root, ["m/src/main/java/A.java"]))
            self.assertEqual([], coverage_ratchet_failures(root, ["docs/readme.md"]))

class WorkflowReadmeContractTest(unittest.TestCase):
    def test_workflow_inventory_runs_before_push_6990(self) -> None:
        ids = {test_id for _predicate, _module, test_id in opp.CHEAP_CI_CONTRACTS}
        self.assertIn("ValidateQualityConfigurationTest.test_repository_configuration_is_valid", ids)

class PrBodyPreflightTest(unittest.TestCase):
    def test_open_pr_body_without_assumption_link_blocks_push(self) -> None:
        bad = {"number": 7, "title": "x", "body": "## What\nstuff"}
        self.assertTrue(opp.pr_body_failures(Path("."), pr_fetcher=lambda _root: bad))
        self.assertEqual([], opp.pr_body_failures(Path("."), pr_fetcher=lambda _root: None))

    def test_cursor_branch_blocks_without_an_open_pr(self) -> None:
        from unittest.mock import patch

        with patch.object(opp, "head_branch_name", return_value="cursor/night"):
            self.assertTrue(opp.cursor_branch_failures(Path(".")))
        with patch.object(opp, "head_branch_name", return_value="fix/human"):
            self.assertEqual([], opp.cursor_branch_failures(Path(".")))
        with patch.object(opp, "head_branch_name", return_value="ChaosEngine/night-cluster-20261106"):
            self.assertEqual([], opp.cursor_branch_failures(Path(".")))


class GhOpenPrForHeadTest(unittest.TestCase):
    """`gh pr view` takes the branch positionally; it has no --head flag."""

    def _run(self, payload: str):
        from unittest import mock

        calls = []

        def fake_run(argv, **_kwargs):
            calls.append(argv)
            out = "feature-x\n" if "rev-parse" in argv else payload
            return subprocess.CompletedProcess(argv, 0, stdout=out, stderr="")

        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(
            opp.shutil, "which", side_effect=lambda name: f"/usr/bin/{name}"
        ), mock.patch.object(opp.subprocess, "run", side_effect=fake_run):
            opp._GH_PR_CACHE.clear()
            try:
                return opp._gh_open_pr_for_head(Path(tmp)), calls
            finally:
                opp._GH_PR_CACHE.clear()

    def test_branch_is_positional_and_open_pr_is_returned(self):
        pr, calls = self._run('{"number": 7, "state": "OPEN", "body": "b"}')
        self.assertEqual(7, pr["number"])
        gh_argv = calls[-1]
        self.assertNotIn("--head", gh_argv)
        self.assertEqual(["pr", "view", "feature-x"], gh_argv[1:4])

    def test_merged_pr_for_a_reused_branch_is_ignored(self):
        pr, _calls = self._run('{"number": 7, "state": "MERGED", "body": "b"}')
        self.assertIsNone(pr)

    def test_missing_state_is_not_treated_as_open(self):
        pr, _calls = self._run('{"number": 7, "body": "b"}')
        self.assertIsNone(pr)


if __name__ == "__main__":
    unittest.main()


class _ProbeContract(unittest.TestCase):
    """Fails only when the cheap-contract test below arms it."""

    armed = False

    def test_probe(self) -> None:
        if _ProbeContract.armed:
            self.fail("budget exceeded probe")


class CheapCiContractTest(unittest.TestCase):
    """#6982: Agent Guidance Gate's cheap tests run in pre-push for their inputs."""

    def test_runs_only_for_matching_paths_and_reports_failure(self) -> None:
        from unittest.mock import patch

        root = Path(__file__).resolve().parents[2]
        probe = ((lambda p: p.endswith(".md"), "tests.scripts.test_overlay_pre_push", "_ProbeContract.test_probe"),)
        with patch.object(opp, "CHEAP_CI_CONTRACTS", probe):
            _ProbeContract.armed = True
            try:
                self.assertEqual([], opp.cheap_ci_contract_failures(root, ["a.py"]))
                failures = opp.cheap_ci_contract_failures(root, ["chaos-engine/x.md"])
            finally:
                _ProbeContract.armed = False
        self.assertEqual(1, len(failures))
        self.assertIn("budget exceeded probe", failures[0])

    def test_real_contracts_pass_on_this_tree(self) -> None:
        root = Path(__file__).resolve().parents[2]
        changed = ["chaos-engine/references/bot-entry.md", "chaos-engine/VERSION", "scripts/ci/harness_pr_gate.py"]
        self.assertEqual([], opp.cheap_ci_contract_failures(root, changed))


class DotDirectoryPathTest(unittest.TestCase):
    """A dot-directory path keeps its leading dot, so the workflows-README row check runs (slipped twice)."""

    def test_workflow_change_runs_the_workflows_readme_contract(self):
        predicates = [pred for pred, mod, _ in opp.CHEAP_CI_CONTRACTS
                      if mod == "tests.scripts.test_validate_quality_configuration"]
        self.assertTrue(any(pred(".github/workflows/new.yml") for pred in predicates))
        source = (Path(__file__).resolve().parents[2] / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        self.assertNotIn('.lstrip("./")', source)


class OverlayScrubLoadDoesNotShadowTestsTest(unittest.TestCase):
    """#8057 e2c: loading the scrub from .chaos-engine/ must not put the
    harness's own ``tests`` package ahead of SHAFT's on sys.path."""

    def test_scrub_from_installed_overlay_leaves_sys_path_alone(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            overlay = root / ".chaos-engine"
            (overlay / "tests").mkdir(parents=True)
            (overlay / "tests" / "__init__.py").write_text("", encoding="utf-8")
            (overlay / "client_name_scrub.py").write_text(
                "def client_name_scrub_failures(root, environ=None):\n    return ['stub-ran']\n",
                encoding="utf-8",
            )
            before = list(_sys.path)
            self.assertEqual(opp.client_name_scrub_failures(root, environ={}), ["stub-ran"])
            self.assertEqual(_sys.path, before)
            self.assertNotIn(str(overlay), _sys.path)
