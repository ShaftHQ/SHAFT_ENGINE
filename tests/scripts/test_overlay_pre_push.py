"""Local overlay pre-push budget, pinned clauses, and reachability."""

from __future__ import annotations

import shutil
import subprocess  # nosec B404 - fixed git init/add against a temp repo only.
import tempfile
import unittest
from pathlib import Path

from scripts.ci.overlay_pre_push import (
    PLAYBOOK,
    overlay_pre_push_failures,
    playbook_contract_failures,
    release_note_label_failures,
    touches_overlay_contract,
)

ROOT = Path(__file__).resolve().parents[2]
PINNED = (
    "Before committing any subagent's work",
    "Before reviewing or shipping any nontrivial diff",
    "deferred/out-of-scope/adjacent-finding/follow-up",
)


def _playbook(text: str) -> str:
    return text


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
        playbook = (ROOT / PLAYBOOK).read_text(encoding="utf-8")
        promotion = (ROOT / "chaos-engine/references/details/work-github-playbook-promotion.md").read_text(
            encoding="utf-8"
        )
        self.assertEqual(playbook_contract_failures(playbook), [])
        self.assertLessEqual(len(playbook.encode("utf-8")) + 1024, 16384)
        self.assertIn("work-github-playbook-promotion.md", playbook)
        self.assertIn("privacy-safe queued learning payload", promotion)
        self.assertIn("caller-supplied participant list", promotion)

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
            "ce_guard_6155", ROOT / "chaos-engine/hooks/guard.py"
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

    def test_live_playbook_and_entrypoint_satisfy_the_contract(self):
        text = (ROOT / PLAYBOOK).read_text(encoding="utf-8")
        self.assertEqual([], playbook_contract_failures(text))
        failures = overlay_pre_push_failures(
            ROOT, ["chaos-engine/skills/chaos-engine/SKILL.md"]
        )
        self.assertEqual([], failures)

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
        from scripts.ci import overlay_pre_push as opp

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



if __name__ == "__main__":
    unittest.main()
