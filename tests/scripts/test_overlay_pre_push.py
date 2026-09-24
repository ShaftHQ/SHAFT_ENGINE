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
        self.assertFalse(any("16384" in failure for failure in failures))

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
            try:
                os.chdir(root)
                blocked = ce_guard._overlay_push_block(("git push origin HEAD",))
            finally:
                os.chdir(previous)
            self.assertTrue(blocked)
            self.assertIn("16384", blocked)

    def test_live_playbook_and_entrypoint_satisfy_the_contract(self):
        text = (ROOT / PLAYBOOK).read_text(encoding="utf-8")
        self.assertEqual([], playbook_contract_failures(text))
        failures = overlay_pre_push_failures(
            ROOT, ["chaos-engine/skills/chaos-engine/SKILL.md"]
        )
        self.assertEqual([], failures)


if __name__ == "__main__":
    unittest.main()
