"""Red/green gates for repeat-failure PDCA (stop after two failures)."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))


def _load():
    path = ROOT / "chaos-engine/rules/lessons_repeat_failure_pdca.py"
    spec = importlib.util.spec_from_file_location("lessons_repeat_failure_pdca", path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class CardsPresentTest(unittest.TestCase):
    def test_card_and_spec(self) -> None:
        card = ROOT / "chaos-engine/references/details/ce-repeat-failure-pdca/repeat-failure-pdca.md"
        self.assertTrue(card.is_file())
        body = card.read_text(encoding="utf-8")
        self.assertIn("lessons_repeat_failure_pdca.py", body)
        self.assertIn("STOP", body.upper())
        self.assertTrue((ROOT / "chaos-engine/guides/ce-repeat-failure-pdca-SPEC.md").is_file())
        self.assertTrue((ROOT / "chaos-engine/references/ce-repeat-failure-pdca-SPEC.md").is_file())


class RepeatFailurePdcaGateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load()

    def test_single_failure_is_green(self) -> None:
        result = self.mod.evaluate_repeat_failure_pdca(
            {
                "stage": "protected-installer-acceptance",
                "consecutive_failures": [{"kind": "timeout", "summary": "once"}],
            }
        )
        self.assertTrue(result["ok"], result)

    def test_two_failures_without_note_is_red(self) -> None:
        result = self.mod.evaluate_repeat_failure_pdca(
            {
                "stage": "Agent Guidance Gate",
                "consecutive_failures": [
                    {"kind": "timeout", "summary": "run-a"},
                    {"kind": "timeout", "summary": "run-b"},
                ],
            }
        )
        self.assertFalse(result["ok"])
        self.assertIn("repeat-failure-without-note", result["hits"])

    def test_two_failures_with_valid_note_is_green(self) -> None:
        note = {
            "stage": "Agent Guidance Gate",
            "failures": [{"kind": "timeout"}],
            "hypothesis": "suite width",
            "evidence": ["run 1", "profile"],
            "plan": ["drop duplicate modules"],
        }
        result = self.mod.evaluate_repeat_failure_pdca(
            {
                "stage": "Agent Guidance Gate",
                "consecutive_failures": [{"kind": "timeout"}, {"kind": "timeout"}],
                "root_cause_note": note,
            }
        )
        self.assertTrue(result["ok"], result)

    def test_budget_overrun_without_note_is_red(self) -> None:
        result = self.mod.evaluate_repeat_failure_pdca(
            {
                "stage": "protected-installer-acceptance",
                "consecutive_failures": [],
                "budget_overrun": True,
            }
        )
        self.assertFalse(result["ok"])
        self.assertIn("budget-overrun-without-note", result["hits"])

    def test_chained_fix_after_fail_is_red(self) -> None:
        result = self.mod.evaluate_repeat_failure_pdca(
            {
                "stage": "x",
                "consecutive_failures": [{"kind": "a"}],
                "chained_fix_after_fail": True,
                "root_cause_note": {
                    "stage": "x",
                    "failures": ["a"],
                    "hypothesis": "h",
                    "evidence": ["e"],
                    "plan": ["p"],
                },
            }
        )
        self.assertFalse(result["ok"])
        self.assertIn("chained-fix-after-fail", result["hits"])

    def test_state_dir_note_covers_consecutive_ci_failures(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            stage = "protected-installer-acceptance"
            path = self.mod.note_path_for_stage(stage, root)
            path.parent.mkdir(parents=True)
            path.write_text(
                json.dumps(
                    {
                        "stage": stage,
                        "failures": [{"kind": "timeout"}],
                        "hypothesis": "duplicated design modules",
                        "evidence": ["run 41444465079"],
                        "plan": ["remove craft/lessons from protected suite"],
                    }
                ),
                encoding="utf-8",
            )
            red = self.mod.evaluate_repeat_failure_pdca(
                {
                    "stage": stage,
                    "consecutive_failures": [{"kind": "ci"}, {"kind": "ci"}],
                    "project_root": str(root),
                }
            )
            self.assertFalse(red["ok"])
            green = self.mod.evaluate_repeat_failure_pdca(
                {
                    "stage": stage,
                    "consecutive_failures": [{"kind": "ci"}, {"kind": "ci"}],
                    "root_cause_note_path": str(path.relative_to(root)),
                    "project_root": str(root),
                }
            )
            self.assertTrue(green["ok"], green)
            via_helper = self.mod.evaluate_state_dir_note(stage, root)
            self.assertTrue(via_helper["ok"], via_helper)


class AlwaysLoadedWiringTest(unittest.TestCase):
    def test_bot_entry_and_skill_name_the_rule(self) -> None:
        bot = (ROOT / "chaos-engine/references/bot-entry.md").read_text(encoding="utf-8")
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("Repeat-failure PDCA", bot)
        self.assertIn("repeat-failure-pdca", bot)
        self.assertIn("lessons_repeat_failure_pdca", bot)
        self.assertIn("Repeat-fail PDCA", skill)
        self.assertIn("bot-entry", skill)


if __name__ == "__main__":
    unittest.main()
