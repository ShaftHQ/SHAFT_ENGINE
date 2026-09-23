"""Harness program rules stay in the GitHub playbook (#6140)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class HarnessProgramTest(unittest.TestCase):
    def test_program_rules_and_interactive_planning_sentence(self):
        text = (ROOT / "chaos-engine/references/work-github-playbook.md").read_text(encoding="utf-8")
        self.assertIn("Skip planning questions only when the owner explicitly asked for unattended planning.", text)
        for phrase in (
            "epic plus its native sub-issues",
            "one orchestrator",
            "local implementers",
            "one adversarial reviewer per PR",
            "one Learning Session after the epic's in-scope pull requests merge",
            "invalid Learning Session skip only after a confirmed delivery",
            "Sub-issues do not re-ask attendance",
            "Each implementation PR names its unittest module",
            "every sub-issue is merged or explicitly dropped",
        ):
            self.assertIn(phrase, text)


if __name__ == "__main__":
    unittest.main()
