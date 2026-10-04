"""#6521: CI waits use an event wake or zero-LLM watch, never an LLM poll."""
import pathlib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2] / "chaos-engine" / "references"


class CiEventWakeRuleTest(unittest.TestCase):
    def test_ci_status_economy_defines_event_wake(self):
        text = (ROOT / "ci-status-economy.md").read_text(encoding="utf-8")
        self.assertIn("## Wait by event wake, never by LLM poll", text)
        for event in ("ci-passed", "ci-failed", "pr-merged", "pr-closed"):
            self.assertIn(event, text)
        self.assertIn("only exception to [single thread]", text)

    def test_permanent_rules_points_at_event_wake(self):
        text = (ROOT / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("ci-status-economy.md#wait-by-event-wake-never-by-llm-poll", text)


if __name__ == "__main__":
    unittest.main()
