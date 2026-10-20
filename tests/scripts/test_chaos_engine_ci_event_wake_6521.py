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

    def test_harness_parity_lessons_tracked(self):
        economy = (ROOT / "ci-status-economy.md").read_text(encoding="utf-8")
        for needle in ("## Merge queue and same-account pull requests", "mergeQueueEntry",
                       "## A rerun replays the old event", "## Release watch", "pending moderation",
                       "Never re-run a publish", "OSV", "(#release-watch)"):
            self.assertIn(needle, economy)
        maintenance = (ROOT / "harness-maintenance.md").read_text(encoding="utf-8")
        for needle in ("Never reinstall after `entry`", "git archive HEAD", "--head BRANCH --base main"):
            self.assertIn(needle, maintenance)
        self.assertIn("## Private add-ons", (ROOT / "addons.md").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
