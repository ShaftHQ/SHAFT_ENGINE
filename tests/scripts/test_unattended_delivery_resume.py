"""Compaction resume for one live unattended pull-request watch (#6153)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

from scripts.agents.unattended_delivery import delivery_claim_rejected, resume_prompt


class UnattendedDeliveryResumeTest(unittest.TestCase):
    def test_compaction_checkpoint_names_the_watch_and_skips_a_router_reread(self):
        prompt = resume_prompt(
            {
                "watchTaskId": "task-6153",
                "pullRequest": 6153,
                "repository": "ShaftHQ/SHAFT_ENGINE",
                "headSha": "abc123",
            }
        )
        compact = re.sub(r"\s+", " ", prompt)
        self.assertIn("task-6153", compact)
        self.assertIn("6153", compact)
        self.assertIn("ShaftHQ/SHAFT_ENGINE", compact)
        self.assertIn("abc123", compact)
        self.assertIn("not done until merged", compact)
        self.assertIn("not-done-until-merged", compact)
        self.assertIn("you will be notified", compact)
        self.assertIn("does not end the turn", compact)
        self.assertIn("wait on that same task id", compact.casefold())
        self.assertNotIn("chaos-engine/skills/chaos-engine/SKILL.md", prompt)

    def test_watch_stays_up_is_rejected(self):
        self.assertTrue(
            delivery_claim_rejected(
                "The pull-request watch stays up.",
                follow_up_open=False,
            )
        )

    def test_precompact_restores_the_watch_and_skips_the_router(self):
        import importlib.util
        import io
        import json
        from contextlib import redirect_stdout

        path = Path(__file__).resolve().parents[2] / "chaos-engine/hooks/guard.py"
        spec = importlib.util.spec_from_file_location("ce_guard_resume", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        stdout = io.StringIO()
        with redirect_stdout(stdout):
            code = module._run_event(
                {
                    "hook_event_name": "PreCompact",
                    "session_id": "resume-6153",
                    "cwd": ".",
                    "unattendedWatch": {
                        "watchTaskId": "task-6153",
                        "pullRequest": 6154,
                        "repository": "ShaftHQ/SHAFT_ENGINE",
                        "headSha": "abc123",
                    },
                },
                "portable",
            )
        self.assertEqual(0, code)
        payload = json.loads(stdout.getvalue())
        context = payload["additionalContext"]
        self.assertIn("task-6153", context)
        self.assertIn("6154", context)
        self.assertIn("ShaftHQ/SHAFT_ENGINE", context)
        self.assertIn("abc123", context)
        self.assertIn("not done until merged", context)
        self.assertNotIn("chaos-engine/skills/chaos-engine/SKILL.md", context)

    def test_watch_running_claim_is_rejected(self):
        self.assertTrue(
            delivery_claim_rejected(
                "One watch is running until the pull request merges.",
                follow_up_open=False,
            )
        )

    def test_earlier_merge_with_follow_up_open_is_rejected(self):
        self.assertTrue(
            delivery_claim_rejected(
                "Pull request 6150 merged.",
                follow_up_open=True,
            )
        )

    def test_merged_at_with_no_follow_up_is_accepted(self):
        self.assertFalse(
            delivery_claim_rejected(
                "Remote mergedAt is 2026-09-23T19:04:34Z.",
                follow_up_open=False,
            )
        )


if __name__ == "__main__":
    unittest.main()
