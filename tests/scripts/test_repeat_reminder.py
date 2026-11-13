"""Advisory repeat reminder and the unknown-outcome checkpoint rule (#8024)."""

from __future__ import annotations

import importlib.util
import io
import json
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "chaos-engine/hooks/repeat_reminder.py"
CHECKPOINT = ROOT / "chaos-engine/references/checkpoint-resume.md"
BUDGET = ROOT / "chaos-engine/always-loaded-budget.json"
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"


def load_module():
    spec = importlib.util.spec_from_file_location("repeat_reminder", MODULE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class RepeatReminderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.module = load_module()
        self.module.reset_session("s")

    def test_third_identical_call_advises_and_does_not_deny(self) -> None:
        args = {"path": "a"}
        self.assertIsNone(self.module.observe("s", "Read", args))
        self.assertIsNone(self.module.observe("s", "Read", {"path": "a"}))
        advice = self.module.observe("s", "Read", args)
        self.assertIn("Read", advice)
        self.assertIn("3", advice)
        payload = self.module.allow_context(advice)
        self.assertEqual(set(payload), {"additionalContext"})
        self.assertNotIn("decision", payload)

    def test_a_different_call_resets_and_an_excluded_call_does_not(self) -> None:
        self.module.observe("s", "Read", {"path": "a"})
        self.module.observe("s", "Read", {"path": "a"})
        self.assertIsNone(self.module.observe("s", "retrieve", {"q": "x"}))
        advice = self.module.observe("s", "Read", {"path": "a"})
        self.assertIn("3", advice)
        self.module.reset_session("other")
        self.assertIsNone(self.module.observe("other", "Read", {"path": "b"}))
        self.assertIsNone(self.module.observe("s", "Grep", {"pattern": "a"}))

    def test_denied_repeats_still_increment(self) -> None:
        args = {"command": "true"}
        self.module.observe("s", "Bash", args)
        self.module.observe("s", "Bash", args)
        advice = self.module.observe("s", "Bash", args)
        payload = self.module.denied_with_advice("safety", advice)
        self.assertEqual(payload["decision"], "block")
        self.assertIn("additionalContext", payload)
        self.assertIn("advice, not a block", payload["additionalContext"])

    def test_a_new_user_turn_resets_the_chain(self) -> None:
        self.module.observe("s", "Read", {"path": "a"})
        self.module.observe("s", "Read", {"path": "a"})
        self.module.reset_session("s")
        self.assertIsNone(self.module.observe("s", "Read", {"path": "a"}))

    def test_invalid_thresholds_fail_at_load(self) -> None:
        for raw in ((), [], "", (1, 3), (3, 3), (3, "no"), (2.5,), (8.0, 3.0, 5.0)):
            with self.subTest(raw=raw):
                with self.assertRaises(self.module.RepeatReminderError):
                    self.module.load_thresholds(raw)
        self.assertEqual(self.module.load_thresholds((8, 3, 5)), (3, 5, 8))

    def test_an_internal_error_fails_open(self) -> None:
        def broken(*_args, **_kwargs):
            raise RuntimeError("boom")

        self.module._observe = broken
        with redirect_stderr(io.StringIO()) as err:
            self.assertIsNone(self.module.observe("s", "Read", {"path": "a"}))
        self.assertIn("failed open", err.getvalue())

    def test_a_repeated_delete_deny_counts(self) -> None:
        path = ROOT / "chaos-engine/hooks/guard.py"
        spec = importlib.util.spec_from_file_location("chaos_engine_guard_repeat", path)
        guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(guard)
        event = {
            "hook_event_name": "PreToolUse",
            "tool_name": "Bash",
            "tool_input": {"command": "rm -rf scratch"},
            "cwd": str(ROOT),
            "session_id": "repeat-delete",
        }
        payloads = []
        with mock.patch.object(guard, "delete_guard_reason", return_value="blocked-delete"), mock.patch.object(
            guard, "_record_denial_with_significance"
        ):
            for _ in range(3):
                buffer = io.StringIO()
                with redirect_stdout(buffer):
                    code = guard._guarded_event(dict(event), "claude")
                self.assertEqual(2, code)
                payloads.append(json.loads(buffer.getvalue()))
        self.assertEqual(payloads[0], {"decision": "block", "reason": "blocked-delete"})
        self.assertNotIn("additionalContext", payloads[1])
        self.assertEqual(payloads[2]["decision"], "block")
        self.assertIn("advice, not a block", payloads[2]["additionalContext"])

    def test_checkpoint_forbids_an_automatic_retry(self) -> None:
        text = CHECKPOINT.read_text(encoding="utf-8")
        self.assertIn("unknown outcome, not automatic retry", text)
        self.assertIn("../hooks/repeat_reminder.py", text)
        for line in text.splitlines():
            if "automatically retry" in line.casefold():
                self.assertIn("do not", line.casefold(), line)
        routes = ROUTER.read_text(encoding="utf-8").split("HARNESS-ROUTES:START", 1)[1]
        self.assertNotIn("repeat_reminder", routes.split("HARNESS-ROUTES:END", 1)[0])
        self.assertNotIn("repeat_reminder", BUDGET.read_text(encoding="utf-8"))
        self.assertFalse((ROOT / "chaos-engine/skills/repeat-reminder").exists())


if __name__ == "__main__":
    unittest.main()
