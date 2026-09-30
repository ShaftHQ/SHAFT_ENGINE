"""Issue #6299: the receipt adapter and a denied retry's fingerprints."""

from __future__ import annotations

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]


def _load_guard():
    path = ROOT / "chaos-engine/hooks/guard.py"
    spec = importlib.util.spec_from_file_location("ce_guard_6299", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ReflectionReceiptAdapterTest(unittest.TestCase):
    def test_same_controller_adapter_is_accepted_and_denial_names_only_the_hook(self) -> None:
        guard = _load_guard()
        previous = Path.cwd()
        os.chdir(ROOT)
        try:
            operation = guard.reflection_recovery(
                "py -3 scripts/agents/reflection.py receipt --session-id recovery"
            )
        finally:
            os.chdir(previous)
        self.assertEqual("receipt", operation)
        reason = guard.checkpoint_reason({"depth": "task", "failureFingerprints": ["abc"]})
        self.assertIn(".chaos-engine/hooks/reflection.py receipt", reason)
        self.assertNotIn("scripts/agents/reflection.py", reason)

    def test_denied_reflection_retry_does_not_append_a_fingerprint(self) -> None:
        guard = _load_guard()
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            session = "denied-receipt"
            for _ in range(3):
                guard.reflection.record_failure(
                    session,
                    phase="tool-outcome",
                    target="command-seed",
                    failure_class="tool-failure",
                    platform="linux",
                    attempted=True,
                )
            before = guard.reflection.pending_checkpoint(session)["failureFingerprints"]
            recorded = guard._record_failed_result(
                {
                    "hook_event_name": "PostToolUseFailure",
                    "tool_response": {"isError": True},
                    "tool_use_id": "denied-receipt-1",
                },
                "PostToolUseFailure",
                ("py -3 /tmp/not-the-controller/reflection.py receipt --session-id recovery",),
                "Bash",
                session,
            )
            self.assertFalse(recorded)
            after = guard.reflection.pending_checkpoint(session)["failureFingerprints"]
            self.assertEqual(before, after)


class OwnerCommitAttributionGuidanceTest(unittest.TestCase):
    def test_playbook_does_not_treat_owner_email_as_unattributed_without_a_reviewer(self) -> None:
        text = (ROOT / "chaos-engine/references/work-github-playbook.md").read_text(encoding="utf-8")
        self.assertIn("F79E3F65DB762CE1", text)
        self.assertIn("Mohab.MohieElDeen@outlook.com", text)
        self.assertIn("are not unattributed changes when no second reviewer exists", text)


if __name__ == "__main__":
    unittest.main()
