"""#6149: receipt at the third failure, one commit, then the fourth blocks."""

from __future__ import annotations

import io
import os
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch

from scripts.agents import guard
from scripts.agents import reflection


def _failure(session_id: str, command: str):
    return {
        "hook_event_name": "PostToolUse",
        "tool_name": "PowerShell",
        "tool_input": {"command": command},
        "tool_response": {"status": "failed", "exit_code": 1},
        "session_id": session_id,
        "cwd": ".",
    }


class ReflectionCommitCreditTest(unittest.TestCase):
    def test_two_failures_commit_after_receipt_and_fourth_failure(self):
        session = "commit-credit"
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            first = _failure(session, "py -3 -m unittest first")
            second = _failure(session, "py -3 -m unittest second")
            third = _failure(session, "py -3 -m unittest third")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(first)
                guard.run_posttooluse(second)
            self.assertIsNone(reflection.pending_checkpoint(session))
            third_output = io.StringIO()
            with redirect_stdout(third_output):
                guard.run_posttooluse(third)
            self.assertIn("reflection", third_output.getvalue().casefold())
            checkpoint = reflection.pending_checkpoint(session)
            self.assertEqual("third-fix", checkpoint["trigger"])
            combined = io.StringIO()
            with redirect_stdout(combined):
                guard.run_pretooluse(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "PowerShell",
                        "tool_input": {
                            "command": (
                                "py -3 scripts/agents/reflection.py receipt "
                                f"--session-id {session} --session-token t --json '{{}}' "
                                "&& git commit -m hide"
                            )
                        },
                        "session_id": session,
                        "cwd": ".",
                    },
                    "portable",
                )
            self.assertIn("Reflection required", combined.getvalue())
            token = reflection.record_session_start(session)
            reflection.record_receipt(
                session,
                {
                    "schemaVersion": 1,
                    "taskId": "issue-6149",
                    "trigger": checkpoint["trigger"],
                    "failureFingerprints": checkpoint["failureFingerprints"],
                    "failedAssumption": "Two failures were enough to stop.",
                    "approachesCompared": ["Stop at two", "Stop at three"],
                    "chosenExperiment": "Require a receipt on the third failure.",
                    "changedApproach": "Authorize one commit after that receipt.",
                    "proofCommandOrCheck": "focused reflection test",
                    "proofOutcome": "The third failure opened the checkpoint.",
                    "durableDisposition": "nothing-durable",
                },
                token,
            )
            self.assertIsNone(reflection.pending_checkpoint(session))
            commit_output = io.StringIO()
            with redirect_stdout(commit_output):
                code_path = guard.run_pretooluse(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "PowerShell",
                        "tool_input": {"command": "git commit -m fix"},
                        "session_id": session,
                        "cwd": ".",
                    },
                    "portable",
                )
            self.assertNotIn("Reflection required", commit_output.getvalue())
            self.assertEqual(0, code_path)
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(
                    {
                        "hook_event_name": "PostToolUse",
                        "tool_name": "PowerShell",
                        "tool_input": {"command": "git commit -m fix"},
                        "tool_response": {"status": "success", "exit_code": 0},
                        "session_id": session,
                        "cwd": ".",
                    }
                )
                guard.run_posttooluse(_failure(session, "py -3 -m unittest fourth"))
            self.assertIsNotNone(reflection.pending_checkpoint(session))
            blocked = io.StringIO()
            with redirect_stdout(blocked):
                guard.run_pretooluse(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "Write",
                        "tool_input": {"file_path": "scripts/agents/probe.py"},
                        "session_id": session,
                        "cwd": ".",
                    },
                    "portable",
                )
            self.assertIn("Reflection required", blocked.getvalue())

    def test_fourth_failure_before_the_credited_commit_blocks_the_next_mutation(self):
        session = "commit-credit-before-commit"
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            commands = (
                "py -3 -m unittest first",
                "py -3 -m unittest second",
                "py -3 -m unittest third",
            )
            with redirect_stdout(io.StringIO()):
                for command in commands:
                    guard.run_posttooluse(_failure(session, command))
            checkpoint = reflection.pending_checkpoint(session)
            self.assertIsNotNone(checkpoint)
            token = reflection.record_session_start(session)
            reflection.record_receipt(
                session,
                {
                    "schemaVersion": 1,
                    "taskId": "issue-6149",
                    "trigger": checkpoint["trigger"],
                    "failureFingerprints": checkpoint["failureFingerprints"],
                    "failedAssumption": "The receipt kept the next failure open.",
                    "approachesCompared": ["Wait for the commit", "Block the fourth failure immediately"],
                    "chosenExperiment": "Record a fourth failure before git commit.",
                    "changedApproach": "Reopen the checkpoint as soon as that failure is stored.",
                    "proofCommandOrCheck": "guard pretooluse after the fourth failure",
                    "proofOutcome": "The next mutation was blocked.",
                    "durableDisposition": "nothing-durable",
                },
                token,
            )
            self.assertIsNone(reflection.pending_checkpoint(session))
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(_failure(session, "py -3 -m unittest fourth"))
            self.assertIsNotNone(reflection.pending_checkpoint(session))
            blocked = io.StringIO()
            with redirect_stdout(blocked):
                guard.run_pretooluse(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "PowerShell",
                        "tool_input": {"command": "git commit -m fix"},
                        "session_id": session,
                        "cwd": ".",
                    },
                    "portable",
                )
            self.assertIn("Reflection required", blocked.getvalue())
            portable = _load_portable_guard()
            portable_output = io.StringIO()
            with redirect_stdout(portable_output):
                code = portable._run_event(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "Write",
                        "tool_input": {"file_path": "scripts/agents/probe.py"},
                        "session_id": session,
                        "cwd": ".",
                    },
                    "portable",
                )
            self.assertEqual(2, code)
            self.assertIn("Reflection required", portable_output.getvalue())


def _load_portable_guard():
    import importlib.util

    path = Path(__file__).resolve().parents[2] / "chaos-engine/hooks/guard.py"
    spec = importlib.util.spec_from_file_location("ce_guard_commit_credit", path)
    if spec is None or spec.loader is None:
        raise AssertionError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


if __name__ == "__main__":
    unittest.main()
