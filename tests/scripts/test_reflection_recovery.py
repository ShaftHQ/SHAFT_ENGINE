"""#6978: actionable reflection recovery and no spurious long-session blocks."""

from __future__ import annotations

import io
import os
import shlex
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from datetime import UTC, datetime, timedelta
from unittest.mock import patch

from scripts.agents import reflection


def _fail(session: str, target: str) -> None:
    reflection.record_failure(session, phase="tool-outcome", target=target, failure_class="tool-failure")


class ReflectionRecoveryTest(unittest.TestCase):
    def setUp(self) -> None:
        self._dir = tempfile.TemporaryDirectory()
        self._env = patch.dict(os.environ, {"TMPDIR": self._dir.name, "TEMP": self._dir.name})
        self._env.start()
        self.addCleanup(self._dir.cleanup)
        self.addCleanup(self._env.stop)

    def test_spread_out_unrelated_failures_do_not_block_a_long_session(self) -> None:
        session = "long-goal"
        reflection.record_session_start(session)
        for target in ("a", "b", "c"):
            _fail(session, target)
        later = datetime.now(UTC) + timedelta(hours=2)
        self.assertIsNone(reflection.pending_checkpoint(session, now=later))
        self.assertEqual("third-fix", reflection.pending_checkpoint(session)["trigger"])

    def test_repeated_fingerprint_still_blocks_after_the_window(self) -> None:
        session = "stuck-loop"
        reflection.record_session_start(session)
        for _ in range(3):
            _fail(session, "same")
        _fail(session, "other")
        later = datetime.now(UTC) + timedelta(hours=2)
        checkpoint = reflection.pending_checkpoint(session, now=later)
        self.assertEqual("repeated-fingerprint", checkpoint["trigger"])

    def test_message_command_records_receipt_without_token_or_ledger_digging(self) -> None:
        session = "root:agent:abc123"
        reflection.record_session_start(session)
        for target in ("a", "b", "c"):
            _fail(session, target)
        checkpoint = reflection.pending_checkpoint(session)
        message = reflection.recovery_message("ctl/reflection.py", session, checkpoint)
        self.assertTrue(message.startswith("Reflection required"))
        self.assertLess(message.index("receipt --session-id"), 200)
        command = message.split("Fill in and run: ", 1)[1].split(" . Then retry", 1)[0]
        argv = shlex.split(command)[2:]
        filled = [
            {
                "<what was wrong>": "flaky check was the cause",
                "<approach not taken>": "rerun unchanged",
                "<what you will do differently>": "pin the fixture",
                "<check you ran>": "focused unittest",
                "<its result>": "passed",
            }.get(item, item)
            for item in argv
        ]
        with redirect_stdout(io.StringIO()):
            self.assertEqual(0, reflection.main(filled))
        self.assertIsNone(reflection.pending_checkpoint(session))

    def test_quick_receipt_names_missing_flags(self) -> None:
        session = "missing"
        reflection.record_session_start(session)
        for target in ("a", "b", "c"):
            _fail(session, target)
        errors = io.StringIO()
        with redirect_stderr(errors), self.assertRaises(SystemExit):
            reflection.main(["receipt", "--session-id", session, "--assumption", "x"])
        self.assertIn("--outcome", errors.getvalue())


if __name__ == "__main__":
    unittest.main()
