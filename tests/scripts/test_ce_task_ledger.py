"""Task ledger lets agent work resume after a host session cap (#6854)."""

from __future__ import annotations

import importlib.util
import io
import os
import subprocess  # nosec B404 - runs the tool CLI under test with fixed argv.
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load():
    spec = importlib.util.spec_from_file_location("ce_task_ledger_under_test", CE / "task_ledger.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class TaskLedgerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        patcher = mock.patch.dict(os.environ, {"CHAOS_ENGINE_TASKS_DIR": self.tmp.name})
        patcher.start()
        self.addCleanup(patcher.stop)
        self.ledger = load()

    def run_main(self, args, command="checkpoint"):
        out = io.StringIO()
        with redirect_stdout(out):
            code = self.ledger.main(args, command)
        return code, out.getvalue()

    def test_brief_merges_latest_fields_and_next_action(self):
        self.ledger.record("t1", "implement", branch="b", pr="12", start=True, now=1000)
        self.ledger.record("t1", "pushed", next_action="wait CI on #12", now=1100)
        text = self.ledger.brief("t1")
        self.assertIn("last step 'pushed'", text)
        self.assertIn("branch b; pr 12", text)
        self.assertTrue(text.endswith("next: wait CI on #12"))
        self.assertEqual(2, len(text.splitlines()))

    def test_budget_exit_codes(self):
        self.ledger.record("t2", "start", start=True, now=0)
        self.assertEqual((False, 10.0), self.ledger.over_budget("t2", 40, now=600))
        self.assertTrue(self.ledger.over_budget("t2", 40, now=41 * 60)[0])
        self.assertEqual(3, self.run_main(["t2", "--budget", "--minutes", "0"])[0])

    def test_new_session_start_resets_budget_and_counts_sessions(self):
        self.ledger.record("t3", "a", start=True, now=0)
        self.ledger.record("t3", "b", start=True, now=3000)
        self.assertFalse(self.ledger.over_budget("t3", 40, now=3060)[0])
        self.assertIn("session 2", self.ledger.brief("t3"))

    def test_resume_lists_only_open_tasks(self):
        self.ledger.record("open", "implement", now=1)
        self.ledger.record("closed", "done", now=1)
        self.assertEqual(["open"], self.ledger.open_tasks())
        self.assertIn("nothing to resume", self.ledger.brief("closed"))

    def test_absent_and_invalid_names(self):
        self.assertEqual(6, self.run_main(["missing"], "resume")[0])
        with mock.patch("sys.stderr", io.StringIO()):
            self.assertEqual(2, self.run_main(["../escape", "step"])[0])

    def test_tool_cli_routes_checkpoint_and_resume(self):
        env = {**os.environ, "CHAOS_ENGINE_TASKS_DIR": self.tmp.name}
        tool = [sys.executable, str(CE / "tool.py")]
        subprocess.run([*tool, "checkpoint", "cli", "implement", "--next", "push"], check=True, env=env)  # nosec B603
        result = subprocess.run([*tool, "resume", "cli"], capture_output=True, text=True, env=env, check=True)  # nosec B603
        self.assertIn("next: push", result.stdout)


if __name__ == "__main__":
    unittest.main()
