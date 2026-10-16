"""Task ledger lets agent work resume after a host session cap (#6854)."""

from __future__ import annotations

import importlib.util
import io
import os
import subprocess  # nosec B404 - runs the tool CLI under test with fixed argv.
import sys
import tempfile
import unittest
import unittest.mock
from contextlib import redirect_stdout
from pathlib import Path

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
        patcher = unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_TASKS_DIR": self.tmp.name})
        patcher.start()
        self.addCleanup(patcher.stop)
        self.ledger = load()
        specs = Path(self.tmp.name) / "specs"
        specs.mkdir()
        for task in ("cli", "all-open"):
            (specs / f"spec-{task}.md").write_text("# Spec\n", encoding="utf-8")

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

    def test_budget_accepts_minutes_inline_6990(self):
        # `checkpoint TASK --budget 40` used to fail with "unrecognized arguments: 40".
        self.ledger.record("t4", "start", start=True, now=0)
        code, out = self.run_main(["t4", "--budget", "40"])
        self.assertIn(code, (0, 3))
        self.assertIn("/40 min", out)

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
        with unittest.mock.patch("sys.stderr", io.StringIO()):
            self.assertEqual(2, self.run_main(["../escape", "step"])[0])

    def test_tool_cli_routes_checkpoint_and_resume(self):
        env = {**os.environ, "CHAOS_ENGINE_TASKS_DIR": self.tmp.name}
        tool = [sys.executable, str(CE / "tool.py")]
        subprocess.run([*tool, "checkpoint", "cli", "implement", "--next", "push"], check=True, env=env)  # nosec B603
        result = subprocess.run([*tool, "resume", "cli"], capture_output=True, text=True, env=env, check=True)  # nosec B603
        self.assertIn("next: push", result.stdout)


class DeliveryLedgerTest(TaskLedgerTest):
    """#6980: multi-item asks persist item -> group -> status."""

    def test_items_table_tracks_latest_status_and_resume_shows_it(self):
        self.run_main(["all-open", "plan", "--start"])
        self.run_main(["all-open", "--item", "#6978", "--group", "G1", "--status", "todo"])
        code, table = self.run_main(["all-open", "--item", "#4307", "--status", "blocked: upstream decision"])
        self.assertEqual(0, code)
        self.assertIn("| #4307 |  | blocked: upstream decision |", table)
        self.run_main(["all-open", "--item", "#6978", "--status", "pr #6979"])
        self.run_main(["all-open", "--item", "#6978", "--status", "merged"])
        code, brief = self.run_main(["all-open"], "resume")
        self.assertEqual(0, code)
        self.assertIn("| #6978 | G1 | merged |", brief)
        self.assertNotIn("pr #6979", brief)
        self.assertIn("next:", brief)
        self.assertEqual(["#6978", "#4307"], list(self.ledger.items("all-open")))

    def test_blocked_needs_reason_and_status_is_validated(self):
        code, _ = self.run_main(["t", "--item", "#1", "--status", "blocked"])
        self.assertEqual(2, code)
        code, _ = self.run_main(["t", "--item", "#1", "--status", "wip"])
        self.assertEqual(2, code)

    def test_core_guidance_requires_the_ledger_before_implementing(self):
        for relative in (
            "skills/chaos-engine/SKILL.md",
            "references/bot-entry.md",
            "references/orchestrator-follow-through.md",
        ):
            text = (CE / relative).read_text(encoding="utf-8")
            self.assertIn("delivery-ledger", text.replace("Delivery ledger", "delivery-ledger").casefold(), relative)
            self.assertIn("wip limit", text.casefold(), relative)
        self.assertIn("--item ID --group G --status todo", (CE / "references/bot-entry.md").read_text(encoding="utf-8").replace("\n", " "))




class SpecGateTest(TaskLedgerTest):
    """An in-flight checkpoint needs a spec on disk; done, --item and --budget stay exempt."""

    def test_in_flight_step_without_spec_is_refused(self):
        err = io.StringIO()
        with unittest.mock.patch("sys.stderr", err):
            code, _ = self.run_main(["nospec", "implement"])
        self.assertEqual(4, code)
        self.assertIn("nospec: no spec; write one before coding", err.getvalue())
        self.assertFalse(self.ledger.ledger("nospec").exists())

    def test_spec_flag_is_recorded_and_reused(self):
        spec = Path(self.tmp.name) / "elsewhere.md"
        spec.write_text("# Spec\nplan\n", encoding="utf-8")
        self.assertEqual(0, self.run_main(["flagged", "implement", "--spec", str(spec)])[0])
        self.assertEqual(0, self.run_main(["flagged", "pushed"])[0])
        self.assertEqual(str(spec.resolve()), self.ledger.entries("flagged")[0]["spec"])

    def test_default_spec_dir_and_exemptions(self):
        self.assertEqual(0, self.run_main(["all-open", "implement"])[0])
        self.assertEqual(0, self.run_main(["nospec2", "done"])[0])
        self.assertEqual(0, self.run_main(["nospec3", "--item", "#1", "--status", "todo"])[0])

    def test_empty_spec_file_does_not_count(self):
        empty = Path(self.tmp.name) / "empty.md"
        empty.write_text("", encoding="utf-8")
        with unittest.mock.patch("sys.stderr", io.StringIO()):
            self.assertEqual(4, self.run_main(["empty", "implement", "--spec", str(empty)])[0])


if __name__ == "__main__":
    unittest.main()
