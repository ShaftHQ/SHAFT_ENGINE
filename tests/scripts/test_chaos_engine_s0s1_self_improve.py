"""Wave S0+S1 (#5653/#5654/#5655/#5656): metrics, silent-verify, CLI-over-MCP, ERL heuristics."""

from __future__ import annotations

import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CATALOG = ROOT / "chaos-engine/references/zero-llm-catalog.md"
SILENT_DOC = ROOT / "chaos-engine/references/silent-verify.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class S0S1SelfImproveTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.learning = load(ROOT / "chaos-engine/learning.py", "ce_learning_s0s1")
        cls.counters = load(
            ROOT / "chaos-engine/learning_counters.py", "ce_counters_s0s1"
        )
        cls.heuristics = load(ROOT / "chaos-engine/heuristics.py", "ce_heuristics_s0s1")
        cls.silent = load(ROOT / "chaos-engine/silent_verify.py", "ce_silent_s0s1")
        cls.lifecycle = load(
            ROOT / "chaos-engine/hooks/lifecycle.py", "ce_lifecycle_s0s1"
        )
        cls.learn_session = load(
            ROOT / "chaos-engine/learning_session.py", "ce_learn_session_s0s1"
        )
        cls.retrieve = load(ROOT / "chaos-engine/retrieve.py", "ce_retrieve_s0s1")
        cls.install = load(ROOT / "chaos-engine/install.py", "ce_install_s0s1")

    def test_learning_metrics_aggregates_queue_and_counters(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            state = project / ".chaos-engine-state" / "learning"
            candidate = {
                "category": "tooling",
                "title": "Prefer doctor fix-next for repair",
                "lesson": "Operators rediscovered repair flags from chat essays",
                "proposedChange": "Surface metrics in doctor json learningMetrics",
                "benefit": "Zero LLM closed loop for adoption rates",
                "estimatedTokens": 90,
            }
            queued = self.learning.queue_learning(state, candidate, "example/chaos-engine")
            self.assertEqual("queued", queued["status"])
            # Mark one submitted without network by writing queue
            document = self.learning.queue_document(state)
            document["items"][0]["status"] = "submitted"
            document["items"][0]["issueUrl"] = "https://github.com/example/chaos-engine/issues/42"
            self.learning.write_queue(state, document)
            self.counters.record_session_start_bytes(1200, project=project)
            self.counters.record_denial(project=project)
            self.counters.record_delivery_digest("deadbeef", project=project)
            # Learning session completion
            sess = project / ".chaos-engine-state" / "learning-session"
            sess.mkdir(parents=True, exist_ok=True)
            (sess / "s1.completion.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "kind": "learning-session-portable-finalize",
                        "disposition": "issues-first",
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            cwd = Path.cwd()
            try:
                os.chdir(project)
                metrics = self.learning.learning_metrics(state, project=project)
            finally:
                os.chdir(cwd)
            self.assertEqual(0, metrics["queued"])
            self.assertEqual(1, metrics["submitted"])
            self.assertEqual(1.0, metrics["submittedRate"])
            self.assertEqual(90, metrics["estimatedTokens"])
            self.assertEqual(1200, metrics["sessionStartBytesLast"])
            self.assertEqual(1, metrics["denials"])
            self.assertEqual(1, metrics["learningSessions"]["completions"])
            doctor = self.learning.doctor_learning_metrics(project)
            self.assertEqual("healthy", doctor["status"])
            self.assertEqual(1, doctor["submitted"])

    def test_silent_verify_success_is_quiet_failure_one_line(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            code = self.silent.silent_result(True, failure_message="should not print")
        self.assertEqual(0, code)
        self.assertEqual("", stdout.getvalue())
        self.assertEqual("", stderr.getvalue())
        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            code = self.silent.silent_result(False, failure_message="fix: run doctor --fix-next-only")
        self.assertEqual(2, code)
        self.assertEqual("", stdout.getvalue())
        self.assertEqual("fix: run doctor --fix-next-only\n", stderr.getvalue())

    def test_silent_session_start_budget_and_finalize(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            cwd = Path.cwd()
            try:
                os.chdir(project)
                stdout = io.StringIO()
                stderr = io.StringIO()
                with redirect_stdout(stdout), redirect_stderr(stderr):
                    code = self.silent.verify_session_start_budget(project)
                self.assertEqual(0, code, msg=stderr.getvalue())
                self.assertEqual("", stdout.getvalue())
                self.assertEqual("", stderr.getvalue())
                stdout = io.StringIO()
                stderr = io.StringIO()
                with redirect_stdout(stdout), redirect_stderr(stderr):
                    code = self.silent.verify_learning_session_finalize("s0s1-silent")
                self.assertEqual(0, code, msg=stderr.getvalue())
                self.assertEqual("", stdout.getvalue())
                # CLI --silent finalize
                completed = subprocess.run(  # nosec B603
                    [
                        sys.executable,
                        str(ROOT / "chaos-engine/learning_session.py"),
                        "finalize",
                        "--session-id",
                        "s0s1-cli-silent",
                        "--silent",
                        "--no-heuristics",
                    ],
                    cwd=project,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(0, completed.returncode, msg=completed.stderr)
                self.assertEqual("", completed.stdout)
            finally:
                os.chdir(cwd)

    def test_cli_over_mcp_iron_law_in_catalog_and_session_start(self):
        text = CATALOG.read_text(encoding="utf-8")
        self.assertIn("CLI-over-MCP iron law", text)
        self.assertIn("`gh`", text)
        self.assertIn("learning.py", text)
        self.assertIn("phase_ledger", text)
        self.assertTrue(SILENT_DOC.is_file())
        context = self.lifecycle.session_start_context("tok", "activation")
        self.assertIn("CLI-over-MCP", context)
        self.assertLessEqual(
            len(context.encode("utf-8")), self.lifecycle.SESSION_START_MAX_BYTES
        )

    def test_erl_heuristics_locator_only_and_retrieve_once(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            added = self.heuristics.add_heuristic(
                "Prefer silent verify on Check path",
                project=project,
                source="learning-session",
            )
            self.assertIn("id", added)
            locator = self.heuristics.session_start_locator(project)
            self.assertIn("heuristics", locator)
            self.assertNotIn("Prefer silent verify", locator)
            top = self.heuristics.retrieve_top(3, project)
            self.assertEqual(1, len(top))
            self.assertEqual("Prefer silent verify on Check path", top[0]["text"])
            receipt = self.retrieve.heuristics_retrieve(3, project)
            self.assertEqual("heuristics-retrieve", receipt["kind"])
            self.assertEqual("once-per-task", receipt["policy"])
            self.assertEqual("used", receipt["status"])
            # Privacy reject
            with self.assertRaises(ValueError):
                self.heuristics.add_heuristic(
                    "token=sk-abcdefghijklmnopqrstuvwxyz", project=project
                )
            # SessionStart still locator-only / under budget
            context = self.lifecycle.session_start_context("tok", "activation")
            self.assertIn("Heuristics:", context)
            self.assertNotIn("Prefer silent verify", context)
            self.assertLessEqual(
                len(context.encode("utf-8")), self.lifecycle.SESSION_START_MAX_BYTES
            )
            # Learning session extract
            state = project / ".chaos-engine-state" / "learning"
            self.learning.queue_learning(
                state,
                {
                    "category": "reliability",
                    "title": "Silent verify avoids green dump rot",
                    "lesson": "Green test dumps waste context on Stop path",
                    "proposedChange": "Wrap finalize with silent verify helper",
                    "benefit": "Lower token spend on healthy Check",
                    "estimatedTokens": 40,
                },
                "example/chaos-engine",
            )
            cwd = Path.cwd()
            try:
                os.chdir(project)
                result = self.learn_session.finalize("erl-extract", disposition="issues-first")
            finally:
                os.chdir(cwd)
            self.assertGreaterEqual(result.get("heuristicsExtracted", 0), 1)
            store = self.heuristics.load_index(project)
            self.assertGreaterEqual(len(store["items"]), 2)

    def test_doctor_diagnostic_fields_include_learning_metrics(self):
        fields = self.install._DIAGNOSTIC_FIELDS["doctor"]
        self.assertIn("learningMetrics", fields)
        self.assertIn("phaseLedger", fields)

    def test_learning_metrics_cli(self):
        completed = subprocess.run(  # nosec B603
            [sys.executable, str(ROOT / "chaos-engine/learning.py"), "metrics", "--project", str(ROOT)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, completed.returncode, msg=completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual("learning-metrics", payload["kind"])


if __name__ == "__main__":
    unittest.main()
