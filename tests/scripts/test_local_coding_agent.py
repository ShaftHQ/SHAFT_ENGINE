"""Contract tests for the workstation local coding-agent helpers (#5017 / #5060)."""

from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "local-coding-agent" / "agent.py"
SCHEMA = ROOT / "scripts" / "local-coding-agent" / "report.schema.json"
README = ROOT / "scripts" / "local-coding-agent" / "README.md"
RUN_AGENT = ROOT / "scripts" / "local-coding-agent" / "run_agent.ps1"
STOP = ROOT / "scripts" / "local-coding-agent" / "stop.ps1"

SPEC = importlib.util.spec_from_file_location("local_coding_agent", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("local coding-agent module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class LocalCodingAgentPreflightTest(unittest.TestCase):
    def test_missing_worktree_is_a_blocker(self):
        blockers = MODULE.preflight("", "spec.md", ["src/Example.java"])
        self.assertTrue(any("worktree" in item.lower() for item in blockers))

    def test_missing_spec_is_a_blocker(self):
        blockers = MODULE.preflight("C:/repo", "", ["src/Example.java"])
        self.assertTrue(any("spec" in item.lower() for item in blockers))

    def test_empty_allowlist_is_a_blocker(self):
        blockers = MODULE.preflight("C:/repo", "spec.md", [])
        self.assertTrue(any("allowlist" in item.lower() for item in blockers))

    def test_push_flag_is_a_blocker(self):
        blockers = MODULE.preflight("C:/repo", "spec.md", ["src/Example.java"], push=True)
        self.assertTrue(any("push" in item.lower() for item in blockers))

    def test_complete_preflight_has_no_blockers(self):
        blockers = MODULE.preflight("C:/repo", "spec.md", ["src/Example.java"])
        self.assertEqual([], blockers)


class LocalCodingAgentSurefireTest(unittest.TestCase):
    def test_shaft_failed_method_is_detected(self):
        output = (
            "Status: Failed\n"
            'Root cause: "java.lang.AssertionError: expected [SHAFT] but found [broken]"\n'
            "[ERROR] Tests run: 1, Failures: 1, Errors: 0, Skipped: 0\n"
        )
        self.assertTrue(MODULE.surefire_failed(output))

    def test_clean_run_is_not_failed(self):
        output = "[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0"
        self.assertFalse(MODULE.surefire_failed(output))


class LocalCodingAgentReportTest(unittest.TestCase):
    def valid_payload(self) -> dict:
        return MODULE.build_report(
            ok=True,
            model="qwen2.5-coder:7b",
            worktree="C:/repo",
            files_allowed=["src/Example.java"],
            files_changed=["src/Example.java"],
            commit="abc123",
            test_command="mvn.cmd -Dtest=ExampleTest test",
            test_exit=0,
            elapsed_ms=1200,
            loopback="127.0.0.1:11434",
            blockers=[],
        )

    def test_valid_report_has_no_blockers(self):
        self.assertEqual([], MODULE.validate_report(self.valid_payload()))

    def test_missing_key_is_a_blocker(self):
        payload = self.valid_payload()
        del payload["commit"]
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("commit" in item.lower() for item in blockers))

    def test_empty_allowlist_in_report_is_a_blocker(self):
        payload = self.valid_payload()
        payload["files_allowed"] = []
        payload["ok"] = False
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("allowlist" in item.lower() for item in blockers))

    def test_non_loopback_is_a_blocker(self):
        payload = self.valid_payload()
        payload["loopback"] = "0.0.0.0:11434"
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("loopback" in item.lower() or "127.0.0.1" in item for item in blockers))

    def test_changed_file_outside_allowlist_is_a_blocker(self):
        payload = self.valid_payload()
        payload["files_changed"] = ["src/Example.java", "README.md"]
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("allowlist" in item.lower() or "readme" in item.lower() for item in blockers))


class LocalCodingAgentPackagingTest(unittest.TestCase):
    def test_schema_lists_required_report_keys(self):
        schema = json.loads(SCHEMA.read_text(encoding="utf-8"))
        required = set(schema["required"])
        self.assertTrue(set(MODULE.REQUIRED_REPORT_KEYS) <= required)

    def test_runbook_names_roles_tooling_and_model(self):
        text = README.read_text(encoding="utf-8")
        for token in (
            "orchestrator",
            "qwen2.5-coder:7b",
            "Aider",
            "Ollama",
            "report.json",
            "127.0.0.1",
            "py -3",
            "mvn.cmd",
        ):
            self.assertIn(token, text)

    def test_wrappers_exist(self):
        self.assertTrue(RUN_AGENT.is_file())
        self.assertTrue(STOP.is_file())
        run_text = RUN_AGENT.read_text(encoding="utf-8")
        stop_text = STOP.read_text(encoding="utf-8")
        self.assertIn("allowlist", run_text.lower())
        self.assertIn("127.0.0.1", run_text)
        self.assertIn("pid", stop_text.lower())


if __name__ == "__main__":
    unittest.main()
