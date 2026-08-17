"""Contract tests for the workstation local coding-agent helpers (#5017 / #5060)."""

from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "local-coding-agent" / "agent.py"
SCHEMA = ROOT / "scripts" / "local-coding-agent" / "report.schema.json"
RUN_AGENT = ROOT / "scripts" / "local-coding-agent" / "run_agent.ps1"
INSTALL = ROOT / "scripts" / "local-coding-agent" / "install.ps1"
STOP = ROOT / "scripts" / "local-coding-agent" / "stop.ps1"
JAVA_AGENT = ROOT / "scripts" / "local-coding-agent" / "shaft-java-agent.ps1"
ARCHITECT = ROOT / "scripts" / "local-coding-agent" / "shaft-architect.ps1"
STOP_CMD = ROOT / "scripts" / "local-coding-agent" / "shaft-local-ai-stop.ps1"

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

    def test_mixed_passing_then_failing_summary_is_failed(self):
        output = (
            "[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0\n"
            "Status: Failed\n"
            "[ERROR] Tests run: 1, Failures: 1, Errors: 0, Skipped: 0\n"
        )
        self.assertTrue(MODULE.surefire_failed(output))

    def test_zero_tests_run_is_failed(self):
        output = "[INFO] Tests run: 0, Failures: 0, Errors: 0, Skipped: 0"
        self.assertTrue(MODULE.surefire_failed(output))


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

    def test_git_status_ignores_aider_sidecars(self):
        status = (
            " M src/Example.java\n"
            "?? .aider.chat.history.md\n"
            "?? .aider.tags.cache.v4/cache.db\n"
        )
        changed = MODULE.changed_paths_from_git_status(status)
        self.assertEqual(["src/Example.java"], changed)

    def test_aider_substring_in_real_filename_is_kept(self):
        status = " M docs/using.aider.md\n"
        changed = MODULE.changed_paths_from_git_status(status)
        self.assertEqual(["docs/using.aider.md"], changed)

    def test_git_status_without_pathspec_finds_extra_files(self):
        status = (
            " M shaft-engine/src/test/java/testPackage/LocalCodingAgentAcceptanceTest.java\n"
            " M shaft-engine/src/main/java/com/shaft/driver/SHAFT.java\n"
        )
        allowed = [
            "shaft-engine/src/test/java/testPackage/LocalCodingAgentAcceptanceTest.java"
        ]
        changed = MODULE.changed_paths_from_git_status(status)
        blockers = MODULE.allowlist_violations(changed, allowed)
        self.assertTrue(any("shaft.java" in item.lower() for item in blockers))

    def test_loopback_rejects_adjacent_address(self):
        payload = self.valid_payload()
        payload["loopback"] = "127.0.0.10:11434"
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("127.0.0.1" in item for item in blockers))

    def test_powershell_unwrapped_changed_string_is_still_checked(self):
        payload = self.valid_payload()
        payload["files_allowed"] = ["src/Example.java", "src/Other.java"]
        payload["files_changed"] = "src/SHAFT.java"
        blockers = MODULE.validate_report(payload)
        self.assertTrue(any("shaft.java" in item.lower() for item in blockers))

    def test_powershell_unwrapped_single_allowlist_string_is_accepted(self):
        payload = self.valid_payload()
        payload["files_allowed"] = "src/Example.java"
        payload["files_changed"] = "src/Example.java"
        self.assertEqual([], MODULE.validate_report(payload))

    def test_write_report_from_porcelain_and_unwrapped_json_fails_sibling(self):
        import tempfile

        status = (
            " M src/Example.java\n"
            " M src/SHAFT.java\n"
        )
        changed = MODULE.changed_paths_from_git_status(status)
        payload = self.valid_payload()
        payload["ok"] = True
        payload["files_allowed"] = "src/Example.java"
        payload["files_changed"] = changed
        report = Path(tempfile.mkdtemp()) / "report.json"
        extra = MODULE.write_report(report, payload)
        saved = json.loads(report.read_text(encoding="utf-8"))
        self.assertTrue(any("shaft.java" in item.lower() for item in extra))
        self.assertIsInstance(saved["files_allowed"], list)
        self.assertIsInstance(saved["files_changed"], list)
        self.assertGreaterEqual(len(saved["files_changed"]), 2)
        self.assertFalse(saved["ok"])


class LocalCodingAgentPackagingTest(unittest.TestCase):
    def test_schema_lists_required_report_keys(self):
        schema = json.loads(SCHEMA.read_text(encoding="utf-8"))
        required = set(schema["required"])
        self.assertTrue(set(MODULE.REQUIRED_REPORT_KEYS) <= required)

    def test_wrappers_name_roles_tooling_and_model(self):
        text = "\n".join(
            path.read_text(encoding="utf-8")
            for path in (RUN_AGENT, INSTALL, STOP)
        )
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
        self.assertFalse((ROOT / "scripts" / "local-coding-agent" / "README.md").exists())
        self.assertFalse((ROOT / "scripts" / "local-coding-agent" / "HANDOFF.md").exists())

    def test_wrappers_exist(self):
        self.assertTrue(RUN_AGENT.is_file())
        self.assertTrue(INSTALL.is_file())
        self.assertTrue(STOP.is_file())
        run_text = RUN_AGENT.read_text(encoding="utf-8")
        stop_text = STOP.read_text(encoding="utf-8")
        self.assertIn("allowlist", run_text.lower())
        self.assertIn("127.0.0.1", run_text)
        self.assertIn("--no-suggest-shell-commands", run_text)
        self.assertIn("--no-gitignore", run_text)
        self.assertIn("aider-chat.md", run_text)
        self.assertIn("git status --porcelain", run_text)
        self.assertNotIn("git status --porcelain --", run_text)
        self.assertIn("git diff --name-only", run_text)
        self.assertIn("RedirectStandardOutput = $false", run_text)
        self.assertIn("pid", stop_text.lower())
        self.assertIn("refusing to stop an unproven process", stop_text)
        install_text = INSTALL.read_text(encoding="utf-8")
        self.assertIn("release asset digest missing", install_text)
        self.assertIn('$agentPy "write"', run_text)
        self.assertIn('$agentPy "changed"', run_text)
        self.assertIn('Filter ".aider*"', run_text)

    def test_named_commands_exist(self):
        self.assertTrue(JAVA_AGENT.is_file())
        self.assertTrue(ARCHITECT.is_file())
        self.assertTrue(STOP_CMD.is_file())
        java_text = JAVA_AGENT.read_text(encoding="utf-8")
        architect_text = ARCHITECT.read_text(encoding="utf-8")
        stop_text = STOP_CMD.read_text(encoding="utf-8")
        self.assertIn("run_agent.ps1", java_text)
        self.assertIn("stop.ps1", stop_text)
        self.assertIn("--dry-run", architect_text)
        self.assertIn("--no-auto-commits", architect_text)
        self.assertIn("--no-gitignore", architect_text)
        self.assertIn("architect-chat.md", architect_text)
        self.assertIn('Filter ".aider*"', architect_text)
        self.assertIn("push is forbidden", architect_text)
        self.assertIn("read-only contract failed", architect_text)
        self.assertIn("yyyyMMddTHHmmssZ", architect_text)
        self.assertIn("$Read", architect_text)
        self.assertIn('"cited"', architect_text)
        self.assertIn("--text-file", architect_text)
        self.assertIn("PositionalBinding = $false", architect_text)

    def test_launchers_downgrade_worktree_index_to_version_2(self):
        for path in (RUN_AGENT, ARCHITECT):
            with self.subTest(path.name):
                text = path.read_text(encoding="utf-8")
                self.assertIn("update-index", text)
                self.assertIn("--index-version", text)
                self.assertIn("--index-version 2", text)


class LocalCodingAgentCitedPathTest(unittest.TestCase):
    HALLUCINATED = "shaft-ai/src/main/java/com/shaft/ai/ollama/OllamaClient.java"
    EXISTING = "scripts/local-coding-agent/run_agent.ps1"
    AGENTS_MD_ONLY_BARE_NAMES = (
        "> Added AGENTS.md to the chat (read-only).\n"
        "See shaft-architect.ps1 and OllamaProvider.java.\n"
    )

    def test_recorded_ollama_client_hallucination_is_missing(self):
        text = (
            "The launcher lives in scripts/local-coding-agent/run_agent.ps1 "
            "and the Java client is shaft-ai/src/main/java/com/shaft/ai/ollama/OllamaClient.java."
        )
        missing = MODULE.missing_cited_paths(text, ROOT)
        self.assertIn(self.HALLUCINATED, missing)

    def test_existing_cited_path_is_kept(self):
        text = f"See {self.EXISTING} for the workstation loop."
        self.assertIn(self.EXISTING, MODULE.cited_repo_paths(text))
        self.assertEqual([], MODULE.missing_cited_paths(text, ROOT))

    def test_https_url_is_ignored(self):
        text = "Docs: https://example.com/shaft-ai/src/main/java/Foo.java and https://github.com/a/b.py"
        self.assertEqual([], MODULE.cited_repo_paths(text))

    def test_bare_class_name_is_ignored(self):
        text = "OllamaClient and OllamaProvider are class names, not repo paths."
        self.assertEqual([], MODULE.cited_repo_paths(text))

    def test_agents_md_only_history_with_bare_real_names_is_ok(self):
        import tempfile

        transcript = Path(tempfile.mkdtemp()) / "history.md"
        transcript.write_text(self.AGENTS_MD_ONLY_BARE_NAMES, encoding="utf-8")
        code = MODULE.main(["cited", "--text-file", str(transcript), "--root", str(ROOT)])
        self.assertEqual(0, code)

    def test_agents_md_only_history_still_blocks_invented_slashy_path(self):
        import tempfile

        transcript = Path(tempfile.mkdtemp()) / "history.md"
        transcript.write_text(
            self.AGENTS_MD_ONLY_BARE_NAMES + f"The Java client is {self.HALLUCINATED}.\n",
            encoding="utf-8",
        )
        code = MODULE.main(["cited", "--text-file", str(transcript), "--root", str(ROOT)])
        self.assertEqual(2, code)

    def test_cited_cli_accepts_real_paths_only(self):
        import tempfile

        transcript = Path(tempfile.mkdtemp()) / "history.md"
        transcript.write_text(f"Read {self.EXISTING} and AGENTS.md stay out.\n", encoding="utf-8")
        code = MODULE.main(["cited", "--text-file", str(transcript), "--root", str(ROOT)])
        self.assertEqual(0, code)

    def test_windows_absolute_paths_are_not_repo_relative(self):
        text = (
            "--chat-history-file D:\\AI\\reports\\20260817T195001Z\\architect-chat.md "
            "--read C:\\Users\\Mohab\\.grok\\worktrees\\shaft-engine-5065\\"
            "scripts\\local-coding-agent\\run_agent.ps1"
        )
        cited = MODULE.cited_repo_paths(text)
        self.assertNotIn("AI/reports/20260817T195001Z/architect-chat.md", cited)
        self.assertFalse(any(item.startswith("Users/") for item in cited))
        self.assertFalse(any(item.startswith("AI/") for item in cited))

    def test_ollama_model_id_is_not_a_cited_path(self):
        text = "Model: ollama_chat/qwen2.5-coder:7b with whole edit format"
        self.assertEqual([], MODULE.cited_repo_paths(text))

    def test_aider_history_header_plus_real_added_files_is_ok(self):
        import tempfile

        text = (
            "D:\\AI\\aider\\.venv\\Scripts\\aider --model ollama_chat/qwen2.5-coder:7b "
            "--chat-history-file D:\\AI\\reports\\20260817T195001Z\\architect-chat.md "
            "--read C:\\repo\\scripts\\local-coding-agent\\run_agent.ps1\n"
            "> Added scripts\\local-coding-agent\\run_agent.ps1 to the chat (read-only).\n"
            "> Added scripts\\local-coding-agent\\shaft-architect.ps1 to the chat (read-only).\n"
            "See shaft-architect.ps1 and OllamaProvider.java.\n"
        )
        transcript = Path(tempfile.mkdtemp()) / "history.md"
        transcript.write_text(text, encoding="utf-8")
        code = MODULE.main(["cited", "--text-file", str(transcript), "--root", str(ROOT)])
        self.assertEqual(0, code)

    def test_aider_history_still_blocks_invented_repo_path(self):
        import tempfile

        text = (
            "> Added scripts\\local-coding-agent\\run_agent.ps1 to the chat (read-only).\n"
            f"The Java client is {self.HALLUCINATED}.\n"
        )
        transcript = Path(tempfile.mkdtemp()) / "history.md"
        transcript.write_text(text, encoding="utf-8")
        code = MODULE.main(["cited", "--text-file", str(transcript), "--root", str(ROOT)])
        self.assertEqual(2, code)


if __name__ == "__main__":
    unittest.main()
