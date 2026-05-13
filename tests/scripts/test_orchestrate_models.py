import sys
import tempfile
import unittest
from pathlib import Path

from scripts.ai.orchestrate_models import (
    AgentCandidate,
    OrchestrationError,
    build_local_agent_prompt,
    build_paid_agent_prompt,
    choose_candidate,
    choose_ollama_model,
    detect_paid_agents,
    make_slug,
    parse_ollama_list,
    prompt_for_task,
    redact_secrets,
    repository_context,
    run_command,
    write_markdown_artifact,
)


class OrchestrateModelsTests(unittest.TestCase):
    def test_make_slug_removes_punctuation_and_normalizes_dashes(self):
        self.assertEqual(make_slug("Fix flaky login test!!!"), "fix-flaky-login-test")

    def test_empty_task_input_is_rejected(self):
        with self.assertRaisesRegex(OrchestrationError, "cannot be empty"):
            prompt_for_task(lambda _: "   ")

    def test_detect_paid_agents_uses_mocked_path_lookup(self):
        paths = {"codex": "/usr/local/bin/codex", "claude": None}
        agents = detect_paid_agents(lambda command: paths[command])
        self.assertEqual(agents, [AgentCandidate("codex", "codex", "/usr/local/bin/codex")])

    def test_one_paid_agent_is_selected_automatically(self):
        agent = AgentCandidate("codex", "codex", "/bin/codex")
        self.assertEqual(choose_candidate([agent], "paid agent"), agent)

    def test_multiple_paid_agents_trigger_selection_path(self):
        agents = [
            AgentCandidate("codex", "codex", "/bin/codex"),
            AgentCandidate("claude", "claude", "/bin/claude"),
        ]
        self.assertEqual(choose_candidate(agents, "paid agent", input_func=lambda _: "2"), agents[1])

    def test_unknown_requested_paid_agent_fails_clearly(self):
        agents = [AgentCandidate("codex", "codex", "/bin/codex")]
        with self.assertRaisesRegex(OrchestrationError, "was not detected"):
            choose_candidate(agents, "paid agent", requested_name="claude")

    def test_non_interactive_multiple_paid_agents_requires_explicit_selection(self):
        agents = [
            AgentCandidate("codex", "codex", "/bin/codex"),
            AgentCandidate("claude", "claude", "/bin/claude"),
        ]
        with self.assertRaisesRegex(OrchestrationError, "pass an explicit selection"):
            choose_candidate(agents, "paid agent", non_interactive=True)

    def test_parse_ollama_list_extracts_model_names(self):
        output = """NAME                    ID              SIZE      MODIFIED
qwen3.5:9b              abc123          5.2 GB    2 days ago
llama3.1:8b             def456          4.7 GB    3 days ago
"""
        self.assertEqual(parse_ollama_list(output), ["qwen3.5:9b", "llama3.1:8b"])

    def test_one_ollama_model_is_selected_automatically(self):
        self.assertEqual(choose_ollama_model(["qwen3.5:9b"]), "qwen3.5:9b")

    def test_multiple_ollama_models_trigger_selection_path(self):
        self.assertEqual(
            choose_ollama_model(["qwen3.5:9b", "llama3.1:8b"], input_func=lambda _: "2"),
            "llama3.1:8b",
        )

    def test_non_interactive_multiple_ollama_models_requires_flag(self):
        with self.assertRaisesRegex(OrchestrationError, "pass --local-model"):
            choose_ollama_model(["qwen3.5:9b", "llama3.1:8b"], non_interactive=True)

    def test_secret_redaction_replaces_common_secret_patterns(self):
        raw = (
            "OPENAI_API_KEY=sk-abcdefghijklmnopqrstuvwxyz\n"
            "GH_TOKEN=ghp_abcdefghijklmnopqrstuvwxyz\n"
            "Authorization: Bearer abc123\n"
            "-----BEGIN PRIVATE KEY-----\nabc\n-----END PRIVATE KEY-----"
        )
        redacted = redact_secrets(raw)
        self.assertIn("OPENAI_API_KEY=<redacted>", redacted)
        self.assertIn("GH_TOKEN=<redacted>", redacted)
        self.assertIn("Authorization: Bearer <redacted>", redacted)
        self.assertIn("<redacted private key>", redacted)
        self.assertNotIn("abcdefghijklmnopqrstuvwxyz", redacted)

    def test_paid_agent_prompt_includes_original_user_request_exactly_once(self):
        request = "Add API retry tests"
        prompt = build_paid_agent_prompt(request, repository_context())
        self.assertEqual(prompt.count(request), 1)
        self.assertIn("<<<USER_REQUEST", prompt)
        self.assertIn("USER_REQUEST\n>>>", prompt)

    def test_local_agent_prompt_includes_smart_plan_content(self):
        prompt = build_local_agent_prompt("# Plan\nDo the thing")
        self.assertIn("# Plan\nDo the thing", prompt)
        self.assertIn("<<<PLAN", prompt)

    def test_non_zero_subprocess_result_is_captured(self):
        result = run_command([sys.executable, "-c", "import sys; sys.stderr.write(\"boom\\n\"); sys.exit(7)"])
        self.assertEqual(result.returncode, 7)
        self.assertIn("boom", result.stderr)

    def test_artifact_writer_creates_metadata_and_body(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "artifact.md"
            write_markdown_artifact(path, {"title": "Title", "Agent": "codex"}, "Body")
            content = path.read_text(encoding="utf-8")
        self.assertIn("# Title", content)
        self.assertIn("- Agent: codex", content)
        self.assertIn("Body", content)


if __name__ == "__main__":
    unittest.main()
