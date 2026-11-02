"""SessionStart locator-only / progressive disclosure parity (#5580)."""

from __future__ import annotations

import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
import uuid
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


def load_module(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {relative}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def unwrap_context(payload: dict) -> str:
    if "additionalContext" in payload:
        return str(payload["additionalContext"])
    specific = payload.get("hookSpecificOutput")
    if isinstance(specific, dict) and "additionalContext" in specific:
        return str(specific["additionalContext"])
    raise AssertionError(f"no additionalContext in {payload!r}")


class SessionStartLocatorParityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.kernel = load_module("ce_kernel_ss", "chaos-engine/hooks/kernel.py")
        cls.lifecycle = load_module("ce_lifecycle_ss", "chaos-engine/hooks/lifecycle.py")
        cls.caveman = (
            ROOT / "chaos-engine/vendor/caveman/skills/caveman/SKILL.md"
        ).read_text(encoding="utf-8")
        cls.ponytail = (
            ROOT / "chaos-engine/vendor/ponytail/skills/ponytail/SKILL.md"
        ).read_text(encoding="utf-8")

    def test_builder_stays_under_byte_budget_and_omits_skill_bodies(self):
        context = self.lifecycle.session_start_context("token-fixture", "activation")
        encoded = context.encode("utf-8")
        self.assertLessEqual(len(encoded), self.lifecycle.SESSION_START_MAX_BYTES)
        # Soft token smoke (~4 chars/token): keep well under a 1k-token dump.
        self.assertLessEqual(len(encoded) // 4, 512)
        self.assertNotIn(self.caveman, context)
        self.assertNotIn(self.ponytail, context)
        for name in ("caveman", "ponytail"):
            self.assertIn(
                f"chaos-engine/companions/{name}-ultra.md", context
            )
        self.assertIn("caveman=ultra; ponytail=ultra", context)

    def test_session_start_omits_a_stopped_companion(self):
        previous = Path.cwd()
        with tempfile.TemporaryDirectory() as temporary:
            os.chdir(temporary)
            try:
                self.lifecycle.record_companion_opt_out("stop-session", "please stop caveman")
                context = self.lifecycle.session_start_context(
                    "token-fixture", "activation", session_id="stop-session"
                )
                self.assertNotIn("caveman-ultra.md", context)
                self.assertIn("ponytail-ultra.md", context)
                self.assertNotIn(self.caveman, context)
                self.lifecycle.record_companion_opt_out("stop-session", "normal mode")
                both = self.lifecycle.session_start_context(
                    "token-fixture", "activation", session_id="stop-session"
                )
                self.assertNotIn("caveman-ultra.md", both)
                self.assertNotIn("ponytail-ultra.md", both)
            finally:
                os.chdir(previous)

    def test_every_host_adapts_identical_locator_context(self):
        token = "parity-session-token"
        activation = "ChaosEngine activation"
        base_context = self.lifecycle.session_start_context(token, activation)

        def session_callback(_event, _host):
            print(json.dumps({"additionalContext": base_context}))
            return 0

        contexts: dict[str, str] = {}
        for host in sorted(self.kernel.HOST_CAPABILITIES):
            stdout = io.StringIO()
            stderr = io.StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = self.lifecycle.run_hook_protocol(
                    json.dumps(
                        {
                            "hook_event_name": "SessionStart",
                            "session_id": "ss-parity",
                        }
                    ),
                    {"SessionStart": session_callback},
                    normalize=self.kernel.normalize_hook_input,
                    host_for_input=lambda _raw, selected=host: selected,
                    adapt_output=self.kernel.adapt_hook_output,
                )
            self.assertEqual(0, code)
            rendered = stdout.getvalue().strip()
            self.assertTrue(rendered, msg=f"empty SessionStart payload host={host}")
            payload = json.loads(rendered.splitlines()[-1])
            context = unwrap_context(payload)
            encoded = json.dumps(payload).encode("utf-8")
            self.assertLessEqual(
                len(encoded),
                self.lifecycle.SESSION_START_MAX_BYTES,
                msg=f"host={host} over budget",
            )
            self.assertNotIn(self.caveman, context)
            self.assertNotIn(self.ponytail, context)
            contexts[host] = context

        # Progressive disclosure contract: every host gets the same locators.
        unique = set(contexts.values())
        self.assertEqual(1, len(unique), msg=contexts)

    def test_subagent_start_uses_the_session_start_channel(self):
        def callback(event, _host):
            self.assertEqual("SessionStart", event["hook_event_name"])
            print(json.dumps({"additionalContext": "caveman=ultra; ponytail=ultra"}))
            return 0

        cases = (
            ("grok", "SubagentStart"),
            ("claude", "SubagentStart"),
            ("codex", "SubagentStart"),
            ("gemini", "SubagentStart"),
            ("copilot", "subagentStart"),
        )
        for host, native in cases:
            stdout = io.StringIO()
            previous = os.environ.get("CHAOS_ENGINE_HOST")
            os.environ["CHAOS_ENGINE_HOST"] = host
            try:
                with redirect_stdout(stdout):
                    code = self.lifecycle.run_hook_protocol(
                        json.dumps({"hook_event_name": native, "session_id": f"sub-{host}"}),
                        {"SessionStart": callback},
                        normalize=self.kernel.normalize_hook_input,
                        host_for_input=lambda _raw, selected=host: selected,
                        adapt_output=self.kernel.adapt_hook_output,
                    )
            finally:
                if previous is None:
                    os.environ.pop("CHAOS_ENGINE_HOST", None)
                else:
                    os.environ["CHAOS_ENGINE_HOST"] = previous
            self.assertEqual(0, code, host)
            payload = json.loads(stdout.getvalue().strip().splitlines()[-1])
            context = unwrap_context(payload)
            self.assertIn("caveman=ultra; ponytail=ultra", context)
            if host == "grok":
                self.assertEqual(payload["systemMessage"], context)

    def test_guard_injects_entry_for_a_grok_subagent_and_once_for_prompt_hosts(self):
        guard = ROOT / "chaos-engine/hooks/guard.py"

        def run(host, payload):
            env = dict(os.environ)
            env["CHAOS_ENGINE_HOST"] = host
            return subprocess.run(
                [sys.executable, str(guard)],
                input=json.dumps(payload),
                text=True,
                capture_output=True,
                env=env,
                cwd=ROOT,
                check=False,
            )

        grok = run("grok", {
            "hook_event_name": "SubagentStart",
            "session_id": f"entry-probe-grok-{uuid.uuid4().hex}",
            "agent_id": "delegate",
        })
        self.assertEqual(0, grok.returncode, grok.stderr)
        grok_payload = json.loads(grok.stdout)
        self.assertIn("caveman=ultra", grok_payload["systemMessage"])
        self.assertIn("ponytail=ultra", grok_payload["systemMessage"])

        for host, event in (("gemini", "BeforeAgent"), ("copilot", "userPromptSubmitted")):
            payload = {
                "hook_event_name": event,
                "session_id": f"entry-probe-{host}-{uuid.uuid4().hex}",
                "prompt": "go",
            }
            first = run(host, payload)
            second = run(host, payload)
            self.assertEqual(0, first.returncode, first.stderr)
            self.assertIn("caveman=ultra", unwrap_context(json.loads(first.stdout)))
            self.assertNotIn("caveman=ultra", second.stdout)

    def test_grok_rules_carry_the_companion_load_outside_agents(self):
        hosts = load_module("ce_hosts_entry_rule", "chaos-engine/hosts.py")
        block = hosts.instruction_block()
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        rules = (ROOT / ".grok/rules/companions.md").read_text(encoding="utf-8")
        self.assertNotIn("caveman=ultra", block)
        self.assertNotIn("caveman=ultra", agents)
        self.assertIn("caveman=ultra", rules)
        self.assertIn("ponytail=ultra", rules)
        self.assertIn("iterative-flow.md", rules)
        self.assertIn("python3 .chaos-engine/tool.py entry", rules)
        self.assertIn("python3 .chaos-engine/tool.py retrieve", rules)
        identity = (ROOT / ".grok/rules/identity.md").read_text(encoding="utf-8")
        entrypoint = (ROOT / ".grok/rules/entrypoint.md").read_text(encoding="utf-8")
        self.assertIn("Speak unabridged truth", identity)
        self.assertIn("## Iron laws", entrypoint)
        self.assertIn("chaos-engine/skills/<name>/SKILL.md", rules)
        self.assertIn("chaos-engine/references/roles.md", rules)
        self.assertIn("chaos-engine/references/execution-workflows.md", rules)
        self.assertIn("SOLO", rules)
        self.assertIn("headed", rules)
        self.assertFalse((ROOT / ".grok/skills/kanban/SKILL.md").exists())

    def test_matrix_sessionstart_row_is_green(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertIn(
            "| SessionStart locator-only / progressive disclosure | A | A | A | A | A |",
            text,
        )
        self.assertIn("test_chaos_engine_sessionstart_locator_parity.py", text)
        self.assertIn("SESSION_START_MAX_BYTES", text)


if __name__ == "__main__":
    unittest.main()
