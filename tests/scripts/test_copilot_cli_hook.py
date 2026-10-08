"""Copilot CLI repo-settings hook must not fail closed on a PowerShell parse error.

The reported session denied every tool with
`Denied by preToolUse hook from "repo settings" (hook errored)`.
Copilot CLI executes `.claude/settings.json` commands through PowerShell on
Windows (github/copilot-cli#4001). A bash-quoted ``python3 -c`` exits 1, and
preToolUse fail-closes. These tests drive the shipped launcher and the
generated settings command.
"""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - fixed repository hook entry.
import tempfile
import unittest
import uuid
from pathlib import Path


def _load_hosts():
    path = ROOT / "chaos-engine/hosts.py"
    specification = importlib.util.spec_from_file_location("chaos_engine_hosts_copilot_hook", path)
    if specification is None or specification.loader is None:
        raise RuntimeError("hosts module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


ROOT = Path(__file__).resolve().parents[2]
HOSTS = _load_hosts()
LAUNCH = ROOT / "chaos-engine/hooks/launch.js"
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
SHELL_GUIDANCE = (
    "Get-Content -Raw '.chaos-engine/skills/chaos-engine/SKILL.md'; "
    "Write-Output \"`n---IDENTITY---\"; "
    "Get-Content -Raw '.chaos-engine/identity.md'; "
    "Write-Output \"`n---AGENTS---\"; "
    "Get-Content -Raw 'AGENTS.md'"
)


def _env(temporary: str) -> dict[str, str]:
    protected = [
        item
        for item in os.environ.get("CHAOS_ENGINE_PROTECTED_CHECKOUTS", "").split(os.pathsep)
        if item
    ]
    protected.append(str(ROOT))
    return {
        **os.environ,
        "CHAOS_ENGINE_PROTECTED_CHECKOUTS": os.pathsep.join(protected),
        "CHAOS_ENGINE_STORE_REFRESH": "0",
        "TMPDIR": temporary,
        "TEMP": temporary,
    }


class CopilotCliHookTest(unittest.TestCase):
    def setUp(self):
        node = shutil.which("node")
        if not node:
            self.skipTest("node is required to drive the Copilot launcher")
        self.node = node

    def _launch(
        self, event: dict[str, object], temporary: str, session: str = "copilot-repro"
    ) -> subprocess.CompletedProcess[str]:
        payload = {"sessionId": session, "cwd": str(ROOT), **event}
        return subprocess.run(  # nosec B603 - fixed node launcher.
            [self.node, str(LAUNCH), "copilot", "preToolUse"],
            input=json.dumps(payload),
            capture_output=True,
            text=True,
            check=False,
            cwd=ROOT,
            env=_env(temporary),
        )

    def _decision(self, result: subprocess.CompletedProcess[str]) -> dict[str, object]:
        self.assertEqual("", result.stderr, result.stderr)
        self.assertTrue(result.stdout.strip(), result.stdout)
        payload = json.loads(result.stdout)
        self.assertIsInstance(payload, dict)
        return payload

    def test_settings_command_is_powershell_safe_and_allows_a_read(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        self.assertIsNone(HOSTS.powershell_hook_parse_error(command))
        self.assertIn(".chaos-engine/hooks/guard.py", command)
        self.assertIn("repository working directory unavailable", command)
        self.assertTrue(command.startswith("python3 -c '"))
        self.assertNotIn('\\"', command.split("'", 1)[0])
        with tempfile.TemporaryDirectory() as temporary:
            result = subprocess.run(  # nosec B602 - generated hook command.
                command,
                shell=True,
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "sessionId": "copilot-repro",
                        "cwd": str(ROOT),
                        "toolName": "view",
                        "toolArgs": json.dumps(
                            {"path": ".chaos-engine/skills/chaos-engine/SKILL.md"}
                        ),
                    }
                ),
                capture_output=True,
                text=True,
                check=False,
                cwd=ROOT,
                env=_env(temporary),
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        payload = json.loads(result.stdout or "{}")
        self.assertNotEqual("deny", payload.get("permissionDecision"))
        self.assertNotIn("hook errored", result.stdout + result.stderr)

    def test_screenshot_sequence_returns_kernel_decisions(self):
        shell = {
            "toolName": "powershell",
            "toolArgs": json.dumps({"command": SHELL_GUIDANCE}),
        }
        reads = [
            {"toolName": "view", "toolArgs": json.dumps({"path": relative})}
            for relative in (
                ".chaos-engine/skills/chaos-engine/SKILL.md",
                ".chaos-engine/identity.md",
                "AGENTS.md",
            )
        ]
        searches = [
            {"toolName": "search", "toolArgs": json.dumps({"query": "**/*"})},
            {"toolName": "grep", "toolArgs": json.dumps({"pattern": "pom.xml"})},
        ]
        task = {
            "toolName": "task",
            "toolArgs": json.dumps(
                {
                    "description": "Implement login smoke test",
                    "agent": "chaos-engine-implementer",
                    "prompt": "Implement login smoke test",
                }
            ),
        }
        phase = {
            "toolName": "edit",
            "toolArgs": json.dumps({"filePath": "src/Foo.java"}),
            "targetPhase": "Check",
        }
        with tempfile.TemporaryDirectory() as temporary:
            for event in (shell, *reads, searches[1], task):
                result = self._launch(event, temporary)
                with self.subTest(tool=event["toolName"], path=event["toolArgs"]):
                    self.assertEqual(0, result.returncode, result.stdout + result.stderr)
                    payload = self._decision(result)
                    self.assertNotEqual("deny", payload.get("permissionDecision"))
            denied = self._launch(phase, temporary)
        with tempfile.TemporaryDirectory() as broad_temporary:
            broad = self._launch(searches[0], broad_temporary, session=f"copilot-broad-{uuid.uuid4().hex}")
        self.assertEqual(0, broad.returncode, broad.stdout + broad.stderr)
        broad_payload = self._decision(broad)
        self.assertIn("retrieve --store graphify", str(broad_payload.get("additionalContext")))
        self.assertNotEqual("deny", broad_payload.get("permissionDecision"))
        self.assertEqual(2, denied.returncode, denied.stdout + denied.stderr)
        decision = self._decision(denied)
        self.assertEqual("deny", decision.get("permissionDecision"))
        self.assertIn(
            "Lifecycle transition ReadOnly to Check is not declared.",
            str(decision.get("permissionDecisionReason")),
        )
        self.assertNotIn("hook errored", denied.stdout + denied.stderr)

    def test_setup_loads_router_and_teardown_closes_owed_retrieve(self):
        session = f"copilot-setup-{uuid.uuid4().hex}"
        with tempfile.TemporaryDirectory() as temporary:
            started = subprocess.run(  # nosec B603 - fixed node launcher.
                [self.node, str(LAUNCH), "copilot", "sessionStart"],
                input=json.dumps({"sessionId": session, "cwd": str(ROOT)}),
                capture_output=True,
                text=True,
                check=False,
                cwd=ROOT,
                env=_env(temporary),
            )
            self.assertEqual(0, started.returncode, started.stdout + started.stderr)
            start_payload = self._decision(started)
            context = str(start_payload.get("additionalContext"))
            self.assertIn(".chaos-engine/skills/chaos-engine/SKILL.md", context)
            self.assertNotIn("One work item owns one actionable problem", context)
            searched = self._launch(
                {"toolName": "search", "toolArgs": json.dumps({"query": "**/*"})},
                temporary,
                session=session,
            )
            self.assertEqual(0, searched.returncode, searched.stdout + searched.stderr)
            stopped = subprocess.run(  # nosec B603 - fixed node launcher.
                [self.node, str(LAUNCH), "copilot", "agentStop"],
                input=json.dumps(
                    {"sessionId": session, "cwd": str(ROOT), "stopHookActive": False}
                ),
                capture_output=True,
                text=True,
                check=False,
                cwd=ROOT,
                env=_env(temporary),
            )
        self.assertEqual(2, stopped.returncode, stopped.stdout + stopped.stderr)
        decision = self._decision(stopped)
        self.assertEqual("deny", decision.get("permissionDecision"))
        self.assertIn("retrieve --store graphify", str(decision.get("permissionDecisionReason")))

    def test_skill_adapter_does_not_copy_the_canonical_body(self):
        body = SKILL.read_text(encoding="utf-8")
        adapter = HOSTS.skill_adapter_bytes("chaos-engine").decode("utf-8")
        self.assertIn("canonical ChaosEngine", adapter)
        self.assertNotIn("Measure thrice, cut once", adapter)
        self.assertNotIn(body, adapter)
        findings = []
        if HOSTS.powershell_hook_parse_error(
            HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        ):
            findings.append({"kind": "powershell-parse", "triage": "open"})
        document = json.loads(HOSTS.copilot_hooks_document())
        for event, handlers in document["hooks"].items():
            command = handlers[0]["bash"]
            if command != f"node .chaos-engine/hooks/launch.js copilot {event}":
                findings.append({"kind": "missing-event", "event": event, "triage": "open"})
            if handlers[0]["powershell"] != command:
                findings.append({"kind": "powershell-drift", "event": event, "triage": "open"})
        self.assertEqual([], [item for item in findings if item["triage"] != "fixed"])
