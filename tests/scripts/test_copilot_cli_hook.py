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
import re
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


def _powershell51_native_argument(argument: str) -> str:
    """Windows PowerShell 5.1 legacy native argument passing.

    An argument with whitespace is wrapped in double quotes; embedded double
    quotes are passed through unescaped.
    """
    if any(character.isspace() for character in argument) and not (
            argument.startswith('"') and argument.endswith('"')):
        return f'"{argument}"'
    return argument


def _windows_argv(command_line: str) -> list[str]:
    """MSVCRT / CommandLineToArgvW splitting of a Windows command line."""
    arguments: list[str] = []
    current: list[str] = []
    quoted = False
    started = False
    index = 0
    while index < len(command_line):
        character = command_line[index]
        if character == "\\":
            run = 0
            while index < len(command_line) and command_line[index] == "\\":
                run += 1
                index += 1
            if index < len(command_line) and command_line[index] == '"':
                current.append("\\" * (run // 2))
                if run % 2:
                    current.append('"')
                    index += 1
            else:
                current.append("\\" * run)
            started = True
            continue
        if character == '"':
            quoted = not quoted
            started = True
        elif character.isspace() and not quoted:
            if started:
                arguments.append("".join(current))
                current, started = [], False
        else:
            current.append(character)
            started = True
        index += 1
    if started:
        arguments.append("".join(current))
    return arguments


def _python_argument_under_powershell51(command: str) -> str:
    """The `-c` source Python receives when PowerShell 5.1 runs a `python3 -c '...'` hook."""
    match = re.fullmatch(r"python3 -c '([^']*)'", command)
    if match is None:
        raise AssertionError(f"not a single-quoted python3 -c command: {command[:40]}")
    line = "python3 -c " + _powershell51_native_argument(match.group(1))
    return _windows_argv(line)[2]


class PowerShell51ArgumentPassingTest(unittest.TestCase):
    """#6632: the #6630 command parsed, but PowerShell 5.1 stripped its quotes."""

    def test_legacy_double_quoted_body_reaches_python_mangled(self):
        script = HOSTS._locator_script("claude")
        legacy = "python3 -c '" + "exec(" + json.dumps(script) + ")" + "'"
        received = _python_argument_under_powershell51(legacy)
        self.assertNotEqual("exec(" + json.dumps(script) + ")", received)
        with self.assertRaises(SyntaxError):
            compile(received, "<hook>", "exec")
        self.assertIsNotNone(HOSTS.powershell_hook_parse_error(legacy))

    def test_quote_free_body_reaches_python_byte_for_byte(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        body = re.fullmatch(r"python3 -c '([^']*)'", command).group(1)
        self.assertEqual(body, _python_argument_under_powershell51(command))
        self.assertFalse(set(body) - set("0123456789,()bytesxc"))
        self.assertEqual(HOSTS._locator_script("claude"), HOSTS.decoded_quote_free_exec(command))


class CopilotRootCwdTest(unittest.TestCase):
    """#6632: Copilot runs repo-settings hooks from `/` without CLAUDE_PROJECT_DIR."""

    def test_guard_is_found_from_the_payload_cwd(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory() as temporary:
            outside = Path(temporary) / "outside"
            outside.mkdir()
            environment = _env(temporary)
            environment.pop("CLAUDE_PROJECT_DIR", None)
            result = subprocess.run(  # nosec B602 - generated hook command.
                command,
                shell=True,
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "sessionId": "copilot-root-cwd",
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
                cwd=outside,
                env=environment,
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertNotIn("guard unavailable", result.stdout)
        payload = json.loads(result.stdout or "{}")
        self.assertNotEqual("deny", payload.get("permissionDecision"))

    def test_no_guard_anywhere_still_denies_with_a_reason(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory() as temporary:
            environment = _env(temporary)
            environment.pop("CLAUDE_PROJECT_DIR", None)
            result = subprocess.run(  # nosec B602 - generated hook command.
                command,
                shell=True,
                input=json.dumps({"hook_event_name": "PreToolUse", "cwd": temporary}),
                capture_output=True,
                text=True,
                check=False,
                cwd=temporary,
                env=environment,
            )
        self.assertEqual(2, result.returncode, result.stdout + result.stderr)
        self.assertEqual("block", json.loads(result.stdout)["decision"])


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
        source = HOSTS.decoded_quote_free_exec(command)
        self.assertIn(".chaos-engine/hooks/guard.py", source)
        self.assertIn("repository working directory unavailable", source)
        self.assertTrue(command.startswith("python3 -c '"))
        self.assertTrue(HOSTS.chaos_hook_command(command))
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
        findings = HOSTS.copilot_surface_findings(ROOT)
        open_findings = [item for item in findings if item.get("triage") != "fixed"]
        self.assertEqual([], open_findings)
