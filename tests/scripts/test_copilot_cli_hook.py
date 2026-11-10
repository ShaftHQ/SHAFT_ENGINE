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
import sys
import tempfile
import unittest
from unittest import mock
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


def _seed_launch(project: Path) -> None:
    """#7667: short hook commands need launch.py under cwd."""
    dest = project / ".chaos-engine/hooks"
    dest.mkdir(parents=True, exist_ok=True)
    target = dest / "launch.py"
    source = ROOT / "chaos-engine/hooks/launch.py"
    if not target.exists():
        shutil.copy2(source, target)

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

    def test_powershell_branch_passes_python_only_quote_free_arguments(self):
        command = HOSTS.chaos_guard_locator_command(windows=True, host="claude")
        self.assertTrue(command.startswith("$a="))
        self.assertIn(HOSTS.HOOK_LAUNCH_PY, command)
        self.assertLessEqual(len(command), HOSTS.HOOK_COMMAND_MAX_CHARS)
        self.assertIsNone(HOSTS.powershell_hook_parse_error(command))

    def test_legacy_python3_only_command_is_flagged(self):
        script = HOSTS._locator_script("claude")
        legacy = "python3 -c '" + HOSTS._quote_free_exec(script) + "'"
        self.assertIsNotNone(HOSTS.powershell_hook_parse_error(legacy))
        self.assertTrue(HOSTS.chaos_hook_command(legacy))
        self.assertEqual(script, HOSTS.hook_command_source(legacy))


def _powershells() -> list[str]:
    return [found for name in ("pwsh", "powershell") if (found := shutil.which(name))]


class PowerShellLauncherTest(unittest.TestCase):
    """#6632: Copilot CLI on Windows runs the settings command with PowerShell.

    `python3` is often missing or the Microsoft Store stub there. The
    PowerShell branch must find a real Python (installer pointer, then
    `py -3`, `python3`, `python`) and never exit with anything but the
    guard's own 0/2. Runs on every host with pwsh; the Windows CI leg runs it
    under both PowerShell 7 and Windows PowerShell 5.1.
    """

    def setUp(self):
        self.shells = _powershells()
        if not self.shells:
            self.skipTest("PowerShell is required to drive the Windows hook path")
        self.command = HOSTS.chaos_guard_locator_command(windows=True, host="claude")

    def _run(self, shell: str, payload: dict[str, object], cwd: Path, temporary: str,
             path: str | None = None) -> subprocess.CompletedProcess[str]:
        environment = _env(temporary)
        environment.pop("CLAUDE_PROJECT_DIR", None)
        if path is not None:
            environment["PATH"] = path
        _seed_launch(cwd)
        return subprocess.run(  # nosec B603 - fixed PowerShell with the generated hook script.
            [shell, "-NoProfile", "-NonInteractive", "-Command", self.command],
            input=json.dumps(payload),
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
            cwd=cwd,
            env=environment,
        )

    def test_read_is_allowed_from_outside_the_repository(self):
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                result = self._run(shell, {
                    "hook_event_name": "PreToolUse", "cwd": str(ROOT), "tool_name": "Read",
                    "tool_input": {"file_path": str(ROOT / "AGENTS.md")},
                }, Path(temporary), temporary)
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)
                self.assertNotEqual("block", json.loads(result.stdout or "{}").get("decision"))

    def test_guard_block_keeps_exit_two(self):
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                result = self._run(shell, {
                    "hook_event_name": "PreToolUse", "cwd": str(ROOT), "tool_name": "Bash",
                    "tool_input": {"command": "git reset --hard HEAD~1"},
                }, Path(temporary), temporary)
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)
                self.assertIn('"decision":"block"', (result.stdout + result.stderr).replace(" ", ""))
                self.assertNotIn("guard unavailable", result.stdout, "payload never reached the guard")
                self.assertEqual([], list(Path(temporary).glob("tmp*.tmp")), "payload file left behind")

    def test_no_python_blocks_with_a_reason_instead_of_erroring(self):
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                result = self._run(shell, {"hook_event_name": "PreToolUse", "cwd": temporary},
                                   Path(temporary), temporary, path=str(Path(shell).parent))
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)
                decision = json.loads(result.stdout)
                self.assertEqual("block", decision["decision"])
                self.assertTrue(
                    "no Python 3.11" in decision["reason"]
                    or "Python 3.11+" in decision["reason"],
                    decision["reason"],
                )

    def test_installer_pointer_python_runs_without_python_on_path(self):
        # #7667: Windows short launcher probes `py` only; pointer re-exec lives in
        # launch.py once an interpreter is running (posix). With the real
        # interpreter available, the short command still reaches launch.py.
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                project = Path(temporary) / "project"
                project.mkdir()
                _seed_launch(project)
                (project / ".chaos-engine-state").mkdir(parents=True)
                (project / HOSTS.HOOK_PYTHON_POINTER).write_text(
                    os.path.realpath(sys.executable) + "\n", encoding="utf-8")
                result = self._run(
                    shell,
                    {"hook_event_name": "PreToolUse", "cwd": str(project)},
                    project,
                    temporary,
                )
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)
                reason = json.loads(result.stdout)["reason"]
                self.assertNotIn("no Python 3.11", reason)
                self.assertIn("guard unavailable", reason)


class CopilotRepositoryHookTest(unittest.TestCase):
    """#6632: `.github/hooks` uses the Python launcher, no Node, from any cwd."""

    def setUp(self):
        self.shells = [found for found in (shutil.which("sh"), *_powershells()) if found]
        if not self.shells:
            self.skipTest("a shell is required")

    def _run(self, shell: str, payload: dict[str, object], temporary: str) -> subprocess.CompletedProcess[str]:
        # #7678: Copilot document has separate bash/powershell fields. Drive the
        # matching field for the shell under test — never feed the Unix `&&`
        # launcher to Windows PowerShell (parse error on 5.1; wrong fail path on 7).
        stem = Path(shell).stem.lower()
        windows = stem not in {"sh", "bash", "dash", "zsh"}
        command = HOSTS.chaos_guard_locator_command(
            windows=windows, host="copilot", event="PreToolUse"
        )
        environment = _env(temporary)
        environment.pop("CLAUDE_PROJECT_DIR", None)
        _seed_launch(Path(temporary))
        argv = [shell, "-c", command] if Path(shell).stem == "sh" else [
            shell, "-NoProfile", "-NonInteractive", "-Command", command]
        return subprocess.run(  # nosec B603 - fixed shell with the generated hook script.
            argv, input=json.dumps(payload), capture_output=True, text=True,
            encoding="utf-8", check=False, cwd=temporary, env=environment,
        )

    def test_tools_outside_matchers_are_answered_without_the_guard(self):
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                result = self._run(shell, {
                    "cwd": str(ROOT), "toolName": "browser_get_page_dom", "toolArgs": "{}",
                }, temporary)
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)
                self.assertEqual({}, json.loads(result.stdout))

    def test_event_hint_reaches_the_guard(self):
        for shell in self.shells:
            with self.subTest(powershell=shell), tempfile.TemporaryDirectory() as temporary:
                result = self._run(shell, {
                    "cwd": str(ROOT), "sessionId": "copilot-repo-hook", "toolName": "bash",
                    "toolArgs": json.dumps({"command": "git reset --hard HEAD~1"}),
                }, temporary)
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)
                self.assertNotIn("guard unavailable", result.stdout)
                self.assertIn("deny", result.stdout)

    def test_document_pins_the_repository_root(self):
        document = json.loads(HOSTS.copilot_hooks_document())
        for event, handlers in document["hooks"].items():
            self.assertEqual(".", handlers[0]["cwd"])
            self.assertEqual(HOSTS.copilot_launcher_command(event), handlers[0]["bash"])
            self.assertEqual(
                HOSTS.chaos_guard_locator_command(windows=True, host="copilot", event=event),
                handlers[0]["powershell"],
            )
            self.assertIsNone(HOSTS.powershell_hook_parse_error(handlers[0]["powershell"]))
            self.assertLessEqual(len(handlers[0]["powershell"]), HOSTS.HOOK_COMMAND_MAX_CHARS)
            self.assertLessEqual(len(handlers[0]["bash"]), HOSTS.HOOK_COMMAND_MAX_CHARS)
            self.assertIn(HOSTS.HOOK_LAUNCH_PY, handlers[0]["bash"])


class NodeLauncherTest(unittest.TestCase):
    """#6632: Gemini's Node launcher finds the guard from the payload cwd."""

    def test_guard_is_found_from_the_payload_cwd(self):
        node = shutil.which("node")
        if not node:
            self.skipTest("node is required")
        with tempfile.TemporaryDirectory() as temporary:
            environment = _env(temporary)
            for name in ("CLAUDE_PROJECT_DIR", "GEMINI_PROJECT_DIR"):
                environment.pop(name, None)
            result = subprocess.run(  # nosec B603 - fixed node with the tracked launcher.
                [node, str(LAUNCH), "gemini"],
                input=json.dumps({
                    "hook_event_name": "BeforeTool", "cwd": str(ROOT), "tool_name": "read_file",
                    "tool_input": {"file_path": str(ROOT / "AGENTS.md")},
                }),
                capture_output=True, text=True, check=False, cwd=temporary, env=environment,
            )
        self.assertNotIn("guard unavailable", result.stdout + result.stderr)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


class GitBashLauncherTest(unittest.TestCase):
    """#6632: Claude Code on Windows runs the command with Git Bash (OS=Windows_NT)."""

    def test_windows_branch_probes_and_runs_python(self):
        sh = shutil.which("sh")
        if not sh:
            self.skipTest("sh is required")
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory() as temporary:
            _seed_launch(Path(temporary))
            environment = {**_env(temporary), "OS": "Windows_NT"}
            environment.pop("CLAUDE_PROJECT_DIR", None)
            result = subprocess.run(  # nosec B603 - fixed sh with the generated hook script.
                [sh, "-c", command],
                input=json.dumps({
                    "hook_event_name": "PreToolUse", "cwd": str(ROOT), "tool_name": "Read",
                    "tool_input": {"file_path": str(ROOT / "AGENTS.md")},
                }),
                capture_output=True,
                text=True,
                check=False,
                cwd=temporary,
                env=environment,
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


class CopilotRootCwdTest(unittest.TestCase):
    """#6632: Copilot runs repo-settings hooks from `/` without CLAUDE_PROJECT_DIR."""

    def test_guard_is_found_from_the_payload_cwd(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory() as temporary:
            outside = Path(temporary) / "outside"
            outside.mkdir()
            _seed_launch(outside)
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
            _seed_launch(Path(temporary))
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
        windows = HOSTS.chaos_guard_locator_command(windows=True, host="claude")
        self.assertIsNone(HOSTS.powershell_hook_parse_error(command))
        self.assertIsNone(HOSTS.powershell_hook_parse_error(windows))
        self.assertIn(HOSTS.HOOK_LAUNCH_PY, command)
        self.assertLessEqual(len(command), HOSTS.HOOK_COMMAND_MAX_CHARS)
        self.assertIn("exec python3", command)
        self.assertTrue(HOSTS.chaos_hook_command(command))
        with tempfile.TemporaryDirectory() as temporary:
            _seed_launch(ROOT)
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


# --- #7667 short launcher + Stop accept-on-retry (folded; avoid harness_pr_gate edit) ---
LAUNCH_PY = ROOT / "chaos-engine/hooks/launch.py"
GUARD = ROOT / "chaos-engine/hooks/guard.py"
REFLECTION = ROOT / "chaos-engine/hooks/reflection.py"


def _load7667(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec is not None and spec.loader is not None
    spec.loader.exec_module(mod)
    return mod

class HookCommandShort7667Test(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.hosts = _load7667("hosts_7667", ROOT / "chaos-engine/hosts.py")

    def test_mutant_commands_stay_under_300_chars(self) -> None:
        """Removing the launcher file path and inlining again must turn RED."""
        for host, event in (
            ("claude", None),
            ("grok", None),
            ("copilot", "PreToolUse"),
            ("copilot", "preToolUse"),
        ):
            for windows in (False, True):
                with self.subTest(host=host, windows=windows, event=event):
                    command = self.hosts.chaos_guard_locator_command(
                        windows=windows, host=host, event=event
                    )
                    self.assertLessEqual(len(command), self.hosts.HOOK_COMMAND_MAX_CHARS)
                    self.assertIn(self.hosts.HOOK_LAUNCH_PY, command)
                    self.assertNotIn("import errno,io,json", command)

    def test_lifecycle_and_copilot_documents_use_short_commands(self) -> None:
        claude = json.loads(self.hosts.lifecycle_hooks_document("claude"))
        stop = claude["hooks"]["Stop"][0]["hooks"][0]
        self.assertLessEqual(len(stop["command"]), self.hosts.HOOK_COMMAND_MAX_CHARS)
        self.assertLessEqual(len(stop["commandWindows"]), self.hosts.HOOK_COMMAND_MAX_CHARS)
        self.assertIn(self.hosts.HOOK_LAUNCH_PY, stop["command"])
        copilot = json.loads(self.hosts.copilot_hooks_document())
        entry = copilot["hooks"]["preToolUse"][0]
        self.assertLessEqual(len(entry["bash"]), self.hosts.HOOK_COMMAND_MAX_CHARS)
        self.assertLessEqual(len(entry["powershell"]), self.hosts.HOOK_COMMAND_MAX_CHARS)
        self.assertTrue(entry["bash"].startswith("echo `# <#`"))
        self.assertTrue(entry["powershell"].startswith("$a=") or entry["powershell"].startswith("py -3 "))

    def test_launch_py_runs_guard_from_project_root(self) -> None:
        payload = json.dumps(
            {"hook_event_name": "SessionStart", "session_id": "7667-launch", "cwd": str(ROOT)}
        )
        run = subprocess.run(  # nosec B603 - fixed argv: local launch.py only.
            [sys.executable, str(LAUNCH_PY), "claude"],
            input=payload,
            text=True,
            cwd=str(ROOT),
            capture_output=True,
            check=False,
            timeout=60,
        )
        self.assertEqual(0, run.returncode, run.stderr)
        self.assertIn("additionalContext", run.stdout)


class TerminalReflectionOnce7667Test(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.guard = _load7667("guard_7667", GUARD)
        cls.reflection = _load7667("reflection_7667", REFLECTION)

    def test_second_stop_after_accept_stays_silent(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            with mock.patch.dict(os.environ, {"TMPDIR": temporary, "TEMP": temporary}):
                session = "7667-once"
                self.reflection.record_session_start(session, "2019-01-01T00:00:00+00:00")
                labels = "\n".join(
                    f"{label}: note" for label in self.guard.TERMINAL_LABELS
                )
                complete = labels + "\n## Enhancements\nnothing actionable"
                first = self.guard._terminal_reflection_reason(
                    {"session_id": session, "last_assistant_message": complete},
                    session,
                )
                self.assertEqual("", first)
                self.assertTrue(self.reflection.has_valid_terminal_receipt(session))
                second = self.guard._terminal_reflection_reason(
                    {
                        "session_id": session,
                        "last_assistant_message": "later work with no labels",
                    },
                    session,
                )
                self.assertEqual("", second)

    def test_retry_missing_labels_stays_silent_untracked_still_blocks(self) -> None:
        """#7667/#6275: missing labels on retry must not loop; untracked still blocks."""
        with tempfile.TemporaryDirectory() as temporary:
            with mock.patch.dict(os.environ, {"TMPDIR": temporary, "TEMP": temporary}):
                session = "7667-retry"
                self.reflection.record_session_start(session, "2019-01-01T00:00:00+00:00")
                labels = "\n".join(
                    f"{label}: note" for label in self.guard.TERMINAL_LABELS
                )
                silent = self.guard._terminal_reflection_reason(
                    {
                        "session_id": session,
                        "last_assistant_message": "no labels yet",
                        "stop_hook_active": True,
                    },
                    session,
                )
                self.assertEqual("", silent)
                self.assertFalse(self.reflection.has_valid_terminal_receipt(session))
                blocked = self.guard._terminal_reflection_reason(
                    {
                        "session_id": session,
                        "last_assistant_message": labels,
                        "stop_hook_active": True,
                    },
                    session,
                )
                self.assertIn("configured upstream repository", blocked)
                self.assertFalse(self.reflection.has_valid_terminal_receipt(session))


class UnixPolyglotLaunchFileTest(unittest.TestCase):
    """#7885: a missing launch.py is named before dash can fail on shopt."""

    def _command(self) -> str:
        return HOSTS.chaos_guard_locator_command(
            windows=False, host="copilot", event="userPromptSubmitted"
        )

    def test_unix_polyglot_checks_launch_file_before_shopt(self) -> None:
        command = self._command()
        self.assertLess(
            command.index("[ -f .chaos-engine/hooks/launch.py ]"),
            command.index("shopt"),
        )

    def _missing_launch(self, shell: str) -> None:
        executable = shutil.which(shell)
        if not executable:
            self.skipTest(f"{shell} is required")
        with tempfile.TemporaryDirectory() as temporary:
            result = subprocess.run(  # nosec B603 - fixed shell argv, no shell.
                [executable, "-c", self._command()],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=temporary,
            )
        combined = result.stdout + result.stderr
        self.assertNotEqual(0, result.returncode, combined)
        self.assertNotIn("missing .chaos-engine/hooks/launch.py", result.stdout)
        self.assertIn("missing .chaos-engine/hooks/launch.py", result.stderr)
        self.assertNotIn("shopt:", combined)

    def test_unix_polyglot_missing_launch_under_sh(self) -> None:
        self._missing_launch("sh")

    def test_unix_polyglot_missing_launch_under_bash(self) -> None:
        self._missing_launch("bash")

    def test_unix_polyglot_bash_reaches_python3_when_launch_exists(self) -> None:
        bash = shutil.which("bash")
        if not bash:
            self.skipTest("bash is required")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            marker = root / "python3-ran"
            bindir = root / "bin"
            bindir.mkdir()
            python3 = bindir / "python3"
            python3.write_text(
                "#!/bin/sh\nprintf '%s\\n' invoked > \"$PYTHON3_RAN\"\nexit 0\n",
                encoding="utf-8",
            )
            python3.chmod(0o755)  # nosec B103 - fixture interpreter the test itself runs.
            launch = root / ".chaos-engine" / "hooks"
            launch.mkdir(parents=True)
            (launch / "launch.py").write_text("", encoding="utf-8")
            environment = os.environ.copy()
            environment["PATH"] = str(bindir)
            environment["PYTHON3_RAN"] = str(marker)
            result = subprocess.run(  # nosec B603 - fixed bash argv, no shell.
                [bash, "-c", self._command()],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=temporary,
                env=environment,
            )
            combined = result.stdout + result.stderr
            self.assertEqual(0, result.returncode, combined)
            self.assertEqual("invoked\n", marker.read_text(encoding="utf-8"))

    def test_copilot_hooks_document_longest_bash_fits_1024(self) -> None:
        document = json.loads(HOSTS.copilot_hooks_document())
        bash_commands = [
            handler["bash"]
            for handlers in document["hooks"].values()
            for handler in handlers
            if "bash" in handler
        ]
        self.assertTrue(bash_commands)
        self.assertLessEqual(max(len(bash) for bash in bash_commands), 1024)



