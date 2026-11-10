"""Parent walk for launch.py. Dash must not execute shopt (#7874, #7885)."""

from __future__ import annotations

import importlib.util
import subprocess  # nosec B404 - fixed hook command under test.
import tempfile
import unittest
from pathlib import Path


def _load_hosts():
    path = ROOT / "chaos-engine/hosts.py"
    specification = importlib.util.spec_from_file_location("chaos_engine_hosts_hook_launch_cwd", path)
    if specification is None or specification.loader is None:
        raise RuntimeError("hosts module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


ROOT = Path(__file__).resolve().parents[2]
HOSTS = _load_hosts()

_LAUNCH_BODY = "import sys\nprint('found-launch')\nprint(sys.argv[1:])\n"


class HookLaunchCwdTest(unittest.TestCase):
    def _commands(self):
        cases = [(host, None) for host in ("claude", "grok", "codex", "copilot")]
        cases.extend(
            ("copilot", event)
            for event in ("userPromptSubmitted", "postToolUseFailure", "preToolUse")
        )
        for host, event in cases:
            for windows in (False, True):
                yield host, event, windows, HOSTS.chaos_guard_locator_command(
                    windows=windows, host=host, event=event
                )

    def test_commands_stay_inside_the_hook_contract(self):
        longest = 0
        for host, event, windows, command in self._commands():
            longest = max(longest, len(command))
            with self.subTest(host=host, event=event, windows=windows):
                self.assertLessEqual(len(command), HOSTS.HOOK_COMMAND_MAX_CHARS)
                self.assertEqual([], HOSTS.powershell_out_file_targets(command))
                self.assertNotIn(">/dev/null", command)
                self.assertIsNone(HOSTS.powershell_hook_parse_error(command))
                self.assertTrue(HOSTS.chaos_hook_command(command))
                if not windows:
                    self.assertEqual([], HOSTS.grok_unresolved_env_refs(command))
                    self.assertIn("exec python3", command)
                    self.assertIn(HOSTS.HOOK_LAUNCH_PY, command)
                elif host == "grok":
                    self.assertEqual([], HOSTS.grok_unresolved_env_refs(command))
                    self.assertTrue(command.startswith("py -3 "))
                    self.assertNotIn("$", command)
                    self.assertIn(HOSTS.HOOK_LAUNCH_PY, command)
                else:
                    self.assertTrue(command.startswith("$a="))
        self.assertLessEqual(longest, HOSTS.HOOK_COMMAND_MAX_CHARS)

    def test_dash_missing_launcher_exits_2_without_running_shopt(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory(dir="/tmp") as temporary:
            result = subprocess.run(  # nosec B603 - fixed /bin/sh argv, no shell.
                ["/bin/sh", "-c", command],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=temporary,
            )
            self.assertEqual(2, result.returncode, result.stdout + result.stderr)
            self.assertNotIn("missing .chaos-engine/hooks/launch.py", result.stdout)
            self.assertIn("missing .chaos-engine/hooks/launch.py", result.stderr)
            self.assertNotIn("shopt:", result.stdout + result.stderr)

    def test_dash_finds_parent_launch_py(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        with tempfile.TemporaryDirectory(dir="/tmp") as temporary:
            root = Path(temporary)
            launch = root / ".chaos-engine/hooks"
            launch.mkdir(parents=True)
            (launch / "launch.py").write_text(_LAUNCH_BODY, encoding="utf-8")
            nested = root / "nested"
            nested.mkdir()
            result = subprocess.run(  # nosec B603 - fixed /bin/sh argv, no shell.
                ["/bin/sh", "-c", command],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=nested,
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn("found-launch", result.stdout)
        self.assertIn("claude", result.stdout)

    def test_grok_windows_script_finds_parent_and_blocks_when_absent(self):
        command = HOSTS.chaos_guard_locator_command(windows=True, host="grok")
        prefix = 'py -3 -c "'
        self.assertTrue(command.startswith(prefix))
        script, separator, tail = command[len(prefix):].partition('"')
        self.assertEqual(separator, '"')
        self.assertIn("subprocess.call", script)
        self.assertNotIn("os.execv", script)
        args = tail.split()
        with tempfile.TemporaryDirectory(dir="/tmp") as temporary:
            root = Path(temporary)
            launch = root / ".chaos-engine/hooks"
            launch.mkdir(parents=True)
            (launch / "launch.py").write_text(_LAUNCH_BODY, encoding="utf-8")
            nested = root / "nested"
            nested.mkdir()
            found = subprocess.run(  # nosec B603 - python3 -c of the generated script.
                ["python3", "-c", script, *args],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=nested,
            )
        with tempfile.TemporaryDirectory(dir="/tmp") as empty:
            missing = subprocess.run(  # nosec B603 - python3 -c of the generated script.
                ["python3", "-c", script, *args],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=empty,
            )
        self.assertEqual(0, found.returncode, found.stdout + found.stderr)
        self.assertIn("found-launch", found.stdout)
        self.assertEqual(2, missing.returncode, missing.stdout + missing.stderr)
        self.assertNotIn("missing", missing.stdout)
        self.assertIn("missing .chaos-engine/hooks/launch.py", missing.stderr)
