"""
Grok refuses a hook command that references an unset plain ``$NAME``.

The project lifecycle command is valid sh and valid PowerShell. Its PowerShell
branch uses ``$LASTEXITCODE``, ``$env``, ``$false``, and other names that are
not environment variables. Grok's runner treats each plain ``$NAME`` as
required and skips the hook. The Grok command keeps that branch, encoded, so
the scanned text assigns every plain ``$NAME`` it still contains.
"""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - fixed repository hook entry.
import tempfile
import unittest
from pathlib import Path


def _load_hosts():
    path = ROOT / "chaos-engine/hosts.py"
    specification = importlib.util.spec_from_file_location("chaos_engine_hosts_grok_hook_env", path)
    if specification is None or specification.loader is None:
        raise RuntimeError("hosts module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


ROOT = Path(__file__).resolve().parents[2]
HOSTS = _load_hosts()


class GrokHookEnvRefTest(unittest.TestCase):
    def test_scanner_matches_the_runner(self):
        cases = {
            's=ok; echo "$s"': [],
            "echo $UNSET_ZZ_SCAN": ["UNSET_ZZ_SCAN"],
            'echo "$OS"': ["OS"],
            "echo $(uname -s)": [],
            "echo ${OS:-none}": [],
            'a=; p=py; echo $a$p': [],
            'echo "$LATE"; LATE=1': [],
            'echo "$CMT" # CMT=1': ["CMT"],
            "echo \"$Q\"; echo 'Q=1'": ["Q"],
            'echo "$false" # false=1': ["false"],
            "echo \"$LASTEXITCODE\"\n# LASTEXITCODE=1": ["LASTEXITCODE"],
        }
        for command, expected in cases.items():
            self.assertEqual(expected, HOSTS.grok_unresolved_env_refs(command), command)

    def test_grok_command_has_no_unresolved_refs_and_other_hosts_keep_the_branch(self):
        grok = HOSTS.chaos_guard_locator_command(windows=False, host="grok")
        claude = HOSTS.chaos_guard_locator_command(windows=False, host="claude")
        self.assertEqual([], HOSTS.grok_unresolved_env_refs(grok))
        self.assertIn("${OS:-}", grok)
        self.assertNotIn("$LASTEXITCODE", grok)
        self.assertIn("Invoke-Expression", grok)
        self.assertIn("$LASTEXITCODE", claude)
        self.assertNotIn("Invoke-Expression", claude)
        self.assertIsNone(HOSTS.powershell_hook_parse_error(grok))
        self.assertIsNone(HOSTS.powershell_hook_parse_error(claude))
        self.assertEqual(HOSTS._locator_script("grok"), HOSTS.hook_command_source(grok))
        self.assertTrue(HOSTS.chaos_hook_command(grok))

    def test_shell_branch_still_reaches_the_guard(self):
        self._assert_guard(HOSTS.chaos_guard_locator_command(windows=False, host="grok"))

    def test_windows_shell_branch_still_reaches_the_guard(self):
        command = HOSTS.chaos_guard_locator_command(windows=False, host="grok")
        self._assert_guard(command, windows=True)

    def test_powershell_branch_still_reaches_the_guard(self):
        executable = shutil.which("pwsh") or shutil.which("powershell")
        if not executable:
            self.skipTest("PowerShell is required to drive the Grok Windows branch")
        command = HOSTS.chaos_guard_locator_command(windows=False, host="grok")
        self._assert_guard(command, posix=False, executable=executable)

    def _assert_guard(self, command: str, *, posix: bool = True, windows: bool = False, executable: str | None = None):
        payload = {
            "hook_event_name": "PreToolUse",
            "cwd": str(ROOT),
            "tool_name": "Read",
            "tool_input": {"file_path": str(ROOT / "AGENTS.md")},
        }
        with tempfile.TemporaryDirectory() as temporary:
            environment = os.environ.copy()
            environment.pop("CLAUDE_PROJECT_DIR", None)
            if windows:
                environment["OS"] = "Windows_NT"
            if posix:
                argv = ["bash", "-c", command]
            else:
                argv = [executable or "pwsh", "-NoProfile", "-NonInteractive", "-Command", command]
            result = subprocess.run(  # nosec B603 - fixed bash or PowerShell argv, no shell.
                argv,
                input=json.dumps(payload),
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=temporary,
                env=environment,
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertNotEqual("block", json.loads(result.stdout or "{}").get("decision"))


if __name__ == "__main__":
    unittest.main()
