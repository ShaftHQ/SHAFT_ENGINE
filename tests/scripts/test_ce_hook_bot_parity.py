"""Hook-only gates have bot twins from one source: real block, then real pass, on an installed overlay."""

from __future__ import annotations

import json
import os
import subprocess  # nosec B404 - runs the installed overlay's own scripts.
import sys
import tempfile
import unittest
from pathlib import Path

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from tests.scripts.ce_installed_fixture import build_installed_project


class HookBotParityTest(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.project = build_installed_project(Path(self._tmp.name) / "adopter")
        subprocess.run(["git", "init", "-q"], cwd=self.project, check=True)  # nosec B603 B607
        self.env = {**os.environ, "CHAOS_ENGINE_HOST": "claude",
                    "CHAOS_ENGINE_TASKS_DIR": str(self.project / "tasks")}

    def tearDown(self) -> None:
        self._tmp.cleanup()

    def tool(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run([sys.executable, ".chaos-engine/tool.py", *args], cwd=self.project,  # nosec B603
                              env=self.env, capture_output=True, text=True, timeout=60, check=False)

    def hook(self, event: dict) -> subprocess.CompletedProcess:
        payload = json.dumps({"session_id": "parity", "cwd": str(self.project), **event})
        return subprocess.run([sys.executable, ".chaos-engine/hooks/guard.py"], cwd=self.project,  # nosec B603
                              env=self.env, input=payload, capture_output=True, text=True, timeout=60, check=False)

    def test_cli_session_start_satisfies_the_bot_load_stamp(self) -> None:
        self.assertTrue(self.tool("resume", "t").stderr.startswith("run entry --full ("))
        self.assertEqual(0, self.hook({"hook_event_name": "SessionStart"}).returncode)
        self.assertNotEqual(2, self.tool("resume", "t").returncode)

    def test_bot_checkpoint_owes_a_retrieve_like_the_cli_stop_gate(self) -> None:
        self.tool("entry", "--full")
        spec = self.project / "spec.md"
        spec.write_text("# spec\n", encoding="utf-8")
        blocked = self.tool("checkpoint", "t", "work", "--spec", str(spec))
        self.assertEqual(2, blocked.returncode)
        self.assertIn("retrieve", blocked.stderr)
        self.tool("retrieve", "--store", "mempalace", "parity")
        self.assertEqual(0, self.tool("checkpoint", "t", "work", "--spec", str(spec)).returncode)


if __name__ == "__main__":
    unittest.main()
