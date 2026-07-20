"""Deploy/drift behavior of scripts/agents/sync_user_harness.py."""

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agents/sync_user_harness.py"
MANIFEST = ("CLAUDE.md", "settings.json", "statusline-command.sh")


class SyncUserHarnessTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.target = Path(self._tmp.name)
        self.addCleanup(self._tmp.cleanup)

    def run_sync(self, *args: str) -> subprocess.CompletedProcess:
        env = dict(os.environ, SHAFT_USER_CLAUDE_DIR=str(self.target))
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            cwd=ROOT,
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
        )

    def repo_agents(self) -> list[Path]:
        return sorted((ROOT / ".claude/agents").glob("*.md"))

    def test_check_reports_missing_manifest_and_agents(self):
        completed = self.run_sync()
        self.assertEqual(completed.returncode, 1)
        for name in MANIFEST:
            self.assertIn(f"MISSING  {name}", completed.stdout)
        self.assertTrue(self.repo_agents(), "repo must ship agent charters")
        for agent in self.repo_agents():
            self.assertIn(f"MISSING  agents/{agent.name}", completed.stdout)

    def test_apply_deploys_everything_then_check_is_clean(self):
        self.assertEqual(self.run_sync("--apply").returncode, 0)
        for name in MANIFEST:
            self.assertTrue((self.target / name).is_file())
        for agent in self.repo_agents():
            deployed = self.target / "agents" / agent.name
            self.assertEqual(
                deployed.read_bytes().replace(b"\r\n", b"\n"),
                agent.read_bytes().replace(b"\r\n", b"\n"),
            )
        self.assertEqual(self.run_sync().returncode, 0)

    def test_drifted_agent_is_reported_backed_up_and_redeployed(self):
        self.run_sync("--apply")
        drifted = self.target / "agents" / self.repo_agents()[0].name
        drifted.write_text("local drift\n", encoding="utf-8")
        completed = self.run_sync()
        self.assertEqual(completed.returncode, 1)
        self.assertIn(f"DRIFTED  agents/{drifted.name}", completed.stdout)
        self.assertEqual(self.run_sync("--apply").returncode, 0)
        self.assertEqual(
            (self.target / "agents" / (drifted.name + ".bak")).read_text(encoding="utf-8"),
            "local drift\n",
        )
        self.assertEqual(self.run_sync().returncode, 0)


if __name__ == "__main__":
    unittest.main()
