"""Deploy/drift behavior of scripts/agents/sync_user_harness.py."""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agents/sync_user_harness.py"
MANIFEST = ("CLAUDE.md", "settings.json")


class SyncUserHarnessTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.target = Path(self._tmp.name)
        self.agents_target = self.target / ".agents"
        self.addCleanup(self._tmp.cleanup)

    def run_sync(self, *args: str) -> subprocess.CompletedProcess:
        env = dict(
            os.environ,
            SHAFT_USER_CLAUDE_DIR=str(self.target),
            SHAFT_USER_AGENTS_DIR=str(self.agents_target),
        )
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

    def canonical_skill_files(self) -> list[Path]:
        return sorted(
            path
            for path in (ROOT / ".agents/skills/act-as-mohab").rglob("*")
            if path.is_file()
        )

    def test_check_reports_missing_manifest_and_agents(self):
        completed = self.run_sync()
        self.assertEqual(completed.returncode, 1)
        for name in MANIFEST:
            self.assertIn(f"MISSING  {name}", completed.stdout)
        self.assertTrue(self.repo_agents(), "repo must ship agent charters")
        for agent in self.repo_agents():
            self.assertIn(f"MISSING  agents/{agent.name}", completed.stdout)
        self.assertTrue(self.canonical_skill_files())
        self.assertIn("MISSING  ../.agents/skills/act-as-mohab/SKILL.md", completed.stdout)
        self.assertIn("MISSING  skills/act-as-mohab/SKILL.md", completed.stdout)

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
        canonical = ROOT / ".agents/skills/act-as-mohab"
        for source in self.canonical_skill_files():
            deployed = self.agents_target / "skills/act-as-mohab" / source.relative_to(canonical)
            self.assertEqual(
                deployed.read_bytes().replace(b"\r\n", b"\n"),
                source.read_bytes().replace(b"\r\n", b"\n"),
            )
        adapter = ROOT / ".claude/skills/act-as-mohab/SKILL.md"
        self.assertEqual(
            (self.target / "skills/act-as-mohab/SKILL.md").read_bytes().replace(b"\r\n", b"\n"),
            adapter.read_bytes().replace(b"\r\n", b"\n"),
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

    def test_settings_merge_preserves_unowned_keys_and_secret_values(self):
        personal_marker = "personal-value-must-survive"
        existing = {
            "env": {"PERSONAL_API_KEY": personal_marker, "ENABLE_TOOL_SEARCH": "old"},
            "enabledPlugins": {"personal@local": True, "mempalace@mempalace": True},
            "personalSetting": {"nested": 7},
        }
        (self.target / "settings.json").write_text(json.dumps(existing), encoding="utf-8")

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertNotIn(personal_marker, completed.stdout)
        deployed = json.loads((self.target / "settings.json").read_text(encoding="utf-8"))
        self.assertEqual(deployed["env"]["PERSONAL_API_KEY"], personal_marker)
        self.assertEqual(deployed["personalSetting"], {"nested": 7})
        self.assertIs(deployed["enabledPlugins"]["personal@local"], True)
        self.assertIs(deployed["enabledPlugins"]["mempalace@mempalace"], False)
        self.assertEqual(deployed["env"]["ENABLE_TOOL_SEARCH"], "true")
        self.assertEqual(self.run_sync().returncode, 0)

    def test_settings_merge_removes_retired_owned_keys_without_exposing_personal_data(self):
        personal_marker = "retired-migration-value"
        existing = {
            "model": "retired-model",
            "effortLevel": "retired-effort",
            "statusLine": {"type": "command", "command": "retired-statusline"},
            "permissions": {"defaultMode": "acceptEdits"},
            "extraKnownMarketplaces": {"mempalace": {"source": "retired-source"}},
            "env": {
                "MEMPALACE_EMBEDDING_MODEL": "retired-embedder",
                "PERSONAL_API_KEY": personal_marker,
            },
            "enabledPlugins": {"personal@local": True},
            "theme": "dark",
        }
        (self.target / "settings.json").write_text(json.dumps(existing), encoding="utf-8")

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertNotIn(personal_marker, completed.stdout + completed.stderr)
        deployed = json.loads((self.target / "settings.json").read_text(encoding="utf-8"))
        for retired in ("model", "effortLevel", "statusLine", "permissions", "extraKnownMarketplaces"):
            self.assertNotIn(retired, deployed)
        self.assertNotIn("MEMPALACE_EMBEDDING_MODEL", deployed["env"])
        self.assertEqual(deployed["env"]["PERSONAL_API_KEY"], personal_marker)
        self.assertIs(deployed["enabledPlugins"]["personal@local"], True)
        self.assertEqual(deployed["theme"], "dark")
        self.assertEqual(self.run_sync().returncode, 0)


if __name__ == "__main__":
    unittest.main()
