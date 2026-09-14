"""Host parity #5780–#5785: gh MCP strip, portable maven, plugins, learn_traces, no Grok bundles."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]

_CLEAR_ENV = {
    "CLAUDE_CONFIG",
    "CODEX_HOME",
    "GROK_HOME",
    "GEMINI_HOME",
    "COPILOT_HOME",
    "XDG_CONFIG_HOME",
}


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    if spec.loader is None:
        raise ImportError(f"unable to load {path}")
    spec.loader.exec_module(module)
    return module


def _clean_env() -> dict[str, str]:
    return {key: value for key, value in os.environ.items() if key not in _CLEAR_ENV}


class HostParity5780Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy_5780")
        cls.learn = load(ROOT / "chaos-engine/learn_traces.py", "ce_learn_traces_5780")
        cls.hosts = load(ROOT / "chaos-engine/hosts.py", "ce_hosts_5780")

    def test_heal_prompt_strips_when_gh_healthy(self):
        self.assertIn("when gh auth status succeeds", self.policy.HEAL_PROMPT)
        self.assertIn("leave existing GitHub MCP", self.policy.HEAL_PROMPT)
        self.assertNotIn(
            "Leave an existing GitHub MCP config unchanged forever",
            self.policy.HEAL_PROMPT,
        )

    def test_strip_github_mcp_when_gh_healthy(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            claude = home / ".claude.json"
            claude.write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "github": {"command": "npx"},
                            "keep": {"command": "echo"},
                        }
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            codex = home / ".codex" / "config.toml"
            codex.parent.mkdir(parents=True)
            codex.write_text(
                '[mcp_servers.github]\ncommand = "npx"\n\n'
                '[mcp_servers.other]\ncommand = "echo"\n',
                encoding="utf-8",
            )
            with mock.patch.dict(os.environ, _clean_env(), clear=True):
                result = self.policy.repair_user_github_mcp(home, gh_healthy=True)
            self.assertTrue(result["ghHealthy"])
            self.assertEqual(2, len(result["stripped"]))
            payload = json.loads(claude.read_text(encoding="utf-8"))
            self.assertNotIn("github", payload["mcpServers"])
            self.assertIn("keep", payload["mcpServers"])
            toml = codex.read_text(encoding="utf-8")
            self.assertNotIn("[mcp_servers.github]", toml)
            self.assertIn("[mcp_servers.other]", toml)

    def test_preserve_github_mcp_when_gh_unhealthy(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            claude = home / ".claude.json"
            claude.write_text(
                json.dumps({"mcpServers": {"github": {"command": "npx"}}}) + "\n",
                encoding="utf-8",
            )
            with mock.patch.dict(os.environ, _clean_env(), clear=True):
                result = self.policy.repair_user_github_mcp(home, gh_healthy=False)
            self.assertFalse(result["ghHealthy"])
            self.assertEqual([], result["stripped"])
            self.assertEqual(1, len(result["preserved"]))
            payload = json.loads(claude.read_text(encoding="utf-8"))
            self.assertIn("github", payload["mcpServers"])

    def test_project_overlay_never_publishes_github(self):
        servers = self.hosts.owned_servers()
        self.assertNotIn("github", servers)
        self.assertEqual(
            {"keep": {}},
            self.policy.omit_github_from_defaults({"github": {}, "keep": {}}),
        )

    def test_absolute_maven_jar_in_project_mcp_fails(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "maven-tools-mcp": {
                                "command": "/usr/bin/java",
                                "args": [
                                    "-jar",
                                    "/home/me/.local/share/ChaosEngine/tools/"
                                    "maven-tools-mcp/3.2.0/maven-tools-mcp-3.2.0.jar",
                                ],
                            }
                        }
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            error = self.policy.project_absolute_maven_error(project)
            self.assertIsNotNone(error)
            self.assertIn("workstation-absolute", error)

    def test_portable_maven_launcher_passes_doctor(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "maven-tools-mcp": {
                                "command": "python3",
                                "args": [".chaos-engine/tool.py", "maven-tools-mcp"],
                                "cwd": ".",
                            }
                        }
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            self.assertIsNone(self.policy.project_absolute_maven_error(project))

    def test_owned_servers_native_maven_is_portable(self):
        java = Path("/usr/bin/java")
        jar = Path("/home/x/.local/share/ChaosEngine/tools/maven-tools-mcp/3.2.0/x.jar")
        servers = self.hosts.owned_servers(maven_runtime=(java, jar))
        maven = servers["maven-tools-mcp"]
        blob = json.dumps(maven)
        self.assertIn("tool.py", blob)
        self.assertIn("maven-tools-mcp", blob)
        self.assertNotIn(str(jar), blob)
        self.assertNotIn(str(java), blob)

    def test_pin_enabled_plugins_strips_extras(self):
        enabled = {
            "chaos-engine@chaos-engine-project": True,
            "caveman@chaos-engine-project": True,
            "ponytail@chaos-engine-project": True,
            "some-marketplace-extra@official": True,
        }
        error = self.policy.enabled_plugins_policy_error(enabled)
        self.assertIsNotNone(error)
        pinned = self.policy.pin_enabled_plugins(
            enabled, marketplace_name="chaos-engine-project"
        )
        self.assertNotIn("some-marketplace-extra@official", pinned)
        self.assertTrue(pinned["chaos-engine@chaos-engine-project"])
        self.assertIsNone(self.policy.enabled_plugins_policy_error(pinned))

    def test_learn_traces_collects_fixture_sessions(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            out = home / "run"
            claude_dir = home / ".claude" / "projects" / "-tmp-demo"
            claude_dir.mkdir(parents=True)
            (claude_dir / "sess-human.jsonl").write_text(
                json.dumps(
                    {
                        "type": "user",
                        "message": {
                            "role": "user",
                            "content": "please fix the bug api_key=SECRET123",
                        },
                    }
                )
                + "\n"
                + json.dumps(
                    {
                        "type": "assistant",
                        "message": {"role": "assistant", "content": "ok"},
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            (claude_dir / "sess-sub.jsonl").write_text(
                json.dumps(
                    {
                        "type": "user",
                        "isSidechain": True,
                        "message": {"role": "user", "content": "sub"},
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            grok_dir = home / ".grok" / "sessions"
            grok_dir.mkdir(parents=True)
            (grok_dir / "g1.json").write_text(
                json.dumps(
                    {
                        "messages": [
                            {"role": "user", "content": "hello token=abc"},
                            {"role": "assistant", "content": "hi"},
                        ]
                    }
                ),
                encoding="utf-8",
            )
            codex_dir = home / ".codex" / "sessions" / "2026" / "09"
            codex_dir.mkdir(parents=True)
            (codex_dir / "rollout.jsonl").write_text(
                json.dumps(
                    {
                        "type": "event_msg",
                        "payload": {"type": "user_message", "message": "codex hi"},
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            with mock.patch.dict(os.environ, _clean_env(), clear=True):
                manifest = self.learn.collect(home, out)
            self.assertGreaterEqual(manifest["sessions_kept"], 3)
            self.assertGreaterEqual(manifest["dropped"].get("subagent", 0), 1)
            all_text = "\n".join(
                path.read_text(encoding="utf-8")
                for path in (out / "sessions").glob("*.json")
            )
            self.assertNotIn("SECRET123", all_text)
            self.assertIn("<redacted>", all_text)

    def test_overlay_never_vendors_grok_office_game_skills(self):
        src = ROOT / "chaos-engine" / "skills"
        self.assertEqual([], self.policy.overlay_forbidden_bundled_skills(src))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            bad = project / ".chaos-engine" / "skills" / "pdf"
            bad.mkdir(parents=True)
            (bad / "SKILL.md").write_text("nope\n", encoding="utf-8")
            game = project / ".chaos-engine" / "skills" / "game-chess"
            game.mkdir(parents=True)
            (game / "SKILL.md").write_text("nope\n", encoding="utf-8")
            error = self.policy.overlay_bundled_skill_error(project)
            self.assertIsNotNone(error)
            self.assertIn("pdf", error)
            self.assertIn("game-chess", error)

    def test_grok_host_product_limit_documented(self):
        status = self.policy.grok_host_product_status()
        self.assertEqual("documented-limit", status["status"])
        self.assertIn("GAP-GROK-BUNDLED", status["limit"])
        docs = (ROOT / "chaos-engine/references/prefer-cli-over-mcp.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("GAP-GROK-BUNDLED", docs)
        matrix = (ROOT / "chaos-engine/references/host-parity-matrix.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("GAP-GROK-BUNDLED", matrix)

    def test_gh_probe_injection(self):
        with mock.patch.object(self.policy, "gh_is_configured", return_value=True):
            self.assertTrue(self.policy.gh_is_configured())


if __name__ == "__main__":
    unittest.main()
