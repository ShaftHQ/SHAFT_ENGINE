"""CLI over MCP, user-home skill collisions, portable learn/deep-research runners."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    if spec.loader is None:
        raise ImportError(f"unable to load {path}")
    spec.loader.exec_module(module)
    return module


class CliOverMcpTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy_cli")
        cls.learn = load(ROOT / "chaos-engine/learn_traces.py", "ce_learn_traces")
        cls.research = load(ROOT / "chaos-engine/deep_research.py", "ce_deep_research")

    def test_heal_prompt_prefers_gh_and_cli(self):
        self.assertIn("CLI over MCP", self.policy.HEAL_PROMPT)
        self.assertIn("gh exists and is configured", self.policy.HEAL_PROMPT)
        self.assertNotIn("Leave an existing GitHub MCP config unchanged", self.policy.HEAL_PROMPT)

    def test_user_skill_collision_detected(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            target = home / ".claude" / "skills" / "graphify"
            target.mkdir(parents=True)
            (target / "SKILL.md").write_text("x\n", encoding="utf-8")
            error = self.policy.user_skill_collision_error(home)
            self.assertIsNotNone(error)
            self.assertIn("graphify", error)

    def test_user_skill_collision_absent_when_clean(self):
        with tempfile.TemporaryDirectory() as temporary:
            self.assertIsNone(self.policy.user_skill_collision_error(Path(temporary)))

    def test_learn_traces_collect_writes_manifest(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            out = home / "run"
            manifest = self.learn.collect(home, out)
            self.assertEqual(1, manifest["schemaVersion"])
            self.assertTrue((out / "manifest.json").is_file())
            self.assertIn("sessions", manifest)
            self.assertIn("dropped", manifest)

    def test_deep_research_init_writes_run(self):
        with tempfile.TemporaryDirectory() as temporary:
            out = Path(temporary) / "run"
            run = self.research.init("token skills", out, 4)
            self.assertEqual("token skills", run["query"])
            self.assertEqual(["plan", "research", "verify", "report"], run["phases"])
            self.assertTrue((out / "run.json").is_file())

    def test_omit_github_from_defaults(self):
        servers = {"github": {}, "keep": {}}
        self.assertEqual({"keep": {}}, self.policy.omit_github_from_defaults(servers))

    def test_router_lists_prefer_cli_and_runners(self):
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("prefer-cli-over-mcp", skill)
        learn = (ROOT / "chaos-engine/references/learn-traces.md").read_text(encoding="utf-8")
        self.assertIn("learn_traces.py", learn)
        research = (ROOT / "chaos-engine/references/deep-research.md").read_text(encoding="utf-8")
        self.assertIn("deep_research.py", research)


if __name__ == "__main__":
    unittest.main()
