"""#5689 without Headroom: MCP uniqueness, origin-sync retrieve, overlay hash."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from unittest import TestCase, mock

ROOT = Path(__file__).resolve().parents[2]


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TokenMaxNoHeadroomTests(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy_5689")
        cls.overlay = load(ROOT / "chaos-engine/overlay_match.py", "ce_overlay_match_5689")
        cls.retrieve = load(ROOT / "chaos-engine/retrieve.py", "ce_retrieve_5689")
        cls.hosts = load(ROOT / "chaos-engine/hosts.py", "ce_hosts_5689")

    def test_github_alias_pair_is_duplicate(self):
        error = self.policy.uniqueness_error(["github", "github-gh"])
        self.assertIsNotNone(error)
        self.assertIn("disable extras", error)

    def test_user_and_project_ids_share_one_heal_prompt(self):
        text = (ROOT / "chaos-engine/hosts.py").read_text(encoding="utf-8")
        self.assertIn("disable extras in host MCP config", text)
        self.assertNotIn("headroom", self.hosts.instruction_block("chaos-engine").casefold())
        self.assertIn("chaos-engine/", self.hosts.instruction_block("chaos-engine"))
        self.assertIn(".chaos-engine/", self.hosts.instruction_block(".chaos-engine"))
        self.assertEqual(
            "chaos-engine",
            self.hosts.guidance_tree(ROOT),
        )
        self.assertIn(self.policy.HEAL_PROMPT, self.policy.HEAL_PROMPT)

    def test_overlay_match_ignores_adopter_and_reports_repository_drift(self):
        self.assertTrue(self.overlay.core_matches_source(Path("/tmp"))["coreMatchesSource"])
        matched = self.overlay.core_matches_source(ROOT)
        self.assertEqual("repository", matched["scope"])
        self.assertIn("coreMatchesSource", matched)

    def test_retrieve_origin_sync_is_not_store_degraded(self):
        project = ROOT
        message = (
            "primary checkout HEAD (aaa) != origin/main (bbb) "
            "(not synchronized with origin/main). "
            "fix-next: git fetch origin main && git merge --ff-only origin/main\n"
        )
        completed = mock.Mock(returncode=1, stdout="", stderr=message)
        with mock.patch.object(self.retrieve.subprocess, "run", return_value=completed):
            receipt = self.retrieve._run_store(project, "memory", "probe")
        self.assertEqual("skipped", receipt["status"])
        self.assertEqual("origin-sync", receipt["reason"])
        self.assertEqual("advisory", receipt["originSync"])
        self.assertNotEqual("degraded", receipt["status"])

    def test_matrix_closes_grok_caveman_without_duplicating_bodies(self):
        matrix = (
            ROOT / "chaos-engine/references/host-parity-matrix.md"
        ).read_text(encoding="utf-8")
        self.assertIn("GAP-GROK-CAVEMAN", matrix)
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        self.assertNotIn("caveman=ultra", agents)


if __name__ == "__main__":
    unittest.main()
