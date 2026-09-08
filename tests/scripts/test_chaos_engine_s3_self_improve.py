"""Wave S3 (#5661/#5662): context-firewall guidance + product-track playbook."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
FIREWALL = ROOT / "chaos-engine/references/context-firewall.md"
PRODUCT = ROOT / "chaos-engine/skills/self-improve/references/product-track.md"
PROFILE_PRODUCT = ROOT / "chaos-engine/profiles/shaft/references/product-track.md"
LEVEL1 = ROOT / "chaos-engine/references/level-1-catalog.md"
CE_SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
SI_SKILL = ROOT / "chaos-engine/skills/self-improve/SKILL.md"
AGENTS = ROOT / "AGENTS.md"
DELEGATION = ROOT / "chaos-engine/references/delegation.md"
CONTEXT = ROOT / "chaos-engine/references/context-economy.md"
ROUTING = ROOT / "chaos-engine/profiles/shaft/references/routing.md"
TAXONOMY = ROOT / "chaos-engine/skills/self-improve/references/observation-taxonomy.md"
RECEIPT = ROOT / "chaos-engine/references/research-receipt.md"


class S3SelfImproveTests(unittest.TestCase):
    def test_context_firewall_doc_invariants(self):
        self.assertTrue(FIREWALL.is_file())
        body = FIREWALL.read_text(encoding="utf-8")
        for needle in (
            "filepath:line",
            "Task Observer",
            "subagent",
            "distillate",
            "Reject",
            "all hosts",
        ):
            self.assertIn(needle, body)
        # Must not authorize always-on observation
        self.assertNotIn("always-on Observer accept", body)
        self.assertIn("never always-on", body.lower())

    def test_product_track_playbook_invariants(self):
        self.assertTrue(PRODUCT.is_file())
        body = PRODUCT.read_text(encoding="utf-8")
        for needle in (
            "ChaosGauge",
            "silent-verify",
            "doctor",
            "CLI",
            "learning.py",
            "Reject Task Observer",
            "harness queued N / product queued N",
            "gh issue create",
        ):
            self.assertIn(needle, body)
        self.assertIn("essay", body.lower())
        self.assertIn("MCP", body)
        self.assertNotIn("shaft", body.casefold())
        self.assertTrue(PROFILE_PRODUCT.is_file())
        profile = PROFILE_PRODUCT.read_text(encoding="utf-8")
        self.assertIn("SHAFT", profile)
        self.assertIn("ChaosGauge", profile)

    def test_overlay_wiring_all_hosts(self):
        level1 = LEVEL1.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", level1)
        self.assertIn("Context firewall", level1)

        skill = CE_SKILL.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", skill)
        self.assertIn("filepath:line", skill)

        agents = AGENTS.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", agents)

        delegation = DELEGATION.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", delegation)
        self.assertIn("Research / explore firewall", delegation)

        context = CONTEXT.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", context)

        receipt = RECEIPT.read_text(encoding="utf-8")
        self.assertIn("context-firewall.md", receipt)

        si = SI_SKILL.read_text(encoding="utf-8")
        self.assertIn("product-track.md", si)
        self.assertIn("context-firewall.md", si)

        tax = TAXONOMY.read_text(encoding="utf-8")
        self.assertIn("product-track.md", tax)

        routing = ROUTING.read_text(encoding="utf-8")
        self.assertIn("product-track.md", routing)
        self.assertIn("context-firewall.md", routing)

    def test_no_observer_language_in_new_docs(self):
        for path in (FIREWALL, PRODUCT):
            lower = path.read_text(encoding="utf-8").lower()
            self.assertIn("reject", lower)
            self.assertIn("task observer", lower)
            # Must not instruct continuous observation
            self.assertNotIn("always-on sessionstart full scan", lower)


if __name__ == "__main__":
    unittest.main()
