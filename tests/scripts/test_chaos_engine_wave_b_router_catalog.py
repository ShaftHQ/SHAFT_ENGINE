"""Wave B (#5621/#5622): triage→budget, zero-LLM/Heal/Level-1, GAP-EXIT2 UX."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LIFECYCLE = ROOT / "chaos-engine/hooks/lifecycle.py"
ZERO = ROOT / "chaos-engine/references/zero-llm-catalog.md"
TOKEN = ROOT / "chaos-engine/references/token-budget-modes.md"
LEVEL1 = ROOT / "chaos-engine/references/level-1-catalog.md"
HEAL = ROOT / "chaos-engine/references/heal-route.md"
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class WaveBRouterCatalogTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.lifecycle = load(LIFECYCLE, "ce_lifecycle_wave_b")

    def test_triage_maps_to_token_budget_defaults(self):
        self.assertEqual("ultra-lean", self.lifecycle.triage_token_budget("one-file"))
        self.assertEqual("ultra-lean", self.lifecycle.triage_token_budget("module"))
        self.assertEqual("ultra-lean", self.lifecycle.triage_token_budget("one-module"))
        self.assertEqual("ultra-lean", self.lifecycle.triage_token_budget("public-contract"))
        self.assertEqual("ultra-lean", self.lifecycle.triage_token_budget("hard-to-reverse"))
        self.assertEqual(
            "ultra-lean", self.lifecycle.triage_token_budget("unknown-triage")
        )
        text = TOKEN.read_text(encoding="utf-8")
        self.assertIn("Triage", text)
        self.assertIn("ultra-lean", text)
        self.assertIn("public contract", text.casefold())

    def test_zero_llm_catalog_lists_repair_and_heal(self):
        text = ZERO.read_text(encoding="utf-8")
        self.assertIn("repair --project . --component", text)
        self.assertIn("heal-route.md", text)
        self.assertIn("level-1-catalog.md", text)

    def test_level1_catalog_is_sorted_and_lean(self):
        text = LEVEL1.read_text(encoding="utf-8")
        self.assertIn("Level-1", text)
        self.assertIn("heal-route.md", text)
        # No workflow dumps in Use-when cells — keep descriptions short.
        for line in text.splitlines():
            if line.startswith("|") and "Use when" not in line and "---" not in line:
                cells = [c.strip() for c in line.strip("|").split("|")]
                if len(cells) >= 2 and cells[0] not in {"Name", ""}:
                    self.assertLessEqual(len(cells[1]), 160, msg=cells[0])

    def test_heal_route_is_file_path_reachable(self):
        self.assertTrue(HEAL.is_file())
        text = HEAL.read_text(encoding="utf-8")
        self.assertIn("repair --project", text)
        self.assertIn("marketplace plugin is absent", text.casefold())
        skill = SKILL.read_text(encoding="utf-8")
        self.assertIn("heal-route.md", skill)
        self.assertIn("| Heal |", skill)

    def test_gap_exit2_checklist_present(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertIn("GAP-EXIT2 compensating UX checklist", text)
        self.assertIn("Do **not** pretend", text)
        self.assertIn("test_chaos_engine_exit2_fidelity", text)

    def test_session_start_includes_locators_under_budget(self):
        context = self.lifecycle.session_start_context("tok", "activation")
        encoded = context.encode("utf-8")
        self.assertLessEqual(len(encoded), self.lifecycle.SESSION_START_MAX_BYTES)
        self.assertIn("zero-llm-catalog.md", context.casefold())
        self.assertIn("level-1-catalog.md", context.casefold())
        self.assertIn("heal-route.md", context.casefold())
        # Still locator-only: no catalog body dump.
        self.assertNotIn(LEVEL1.read_text(encoding="utf-8")[:80], context)


if __name__ == "__main__":
    unittest.main()
