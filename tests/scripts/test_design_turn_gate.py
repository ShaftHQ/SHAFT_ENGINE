"""Unit tests for local-agency design_turn_gate (#intellij-llama coach)."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HELPER = ROOT / "chaos-engine/skills/local-agency/scripts/design_turn_gate.py"


def _load():
    spec = importlib.util.spec_from_file_location("design_turn_gate", HELPER)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


class DesignTurnGateTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.g = _load()

    def test_citation_pass_order_insensitive(self):
        brief = "a.md\nb.md\n"
        writer = "body\n\nCE_BRIEF_LOCATORS: b.md | a.md\n"
        result = self.g.gate_citation(writer, brief)
        self.assertTrue(result["ok"])

    def test_citation_reject_product_labels(self):
        brief = "chaos-engine/identity.md\n"
        writer = "body\nCE_BRIEF_LOCATORS: SHAFT_ENGINE | CHAOS_ENGINE\n"
        result = self.g.gate_citation(writer, brief)
        self.assertFalse(result["ok"])
        self.assertEqual(result["gate"], "brief_citation")

    def test_citation_reject_missing_line(self):
        result = self.g.gate_citation("no closing", "a.md\n")
        self.assertFalse(result["ok"])

    def test_schema_requires_headers(self):
        bad = "## Goal\n\n## Context\n"
        result = self.g.gate_schema(bad, self.g.SPEC_HEADERS)
        self.assertFalse(result["ok"])
        self.assertIn("Acceptance", result["missing"])

    def test_schema_pass(self):
        text = "\n".join(f"## {h}\n" for h in self.g.SPEC_HEADERS)
        result = self.g.gate_schema(text, self.g.SPEC_HEADERS)
        self.assertTrue(result["ok"])


if __name__ == "__main__":
    unittest.main()
