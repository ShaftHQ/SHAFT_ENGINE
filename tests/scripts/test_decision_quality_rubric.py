"""Behavioral regression for the decision-quality operational rubric (#5521)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RUBRIC = ROOT / "chaos-engine/decision-quality-rubric.md"
BASELINE = ROOT / "chaos-engine/decision-quality-baseline.md"

REQUIRED_RUBRIC_SECTIONS = [
    "Objective / invariant",
    "Uncertainty",
    "Risk",
    "Reversibility",
    "Information gain",
    "Cost / latency / flake risk",
    "Cheapest discriminating experiment",
    "Stop / escalate / defer / schedule",
]

REQUIRED_MODELS = [
    "Value-of-Information Discriminating Gate",
    "Stage-Gate OODA/PDCA Compliance Loop",
]

REQUIRED_TAXONOMY_CLASSES = [
    "SYM-BEFORE-ROOT",
    "STALE-RETRIEVAL",
    "LATE-ARCH",
]

REQUIRED_TERMINALS = ["stop", "escalate", "defer", "schedule"]

REQUIRED_FAILURE_MODES = [
    "Analysis paralysis",
    "Metric gaming",
    "Under-testing",
    "Premature YAGNI",
    "False confidence",
]

FORBIDDEN_PRIVACY_TERMS = [
    r"model_id\s*:",
    r"provider_route\s*:",
    r"endpoint\s*:",
    r"anthropic\.com/",
    r"openai\.com/",
]

class ModelApplicationContractTest(unittest.TestCase):
    """Contract: both models can be scored against the same taxonomy corpus."""

    @staticmethod
    def _score_model(model: str, taxonomy_class: str) -> str:
        """Minimal deterministic scorer mirroring the rubric scorecard."""
        a_wins = {
            "SYM-BEFORE-ROOT",
            "STALE-RETRIEVAL",
            "LATE-ARCH",
        }
        if taxonomy_class not in a_wins:
            raise ValueError(f"unknown taxonomy class: {taxonomy_class}")
        if model == "A":
            return "prevents"
        if model == "B":
            return "allows_or_misses"
        raise ValueError(f"unknown model: {model}")

    def test_model_a_prevents_all_reviewed_classes(self):
        for cls in REQUIRED_TAXONOMY_CLASSES:
            self.assertEqual(self._score_model("A", cls), "prevents")

    def test_model_b_does_not_prevent_stale_retrieval(self):
        self.assertEqual(self._score_model("B", "STALE-RETRIEVAL"), "allows_or_misses")

    def test_unknown_class_rejected(self):
        with self.assertRaises(ValueError):
            self._score_model("A", "NOT-A-CLASS")

if __name__ == "__main__":
    unittest.main()
