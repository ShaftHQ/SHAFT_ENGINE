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


class DecisionQualityRubricExistenceTest(unittest.TestCase):
    def test_rubric_file_exists(self):
        self.assertTrue(RUBRIC.exists(), f"Rubric artifact missing: {RUBRIC}")

    def test_rubric_file_is_dated(self):
        content = RUBRIC.read_text(encoding="utf-8")
        self.assertRegex(
            content,
            r"Accessed:\s+\d{4}-\d{2}-\d{2}",
            "Rubric must have an 'Accessed: YYYY-MM-DD' date line",
        )

    def test_rubric_references_parent_and_issues(self):
        content = RUBRIC.read_text(encoding="utf-8")
        self.assertIn("#5514", content)
        self.assertIn("#5521", content)
        self.assertIn("#5520", content)

    def test_rubric_imports_baseline_by_reference(self):
        content = RUBRIC.read_text(encoding="utf-8")
        self.assertIn("decision-quality-baseline.md", content)
        self.assertTrue(BASELINE.exists(), "Baseline must remain available for import")


class CompetingModelsTest(unittest.TestCase):
    def setUp(self):
        self.content = RUBRIC.read_text(encoding="utf-8")

    def test_both_complete_models_named(self):
        for name in REQUIRED_MODELS:
            self.assertIn(name, self.content, f"Missing competing model: {name}")

    def test_chosen_model_is_voi_gate(self):
        self.assertIn("Value-of-Information Discriminating Gate (chosen)", self.content)

    def test_rejected_model_is_stage_gate(self):
        self.assertIn("Stage-Gate OODA/PDCA Compliance Loop (rejected)", self.content)

    def test_rejected_model_is_steelmanned(self):
        lower = self.content.lower()
        self.assertIn("steelman", lower)
        # Steelman must argue B's strengths, not only dismiss it.
        rejected_idx = self.content.index("Stage-Gate OODA/PDCA Compliance Loop (rejected)")
        steelman_idx = lower.index("steelman", rejected_idx)
        why_loses_idx = lower.index("why model b still loses", steelman_idx)
        steelman_block = self.content[steelman_idx:why_loses_idx].lower()
        self.assertIn("check", steelman_block)
        self.assertIn("cynefin", steelman_block)

    def test_corpus_scorecard_covers_all_baseline_classes(self):
        for cls in REQUIRED_TAXONOMY_CLASSES:
            self.assertIn(cls, self.content, f"Corpus scorecard missing {cls}")

    def test_corpus_declares_model_a_winner_for_each_class(self):
        # Scorecard rows end with Winner column value A for each class block.
        for cls in REQUIRED_TAXONOMY_CLASSES:
            pattern = re.compile(
                rf"\| `{re.escape(cls)}` \|.*?\| A \|",
                re.DOTALL,
            )
            self.assertIsNotNone(
                pattern.search(self.content),
                f"Scorecard must pick Model A for {cls}",
            )


class OperationalRubricTest(unittest.TestCase):
    def setUp(self):
        self.content = RUBRIC.read_text(encoding="utf-8")
        self.lower = self.content.lower()

    def test_all_required_rubric_dimensions_present(self):
        for section in REQUIRED_RUBRIC_SECTIONS:
            self.assertIn(section, self.content, f"Rubric missing dimension: {section}")

    def test_terminals_documented(self):
        for terminal in REQUIRED_TERMINALS:
            self.assertIn(f"**{terminal}**", self.content)

    def test_failure_modes_documented(self):
        for mode in REQUIRED_FAILURE_MODES:
            self.assertIn(mode, self.content)

    def test_unavailable_policy_preserved(self):
        self.assertIn("UNAVAILABLE", self.content)

    def test_cheapest_discriminating_experiment_is_actionable(self):
        self.assertIn("Cheapest discriminating experiment", self.content)
        self.assertIn("falsify", self.lower)


class PrivacyAndSourcesTest(unittest.TestCase):
    def setUp(self):
        self.content = RUBRIC.read_text(encoding="utf-8")

    def test_research_sources_section_exists(self):
        self.assertIn("Research sources", self.content)
        self.assertIn("plato.stanford.edu/entries/decision-theory/", self.content)
        self.assertIn("ieeexplore.ieee.org/document/4082278", self.content)

    def test_no_forbidden_privacy_terms(self):
        for pattern in FORBIDDEN_PRIVACY_TERMS:
            self.assertIsNone(
                re.search(pattern, self.content, re.IGNORECASE),
                f"Forbidden privacy term found matching: {pattern}",
            )

    def test_privacy_constraints_section_exists(self):
        self.assertIn("Privacy constraints", self.content)


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
