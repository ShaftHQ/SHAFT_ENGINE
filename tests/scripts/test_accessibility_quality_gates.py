from __future__ import annotations

import json
import unittest
from pathlib import Path

from scripts.ci import accessibility_quality_gates as gates


ROOT = Path(__file__).resolve().parents[2]
SAMPLE = ROOT / "tests/fixtures/accessibility_quality_gates_sample.json"


def _valid_surface(**overrides: object) -> dict[str, object]:
    surface = dict(json.loads(SAMPLE.read_text(encoding="utf-8"))["surfaces"][0])
    surface.update(overrides)
    return surface


class AccessibilityQualityGatesTest(unittest.TestCase):
    def test_sample_manifest_passes(self) -> None:
        self.assertEqual(0, gates.main([str(SAMPLE)]))

    def test_low_contrast_fails(self) -> None:
        errors = gates.evaluate_surface(
            _valid_surface(foreground="#777777", background="#888888")
        )
        self.assertTrue(any("contrast" in error for error in errors))

    def test_missing_partial_state_fails(self) -> None:
        surface = _valid_surface()
        surface["states"] = [s for s in surface["states"] if s != "partial"]
        errors = gates.evaluate_surface(surface)
        self.assertTrue(any("partial" in error for error in errors))

    def test_missing_labeling_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(labeledControls=False))
        self.assertTrue(any("labeledControls" in error for error in errors))

    def test_missing_status_messages_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(statusMessages=False))
        self.assertTrue(any("statusMessages" in error for error in errors))

    def test_narrow_layout_required(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(narrowLayoutOk=False))
        self.assertTrue(any("narrowLayoutOk" in error for error in errors))

    def test_missing_reduced_motion_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(reducedMotionEquivalent=False))
        self.assertTrue(any("reducedMotionEquivalent" in error for error in errors))

    def test_layout_shift_animation_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(avoidsLayoutShiftAnimation=False))
        self.assertTrue(any("avoidsLayoutShiftAnimation" in error for error in errors))

    def test_motion_gating_content_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(motionGatesContent=True))
        self.assertTrue(any("motion must not gate content" in error for error in errors))

    def test_unjustified_motion_fails(self) -> None:
        errors = gates.evaluate_surface(
            _valid_surface(motionPurpose="none", maxMotionDurationMs=120)
        )
        self.assertTrue(any("forbids timed motion" in error for error in errors))

    def test_layout_shift_budget_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(maxLayoutShift=0.25))
        self.assertTrue(any("maxLayoutShift" in error for error in errors))

    def test_motion_budget_fails(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(maxMotionDurationMs=900))
        self.assertTrue(any("maxMotionDurationMs" in error for error in errors))

    def test_performance_evidence_required(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(hasPerformanceEvidence=False))
        self.assertTrue(any("hasPerformanceEvidence" in error for error in errors))

    def test_visual_regression_evidence_required(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(hasVisualRegressionEvidence=False))
        self.assertTrue(any("hasVisualRegressionEvidence" in error for error in errors))

    def test_consistent_action_labels_required(self) -> None:
        errors = gates.evaluate_surface(_valid_surface(actionLabelsConsistent=False))
        self.assertTrue(any("actionLabelsConsistent" in error for error in errors))


if __name__ == "__main__":
    unittest.main()
