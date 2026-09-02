from __future__ import annotations

import json
import unittest
from pathlib import Path

from scripts.ci import accessibility_quality_gates as gates


ROOT = Path(__file__).resolve().parents[2]
SAMPLE = ROOT / "tests/fixtures/accessibility_quality_gates_sample.json"


class AccessibilityQualityGatesTest(unittest.TestCase):
    def test_sample_manifest_passes(self) -> None:
        self.assertEqual(0, gates.main([str(SAMPLE)]))

    def test_low_contrast_fails(self) -> None:
        surface = {
            "name": "bad-contrast",
            "foreground": "#777777",
            "background": "#888888",
            "states": ["loading", "empty", "error", "recovery", "success"],
            "minTargetPx": 24,
            "keyboardAccessible": True,
            "visibleFocus": True,
            "reducedMotionEquivalent": True,
            "motionGatesContent": False,
        }
        errors = gates.evaluate_surface(surface)
        self.assertTrue(any("contrast" in error for error in errors))

    def test_missing_reduced_motion_fails(self) -> None:
        surface = dict(json.loads(SAMPLE.read_text(encoding="utf-8"))["surfaces"][0])
        surface["reducedMotionEquivalent"] = False
        errors = gates.evaluate_surface(surface)
        self.assertTrue(any("reducedMotionEquivalent" in error for error in errors))

    def test_motion_gating_content_fails(self) -> None:
        surface = dict(json.loads(SAMPLE.read_text(encoding="utf-8"))["surfaces"][0])
        surface["motionGatesContent"] = True
        errors = gates.evaluate_surface(surface)
        self.assertTrue(any("motion must not gate content" in error for error in errors))
