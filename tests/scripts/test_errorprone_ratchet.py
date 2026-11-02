"""Tests for scripts/ci/errorprone_ratchet.py (#6999)."""
from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("errorprone_ratchet", ROOT / "scripts/ci/errorprone_ratchet.py")
ratchet = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ratchet)

LINE = "[WARNING] /w/shaft-ai/src/main/java/com/shaft/A.java:[{n},3] [NullAway] returning @Nullable"


class ErrorProneRatchetTest(unittest.TestCase):
    def test_counts_per_module_and_check_once_per_line(self) -> None:
        lines = [LINE.format(n=1), LINE.format(n=1), LINE.format(n=2), "[WARNING] unrelated"]
        self.assertEqual({"shaft-ai::NullAway": 2}, dict(ratchet.count(lines)))

    def test_only_changed_modules_are_judged(self) -> None:
        current = ratchet.count([LINE.format(n=1), LINE.format(n=2)])
        self.assertEqual(["shaft-ai::NullAway: 2 > baseline 1"],
                         ratchet.regressions(current, {"shaft-ai::NullAway": 1}, {"shaft-ai"}))
        self.assertEqual([], ratchet.regressions(current, {}, {"shaft-mcp"}))

    def test_baseline_profile_and_gate_leg_are_wired(self) -> None:
        self.assertIsInstance(json.loads((ROOT / ".github/errorprone-baseline.json").read_text()), dict)
        self.assertIn("<id>errorprone</id>", (ROOT / "pom.xml").read_text(encoding="utf-8"))
        jobs = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))["jobs"]
        self.assertIn("bug-analysis", jobs["summary"]["needs"])
        self.assertIn("errorprone_ratchet.py", str(jobs["bug-analysis"]["steps"]))


if __name__ == "__main__":
    unittest.main()
