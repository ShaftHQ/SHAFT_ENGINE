"""Quality layers for #6990: blocking diff-scoped analysis, weekly trend."""

import importlib.util
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("qt", ROOT / "scripts/ci/quality_trend.py")
qt = importlib.util.module_from_spec(spec)
spec.loader.exec_module(qt)


class QualityLayersTest(unittest.TestCase):
    def test_static_analysis_blocks_through_the_summary(self) -> None:
        jobs = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))["jobs"]
        self.assertIn("static-analysis", jobs["summary"]["needs"])
        run = " ".join(step.get("run", "") for step in jobs["static-analysis"]["steps"])
        self.assertIn("code_quality_preflight.py --base", run)

    def test_codacy_failure_or_coverage_drop_is_a_regression(self) -> None:
        rows = qt.codacy_rows([{"conclusion": "failure", "output": {"title": "2 new issues (0 max.)"}}])
        self.assertEqual(2, rows[0]["new_issues"])
        self.assertTrue(qt.summarize(rows, 70.0, 61.0)["regression"])
        self.assertTrue(qt.summarize([], 60.0, 61.0)["regression"])
        self.assertFalse(qt.summarize([], 62.0, 61.0)["regression"])

    def test_weekly_workflow_tracks_quality_regressions(self) -> None:
        text = (ROOT / ".github/workflows/pr-time-to-merge.yml").read_text(encoding="utf-8")
        self.assertIn("quality_trend.py", text)
        self.assertIn("ci-quality", text)


if __name__ == "__main__":
    unittest.main()
