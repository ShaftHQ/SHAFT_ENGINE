"""Tests for the PR time-to-merge report (#6990 phase 2)."""

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("ttm", ROOT / "scripts/ci/pr_time_to_merge.py")
ttm = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ttm)


def pr(minutes: int) -> dict:
    return {"created_at": "2026-10-15T10:00:00Z",
            "merged_at": f"2026-10-15T{10 + minutes // 60:02d}:{minutes % 60:02d}:00Z"}


class TimeToMergeTest(unittest.TestCase):
    def test_median_within_target_is_ok(self) -> None:
        summary = ttm.summarize([pr(5), pr(10), pr(40)])
        self.assertEqual(10.0, summary["median_minutes"])
        self.assertFalse(summary["breach"])

    def test_median_above_target_breaches(self) -> None:
        self.assertTrue(ttm.summarize([pr(20), pr(30), pr(5)])["breach"])

    def test_unmerged_and_empty_are_ignored(self) -> None:
        self.assertFalse(ttm.summarize([{"created_at": "2026-10-15T10:00:00Z", "merged_at": None}])["breach"])

    def test_workflow_alerts_through_one_issue(self) -> None:
        text = (ROOT / ".github/workflows/pr-time-to-merge.yml").read_text(encoding="utf-8")
        self.assertIn("ci-pr-time", text)
        self.assertIn("timeout-minutes", text)


if __name__ == "__main__":
    unittest.main()
