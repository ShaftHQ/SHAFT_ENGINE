"""Tests for the PR time-to-merge report (#6990 / #7184)."""

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("ttm", ROOT / "scripts/ci/pr_time_to_merge.py")
ttm = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ttm)


def pr(minutes: int, *, bot: bool = False, last: int | None = None, number: int = 1) -> dict:
    item = {
        "number": number,
        "created_at": "2026-10-15T10:00:00Z",
        "merged_at": f"2026-10-15T{10 + minutes // 60:02d}:{minutes % 60:02d}:00Z",
        "user": {"login": "dependabot[bot]" if bot else "human"},
    }
    if last is not None:
        item["last_commit_at"] = f"2026-10-15T{10 + last // 60:02d}:{last % 60:02d}:00Z"
    return item


class TimeToMergeTest(unittest.TestCase):
    def test_primary_uses_last_commit_for_non_bots(self) -> None:
        # open→merge would be 40/10/5; last-commit→merge is 10/10/5 → median 10
        summary = ttm.summarize(
            [
                pr(40, last=30, number=1),  # last→merge 10
                pr(10, last=0, number=2),  # last→merge 10
                pr(5, last=0, number=3),  # last→merge 5
            ]
        )
        self.assertEqual("last_commit_to_merge_non_bot", summary["metric"])
        self.assertEqual(10.0, summary["median_minutes"])
        self.assertFalse(summary["breach"])
        self.assertEqual(10.0, summary["open_to_merge_non_bot"]["median_minutes"])

    def test_bots_excluded_from_primary(self) -> None:
        summary = ttm.summarize(
            [
                pr(50, bot=True, last=0, number=1),
                pr(8, last=0, number=2),
                pr(9, last=0, number=3),
                pr(10, last=0, number=4),
            ]
        )
        self.assertEqual(3, summary["count"])
        self.assertEqual(9.0, summary["median_minutes"])
        self.assertFalse(summary["breach"])

    def test_median_above_target_breaches(self) -> None:
        self.assertTrue(ttm.summarize([pr(20, last=0), pr(30, last=0), pr(5, last=0)])["breach"])

    def test_fallback_without_last_commit(self) -> None:
        summary = ttm.summarize([pr(5), pr(10), pr(40)])
        self.assertEqual("open_to_merge_fallback", summary["metric"])
        self.assertEqual(10.0, summary["median_minutes"])
        self.assertFalse(summary["breach"])

    def test_unmerged_and_empty_are_ignored(self) -> None:
        self.assertFalse(
            ttm.summarize([{"created_at": "2026-10-15T10:00:00Z", "merged_at": None, "user": {"login": "h"}}])[
                "breach"
            ]
        )

    def test_avg_pr_gate_runs(self) -> None:
        summary = ttm.summarize(
            [pr(8, last=0, number=1), pr(9, last=0, number=2)],
            gate_runs={1: 2, 2: 4},
        )
        self.assertEqual(3.0, summary["avg_pr_gate_runs"])

    def test_workflow_alerts_through_one_issue(self) -> None:
        text = (ROOT / ".github/workflows/pr-time-to-merge.yml").read_text(encoding="utf-8")
        self.assertIn("ci-pr-time", text)
        self.assertIn("timeout-minutes", text)


if __name__ == "__main__":
    unittest.main()
