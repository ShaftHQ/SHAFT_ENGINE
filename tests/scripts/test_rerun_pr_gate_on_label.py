"""Tests for the ci:installer-macos PR Gate rerun helper (#6208)."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

import yaml

from scripts.ci import rerun_pr_gate_on_label as rerun

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github/workflows/installer-macos-rerun.yml"


class FakeGh:
    def __init__(self, runs: list[dict], statuses: list[str] | None = None) -> None:
        self.runs = runs
        self.statuses = list(statuses or [])
        self.calls: list[tuple[str, ...]] = []

    def __call__(self, *args: str, mutate: bool = False) -> str:
        self.calls.append(args)
        if args[0] == "api" and "/workflows/" in args[1]:
            return json.dumps({"workflow_runs": self.runs})
        if args[0] == "api":
            return self.statuses.pop(0) if self.statuses else "completed"
        return ""


class PlanTest(unittest.TestCase):
    def test_no_run_needs_nothing_because_the_gate_reads_labels_live(self) -> None:
        self.assertEqual("none", rerun.plan_action(None))

    def test_completed_run_is_rerun_and_active_run_is_cancelled_first(self) -> None:
        self.assertEqual("rerun", rerun.plan_action({"status": "completed"}))
        for status in sorted(rerun.ACTIVE_STATUSES):
            with self.subTest(status=status):
                self.assertEqual("cancel-then-rerun", rerun.plan_action({"status": status}))

    def test_newest_run_wins(self) -> None:
        runs = [{"id": 1, "created_at": "2026-09-25T00:00:00Z"}, {"id": 2, "created_at": "2026-09-25T01:00:00Z"}]
        self.assertEqual(2, rerun.newest_run(runs)["id"])
        self.assertIsNone(rerun.newest_run([]))

    def test_wait_polls_until_completed_or_times_out(self) -> None:
        gh = FakeGh([], ["in_progress", "in_progress", "completed"])
        self.assertTrue(rerun.wait_until_completed(gh, "o/r", 7, interval=1, sleep=lambda _: None))
        gh = FakeGh([], ["in_progress"] * 10)
        self.assertFalse(rerun.wait_until_completed(gh, "o/r", 7, timeout=3, interval=1, sleep=lambda _: None))


class WorkflowTest(unittest.TestCase):
    def workflow(self) -> dict:
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        if True in workflow:
            workflow["on"] = workflow.pop(True)
        return workflow

    def test_only_the_opt_in_label_on_same_repo_prs_triggers_a_rerun(self) -> None:
        workflow = self.workflow()
        self.assertEqual({"pull_request": {"types": ["labeled"]}}, workflow["on"])
        job = workflow["jobs"]["rerun"]
        self.assertIn("github.event.label.name == 'ci:installer-macos'", job["if"])
        self.assertIn("head.repo.full_name == github.repository", job["if"])
        self.assertEqual({"actions": "write", "contents": "read"}, job["permissions"])
        self.assertEqual({"contents": "read"}, workflow["permissions"])

    def test_the_label_name_matches_the_installer_tier(self) -> None:
        from scripts.ci import chaos_installer_tier as tier

        self.assertIn("ci:installer-macos", WORKFLOW.read_text(encoding="utf-8"))
        self.assertIn("macos-15", tier.installer_os("pull_request", {"ci:installer-macos"}))


if __name__ == "__main__":
    unittest.main()
