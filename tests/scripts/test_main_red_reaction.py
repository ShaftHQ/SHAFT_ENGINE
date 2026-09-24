"""Post-merge main runs are trustworthy: never cancelled, red is reacted to (#6185)."""

from __future__ import annotations

import json
import unittest
from pathlib import Path
from unittest.mock import patch

import yaml

from scripts.ci import main_red_reaction as reaction
from scripts.ci.main_red_reaction import Job, build_plan, issue_title, parse_jobs

ROOT = Path(__file__).resolve().parents[2]
PR_GATE = ROOT / ".github/workflows/pr-gate.yml"


def _job(name: str, conclusion: str) -> Job:
    return Job(name, conclusion, f"https://example.invalid/{name}")


class PlanTest(unittest.TestCase):
    def test_failing_leg_without_issue_is_created_and_summary_is_ignored(self) -> None:
        jobs = [
            _job("Unit Tests (shaft-engine)", "failure"),
            _job("PR Gate Summary", "failure"),
            _job("Main red reaction", ""),
        ]
        plan = build_plan(jobs, {})
        self.assertEqual(["Unit Tests (shaft-engine)"], [job.name for job in plan.create])
        self.assertEqual((), plan.comment)
        self.assertFalse(plan.revert)

    def test_existing_issue_is_commented_not_duplicated(self) -> None:
        title = issue_title("Agent Guidance Gate")
        plan = build_plan([_job("Agent Guidance Gate", "timed_out")], {title: 7})
        self.assertEqual((), plan.create)
        self.assertEqual([7], [number for number, _ in plan.comment])

    def test_recovered_leg_closes_only_its_own_issue(self) -> None:
        issues = {
            issue_title("Build IntelliJ plugin"): 11,
            issue_title("Unit Tests (shaft-mcp)"): 12,
        }
        jobs = [_job("Build IntelliJ plugin", "success"), _job("Unit Tests (shaft-mcp)", "skipped")]
        plan = build_plan(jobs, issues)
        self.assertEqual(((11, "Build IntelliJ plugin"),), plan.close)

    def test_installer_leg_failure_requests_a_revert_pr(self) -> None:
        plan = build_plan([_job("ChaosEngine fresh installer (macos-15)", "failure")], {})
        self.assertTrue(plan.revert)

    def test_parse_jobs_reads_gh_json_lines(self) -> None:
        lines = "\n".join(
            json.dumps({"name": name, "conclusion": conclusion, "html_url": "u"})
            for name, conclusion in (("a", "success"), ("b", None))
        )
        self.assertEqual([Job("a", "success", "u"), Job("b", "", "u")], parse_jobs(lines + "\n"))

    def test_issue_body_names_run_commit_pr_and_revert(self) -> None:
        body = reaction.issue_body(
            _job("leg", "failure"),
            {"run_url": "RUN", "sha": "SHA", "pr": "#1 title", "revert": "REVERT"},
        )
        for token in ("RUN", "SHA", "#1 title", "REVERT", "P0"):
            self.assertIn(token, body)

    def test_revert_without_bot_token_is_reported_not_attempted(self) -> None:
        runner = reaction.Runner(dry_run=True)
        with patch.object(runner, "run") as run:
            result = reaction.open_revert_pr(runner, "o/r", "a" * 40, "#1", "")
        run.assert_not_called()
        self.assertIn("BOT_TOKEN", result)

    def test_dry_run_never_mutates(self) -> None:
        runner = reaction.Runner(dry_run=True)
        with patch("scripts.ci.main_red_reaction.subprocess.run") as run:
            self.assertEqual("", runner.run(["gh", "issue", "create"], mutate=True))
        run.assert_not_called()


class WorkflowWiringTest(unittest.TestCase):
    def workflow(self) -> dict:
        return yaml.load(PR_GATE.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)

    def test_main_runs_are_never_cancelled_or_dropped(self) -> None:
        concurrency = self.workflow()["concurrency"]
        self.assertEqual(
            "${{ github.event_name == 'pull_request' }}", concurrency["cancel-in-progress"]
        )
        # One group per main commit: a shared ref group would still drop a
        # queued pending run when a third merge lands.
        self.assertIn("github.event.pull_request.number || github.sha", concurrency["group"])

    def test_reaction_job_follows_summary_on_main_pushes(self) -> None:
        job = self.workflow()["jobs"]["main-red-reaction"]
        self.assertEqual("summary", job["needs"])
        self.assertIn("always()", job["if"])
        self.assertIn("github.event_name == 'push'", job["if"])
        self.assertIn("needs.summary.result == 'failure'", job["if"])
        self.assertEqual("write", job["permissions"]["issues"])
        run = next(step["run"] for step in job["steps"] if "run" in step)
        self.assertIn("scripts/ci/main_red_reaction.py", run)
        self.assertNotIn("main-red-reaction", self.workflow()["jobs"]["summary"]["needs"])

    def test_reaction_script_and_test_are_in_a_rerun_filter(self) -> None:
        text = PR_GATE.read_text(encoding="utf-8")
        self.assertIn("'scripts/ci/main_red_reaction.py'", text)
        self.assertIn("tests.scripts.test_main_red_reaction", text)


if __name__ == "__main__":
    unittest.main()
