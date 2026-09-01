import json
import os
import subprocess  # nosec B404 - integration fixture invokes fixed bash with repository-owned action text.
import tempfile
import unittest
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github/actions/post-test-report/action.yml"


class PostTestReportActionTest(unittest.TestCase):
    def run_summary(self, *, total, passed, failed, broken, skipped, unknown=None,
                    schema="nested", duration=None):
        action = yaml.safe_load(ACTION.read_text(encoding="utf-8"))
        step = next(step for step in action["runs"]["steps"]
                    if step.get("id") == "collect_results")

        with tempfile.TemporaryDirectory() as temporary_directory:
            module = Path(temporary_directory) / "shaft-engine"
            results = module / "allure-results"
            report = module / "allure-report"
            results.mkdir(parents=True)
            report.mkdir()
            (results / "test-result.json").write_text("{}", encoding="utf-8")
            statistic = {
                "total": total,
                "passed": passed,
                "failed": failed,
                "broken": broken,
                "skipped": skipped,
            }
            if unknown is not None:
                statistic["unknown"] = unknown
            if schema == "flat":
                document = dict(statistic)
                if duration is not None:
                    document["duration"] = duration
            else:
                document = {"statistic": statistic}
                if duration is not None:
                    document["time"] = {"duration": duration}
            (report / "summary.json").write_text(
                json.dumps(document), encoding="utf-8"
            )
            script = step["run"].replace("${{ inputs.module-directory }}", str(module))
            script = script.replace("${{ inputs.job-name }}", "Safari shard 1")
            environment = os.environ.copy()
            environment.update({
                "GITHUB_STEP_SUMMARY": str(Path(temporary_directory) / "summary.md"),
                "GITHUB_OUTPUT": str(Path(temporary_directory) / "outputs.txt"),
                "SHAFT_JOB_STARTED_S": "",
            })

            completed = subprocess.run(  # nosec B603 B607 - fixed bash executes trusted action fixture text.
                ["bash", "-c", script], capture_output=True, text=True, env=environment, check=False
            )
        return completed

    def test_rejects_all_unknown_summary_without_known_status_verdicts(self):
        completed = self.run_summary(total=14, passed=0, failed=0, broken=0, skipped=0, unknown=14)

        self.assertNotEqual(0, completed.returncode)
        self.assertIn("0 known status verdicts", completed.stdout)

    def test_accepts_unknown_entries_alongside_accounted_known_verdicts(self):
        completed = self.run_summary(
            total=1434, passed=1408, failed=0, broken=0, skipped=12, unknown=14
        )

        self.assertEqual(0, completed.returncode, completed.stdout)

    def test_accepts_flat_allure3_statistics_with_unknown_entries(self):
        completed = self.run_summary(
            total=1434, passed=1408, failed=0, broken=0, skipped=12, unknown=14,
            schema="flat", duration=123456
        )

        self.assertEqual(0, completed.returncode, completed.stdout)

    def test_rejects_fewer_status_verdicts_than_total(self):
        completed = self.run_summary(total=14, passed=13, failed=0, broken=0, skipped=0)

        self.assertNotEqual(0, completed.returncode)
        self.assertIn("14 total tests but 13 status verdicts", completed.stdout)

    def test_rejects_more_status_verdicts_than_total(self):
        completed = self.run_summary(total=14, passed=14, failed=0, broken=0, skipped=0, unknown=1)

        self.assertNotEqual(0, completed.returncode)
        self.assertIn("14 total tests but 15 status verdicts", completed.stdout)

    def test_accepts_summary_when_total_matches_status_verdicts(self):
        completed = self.run_summary(total=14, passed=13, failed=0, broken=0, skipped=1)

        self.assertEqual(0, completed.returncode, completed.stdout)

    def test_rejects_malformed_present_statistics(self):
        cases = (
            ("negative", {"passed": -1, "skipped": 1}),
            ("fractional", {"passed": 0.5, "skipped": 1}),
            ("string", {"passed": "1", "skipped": 1}),
            ("boolean", {"passed": 1, "unknown": True}),
        )
        for schema in ("nested", "flat"):
            for name, overrides in cases:
                values = {"total": 1, "passed": 0, "failed": 0, "broken": 0, "skipped": 0}
                values.update(overrides)
                with self.subTest(schema=schema, name=name):
                    completed = self.run_summary(**values, schema=schema)

                    self.assertNotEqual(0, completed.returncode)
                    self.assertIn("expected a non-negative integer", completed.stderr)

    def test_preserves_failed_and_broken_verdict_failures(self):
        for failed, broken in ((1, 0), (0, 1)):
            with self.subTest(failed=failed, broken=broken):
                completed = self.run_summary(
                    total=14, passed=13, failed=failed, broken=broken, skipped=0
                )

                self.assertNotEqual(0, completed.returncode)
                self.assertIn(
                    f"Allure report shows {failed} failed and {broken} broken test(s)",
                    completed.stdout,
                )


if __name__ == "__main__":
    unittest.main()
