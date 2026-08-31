import os
import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github/actions/post-test-report/action.yml"


class PostTestReportActionTest(unittest.TestCase):
    def test_rejects_nonzero_total_without_any_status_verdicts(self):
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
            (report / "summary.json").write_text(
                '{"statistic":{"total":14,"passed":0,"failed":0,"broken":0,"skipped":0}}',
                encoding="utf-8",
            )
            script = step["run"].replace("${{ inputs.module-directory }}", str(module))
            script = script.replace("${{ inputs.job-name }}", "Safari shard 1")
            environment = os.environ.copy()
            environment.update({
                "GITHUB_STEP_SUMMARY": str(Path(temporary_directory) / "summary.md"),
                "GITHUB_OUTPUT": str(Path(temporary_directory) / "outputs.txt"),
                "SHAFT_JOB_STARTED_S": "",
            })

            completed = subprocess.run(
                ["bash", "-c", script], capture_output=True, text=True, env=environment, check=False
            )

        self.assertNotEqual(0, completed.returncode)
        self.assertIn("14 total tests but 0 status verdicts", completed.stdout)


if __name__ == "__main__":
    unittest.main()
