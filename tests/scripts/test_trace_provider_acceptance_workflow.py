import unittest
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "trace-viewer-acceptance.yml"


class TraceProviderAcceptanceWorkflowTest(unittest.TestCase):
    def test_real_playwright_trace_matrix_covers_all_supported_engines(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        job = workflow["jobs"]["playwright-native-trace"]
        matrix = job["strategy"]["matrix"]["browser"]

        self.assertEqual(["chromium", "firefox", "webkit"], matrix)

        steps = {step.get("name"): step for step in job["steps"] if step.get("name")}
        install = steps["Install Playwright browser"]["run"]
        acceptance = steps["Run native trace parity acceptance"]["run"]

        self.assertIn("com.microsoft.playwright.CLI", install)
        self.assertIn('install --with-deps ${{ matrix.browser }}', install)
        self.assertIn("PlaywrightTraceParityAcceptanceTest", acceptance)
        self.assertIn('-Dsurefire.excludedGroups=', acceptance.split())
        self.assertIn('-Dshaft.trace.acceptance.browser=${{ matrix.browser }}', acceptance)

    def test_offline_job_provisions_and_runs_both_allure_acceptance_tests(self):  # #6752
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        steps = {step.get("name"): step for step in workflow["jobs"]["chromium"]["steps"] if step.get("name")}
        provision = steps["Provision the Allure 2 and Allure 3 CLIs"]["run"]
        acceptance = steps["Run offline trace viewer acceptance"]["run"]

        self.assertIn("allure/allure-cli/${ALLURE3_CLI_VERSION}", provision)
        self.assertIn("allure/allure2-cli/${ALLURE2_CLI_VERSION}", provision)
        self.assertIn("TraceViewerAllureAcceptanceTest", acceptance)
        self.assertIn("BidiRequestBodyAcceptanceTest", acceptance)  # #6740
        self.assertIn("-Dshaft.allure.cli.version=${ALLURE3_CLI_VERSION}", acceptance.split())
        self.assertIn("-Dshaft.allure2.cli.version=${ALLURE2_CLI_VERSION}", acceptance.split())
        self.assertEqual(
            ["3.20.1", "2.46.1"], [workflow["env"]["ALLURE3_CLI_VERSION"], workflow["env"]["ALLURE2_CLI_VERSION"]]
        )
        for trigger in ("pull_request", "push"):
            self.assertIn(
                "shaft-engine/src/test/java/com/shaft/tools/io/internal/TraceViewerAllureAcceptanceTest.java",
                workflow[True][trigger]["paths"],
            )



if __name__ == "__main__":
    unittest.main()
