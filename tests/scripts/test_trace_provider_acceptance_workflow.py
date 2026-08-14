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


if __name__ == "__main__":
    unittest.main()
