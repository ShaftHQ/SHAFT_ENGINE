from pathlib import Path
import unittest

import yaml


WORKFLOW = Path(__file__).resolve().parents[2] / ".github" / "workflows" / "e2eTests.yml"


class VisualOcrWorkflowTest(unittest.TestCase):
    def test_jobs_forward_manual_test_selector_and_keep_defaults(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        expected_defaults = {
            "Android_Visual_Ocr_BrowserStack":
                "AndroidBasicInteractionsTests#visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls+"
                "visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl",
            "iOS_Visual_Ocr_BrowserStack":
                "IOSBasicInteractionsTest#visualAndOcrTargetsShouldInteractWithNativeControls",
        }

        for job_name, expected_default in expected_defaults.items():
            run_steps = [step["run"] for step in workflow["jobs"][job_name]["steps"] if "run" in step]
            test_command = next(command for command in run_steps if "-Dtest=" in command)
            self.assertIn("github.event.inputs.tests", test_command)
            self.assertIn("github.event.inputs.tests != ''", test_command)
            self.assertIn(expected_default, test_command)
            verification = next(command for command in run_steps if "assert_tests_executed.py" in command)
            self.assertIn("find shaft-engine/target/surefire-reports shaft-browserstack/target/surefire-reports", verification)
            self.assertIn('"${reports[@]}" --min-executed 1', verification)
            self.assertNotIn("TEST-testPackage.appium", verification)
