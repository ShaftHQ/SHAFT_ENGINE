from pathlib import Path
import re
import shlex
import unittest

import yaml


WORKFLOW = Path(__file__).resolve().parents[2] / ".github" / "workflows" / "e2eTests.yml"
ANDROID_TESTS = (Path(__file__).resolve().parents[2] / "shaft-engine" / "src" / "test" / "java" /
                 "testPackage" / "appium" / "AndroidBasicInteractionsTests.java")


class VisualOcrWorkflowTest(unittest.TestCase):
    def test_regular_android_nightly_excludes_manual_visual_ocr_acceptance(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["Android_Native_BrowserStack"]["steps"]
        test_command = next(step["run"] for step in steps if "-Dtest=" in step.get("run", ""))
        excluded_groups = [argument for argument in shlex.split(test_command)
                           if argument.startswith("-Dsurefire.excludedGroups=")]

        self.assertEqual(len(excluded_groups), 1)
        self.assertIn("visual-ocr-mobile-acceptance",
                      excluded_groups[0].removeprefix("-Dsurefire.excludedGroups=").split(","))

        android_tests = ANDROID_TESTS.read_text(encoding="utf-8")
        for method in (
                "visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls",
                "visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl",
        ):
            annotation_and_method = re.search(
                rf"@Test\(groups\s*=\s*\{{(?P<groups>[^}}]+)}}\)\s+public void {method}\(",
                android_tests,
            )
            self.assertIsNotNone(annotation_and_method, method)
            self.assertIn('"visual-ocr-mobile-acceptance"', annotation_and_method.group("groups"))

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
            self.assertIn("github.event_name == 'workflow_dispatch'", workflow["jobs"][job_name]["if"])
            run_steps = [step["run"] for step in workflow["jobs"][job_name]["steps"] if "run" in step]
            test_command = next(command for command in run_steps if "-Dtest=" in command)
            self.assertIn("-DincludeVisualTestRuntime", test_command)
            self.assertIn("-DincludeOcrTestRuntime", test_command)
            self.assertIn("-Dshaft.ocr.downloadEnabled=false", test_command)
            excluded_groups = [argument for argument in shlex.split(test_command)
                               if argument.startswith("-Dsurefire.excludedGroups=")]
            self.assertTrue(all("visual-ocr-mobile-acceptance" not in argument.removeprefix(
                "-Dsurefire.excludedGroups=").split(",") for argument in excluded_groups))
            self.assertIn("github.event.inputs.tests", test_command)
            self.assertIn("github.event.inputs.tests != ''", test_command)
            self.assertIn(expected_default, test_command)
            prewarm = next(command for command in run_steps
                           if "setup plan --profile OCR" in command)
            self.assertIn("setup install --plan", prewarm)
            self.assertIn("setup verify --profile OCR", prewarm)
            self.assertIn("shaft-cli-*[0-9].jar", prewarm)
            build = next(command for command in run_steps if "build_retry.sh" in command)
            self.assertIn("shaft-cli", build)
            verification = next(command for command in run_steps if "assert_tests_executed.py" in command)
            self.assertIn("find shaft-engine/target/surefire-reports shaft-browserstack/target/surefire-reports", verification)
            self.assertIn('"${reports[@]}" --min-executed 1', verification)
            self.assertNotIn("TEST-testPackage.appium", verification)
