from pathlib import Path
import re
import shlex
import unittest

import yaml


WORKFLOW = Path(__file__).resolve().parents[2] / ".github" / "workflows" / "e2eTests.yml"
ANDROID_TESTS = (Path(__file__).resolve().parents[2] / "shaft-engine" / "src" / "test" / "java" /
                 "testPackage" / "appium" / "AndroidBasicInteractionsTests.java")


class VisualOcrWorkflowTest(unittest.TestCase):
    def test_regular_android_nightly_excludes_dedicated_visual_ocr_acceptance(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["Android_Native_BrowserStack"]["steps"]
        test_command = next(step["run"] for step in steps if "-Dtest=" in step.get("run", ""))
        excluded_groups = [argument for argument in shlex.split(test_command)
                           if argument.startswith("-Dsurefire.excludedGroups=")]

        self.assertEqual(len(excluded_groups), 1)
        self.assertIn("visual-ocr-mobile-acceptance",
                      excluded_groups[0].removeprefix("-Dsurefire.excludedGroups=").split(","))
        self.assertNotIn("%regex[.*FlutterTest.*]", test_command)
        self.assertNotIn("-Dshaft.enableFlutterE2E=true", test_command)

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

    def test_jobs_forward_dispatch_test_selector_and_keep_defaults(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        expected_defaults = {
            "Android_Visual_Ocr_BrowserStack":
                "AndroidBasicInteractionsTests#visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls+"
                "visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl",
            "iOS_Visual_Ocr_BrowserStack":
                "IOSBasicInteractionsTest#visualAndOcrTargetsShouldInteractWithNativeControls",
        }

        for job_name, expected_default in expected_defaults.items():
            self.assertEqual(
                "github.event_name != 'workflow_dispatch' || "
                "github.event.inputs.jobs == '' || "
                "github.event.inputs.jobs == 'all' || "
                f"contains(format(',{{0}},', github.event.inputs.jobs), ',{job_name},')",
                workflow["jobs"][job_name]["if"],
            )
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

    def test_flutter_emulator_keeps_jdk17_for_apk_and_jdk25_for_maven(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["Android_Flutter_Emulator_E2E"]["steps"]
        env_step = next(step for step in steps if step.get("name") == "Setup Test Environment")
        self.assertEqual("17", env_step["with"]["java-version"])
        maven_jdk = next(step for step in steps if step.get("name") == "Set up JDK 25 for Maven")
        self.assertEqual("25", maven_jdk["with"]["java-version"])
        names = [step.get("name") for step in steps]
        self.assertLess(names.index("Build demo-app debug APK wired for the Appium Flutter Integration Server"),
                        names.index("Set up JDK 25 for Maven"))
        self.assertLess(names.index("Set up JDK 25 for Maven"),
                        names.index("Install engine dependencies for FlutterTest"))
