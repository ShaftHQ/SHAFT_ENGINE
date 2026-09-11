from pathlib import Path
import re
import shlex
import unittest

import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "e2eTests.yml"
LOCAL_WORKFLOW = ROOT / ".github" / "workflows" / "e2eLocalTests.yml"
ANDROID_TESTS = (ROOT / "shaft-engine" / "src" / "test" / "java" /
                 "testPackage" / "appium" / "AndroidBasicInteractionsTests.java")
IOS_TESTS = (ROOT / "shaft-engine" / "src" / "test" / "java" /
             "testPackage" / "appium" / "IOSBasicInteractionsTest.java")
WINAPPDRIVER_INSTALLER = ROOT / "scripts" / "ci" / "install_winappdriver.ps1"


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

    def test_android_flutter_browserstack_is_sibling_not_mixed_into_native(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        self.assertNotIn("Android_Flutter_Emulator_E2E", workflow["jobs"])
        self.assertNotIn("enableFlutterEmulatorE2E", workflow.get("on", {}).get("workflow_dispatch", {}).get("inputs", {}))
        native = workflow["jobs"]["Android_Native_BrowserStack"]
        flutter = workflow["jobs"]["Android_Flutter_BrowserStack"]
        self.assertEqual("Flutter_Demo_App_Build", flutter["needs"])
        build_if = workflow["jobs"]["Flutter_Demo_App_Build"]["if"]
        self.assertIn(",Android_Flutter_BrowserStack,", build_if)
        self.assertIn(",Android_Native_BrowserStack,", build_if)
        flutter_run = next(step["run"] for step in flutter["steps"] if "-Dtest=" in step.get("run", ""))
        native_run = next(step["run"] for step in native["steps"] if "-Dtest=" in step.get("run", ""))
        self.assertIn("-Dshaft.enableFlutterE2E=true", flutter_run)
        self.assertIn("-DexecutionAddress=browserstack", flutter_run)
        self.assertIn("-Dmobile_automationName=FlutterIntegration", flutter_run)
        self.assertIn("Google Pixel 7", flutter_run)
        self.assertIn("13.0", flutter_run)
        self.assertIn("-DbrowserStack.appUrl=", flutter_run)
        self.assertIn("-DdefaultElementIdentificationTimeout=60", flutter_run)
        self.assertIn("-DretryMaximumNumberOfAttempts=2", flutter_run)
        self.assertIn("FlutterTest", flutter_run)
        self.assertIn("github.event.inputs.tests", flutter_run)
        self.assertNotIn("-Dshaft.enableFlutterE2E=true", native_run)
        self.assertNotIn("%regex[.*FlutterTest.*]", native_run)
        self.assertIn("Android_Flutter_BrowserStack", workflow["jobs"]["Workflow_Summary"]["needs"])
        self.assertIn("Android_Flutter_BrowserStack", workflow["jobs"]["notify_e2e_tests_failure"]["needs"])
        self.assertNotIn("Android_Flutter_Emulator_E2E", workflow["jobs"]["Workflow_Summary"]["needs"])
        wire = next(step["run"] for step in flutter["steps"]
                    if "flutter-demo.apk" in step.get("run", "") and "cp " in step.get("run", ""))
        self.assertIn("shaft-engine/src/test/resources/testDataFiles/apps/flutter-demo.apk", wire)

    def test_flutter_emulator_keeps_jdk17_for_apk_and_jdk25_for_maven(self):
        workflow = yaml.safe_load(LOCAL_WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["Ubuntu_Flutter_Emulator_Local"]["steps"]
        env_step = next(step for step in steps if step.get("name") == "Setup Test Environment")
        self.assertEqual("17", env_step["with"]["java-version"])
        maven_jdk = next(step for step in steps if step.get("name") == "Set up JDK 25 for Maven")
        self.assertEqual("25", maven_jdk["with"]["java-version"])
        names = [step.get("name") for step in steps]
        self.assertLess(names.index("Build demo-app debug APK wired for the Appium Flutter Integration Server"),
                        names.index("Set up JDK 25 for Maven"))
        self.assertLess(names.index("Set up JDK 25 for Maven"),
                        names.index("Install engine dependencies for FlutterTest"))
        self.assertLess(names.index("Flutter emulator preflight"),
                        names.index("Boot emulator, open Flutter session, run FlutterTest tap/type/text"))

    def test_flutter_emulator_script_avoids_pipefail_under_dash(self):
        workflow = yaml.safe_load(LOCAL_WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["Ubuntu_Flutter_Emulator_Local"]["steps"]
        emulator = next(step for step in steps if step.get("uses", "").startswith(
            "reactivecircus/android-emulator-runner@"))
        script = emulator["with"]["script"]
        self.assertNotRegex(script, r"(?m)^\s*set -[^\n]*pipefail")
        self.assertRegex(script, r"(?m)^\s*set -eu\s*$")
        self.assertIn('working-directory', emulator["with"])
        self.assertIn('cd "$GITHUB_WORKSPACE"', script)
        self.assertIn("-Dallure.automaticallyOpen=false", script)
        self.assertIn("-DheadlessExecution=true", script)
        self.assertIn("shaft-engine/allure-results", script)
        self.assertIn("127.0.0.1:4723", script)

    def test_ubuntu_flutter_emulator_local_is_scheduled_like_windows_chrome(self):
        workflow = yaml.safe_load(LOCAL_WORKFLOW.read_text(encoding="utf-8"))
        chrome_if = workflow["jobs"]["Windows_Chrome_Local"]["if"]
        flutter_if = workflow["jobs"]["Ubuntu_Flutter_Emulator_Local"]["if"]
        self.assertEqual(chrome_if.replace("Windows_Chrome_Local", "JOB"),
                         flutter_if.replace("Ubuntu_Flutter_Emulator_Local", "JOB"))
        self.assertIn("Ubuntu_Flutter_Emulator_Local", workflow["jobs"]["Workflow_Summary"]["needs"])
        self.assertIn("Ubuntu_Flutter_Emulator_Local",
                      workflow["jobs"]["notify_local_e2e_tests_failure"]["needs"])
        preflight = (ROOT / "scripts" / "ci" / "flutter_emulator_preflight.sh").read_text(encoding="utf-8")
        self.assertIn("/dev/kvm", preflight)
        self.assertIn("sdkmanager", preflight)
        self.assertIn("Android XR", preflight)
        self.assertIn("appium-flutter-integration-driver", preflight)
        self.assertIn("adb", preflight)
        names = [step.get("name") for step in workflow["jobs"]["Ubuntu_Flutter_Emulator_Local"]["steps"]]
        self.assertIn("Flutter emulator preflight", names)

    def test_ios_visual_ocr_uses_shared_locators_and_opens_text_screen(self):
        ios_tests = IOS_TESTS.read_text(encoding="utf-8")
        self.assertIn('TEXT_BUTTON = AppiumBy.accessibilityId("Text Button")', ios_tests)
        self.assertIn('TEXT_INPUT = AppiumBy.accessibilityId("Text Input")', ios_tests)
        method = re.search(
            r"public void visualAndOcrTargetsShouldInteractWithNativeControls\(\) \{(.*?)\n    @",
            ios_tests,
            flags=re.S,
        )
        self.assertIsNotNone(method)
        body = method.group(1)
        self.assertIn("tap(TEXT_BUTTON)", body)
        self.assertIn("findElement(TEXT_INPUT)", body)
        self.assertNotRegex(body, r'AppiumBy\.accessibilityId\("Text Input"\)')
        self.assertIn("isAccessibilityFocused(TEXT_INPUT)", body)
        self.assertNotIn("switchTo().activeElement()", body)

    def test_android_visual_ocr_avoids_ambiguous_auto_group1_image_target(self):
        android_tests = ANDROID_TESTS.read_text(encoding="utf-8")
        method = re.search(
            r"public void visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls\(\) \{(.*?)\n    @",
            android_tests,
            flags=re.S,
        )
        self.assertIsNotNone(method)
        body = method.group(1)
        self.assertIn('OcrTarget.exact("Group 1")', body)
        self.assertIsNone(re.search(
            r"ImageTarget\.fromBytes\(group1Screenshot\).*ImageMatchingMode\.AUTO",
            body,
            flags=re.S,
        ))

    def test_android_visual_ocr_horizontal_uses_tab12_image_not_exact_ocr(self):
        android_tests = ANDROID_TESTS.read_text(encoding="utf-8")
        method = re.search(
            r"public void visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl\(\) \{(.*?)\n    @",
            android_tests,
            flags=re.S,
        )
        self.assertIsNotNone(method)
        body = method.group(1)
        self.assertIn("tab12Screenshot", body)
        self.assertIn("ImageTarget.fromBytes(tab12Screenshot)", body)
        self.assertNotIn('OcrTarget.exact("TAB 12")', body)
        self.assertNotIn('OcrTarget.containing("TAB 1")', body)
        self.assertIn('OcrTarget.exact("TAB 1")', body)

    def test_windows_appium_desktop_pins_winappdriver_without_releases_api(self):
        local_workflow = LOCAL_WORKFLOW.read_text(encoding="utf-8")
        installer = WINAPPDRIVER_INSTALLER.read_text(encoding="utf-8")
        self.assertNotRegex(local_workflow, r"(?m)^\s*[^#\n]*install-wad")
        self.assertNotRegex(local_workflow, r"(?m)^\s*[^#\n]*api\.github\.com/repos/microsoft/winappdriver")
        self.assertIn("install_winappdriver.ps1", local_workflow)
        self.assertIn("WindowsApplicationDriver_", installer)
        self.assertIn("api\\.github\\.com/repos/microsoft/winappdriver", installer)
        self.assertIn("Refusing WinAppDriver install URL that hits the GitHub Releases API", installer)
        self.assertIn("a76a8f4e44b29bad331acf6b6c248fcc65324f502f28826ad2acd5f3c80857fe", installer)
        self.assertIn("Get-FileHash -Algorithm SHA256", installer)
