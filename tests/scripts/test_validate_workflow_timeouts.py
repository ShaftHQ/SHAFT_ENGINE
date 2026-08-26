import shlex
import re
import tempfile
import unittest
from pathlib import Path

import yaml

from scripts.ci.validate_workflow_timeouts import validate_repository


class ValidateWorkflowTimeoutsTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_repository(self.root)}

    def test_job_missing_timeout_minutes_is_flagged(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: echo hi
""",
        )
        errors = validate_repository(self.root)
        self.assertEqual(len(errors), 1)
        self.assertEqual(errors[0]["code"], "workflow-timeout-missing")
        self.assertEqual(errors[0]["path"], ".github/workflows/example.yml")
        self.assertIn("build", errors[0]["message"])

    def test_job_with_timeout_minutes_passes(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 30
    steps:
      - run: echo hi
""",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_reusable_workflow_call_job_is_exempt(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  call-shared:
    uses: ./.github/workflows/shared.yml
""",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_multiple_jobs_each_flagged_by_name(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: echo hi
  notify:
    runs-on: ubuntu-latest
    timeout-minutes: 10
    steps:
      - run: echo notify
  deploy:
    runs-on: ubuntu-latest
    steps:
      - run: echo deploy
""",
        )
        errors = validate_repository(self.root)
        flagged_jobs = {error["message"].split("'")[1] for error in errors}
        self.assertEqual(flagged_jobs, {"build", "deploy"})

    def test_no_workflows_directory_returns_no_errors(self):
        self.assertEqual(validate_repository(self.root), [])

    def test_current_repository_workflows_all_declare_timeout_minutes(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])

    def test_agent_guidance_job_enforces_the_fast_changed_surface_budget(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = yaml.safe_load(
            (repository_root / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        )
        job = workflow["jobs"]["agent-guidance"]
        commands = " ".join(str(step.get("run", "")) for step in job["steps"])
        self.assertLessEqual(job.get("timeout-minutes", 999), 6)
        self.assertIn("scripts/ci/harness_pr_gate.py", commands)
        self.assertIn("--budget-seconds 240", commands)
        checkout = next(step for step in job["steps"] if step.get("uses") == "actions/checkout@v7")
        self.assertLessEqual(checkout["with"]["fetch-depth"], 2)
        self.assertIn('git cat-file -e "${BASE_SHA}^{commit}"', commands)
        self.assertIn('git fetch --no-tags --depth=1 origin "$BASE_SHA"', commands)
        self.assertNotIn("matrix", job)

    def test_capture_browser_e2e_job_allows_prerequisite_and_browser_runtime_headroom(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = yaml.safe_load(
            (repository_root / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        )
        timeout = workflow["jobs"]["capture-e2e"].get("timeout-minutes", 0)
        self.assertGreaterEqual(timeout, 15)

    def test_ios_web_safari_job_allows_observed_nightly_runtime_headroom(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = yaml.safe_load(
            (repository_root / ".github/workflows/e2eTests.yml").read_text(encoding="utf-8")
        )
        timeout = workflow["jobs"]["iOS_Web_SAFARI_BrowserStack"].get("timeout-minutes", 0)
        self.assertEqual(timeout, 15)

    def test_local_safari_job_allows_observed_nightly_runtime_headroom(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = yaml.safe_load(
            (repository_root / ".github/workflows/e2eLocalTests.yml").read_text(encoding="utf-8")
        )
        timeout = workflow["jobs"]["MacOSX_Safari_Local"].get("timeout-minutes", 0)
        self.assertEqual(timeout, 120)


if __name__ == "__main__":
    unittest.main()


class TimeoutGuardIsReRunByItsOwnEditsTest(unittest.TestCase):
    """#4529: the guard's own files must re-trigger the job that runs them.

    The issue reported that three of the four workflow files the timeout
    assertion covers matched no path filter, so lowering a timeout would land
    green. That half is no longer true: `workflows` includes
    `.github/workflows/**`, and the Workflow Timeout Guard job runs on it, so
    any workflow edit does re-run the assertion. Verified by reading the
    filter rather than assumed from the issue.

    What is still open is worse. The guard's *own* implementation and test --
    `scripts/ci/validate_workflow_timeouts.py` and this module -- are in no
    filter at all. So a timeout cannot be lowered unnoticed, but the check
    that prevents it can be gutted and land green. A guard that cannot see
    edits to itself is the unbound-check shape one level up.

    `tests/scripts/test_intellij_verify_retry.py`, which the same job runs, is
    in the filter with a comment explaining why it belongs there. Its
    neighbours being absent is an oversight rather than a decision, which is
    the argument for pinning it here rather than only fixing it.
    """

    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"

    def test_the_guard_reruns_when_its_own_implementation_or_test_changes(self):
        content = self.WORKFLOW.read_text(encoding="utf-8")
        missing = [
            path
            for path in (
                "scripts/ci/validate_workflow_timeouts.py",
                "tests/scripts/test_validate_workflow_timeouts.py",
            )
            if f"- '{path}'" not in content
        ]
        self.assertEqual(
            missing,
            [],
            "a check whose own edits do not re-run it can be weakened green",
        )


class CliGatePackagingContractTest(unittest.TestCase):
    """#4606: keep the CLI binary smoke test without packaging the same JAR twice."""

    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"

    def test_cli_smoke_reuses_the_jar_created_by_reactor_install(self):
        content = self.WORKFLOW.read_text(encoding="utf-8")
        self.assertIn(
            "mvn --batch-mode -pl shaft-cli -am -DskipTests install -q",
            content,
        )
        self.assertIn("- name: Run packaged shaft-cli binary", content)
        self.assertNotIn(
            "mvn --batch-mode -pl shaft-cli package -DskipTests",
            content,
        )


class PathFilterComparisonContractTest(unittest.TestCase):
    """#4743: keep event-native path comparisons free of ignored inputs."""

    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"

    def test_filter_uses_event_native_base_with_a_non_empty_token(self):
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["changes"]["steps"]
        filter_steps = [step for step in steps if step.get("id") == "filter"]
        self.assertEqual(
            len(filter_steps),
            1,
            "jobs.changes must have exactly one path-filter owner with id=filter",
        )

        inputs = filter_steps[0]["with"]
        self.assertNotIn(
            "base",
            inputs,
            "pull-request API comparison ignores base when the token is non-empty",
        )
        token = inputs.get("token", "${{ github.token }}")
        self.assertIsInstance(token, str)
        self.assertTrue(
            token.strip(),
            "an empty token switches pull requests from the API to git diff",
        )


class MobileRecordingAcceptanceWorkflowContractTest(unittest.TestCase):
    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/e2eTests.yml"
    ANDROID_TEST = (
        Path(__file__).resolve().parents[2]
        / "shaft-engine/src/test/java/testPackage/appium/AndroidBasicInteractionsTests.java"
    )
    IOS_TEST = (
        Path(__file__).resolve().parents[2]
        / "shaft-engine/src/test/java/testPackage/appium/IOSBasicInteractionsTest.java"
    )

    def test_ios_recording_acceptance_uses_verified_immutable_app_and_exact_guard(self):
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        steps = workflow["jobs"]["iOS_Recording_BrowserStack"]["steps"]
        steps_by_name = {step["name"]: step for step in steps}
        names = [step["name"] for step in steps]

        expected_order = [
            "Setup Test Environment",
            "Fetch provider-compatible iOS sample app",
            "Run iOS recording acceptance",
            "Verify iOS recording acceptance executed",
            "Post-Test Report and Check",
        ]
        self.assertEqual(
            [name for name in names if name in expected_order],
            expected_order,
        )

        fetch = steps_by_name["Fetch provider-compatible iOS sample app"]["run"]
        app_path = "shaft-engine/src/test/resources/testDataFiles/apps/BStackSampleApp.ipa"
        source = (
            "https://raw.githubusercontent.com/browserstack/"
            "testng-appium-app-browserstack/"
            "71e73f10a613a7bb765bde05a1700829a8d5e057/"
            "ios/testng-examples/BStackSampleApp.ipa"
        )
        digest = "76a8bb0250f6d8c0a6bb0b71fcddf60515de92f5920d5624f790da1ecdbc87d9"
        expected_fetch = (
            f'app_path="{app_path}"\n'
            "curl --fail --location --silent --show-error \\\n"
            '  --output "$app_path" \\\n'
            f'  "{source}"\n'
            f'echo "{digest}  $app_path" \\\n'
            "  | sha256sum --check --strict"
        )
        self.assertEqual(expected_fetch, fetch.strip())

        execution_steps = [
            (
                workflow["jobs"]["Android_Recording_BrowserStack"]["steps"],
                "Run Android recording acceptance",
            ),
            (steps, "Run iOS recording acceptance"),
        ]
        executions = []
        execution_arguments = []
        for job_steps, step_name in execution_steps:
            matches = [step for step in job_steps if step.get("name") == step_name]
            self.assertEqual(1, len(matches))
            provider_execution = matches[0]["run"]
            self.assertNotIn("\n", provider_execution)
            self.assertNotIn("\r", provider_execution)
            lexer = shlex.shlex(provider_execution, posix=True, punctuation_chars=";&|")
            lexer.whitespace_split = True
            lexer.commenters = "#"
            arguments = list(lexer)
            self.assertEqual("mvn", arguments[0])
            self.assertEqual(1, arguments.count("mvn"))
            version_arguments = [
                argument for argument in arguments if argument.startswith("-DbrowserStack.appiumVersion=")
            ]
            self.assertEqual(["-DbrowserStack.appiumVersion=3.3.0"], version_arguments)
            self.assertFalse(any(all(character in ";&|" for character in argument) for argument in arguments))
            executions.append(provider_execution)
            execution_arguments.append(arguments)

        expected_selectors = [
            "AndroidBasicInteractionsTests#screenRecordingShouldPreserveUnsupportedProviderFailureAndResetState",
            "IOSBasicInteractionsTest#screenRecordingShouldPreserveUnsupportedProviderFailureAndResetState",
        ]
        for arguments, expected_selector in zip(execution_arguments, expected_selectors):
            selectors = [argument for argument in arguments if argument.startswith("-Dtest=")]
            self.assertEqual([f"-Dtest={expected_selector}"], selectors)

        broad_android_matches = [
            step
            for step in workflow["jobs"]["Android_Native_BrowserStack"]["steps"]
            if step.get("name") == "Run tests"
        ]
        self.assertEqual(1, len(broad_android_matches))
        broad_android_execution = broad_android_matches[0]["run"]
        self.assertNotIn("\n", broad_android_execution)
        self.assertNotIn("\r", broad_android_execution)
        broad_lexer = shlex.shlex(broad_android_execution, posix=True, punctuation_chars=";&|")
        broad_lexer.whitespace_split = True
        broad_lexer.commenters = "#"
        broad_arguments = list(broad_lexer)
        self.assertEqual("mvn", broad_arguments[0])
        self.assertEqual(1, broad_arguments.count("mvn"))
        self.assertFalse(
            any(all(character in ";&|" for character in argument) for argument in broad_arguments)
        )
        compatible_group = "mobile-recording-compatible-provider"
        exclusions = [
            argument for argument in broad_arguments if argument.startswith("-Dsurefire.excludedGroups=")
        ]
        self.assertEqual(
            [f"-Dsurefire.excludedGroups=allure3-visual-demo,{compatible_group},mobile-evidence-real-provider,"
             "visual-ocr-mobile-acceptance"],
            exclusions,
        )
        positive_methods = [
            (
                self.ANDROID_TEST,
                f'@Test(groups = {{"ApiDemosDebug", "{compatible_group}"}})\n',
            ),
            (
                self.IOS_TEST,
                f'@Test(groups = {{"{compatible_group}"}})\n',
            ),
        ]
        for source, annotation in positive_methods:
            test_source = source.read_text(encoding="utf-8")
            self.assertIn(
                annotation + "    public void screenRecordingShouldReturnAndSaveBoundedMedia()",
                test_source,
            )

        execution = executions[1]
        self.assertIn('"-Dshaft.enableNativeIosE2E=true"', execution)

        expected_guards = [
            (
                workflow["jobs"]["Android_Recording_BrowserStack"]["steps"],
                "Verify Android recording acceptance executed",
                "TEST-testPackage.appium.AndroidBasicInteractionsTests.xml",
            ),
            (
                steps,
                "Verify iOS recording acceptance executed",
                "TEST-testPackage.appium.IOSBasicInteractionsTest.xml",
            ),
        ]
        for job_steps, guard_name, report in expected_guards:
            matches = [step for step in job_steps if step.get("name") == guard_name]
            self.assertEqual(1, len(matches))
            guard_step = matches[0]
            self.assertFalse(guard_step.get("continue-on-error", False))
            expected_guard = (
                "python3 scripts/ci/assert_tests_executed.py "
                f"shaft-engine/target/surefire-reports/{report} "
                "--min-executed 1"
            )
            self.assertEqual(expected_guard, guard_step["run"].strip())


class MobileEvidenceAcceptanceWorkflowContractTest(unittest.TestCase):
    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/e2eTests.yml"
    ANDROID_TEST = (
        Path(__file__).resolve().parents[2]
        / "shaft-engine/src/test/java/testPackage/appium/AndroidBasicInteractionsTests.java"
    )
    IOS_TEST = (
        Path(__file__).resolve().parents[2]
        / "shaft-engine/src/test/java/testPackage/appium/IOSBasicInteractionsTest.java"
    )

    def test_mobile_evidence_acceptance_jobs_are_exact_and_fail_closed(self):
        workflow = yaml.safe_load(self.WORKFLOW.read_text(encoding="utf-8"))
        expected_jobs = {
            "Android_Evidence_BrowserStack": (
                "Run Android evidence acceptance",
                "AndroidBasicInteractionsTests#mobileEvidenceShouldPublishAResolvedBoundedArchive",
                "Verify Android evidence acceptance executed",
                "TEST-testPackage.appium.AndroidBasicInteractionsTests.xml",
                False,
            ),
            "iOS_Evidence_BrowserStack": (
                "Run iOS evidence acceptance",
                "IOSBasicInteractionsTest#mobileEvidenceShouldPublishAResolvedBoundedArchive",
                "Verify iOS evidence acceptance executed",
                "TEST-testPackage.appium.IOSBasicInteractionsTest.xml",
                True,
            ),
        }
        for job_name in expected_jobs:
            self.assertIn(job_name, workflow["jobs"])
            self.assertIn(job_name, workflow["jobs"]["Workflow_Summary"]["needs"])
            self.assertIn(job_name, workflow["jobs"]["notify_e2e_tests_failure"]["needs"])

        compatible_group = "mobile-evidence-real-provider"
        expected_methods = [
            (
                self.ANDROID_TEST,
                f'@Test(groups = {{"ApiDemosDebug", "{compatible_group}"}})\n',
            ),
            (self.IOS_TEST, f'@Test(groups = {{"{compatible_group}"}})\n'),
        ]
        for source, annotation in expected_methods:
            test_source = source.read_text(encoding="utf-8")
            expected_fragment = annotation.strip()
            self.assertEqual(
                1,
                len(re.findall(
                    rf"(?m)^    {re.escape(expected_fragment)}\r?\n"
                    r"^    public void mobileEvidenceShouldPublishAResolvedBoundedArchive\(\)",
                    test_source,
                )),
            )

        for job_name, (run_name, selector, guard_name, report, requires_ios_gate) in expected_jobs.items():
            job = workflow["jobs"][job_name]
            self.assertEqual(
                "github.event_name == 'workflow_dispatch' && "
                f"contains(format(',{{0}},', github.event.inputs.jobs), ',{job_name},')",
                job["if"],
            )
            steps = job["steps"]
            run_matches = [step for step in steps if step.get("name") == run_name]
            self.assertEqual(1, len(run_matches))
            execution = run_matches[0]["run"]
            self.assertNotIn("\n", execution)
            self.assertNotIn("\r", execution)
            lexer = shlex.shlex(execution, posix=True, punctuation_chars=";&|")
            lexer.whitespace_split = True
            lexer.commenters = "#"
            arguments = list(lexer)
            expected_arguments = [
                "mvn", "-pl", "shaft-browserstack", "-am", "-e", "test",
                "-Dallure.automaticallyOpen=false",
                "-DdefaultElementIdentificationTimeout=60",
                "-DretryMaximumNumberOfAttempts=0",
                "-DexecutionAddress=browserstack",
                f"-DtargetOperatingSystem={'iOS' if requires_ios_gate else 'android'}",
                f"-Dmobile_automationName={'XCuiTest' if requires_ios_gate else 'UIAutomator2'}",
                "-DbrowserStack.appiumVersion=3.3.0",
                f"-DbrowserStack.platformVersion={'16' if requires_ios_gate else '13.0'}",
                f"-DbrowserStack.deviceName={'iPhone 14' if requires_ios_gate else 'Google Pixel 7'}",
                "-DbrowserStack.appUrl=",
            ]
            if requires_ios_gate:
                expected_arguments.append("-Dshaft.enableNativeIosE2E=true")
            expected_arguments.extend([
                "-DgenerateAllureReportArchive=true",
                f"-Dtest={selector}",
            ])
            self.assertEqual(expected_arguments, arguments)

            guard_matches = [step for step in steps if step.get("name") == guard_name]
            self.assertEqual(1, len(guard_matches))
            guard = guard_matches[0]
            self.assertFalse(guard.get("continue-on-error", False))
            self.assertEqual(
                "python3 scripts/ci/assert_tests_executed.py "
                f"shaft-engine/target/surefire-reports/{report} --min-executed 1",
                guard["run"].strip(),
            )
            report_matches = [step for step in steps if step.get("name") == "Post-Test Report and Check"]
            self.assertEqual(1, len(report_matches))
            report_step = report_matches[0]
            self.assertEqual("always()", report_step.get("if"))
            self.assertFalse(report_step.get("continue-on-error", False))
            self.assertEqual("./.github/actions/post-test-report", report_step.get("uses"))
            self.assertEqual(job_name, report_step.get("with", {}).get("job-name"))

        ios_steps = workflow["jobs"]["iOS_Evidence_BrowserStack"]["steps"]
        fetch_matches = [step for step in ios_steps if step.get("name") == "Fetch provider-compatible iOS sample app"]
        self.assertEqual(1, len(fetch_matches))
        fetch = fetch_matches[0]["run"].strip()
        app_path = "shaft-engine/src/test/resources/testDataFiles/apps/BStackSampleApp.ipa"
        source = (
            "https://raw.githubusercontent.com/browserstack/testng-appium-app-browserstack/"
            "71e73f10a613a7bb765bde05a1700829a8d5e057/"
            "ios/testng-examples/BStackSampleApp.ipa"
        )
        digest = "76a8bb0250f6d8c0a6bb0b71fcddf60515de92f5920d5624f790da1ecdbc87d9"
        self.assertEqual(
            f'app_path="{app_path}"\n'
            "curl --fail --location --silent --show-error \\\n"
            '  --output "$app_path" \\\n'
            f'  "{source}"\n'
            f'echo "{digest}  $app_path" \\\n'
            "  | sha256sum --check --strict",
            fetch,
        )

        broad_steps = workflow["jobs"]["Android_Native_BrowserStack"]["steps"]
        broad_matches = [step for step in broad_steps if step.get("name") == "Run tests"]
        self.assertEqual(1, len(broad_matches))
        broad_execution = broad_matches[0]["run"]
        broad_lexer = shlex.shlex(broad_execution, posix=True, punctuation_chars=";&|")
        broad_lexer.whitespace_split = True
        broad_lexer.commenters = "#"
        broad_arguments = list(broad_lexer)
        exclusions = [arg for arg in broad_arguments if arg.startswith("-Dsurefire.excludedGroups=")]
        self.assertEqual(
            ["-Dsurefire.excludedGroups=allure3-visual-demo,"
             "mobile-recording-compatible-provider,mobile-evidence-real-provider,"
             "visual-ocr-mobile-acceptance"],
            exclusions,
        )
