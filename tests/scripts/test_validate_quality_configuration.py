import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_quality_configuration import (
    validate_browser_matrix_scope_policy,
    validate_maven_jvm_configuration,
    validate_quality_configuration,
    validate_surefire_jacoco_arg_lines,
    validate_windows_appium_retry,
    validate_workflow_coverage_policy,
    validate_workflow_readme_inventory,
)

PLAYWRIGHT_GRID_EXCLUSION = "!%regex[.*playwright.*PlaywrightActionsE2ETest.*]"
LAZY_GRID_EXCLUSION = "!%regex[.*LazyLoadingFixtureLiveTest.*]"
UNIT_PACKAGE_EXCLUSION = "!%regex[.*testPackage.unitTests.*]"


def workflow_scope(*tokens: str) -> str:
    return f'env:\n  GLOBAL_TESTING_SCOPE: "{", ".join(tokens)}"\n'


def pr_gate_unit_job(selector_lines: str, step_name: str = "Run shaft-engine unit tests") -> str:
    indented_selector = "\n".join(f"          {line}" for line in selector_lines.splitlines())
    return (
        "jobs:\n"
        "  unit-tests:\n"
        "    steps:\n"
        f"      - name: {step_name}\n"
        "        run: >-\n"
        "          mvn --batch-mode -pl shaft-engine test\n"
        f"{indented_selector}\n"
    )


class WorkflowReadmeInventoryTest(unittest.TestCase):
    def test_requires_every_active_workflow_in_local_inventory(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "pr-gate.yml").write_text("name: PR Gate\n", encoding="utf-8")
            (workflows / "live-tools-nightly.yml").write_text("name: Live tools\n", encoding="utf-8")
            (workflows / "README.md").write_text(
                "## Active inventory\n\n"
                "| File | Trigger |\n"
                "|---|---|\n"
                "| `pr-gate.yml` | pull request |\n\n"
                "`live-tools-nightly.yml` is mentioned outside the table.\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_readme_inventory(root),
                [".github/workflows/README.md is missing active workflow: live-tools-nightly.yml"],
            )

            (workflows / "README.md").write_text(
                "## Active inventory\n\n"
                "| File | Trigger |\n"
                "|---|---|\n"
                "| `pr-gate.yml` | pull request |\n"
                "| `live-tools-nightly.yml` | nightly |\n",
                encoding="utf-8",
            )
            self.assertEqual(validate_workflow_readme_inventory(root), [])


def missing_coverage_error(workflow: str, job: str) -> str:
    return (
        f".github/workflows/{workflow} job {job!r} runs JVM tests without "
        "upload-jacoco-coverage or post-test-report"
    )


class ValidateQualityConfigurationTest(unittest.TestCase):
    def test_windows_appium_maven_launch_is_bound_to_transfer_retry(self):
        workflow = """
jobs:
  Windows_Appium_Desktop_Local:
    steps:
      - name: Run tests
        run: |
          Start-Appium
          bash scripts/ci/build_retry.sh 2 60 mvn -pl shaft-engine -am -e test "-DrunWindowsDesktopE2E=true"
"""

        self.assertEqual(validate_windows_appium_retry(workflow), [])

    def test_windows_appium_retry_comment_cannot_mask_an_unwrapped_launch(self):
        workflow = """
jobs:
  Windows_Appium_Desktop_Local:
    steps:
      - name: Run tests
        run: |
          # bash scripts/ci/build_retry.sh 2 60
          mvn -pl shaft-engine -am -e test "-DrunWindowsDesktopE2E=true"
"""

        self.assertEqual(
            validate_windows_appium_retry(workflow),
            ["e2eLocalTests.yml Windows Appium Maven launch must use transfer-only fresh-process retry"],
        )

    def test_repository_configuration_is_valid(self):
        self.assertEqual(validate_quality_configuration(), [])

    def test_local_browser_matrix_excludes_pr_gate_owned_unit_tests(self):
        root = Path(__file__).resolve().parents[2]
        local_e2e_workflow = (root / ".github" / "workflows" / "e2eLocalTests.yml").read_text(
            encoding="utf-8"
        )

        self.assertIn("!%regex[.*testPackage.unitTests.*]", local_e2e_workflow)

    def test_rejects_local_browser_scope_that_omits_pr_gate_owned_unit_tests(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "e2eTests.yml").write_text(
                workflow_scope(
                    "!%regex[.*DatabaseActions.*]",
                    PLAYWRIGHT_GRID_EXCLUSION,
                    LAZY_GRID_EXCLUSION,
                    UNIT_PACKAGE_EXCLUSION,
                    "!%regex[.*ExampleUnitTest.*]",
                ),
                encoding="utf-8",
            )
            (workflows / "e2eLocalTests.yml").write_text(
                workflow_scope("!%regex[.*DatabaseActions.*]"),
                encoding="utf-8",
            )
            (workflows / "pr-gate.yml").write_text(
                pr_gate_unit_job("'-Dtest=testPackage/unitTests/*, ExampleUnitTest'"),
                encoding="utf-8",
            )

            self.assertEqual(
                validate_browser_matrix_scope_policy(root),
                [
                    "e2eLocalTests.yml must exclude PR-gate-owned unit tests: "
                    "!%regex[.*testPackage.unitTests.*], !%regex[.*ExampleUnitTest.*]"
                ],
            )

    def test_rejects_browser_exclusion_that_loses_pr_gate_ownership(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            local_scope = workflow_scope(
                UNIT_PACKAGE_EXCLUSION,
                "!%regex[.*ExampleUnitTest.*]",
            )
            (workflows / "e2eTests.yml").write_text(
                workflow_scope(
                    PLAYWRIGHT_GRID_EXCLUSION,
                    LAZY_GRID_EXCLUSION,
                    UNIT_PACKAGE_EXCLUSION,
                    "!%regex[.*ExampleUnitTest.*]",
                ),
                encoding="utf-8",
            )
            (workflows / "e2eLocalTests.yml").write_text(local_scope, encoding="utf-8")
            (workflows / "pr-gate.yml").write_text(
                pr_gate_unit_job("'-Dtest=testPackage/unitTests/*'"),
                encoding="utf-8",
            )

            self.assertEqual(
                validate_browser_matrix_scope_policy(root),
                [
                    "pr-gate.yml must retain unit-test ownership for browser-matrix exclusions: "
                    "ExampleUnitTest"
                ],
            )

    def test_rejects_grid_only_exclusion_in_local_browser_scope(self):
        for forbidden in (
            PLAYWRIGHT_GRID_EXCLUSION,
            LAZY_GRID_EXCLUSION,
        ):
            with self.subTest(forbidden=forbidden), tempfile.TemporaryDirectory() as temp_dir:
                root = Path(temp_dir)
                workflows = root / ".github" / "workflows"
                workflows.mkdir(parents=True)
                (workflows / "e2eTests.yml").write_text(
                    workflow_scope(
                        "!%regex[.*DatabaseActions.*]",
                        PLAYWRIGHT_GRID_EXCLUSION,
                        LAZY_GRID_EXCLUSION,
                        UNIT_PACKAGE_EXCLUSION,
                    ),
                    encoding="utf-8",
                )
                (workflows / "e2eLocalTests.yml").write_text(
                    workflow_scope("!%regex[.*DatabaseActions.*]", forbidden, UNIT_PACKAGE_EXCLUSION),
                    encoding="utf-8",
                )
                (workflows / "pr-gate.yml").write_text(
                    pr_gate_unit_job("'-Dtest=testPackage/unitTests/*'"),
                    encoding="utf-8",
                )

                self.assertEqual(
                    validate_browser_matrix_scope_policy(root),
                    [
                        "e2eLocalTests.yml contains exclusions outside the shared "
                        f"local-browser scope: {forbidden}"
                    ],
                )

    def test_pr_gate_ownership_requires_exact_positive_selector(self):
        selector_lines = {
            "comment-only": (
                "# '-Dtest=testPackage/unitTests/*, ExampleUnitTest'\n"
                "'-Dtest=testPackage/unitTests/*'"
            ),
            "negative": "'-Dtest=testPackage/unitTests/*, !ExampleUnitTest'",
            "prefix-collision": "'-Dtest=testPackage/unitTests/*, ExampleUnitTestExtra'",
        }
        for scenario, selector in selector_lines.items():
            with self.subTest(scenario=scenario), tempfile.TemporaryDirectory() as temp_dir:
                root = Path(temp_dir)
                workflows = root / ".github" / "workflows"
                workflows.mkdir(parents=True)
                local_scope = workflow_scope(
                    UNIT_PACKAGE_EXCLUSION,
                    "!%regex[.*ExampleUnitTest.*]",
                )
                (workflows / "e2eTests.yml").write_text(
                    workflow_scope(
                        PLAYWRIGHT_GRID_EXCLUSION,
                        LAZY_GRID_EXCLUSION,
                        UNIT_PACKAGE_EXCLUSION,
                        "!%regex[.*ExampleUnitTest.*]",
                    ),
                    encoding="utf-8",
                )
                (workflows / "e2eLocalTests.yml").write_text(local_scope, encoding="utf-8")
                (workflows / "pr-gate.yml").write_text(
                    pr_gate_unit_job(selector),
                    encoding="utf-8",
                )

                self.assertEqual(
                    validate_browser_matrix_scope_policy(root),
                    [
                        "pr-gate.yml must retain unit-test ownership for browser-matrix "
                        "exclusions: ExampleUnitTest"
                    ],
                )

    def test_requires_each_grid_only_exclusion_exactly_once(self):
        for missing in (PLAYWRIGHT_GRID_EXCLUSION, LAZY_GRID_EXCLUSION):
            with self.subTest(missing=missing), tempfile.TemporaryDirectory() as temp_dir:
                root = Path(temp_dir)
                workflows = root / ".github" / "workflows"
                workflows.mkdir(parents=True)
                grid_scope = [PLAYWRIGHT_GRID_EXCLUSION, LAZY_GRID_EXCLUSION, UNIT_PACKAGE_EXCLUSION]
                grid_scope.remove(missing)
                (workflows / "e2eTests.yml").write_text(workflow_scope(*grid_scope), encoding="utf-8")
                (workflows / "e2eLocalTests.yml").write_text(
                    workflow_scope(UNIT_PACKAGE_EXCLUSION),
                    encoding="utf-8",
                )
                (workflows / "pr-gate.yml").write_text(
                    pr_gate_unit_job("'-Dtest=testPackage/unitTests/*'"),
                    encoding="utf-8",
                )

                self.assertEqual(
                    validate_browser_matrix_scope_policy(root),
                    [f"e2eTests.yml must contain each grid-only exclusion exactly once: {missing}"],
                )

    def test_ignores_unit_selector_outside_the_owning_job_and_step(self):
        unrelated_selector = "'-Dtest=testPackage/unitTests/*, ExampleUnitTest'"
        scenarios = {
            "disabled-job": (
                "jobs:\n"
                "  disabled-tests:\n"
                "    if: false\n"
                "    steps:\n"
                "      - name: Run shaft-engine unit tests\n"
                "        run: >-\n"
                f"          {unrelated_selector}\n"
                + pr_gate_unit_job("'-Dtest=testPackage/unitTests/*'").removeprefix("jobs:\n")
            ),
            "wrong-step": pr_gate_unit_job(unrelated_selector, step_name="Document unit selectors")
            + "      - name: Run shaft-engine unit tests\n"
            + "        run: >-\n"
            + "          '-Dtest=testPackage/unitTests/*'\n",
            "unnamed-disabled-step": pr_gate_unit_job("'-Dtest=testPackage/unitTests/*'")
            + "      - if: false\n"
            + "        run: >-\n"
            + f"          {unrelated_selector}\n",
        }
        for scenario, pr_gate in scenarios.items():
            with self.subTest(scenario=scenario), tempfile.TemporaryDirectory() as temp_dir:
                root = Path(temp_dir)
                workflows = root / ".github" / "workflows"
                workflows.mkdir(parents=True)
                (workflows / "e2eTests.yml").write_text(
                    workflow_scope(
                        PLAYWRIGHT_GRID_EXCLUSION,
                        LAZY_GRID_EXCLUSION,
                        UNIT_PACKAGE_EXCLUSION,
                        "!%regex[.*ExampleUnitTest.*]",
                    ),
                    encoding="utf-8",
                )
                (workflows / "e2eLocalTests.yml").write_text(
                    workflow_scope(UNIT_PACKAGE_EXCLUSION, "!%regex[.*ExampleUnitTest.*]"),
                    encoding="utf-8",
                )
                (workflows / "pr-gate.yml").write_text(pr_gate, encoding="utf-8")

                self.assertEqual(
                    validate_browser_matrix_scope_policy(root),
                    [
                        "pr-gate.yml must retain unit-test ownership for browser-matrix "
                        "exclusions: ExampleUnitTest"
                    ],
                )

    def test_browser_matrix_scope_policy_fails_closed_when_workflow_is_missing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "e2eTests.yml").write_text("", encoding="utf-8")

            self.assertEqual(
                validate_browser_matrix_scope_policy(root),
                [
                    "browser-matrix scope policy is missing workflow files: "
                    "e2eLocalTests.yml, pr-gate.yml"
                ],
            )

    def test_rejects_java_25_only_maven_startup_option(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / ".mvn").mkdir()
            (root / ".mvn" / "jvm.config").write_text(
                "--sun-misc-unsafe-memory-access=allow\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_maven_jvm_configuration(root),
                [
                    "Maven JVM configuration must guard Java 25-only options "
                    "for the dependency submission Java 21 runtime"
                ],
            )

    def test_accepts_guarded_java_25_maven_startup_option(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / ".mvn").mkdir()
            (root / ".mvn" / "jvm.config").write_text(
                "-XX:+IgnoreUnrecognizedVMOptions\n"
                "--sun-misc-unsafe-memory-access=allow\n",
                encoding="utf-8",
            )

            self.assertEqual(validate_maven_jvm_configuration(root), [])

    def test_rejects_surefire_arg_line_that_drops_jacoco_injection(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            module = root / "shaft-visual"
            module.mkdir()
            (module / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <build>
        <plugins>
            <plugin>
                <artifactId>maven-surefire-plugin</artifactId>
                <configuration>
                    <argLine>${surefireArgLine} ${mockitoAgentArgLine}</argLine>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>""",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_surefire_jacoco_arg_lines(root),
                ["shaft-visual Surefire argLine must preserve JaCoCo's @{argLine} injection"],
            )

    def test_rejects_optional_jacoco_coverage_in_workflows(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "e2eTests.yml").write_text(
                "steps:\n"
                "  - uses: ./.github/actions/post-test-report\n"
                "    with:\n"
                "      require-coverage: false\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [
                    ".github/workflows/e2eTests.yml must not mark JaCoCo coverage "
                    "optional with 'require-coverage: false'"
                ],
            )

    def test_rejects_jvm_test_job_without_jacoco_upload(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "pr-gate.yml").write_text(
                "jobs:\n"
                "  java-tests:\n"
                "    steps:\n"
                "      - name: Run Java tests\n"
                "        run: mvn --batch-mode -pl shaft-cli test\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [missing_coverage_error("pr-gate.yml", "java-tests")],
            )

    def test_rejects_jvm_test_job_when_only_sibling_job_uploads_coverage(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yaml").write_text(
                "jobs:\n"
                "  java-tests:\n"
                "    steps:\n"
                "      - run: |\n"
                "          mvn --batch-mode\n"
                "          -pl shaft-cli test\n"
                "  unrelated-upload:\n"
                "    steps:\n"
                "      - uses: ./.github/actions/upload-jacoco-coverage\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [missing_coverage_error("coverage.yaml", "java-tests")],
            )

    def test_accepts_same_job_coverage_actions_and_ignores_skip_tests_setup(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  direct-upload:\n"
                "    steps:\n"
                "      - run: mvn -pl shaft-cli test\n"
                "      - if: always()\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "  post-test-report:\n"
                "    steps:\n"
                "      - run: gradle -p shaft-intellij test\n"
                "      - if: always()\n"
                "        uses: ./.github/actions/post-test-report\n"
                "  setup-only:\n"
                "    steps:\n"
                "      - run: mvn -pl shaft-mcp -am install -DskipTests\n",
                encoding="utf-8",
            )

            self.assertEqual(validate_workflow_coverage_policy(root), [])

    def test_detects_maven_wrapper_lifecycle_and_explicit_skip_tests_false(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  wrapper-verify:\n"
                "    steps:\n"
                "      - run: ./mvnw --batch-mode verify\n"
                "  tests-enabled:\n"
                "    steps:\n"
                "      - run: mvn test -DskipTests=false\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [
                    missing_coverage_error("coverage.yml", "wrapper-verify"),
                    missing_coverage_error("coverage.yml", "tests-enabled"),
                ],
            )

    def test_ignores_echoed_commands_and_rejects_disabled_or_fake_coverage_steps(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  echo-only:\n"
                "    steps:\n"
                "      - run: echo mvn test\n"
                "  disabled-upload:\n"
                "    steps:\n"
                "      - run: mvn test\n"
                "      - if: false\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "  fake-upload:\n"
                "    steps:\n"
                "      - run: |\n"
                "          echo 'uses: ./.github/actions/upload-jacoco-coverage'\n"
                "          mvn test\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [
                    missing_coverage_error("coverage.yml", "disabled-upload"),
                    missing_coverage_error("coverage.yml", "fake-upload"),
                ],
            )

    def test_rejects_coverage_that_cannot_run_after_the_last_test_on_main(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  before-test:\n"
                "    steps:\n"
                "      - if: always()\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "      - run: mvn verify\n"
                "  wrong-ref:\n"
                "    steps:\n"
                "      - run: mvn test\n"
                "      - if: always() && github.ref == 'refs/heads/not-main'\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "  success-only:\n"
                "    steps:\n"
                "      - run: gradle test\n"
                "      - uses: ./.github/actions/upload-jacoco-coverage\n"
                "  constant-false:\n"
                "    steps:\n"
                "      - run: mvn test\n"
                "      - if: always() && false\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "  pull-request-only:\n"
                "    steps:\n"
                "      - run: mvn test\n"
                "      - if: always() && github.event_name == 'pull_request'\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n"
                "  unmatched-environment:\n"
                "    steps:\n"
                "      - run: mvn test\n"
                "      - if: always() && env.NEVER_SET != ''\n"
                "        uses: ./.github/actions/upload-jacoco-coverage\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [
                    missing_coverage_error("coverage.yml", "before-test"),
                    missing_coverage_error("coverage.yml", "wrong-ref"),
                    missing_coverage_error("coverage.yml", "success-only"),
                    missing_coverage_error("coverage.yml", "constant-false"),
                    missing_coverage_error("coverage.yml", "pull-request-only"),
                    missing_coverage_error("coverage.yml", "unmatched-environment"),
                ],
            )

    def test_detects_environment_prefixed_jvm_test_command(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  environment-prefix:\n"
                "    steps:\n"
                "      - run: FOO=bar JAVA_TOOL_OPTIONS=-Xmx1g mvn test\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [missing_coverage_error("coverage.yml", "environment-prefix")],
            )

    def test_detects_jvm_tests_inside_local_composite_action(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            action = root / ".github" / "actions" / "intellij-verify"
            workflows.mkdir(parents=True)
            action.mkdir(parents=True)
            action.joinpath("action.yml").write_text(
                "runs:\n"
                "  using: composite\n"
                "  steps:\n"
                "    - shell: bash\n"
                "      run: bash scripts/ci/build_retry.sh 2 15 bash shaft-intellij/gradlew -p shaft-intellij check\n",
                encoding="utf-8",
            )
            (workflows / "coverage.yml").write_text(
                "jobs:\n"
                "  intellij:\n"
                "    steps:\n"
                "      - uses: ./.github/actions/intellij-verify\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [missing_coverage_error("coverage.yml", "intellij")],
            )

    def test_reports_missing_aggregate_module(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                '<project xmlns="http://maven.apache.org/POM/4.0.0"><modules/></project>',
                encoding="utf-8",
            )
            (root / ".github" / "workflows").mkdir(parents=True)
            (root / ".github" / "actions").mkdir(parents=True)
            (root / "shaft-engine").mkdir()
            (root / ".github" / "dependabot.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "coverage-readiness.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "security.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "e2eTests.yml").write_text("", encoding="utf-8")
            (root / "shaft-engine" / "pom.xml").write_text("", encoding="utf-8")

            errors = validate_quality_configuration(root)

            self.assertIn("root pom.xml must include report-aggregate", errors)
            self.assertIn("report-aggregate/pom.xml is missing", errors)


if __name__ == "__main__":
    unittest.main()
