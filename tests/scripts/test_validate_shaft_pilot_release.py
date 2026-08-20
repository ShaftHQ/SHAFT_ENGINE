import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/validate_shaft_pilot_release.py"
SPEC = importlib.util.spec_from_file_location("validate_shaft_pilot_release", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)
WORKFLOW_PATH = ROOT / ".github/workflows/shaft-pilot-release.yml"
ISOLATED_SLICES = (
    "intellij-verify",
    "release-contracts",
    "pilot-tests",
    "capture-journey",
    "package-and-validate",
    "container-smoke",
)


def _write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _write_minimal_reactor(root: Path, reactor_version: str, plugin_version: str) -> None:
    modules = "\n".join(f"                <module>{module}</module>" for module in MODULE.PILOT_MODULES)
    _write_text(
        root / "pom.xml",
        f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>io.github.shafthq</groupId>
            <artifactId>shaft-parent</artifactId>
            <version>{reactor_version}</version>
            <modules>
{modules}
            </modules>
        </project>
        """,
    )
    _write_text(
        root / "shaft-bom/pom.xml",
        f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
            <dependencyManagement>
                <dependencies>
                    {"".join(
                        f'<dependency><artifactId>{artifact}</artifactId></dependency>'
                        for artifact in MODULE.PUBLIC_ARTIFACTS.values()
                    )}
                </dependencies>
            </dependencyManagement>
        </project>
        """,
    )
    _write_text(
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java",
        f"""package com.shaft.properties.internal;

        import org.aeonbits.owner.Config;

        public interface Internal extends Config {{
            @Key("shaftEngineVersion")
            @DefaultValue("{reactor_version}")
            String shaftEngineVersion();

            @Key("allure3Version")
            @DefaultValue("2.35.3")
            String allure3Version();

            @Key("nodeLtsVersion")
            @DefaultValue("22.17.0")
            String nodeLtsVersion();

            @Key("appiumServerVersion")
            @DefaultValue("10.1.1")
            String appiumServerVersion();

            @Key("appiumInspectorPluginVersion")
            @DefaultValue("2026.1.0")
            String appiumInspectorPluginVersion();

            @Key("appiumUiAutomator2DriverVersion")
            @DefaultValue("10.1.1")
            String appiumUiAutomator2DriverVersion();

            @Key("appiumXcuitestDriverVersion")
            @DefaultValue("10.1.1")
            String appiumXcuitestDriverVersion();

            @Key("androidCommandLineToolsVersion")
            @DefaultValue("11076708")
            String androidCommandLineToolsVersion();
        }}
        """,
    )
    _write_text(
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Pilot.java",
        """package com.shaft.properties.internal;

@Key("pilot.ai.enabled")
    @DefaultValue("false")
String pilotAiEnabled();

@Key("pilot.ai.provider")
    @DefaultValue("none")
String pilotAiProvider();

@Key("pilot.ai.consent.local")
    @DefaultValue("false")
String pilotConsentLocal();

@Key("pilot.ai.consent.remote")
    @DefaultValue("false")
String pilotConsentRemote();

@Key("pilot.ai.telemetry.enabled")
    @DefaultValue("false")
String pilotTelemetryEnabled();
""",
    )
    _write_text(
        root / "shaft-intellij/gradle.properties",
        f"""pluginGroup=io.github.shafthq
pluginVersion={plugin_version}
pluginSinceBuild=243
platformVersion=2024.3
""",
    )
    _write_text(
        root / "shaft-intellij/build.gradle.kts",
        """import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType

        intellijPlatform {
            publishing {
                channels = listOf("default")
            }
        }
        """,
    )
    _write_text(
        root / "shaft-intellij/src/main/java/com/shaft/intellij/mcp/ShaftMcpStdioClient.java",
        """package com.shaft.intellij.mcp;

        public class ShaftMcpStdioClient {
            private void init() {
                clientInfo.addProperty("version", pluginVersion());
            }

            private static String pluginVersion() {
                return "fromBundle";
            }
        }
        """,
    )
    _write_text(
        root / "shaft-mcp/src/test/resources/fixtures/shaft-pilot/fixture.json",
        """{"status":"passed"}""",
    )
    _write_text(
        root / ".github/actions/intellij-verify/action.yml",
        """name: 'Verify or Publish IntelliJ Plugin'
        runs:
          using: 'composite'
          steps:
            - name: Run Gradle build
              shell: bash
              run: bash shaft-intellij/gradlew -p shaft-intellij check buildPlugin verifyPlugin
        """,
    )
    _write_text(
        root / ".github/workflows/mavenCentral_cd.yml",
        """Verify IntelliJ plugin release candidate
      uses: ./.github/actions/intellij-verify
Validate SHAFT Pilot release contract
Run deterministic SHAFT Pilot tests
Run headless SHAFT Capture release journey
Validate Maven publication
Deploy to Maven Central
Verify published Maven Central coordinates
""",
    )
    _write_text(
        root / ".github/workflows/publish-intellij-plugin.yml",
        _minimal_publish_intellij_workflow_text(),
    )
    isolated = _minimal_isolated_workflow_text()
    # Named-check tokens remain discoverable for static contract scans that
    # still look for the historical release-step phrases in this fixture.
    _write_text(
        root / ".github/workflows/shaft-pilot-release.yml",
        isolated
        + """
# Fixture tokens retained for static phrase scans:
# Verify IntelliJ plugin release candidate
# uses: ./.github/actions/intellij-verify
# Validate SHAFT Pilot release contract
# Run deterministic SHAFT Pilot tests
# Run headless SHAFT Capture release journey
# Validate Maven publication
# Deploy to Maven Central
# Verify published Maven Central coordinates
""",
    )


def _needs_list(job: dict) -> list[str]:
    needs = job.get("needs", [])
    if needs is None:
        return []
    if isinstance(needs, str):
        return [needs]
    return list(needs)


def _step_blob(job: dict) -> str:
    parts = []
    for step in job.get("steps") or []:
        if not isinstance(step, dict):
            continue
        parts.append(str(step.get("name") or ""))
        parts.append(str(step.get("uses") or ""))
        parts.append(str(step.get("run") or ""))
    return "\n".join(parts)


SLICE_CHANGED_IF = "needs.detect-shaft-version-change.outputs.changed == 'true'"


def _minimal_publish_intellij_workflow_text() -> str:
    return """name: Publish IntelliJ Plugin
jobs:
  verify:
    runs-on: ubuntu-22.04
    timeout-minutes: 20
    steps:
      - uses: ./.github/actions/intellij-verify
  publish:
    needs: verify
    runs-on: ubuntu-22.04
    timeout-minutes: 15
    steps:
      - uses: ./.github/actions/intellij-verify
        with:
          publish: 'true'
          verify: 'false'
      - run: python3 scripts/ci/assert_intellij_marketplace_receipt.py --log log --properties props
  notify-missed-publish:
    if: always()
    needs: [verify, publish]
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    permissions:
      issues: write
    steps:
      - uses: ./.github/actions/notify-intellij-marketplace-publish
        with:
          label: intellij-marketplace-publish-miss
"""


def _minimal_isolated_workflow_text() -> str:
    slice_jobs = []
    for name in ISOLATED_SLICES:
        slice_jobs.append(
            f"""  {name}:
    needs: detect-shaft-version-change
    if: {SLICE_CHANGED_IF}
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v7
"""
        )
    needs_lines = "\n".join(f"      - {name}" for name in ISOLATED_SLICES)
    return f"""name: SHAFT Pilot Release Candidate
jobs:
  detect-shaft-version-change:
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    outputs:
      changed: ${{{{ steps.version-diff.outputs.changed }}}}
    steps:
      - uses: actions/checkout@v7
{"".join(slice_jobs)}  release-candidate:
    needs:
      - detect-shaft-version-change
{needs_lines}
    if: always()
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    steps:
      - name: Evaluate isolated slice results
        env:
          DETECT_RESULT: ${{{{ needs.detect-shaft-version-change.result }}}}
          DETECT_CHANGED: ${{{{ needs.detect-shaft-version-change.outputs.changed }}}}
          INTELLIJ_RESULT: ${{{{ needs.intellij-verify.result }}}}
          CONTRACTS_RESULT: ${{{{ needs.release-contracts.result }}}}
          PILOT_RESULT: ${{{{ needs.pilot-tests.result }}}}
          CAPTURE_RESULT: ${{{{ needs.capture-journey.result }}}}
          PACKAGE_RESULT: ${{{{ needs.package-and-validate.result }}}}
          CONTAINER_RESULT: ${{{{ needs.container-smoke.result }}}}
        run: |
          if [ "${{DETECT_RESULT}}" != "success" ]; then
            echo "::error::detect-shaft-version-change did not succeed (${{DETECT_RESULT}})"
            exit 1
          fi
          if [ "${{DETECT_CHANGED}}" != "true" ]; then
            echo "No version or release-candidate infra change; isolated slices skipped."
            exit 0
          fi
          status=0
          for result in "${{INTELLIJ_RESULT}}" "${{CONTRACTS_RESULT}}" "${{PILOT_RESULT}}" "${{CAPTURE_RESULT}}" "${{PACKAGE_RESULT}}" "${{CONTAINER_RESULT}}"; do
            case "$result" in
              success) ;;
              *)
                echo "::error::a release-candidate slice did not pass (result: ${{result}})"
                status=1
                ;;
            esac
          done
          exit "${{status}}"
"""


class ShaftPilotReleaseValidatorTest(unittest.TestCase):
    def test_repository_static_contract_is_valid(self):
        self.assertEqual([], MODULE.validate_static(ROOT))

    def test_credential_shaped_values_are_rejected(self):
        errors = MODULE.scan_bytes("fixture", b"token=ghp_12345678901234567890")

        self.assertTrue(errors)

    def test_private_key_detection_marker_is_not_treated_as_key_material(self):
        errors = MODULE.scan_bytes("fixture", b"-----BEGIN PRIVATE KEY-----")

        self.assertEqual([], errors)

    def test_private_key_material_is_rejected(self):
        content = (
            b"-----BEGIN PRIVATE KEY-----\n"
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwx"
        )

        self.assertTrue(MODULE.scan_bytes("fixture", content))

    def test_allure_attachments_are_scanned_for_canaries(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            for module in MODULE.PILOT_MODULES:
                results = root / module / "allure-results"
                results.mkdir(parents=True)
                (results / f"{module}-result.json").write_text(
                    '{"status":"passed"}',
                    encoding="utf-8",
                )
            (
                root / "shaft-capture/allure-results/browser-attachment.txt"
            ).write_text("capture-browser-secret-canary", encoding="utf-8")

            errors = MODULE.validate_test_results(root)

        self.assertTrue(
            any("capture-browser-secret-canary" in error for error in errors)
        )

    def test_intellij_verify_composite_reference_satisfies_release_gate(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )

            errors = MODULE.validate_static(root)

        self.assertEqual([], errors)

    def test_missing_intellij_verify_composite_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            (root / ".github/actions/intellij-verify/action.yml").unlink()

            errors = MODULE.validate_static(root)

        self.assertIn(
            "intellij-verify composite action must verify the IntelliJ plugin release candidate",
            errors,
        )

    def test_intellij_verify_composite_missing_gradle_command_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            _write_text(
                root / ".github/actions/intellij-verify/action.yml",
                """name: 'Verify or Publish IntelliJ Plugin'
                runs:
                  using: 'composite'
                  steps:
                    - name: Run Gradle build
                      shell: bash
                      run: echo "gradle command removed"
                """,
            )

            errors = MODULE.validate_static(root)

        self.assertIn(
            "intellij-verify composite action must verify the IntelliJ plugin release candidate",
            errors,
        )

    def test_workflows_missing_intellij_verify_reference_are_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            for workflow in ("mavenCentral_cd.yml", "shaft-pilot-release.yml"):
                _write_text(
                    root / ".github/workflows" / workflow,
                    """Verify IntelliJ plugin release candidate
Validate SHAFT Pilot release contract
Run deterministic SHAFT Pilot tests
Run headless SHAFT Capture release journey
Validate Maven publication
Deploy to Maven Central
Verify published Maven Central coordinates
""",
                )

            errors = MODULE.validate_static(root)

        self.assertIn(
            "mavenCentral_cd.yml must verify the IntelliJ plugin release candidate", errors
        )
        self.assertIn(
            "shaft-pilot-release.yml must verify the IntelliJ plugin release candidate", errors
        )

    def test_plugin_version_must_match_reactor_version(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630-beta.0"
            )

            errors = MODULE.validate_static(root)

        self.assertTrue(
            any(
                "shaft-intellij pluginVersion must match the reactor version"
                in error
                for error in errors
            )
        )

    def test_plugin_publish_channel_must_be_stable(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            _write_text(
                root / "shaft-intellij/build.gradle.kts",
                """intellijPlatform {
                    publishing {
                        channels = listOf("beta")
                    }
                }
                """,
            )
            errors = MODULE.validate_static(root)

        self.assertTrue(
            any(
                "shaft-intellij publishing channel must explicitly target the stable Marketplace channel"
                in error
                for error in errors
            )
        )

    def test_plugin_publish_channel_must_be_explicit(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            _write_text(
                root / "shaft-intellij/build.gradle.kts",
                """intellijPlatform {
                    publishing {
                    }
                }
                """,
            )
            errors = MODULE.validate_static(root)

        self.assertTrue(
            any(
                "shaft-intellij publishing channel must explicitly target the stable Marketplace channel"
                in error
                for error in errors
            )
        )

    def test_packaged_secret_canaries_are_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            version = "10.2.20260612"
            (root / "pom.xml").write_text(
                f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>io.github.shafthq</groupId>
                <artifactId>shaft-parent</artifactId>
                <version>{version}</version>
                </project>""",
                encoding="utf-8",
            )
            for module, artifact in MODULE.PUBLIC_ARTIFACTS.items():
                target = root / module / "target"
                target.mkdir(parents=True)
                with zipfile.ZipFile(target / f"{artifact}-{version}.jar", "w") as jar:
                    content = (
                        b"capture-browser-secret-canary"
                        if module == "shaft-capture"
                        else b"safe"
                    )
                    jar.writestr("fixture.txt", content)
            (root / "target").mkdir()
            (root / "target/bom.json").write_text("{}", encoding="utf-8")

            errors = MODULE.validate_build_outputs(root)

        self.assertTrue(
            any("capture-browser-secret-canary" in error for error in errors)
        )

    def test_failed_allure_status_is_rejected_across_all_modules(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            for module in MODULE.PILOT_MODULES:
                results = root / module / "allure-results"
                results.mkdir(parents=True)
                (results / f"{module}-result.json").write_text(
                    '{"status":"failed"}',
                    encoding="utf-8",
                )

            errors = MODULE.validate_test_results(root)

        self.assertTrue(
            any("failed" in error for error in errors),
            f"Expected failed status error, got: {errors}",
        )
        # Verify that all modules are checked, not just one
        failed_modules = [
            error for error in errors if "Allure result contains a failed test status" in error
        ]
        self.assertEqual(
            len(failed_modules),
            len(MODULE.PILOT_MODULES),
            f"Expected {len(MODULE.PILOT_MODULES)} failed modules, got {len(failed_modules)}: {failed_modules}",
        )

    def test_serial_release_candidate_job_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            _write_text(
                root / ".github/workflows/shaft-pilot-release.yml",
                """name: SHAFT Pilot Release Candidate
jobs:
  detect-shaft-version-change:
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    steps:
      - uses: actions/checkout@v7
  release-candidate:
    needs: detect-shaft-version-change
    runs-on: ubuntu-22.04
    timeout-minutes: 60
    steps:
      - name: Verify IntelliJ plugin release candidate
        uses: ./.github/actions/intellij-verify
      - name: Run deterministic SHAFT Pilot tests
        run: echo tests
""",
            )

            errors = MODULE.validate_static(root)

        self.assertTrue(
            any("must isolate" in error for error in errors),
            errors,
        )

    def test_broken_allure_status_is_rejected_across_all_modules(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            for module in MODULE.PILOT_MODULES:
                results = root / module / "allure-results"
                results.mkdir(parents=True)
                (results / f"{module}-result.json").write_text(
                    '{"status":"broken"}',
                    encoding="utf-8",
                )

            errors = MODULE.validate_test_results(root)

        self.assertTrue(
            any("broken" in error for error in errors),
            f"Expected broken status error, got: {errors}",
        )


class ShaftPilotReleaseIsolationTest(unittest.TestCase):
    def test_detect_fetches_only_exact_pull_request_commits(self):
        workflow = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
        detect_job = workflow["jobs"]["detect-shaft-version-change"]
        checkout = detect_job["steps"][0]
        detect = _step_blob(detect_job)

        self.assertEqual(1, checkout.get("with", {}).get("fetch-depth"))
        self.assertEqual(
            "ref: ${{ github.event.pull_request.head.sha || github.sha }}",
            "ref: " + checkout.get("with", {}).get("ref", ""),
        )
        self.assertIn(
            'git fetch --no-tags --depth=1 origin "${BASE_SHA}"',
            detect,
        )

    def test_isolated_slices_run_in_parallel_after_detect_only(self):
        workflow = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
        jobs = workflow["jobs"]

        for name in ISOLATED_SLICES:
            self.assertIn(name, jobs, f"missing isolated job {name}")
            self.assertEqual(
                ["detect-shaft-version-change"],
                _needs_list(jobs[name]),
                f"{name} must depend only on detect-shaft-version-change",
            )
            self.assertEqual(
                SLICE_CHANGED_IF,
                jobs[name].get("if"),
                f"{name} must gate on detect changed output",
            )
            self.assertIn("timeout-minutes", jobs[name], name)
            self.assertIn("actions/checkout@", _step_blob(jobs[name]), name)

    def test_release_candidate_aggregates_isolated_slices(self):
        workflow = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
        aggregator = workflow["jobs"]["release-candidate"]
        needs = _needs_list(aggregator)
        blob = _step_blob(aggregator)

        self.assertEqual("always()", aggregator.get("if"))
        self.assertIn("detect-shaft-version-change", needs)
        for name in ISOLATED_SLICES:
            self.assertIn(name, needs)
        self.assertIn("DETECT_RESULT", blob)
        self.assertIn("DETECT_CHANGED", blob)
        self.assertIn('!= "success"', blob)
        self.assertIn("success)", blob)
        self.assertNotIn("mvn --batch-mode", blob)
        self.assertNotIn("./.github/actions/intellij-verify", blob)

    def test_isolated_slices_keep_the_named_release_checks(self):
        workflow = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
        jobs = workflow["jobs"]
        required = {
            "intellij-verify": "./.github/actions/intellij-verify",
            "release-contracts": "./.github/actions/release-contract-validators",
            "pilot-tests": "Run deterministic SHAFT Pilot tests",
            "capture-journey": "./.github/actions/capture-browser-e2e",
            "package-and-validate": "Validate packaged release outputs and MCP transports",
            "container-smoke": "./.github/actions/mcp-container-smoke",
        }

        for name, token in required.items():
            self.assertIn(name, jobs, f'missing isolated job {name}')
            self.assertIn(token, _step_blob(jobs[name]), name)

    def test_dropping_slice_changed_if_fails_isolation(self):
        text = WORKFLOW_PATH.read_text(encoding="utf-8")
        mutated = text.replace(
            f"    if: {SLICE_CHANGED_IF}\n",
            "",
        )
        self.assertNotEqual(text, mutated)
        errors = MODULE.validate_release_candidate_isolation(mutated)
        self.assertTrue(
            any("changed" in error for error in errors),
            errors,
        )

    def test_stub_exit_0_aggregator_fails_isolation(self):
        text = WORKFLOW_PATH.read_text(encoding="utf-8")
        rc_at = text.index("  release-candidate:")
        env_at = text.index("        env:", rc_at)
        mutated = text[:env_at] + "        run: exit 0\n"
        rc_blob = mutated[mutated.index("  release-candidate:") :]
        self.assertNotIn("DETECT_RESULT", rc_blob)
        self.assertIn("exit 0", rc_blob)
        errors = MODULE.validate_release_candidate_isolation(mutated)
        self.assertTrue(
            any("fail-closed" in error or "DETECT_RESULT" in error for error in errors),
            errors,
        )

    def test_always_must_bind_to_release_candidate_job(self):
        text = WORKFLOW_PATH.read_text(encoding="utf-8")
        # Move job-level if: always() onto detect so a width-blind four-space
        # search still matches, but release-candidate no longer owns it.
        mutated = text.replace(
            "    if: always()\n    runs-on: ubuntu-22.04\n    timeout-minutes: 5\n    steps:\n      - name: Evaluate isolated slice results",
            "    runs-on: ubuntu-22.04\n    timeout-minutes: 5\n    steps:\n      - name: Evaluate isolated slice results",
            1,
        )
        mutated = mutated.replace(
            "  detect-shaft-version-change:\n    runs-on: ubuntu-22.04\n",
            "  detect-shaft-version-change:\n    if: always()\n    runs-on: ubuntu-22.04\n",
            1,
        )
        self.assertIn("  detect-shaft-version-change:\n    if: always()\n", mutated)
        rc_start = mutated.index("  release-candidate:")
        rc_block = mutated[rc_start:]
        self.assertNotIn("\n    if: always()\n", rc_block)
        self.assertIn("    if: always()", mutated)
        errors = MODULE.validate_release_candidate_isolation(mutated)
        self.assertTrue(
            any("release-candidate" in error and "always()" in error for error in errors),
            errors,
        )

    def test_workflow_without_jobs_fails_isolation(self):
        stub = """Verify IntelliJ plugin release candidate
      uses: ./.github/actions/intellij-verify
Validate SHAFT Pilot release contract
Run deterministic SHAFT Pilot tests
"""
        self.assertNotIn("jobs:", stub)
        errors = MODULE.validate_release_candidate_isolation(stub)
        self.assertTrue(errors, "missing jobs: must fail closed, not return []")

    def test_minimal_isolated_fixture_passes_isolation(self):
        errors = MODULE.validate_release_candidate_isolation(
            _minimal_isolated_workflow_text()
        )
        self.assertEqual([], errors)


class IntellijMarketplacePublishContractTest(unittest.TestCase):
    def test_live_publish_workflow_matches_the_marketplace_contract(self):
        workflow = (
            ROOT / ".github/workflows/publish-intellij-plugin.yml"
        ).read_text(encoding="utf-8")
        action = (ROOT / ".github/actions/intellij-verify/action.yml").read_text(
            encoding="utf-8"
        )
        self.assertEqual([], MODULE.validate_intellij_marketplace_publish(workflow, action))

    def test_minimal_publish_fixture_passes(self):
        self.assertEqual(
            [],
            MODULE.validate_intellij_marketplace_publish(
                _minimal_publish_intellij_workflow_text()
            ),
        )

    def test_one_job_verify_and_publish_fails(self):
        collapsed = """name: Publish IntelliJ Plugin
jobs:
  publish:
    timeout-minutes: 20
    steps:
      - uses: ./.github/actions/intellij-verify
        with:
          publish: 'true'
      - run: python3 scripts/ci/assert_intellij_marketplace_receipt.py
  notify-missed-publish:
    if: always()
    permissions:
      issues: write
    steps:
      - uses: ./.github/actions/notify-intellij-marketplace-publish
        with:
          label: intellij-marketplace-publish-miss
"""
        errors = MODULE.validate_intellij_marketplace_publish(collapsed)
        self.assertTrue(
            any("isolate verify" in error or "skip verifyPlugin" in error for error in errors),
            errors,
        )

    def test_always_must_bind_to_notify_missed_publish_job(self):
        workflow = (
            ROOT / ".github/workflows/publish-intellij-plugin.yml"
        ).read_text(encoding="utf-8")
        action = (ROOT / ".github/actions/intellij-verify/action.yml").read_text(
            encoding="utf-8"
        )
        mutated = workflow.replace(
            "  notify-missed-publish:\n"
            "    name: Fail closed on missed Marketplace publish\n"
            "    needs: [verify, publish]\n"
            "    if: always()\n",
            "  notify-missed-publish:\n"
            "    name: Fail closed on missed Marketplace publish\n"
            "    needs: [verify, publish]\n",
            1,
        )
        self.assertNotEqual(workflow, mutated)
        self.assertIn("if: always()", mutated)
        notify_at = mutated.index("  notify-missed-publish:")
        notify_block = mutated[notify_at:]
        next_job = notify_block.find("\n  ", 4)
        if next_job != -1:
            notify_block = notify_block[:next_job]
        self.assertNotIn("if: always()", notify_block)
        errors = MODULE.validate_intellij_marketplace_publish(mutated, action)
        self.assertTrue(
            any("always()" in error and "notify" in error for error in errors),
            errors,
        )

    def test_nightly_cancelled_skip_fails(self):
        mutated = _minimal_publish_intellij_workflow_text().replace(
            "if: always()",
            "uses: ./.github/actions/notify-nightly-failure\n"
            "        with:\n"
            "          outcome: ${{ contains(needs.*.result, 'cancelled') && 'skip' || 'success' }}",
            1,
        )
        errors = MODULE.validate_intellij_marketplace_publish(mutated)
        self.assertTrue(
            any("nightly" in error or "skip" in error for error in errors),
            errors,
        )

    def test_missing_receipt_pin_fails(self):
        mutated = _minimal_publish_intellij_workflow_text().replace(
            "assert_intellij_marketplace_receipt.py --log log --properties props",
            "echo no receipt",
        )
        errors = MODULE.validate_intellij_marketplace_publish(mutated)
        self.assertTrue(any("receipt" in error for error in errors), errors)

    def test_publish_wrapped_in_build_retry_fails(self):
        action = """
    - name: Sign and publish the plugin
      run: bash scripts/ci/build_retry.sh 2 15 bash shaft-intellij/gradlew signPlugin publishPlugin
"""
        errors = MODULE.validate_intellij_marketplace_publish(
            _minimal_publish_intellij_workflow_text(),
            action,
        )
        self.assertTrue(any("retry publishPlugin" in error for error in errors), errors)

    def test_missing_publish_workflow_fails_static_contract(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_reactor(
                root, reactor_version="10.2.20260630", plugin_version="10.2.20260630"
            )
            (root / ".github/workflows/publish-intellij-plugin.yml").unlink()
            errors = MODULE.validate_static(root)
        self.assertTrue(
            any("publish-intellij-plugin.yml" in error for error in errors),
            errors,
        )


if __name__ == "__main__":
    unittest.main()
