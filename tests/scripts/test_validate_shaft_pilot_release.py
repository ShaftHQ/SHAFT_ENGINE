import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/validate_shaft_pilot_release.py"
SPEC = importlib.util.spec_from_file_location("validate_shaft_pilot_release", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


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
        root / ".github/workflows/mavenCentral_cd.yml",
        """Verify IntelliJ plugin release candidate
      gradle -p shaft-intellij check buildPlugin verifyPlugin
Validate SHAFT Pilot release contract
Run deterministic SHAFT Pilot tests
Run headless SHAFT Capture release journey
Validate Maven publication
Deploy to Maven Central
Verify published Maven Central coordinates
""",
    )
    _write_text(
        root / ".github/workflows/shaft-pilot-release.yml",
        """Verify IntelliJ plugin release candidate
      gradle -p shaft-intellij check buildPlugin verifyPlugin
Validate SHAFT Pilot release contract
Run deterministic SHAFT Pilot tests
Run headless SHAFT Capture release journey
Validate Maven publication
Deploy to Maven Central
Verify published Maven Central coordinates
""",
    )


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


if __name__ == "__main__":
    unittest.main()
