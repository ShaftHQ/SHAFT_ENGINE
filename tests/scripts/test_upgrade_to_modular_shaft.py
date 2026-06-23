import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
UPGRADER_PATH = ROOT / "shaft-upgrader/upgrade_to_modular_shaft.py"
SPEC = importlib.util.spec_from_file_location("shaft_upgrade_module", UPGRADER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"Cannot load upgrader module from {UPGRADER_PATH}")
upgrade = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = upgrade
SPEC.loader.exec_module(upgrade)


SIMPLE_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <groupId>example</groupId>
    <artifactId>demo</artifactId>
    <version>1.0.0</version>
    <dependencies>
        <dependency>
            <groupId>io.github.shafthq</groupId>
            <artifactId>SHAFT_ENGINE</artifactId>
            <version>9.3.20250928</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
</project>
"""


def command_result(returncode: int, output: str = "") -> upgrade.CommandResult:
    return upgrade.CommandResult(("mvn", "test-compile"), returncode, output, "")


class FakeRepairClient:
    def __init__(self, responses):
        self.responses = list(responses)
        self.calls = 0

    def request_repair(self, compile_output, files, shaft_version, modules):
        self.calls += 1
        if self.responses:
            return self.responses.pop(0)
        return {"summary": "No change", "changes": []}


class UpgradeToModularShaftTests(unittest.TestCase):
    def test_metadata_release_prefers_release(self):
        metadata = """<metadata><versioning><latest>2.0</latest><release>1.9</release>
        <versions><version>1.8</version><version>1.9</version></versions>
        </versioning></metadata>"""
        self.assertEqual(upgrade.metadata_release(metadata), "1.9")

    def test_metadata_release_falls_back_when_release_is_snapshot(self):
        metadata = """<metadata><versioning><latest>2.1-SNAPSHOT</latest><release>2.1-SNAPSHOT</release>
        <versions><version>2.0</version><version>2.1-SNAPSHOT</version></versions>
        </versioning></metadata>"""
        self.assertEqual(upgrade.metadata_release(metadata), "2.0")

    def test_metadata_release_ignores_unstable_qualifiers(self):
        metadata = """<metadata><versioning>
        <versions><version>2.0</version><version>2.1-rc1</version><version>2.2-M1</version></versions>
        </versioning></metadata>"""
        self.assertEqual(upgrade.metadata_release(metadata), "2.0")

    def test_transform_replaces_legacy_dependency_and_imports_bom(self):
        transformed = upgrade.transform_pom_bytes(
            SIMPLE_POM.encode(),
            "10.2.20260609",
            ("shaft-visual",),
        )
        root = upgrade.parse_xml(transformed)
        properties = upgrade.direct_child(root, "properties")
        self.assertEqual(upgrade.child_text(properties, "shaft.version"), "10.2.20260609")

        dependencies = upgrade.direct_child(root, "dependencies")
        artifacts = {
            upgrade.child_text(dependency, "artifactId"): upgrade.child_text(dependency, "scope")
            for dependency in dependencies
            if upgrade.local_name(dependency.tag) == "dependency"
        }
        self.assertEqual(
            artifacts,
            {"shaft-engine": "test", "shaft-visual": "test"},
        )

        dependency_management = upgrade.direct_child(root, "dependencyManagement")
        managed = upgrade.direct_child(dependency_management, "dependencies")
        bom = next(iter(managed))
        self.assertEqual(upgrade.child_text(bom, "artifactId"), "shaft-bom")
        self.assertEqual(upgrade.child_text(bom, "version"), "${shaft.version}")
        self.assertEqual(upgrade.child_text(bom, "type"), "pom")
        self.assertEqual(upgrade.child_text(bom, "scope"), "import")
        self.assertNotIn("SHAFT_ENGINE", transformed.decode())

    def test_transform_removes_legacy_shaft_version_property(self):
        pom = """<project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>example</groupId>
            <artifactId>demo</artifactId>
            <version>1.0.0</version>
            <properties>
                <shaft_engine.version>9.3.20250928</shaft_engine.version>
            </properties>
            <dependencies>
                <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>SHAFT_ENGINE</artifactId>
                    <version>${shaft_engine.version}</version>
                </dependency>
            </dependencies>
        </project>
        """

        transformed = upgrade.transform_pom_bytes(pom.encode(), "10.2.20260609", ())
        root = upgrade.parse_xml(transformed)
        properties = upgrade.direct_child(root, "properties")

        self.assertEqual(upgrade.child_text(properties, "shaft.version"), "10.2.20260609")
        self.assertIsNone(upgrade.direct_child(properties, "shaft_engine.version"))

    def test_transform_removes_android_json_and_excludes_jsonassert(self):
        pom = """<project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>example</groupId>
            <artifactId>demo</artifactId>
            <version>1.0.0</version>
            <dependencyManagement>
                <dependencies>
                    <dependency>
                        <groupId>com.vaadin.external.google</groupId>
                        <artifactId>android-json</artifactId>
                        <version>0.0.20131108.vaadin1</version>
                    </dependency>
                    <dependency>
                        <groupId>org.skyscreamer</groupId>
                        <artifactId>jsonassert</artifactId>
                        <version>1.5.3</version>
                    </dependency>
                </dependencies>
            </dependencyManagement>
            <dependencies>
                <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>SHAFT_ENGINE</artifactId>
                    <version>9.3.20250928</version>
                    <scope>test</scope>
                </dependency>
                <dependency>
                    <groupId>com.vaadin.external.google</groupId>
                    <artifactId>android-json</artifactId>
                    <version>0.0.20131108.vaadin1</version>
                    <scope>test</scope>
                </dependency>
                <dependency>
                    <groupId>org.skyscreamer</groupId>
                    <artifactId>jsonassert</artifactId>
                    <version>1.5.3</version>
                    <scope>test</scope>
                </dependency>
            </dependencies>
        </project>
        """

        transformed = upgrade.transform_pom_bytes(
            pom.encode(),
            "10.2.20260609",
            (),
        )
        root = upgrade.parse_xml(transformed)
        dependency_management = upgrade.direct_child(root, "dependencyManagement")
        managed = upgrade.direct_child(dependency_management, "dependencies")
        dependencies = upgrade.direct_child(root, "dependencies")
        all_dependencies = [
            dependency
            for container in (managed, dependencies)
            for dependency in container
            if upgrade.local_name(dependency.tag) == "dependency"
        ]
        coordinates = {
            (
                upgrade.child_text(dependency, "groupId"),
                upgrade.child_text(dependency, "artifactId"),
            )
            for dependency in all_dependencies
        }
        self.assertNotIn(
            (upgrade.ANDROID_JSON_GROUP, upgrade.ANDROID_JSON_ARTIFACT),
            coordinates,
        )

        jsonassert_dependencies = [
            dependency
            for dependency in all_dependencies
            if (
                upgrade.child_text(dependency, "groupId"),
                upgrade.child_text(dependency, "artifactId"),
            )
            == (upgrade.JSONASSERT_GROUP, upgrade.JSONASSERT_ARTIFACT)
        ]
        self.assertEqual(len(jsonassert_dependencies), 2)
        for dependency in jsonassert_dependencies:
            exclusions = upgrade.direct_child(dependency, "exclusions")
            self.assertIsNotNone(exclusions)
            exclusion_coordinates = {
                (
                    upgrade.child_text(exclusion, "groupId"),
                    upgrade.child_text(exclusion, "artifactId"),
                )
                for exclusion in exclusions
                if upgrade.local_name(exclusion.tag) == "exclusion"
            }
            self.assertIn(
                (upgrade.ANDROID_JSON_GROUP, upgrade.ANDROID_JSON_ARTIFACT),
                exclusion_coordinates,
            )

    def test_scan_optional_modules_records_code_and_property_evidence(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "src/test/java/Demo.java"
            java.parent.mkdir(parents=True)
            java.write_text(
                """class Demo {
                void check() {
                    element.assertThat(logo).matchesReferenceImage();
                    RecordManager.startVideoRecording();
                }
                }""",
                encoding="utf-8",
            )
            properties = root / "src/test/resources/custom.properties"
            properties.parent.mkdir(parents=True)
            properties.write_text(
                "browserStack.platformsList=[{\"os\":\"Windows\"}]\n",
                encoding="utf-8",
            )

            evidence = upgrade.scan_optional_modules(root)

        self.assertTrue(evidence["shaft-visual"])
        self.assertTrue(evidence["shaft-video"])
        self.assertTrue(evidence["shaft-browserstack"])

    def test_legacy_pom_dependency_coordinates_trigger_optional_modules(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>legacy</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>io.github.shafthq</groupId>
                    <artifactId>SHAFT_ENGINE</artifactId><version>9.3.20250928</version></dependency>
                  <dependency><groupId>com.browserstack</groupId>
                    <artifactId>browserstack-java-sdk</artifactId><version>1</version></dependency>
                  <dependency><groupId>com.automation-remarks</groupId>
                    <artifactId>video-recorder-junit5</artifactId><version>1</version></dependency>
                  <dependency><groupId>org.openpnp</groupId>
                    <artifactId>opencv</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(
            analysis.optional_modules,
            ("shaft-browserstack", "shaft-video", "shaft-visual"),
        )

    def test_scan_optional_modules_accepts_current_shaft_property_styles(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            properties = root / "src/test/resources/custom.properties"
            properties.parent.mkdir(parents=True)
            properties.write_text("videoParams_recordVideo=true\n", encoding="utf-8")
            java = root / "src/test/java/Demo.java"
            java.parent.mkdir(parents=True)
            java.write_text(
                """class Demo {
                    void configure() {
                        SHAFT.Properties.browserStack.set().platformsList("[{}]");
                        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
                    }
                }""",
                encoding="utf-8",
            )

            evidence = upgrade.scan_optional_modules(root)

        self.assertTrue(evidence["shaft-video"])
        self.assertTrue(evidence["shaft-browserstack"])

    def test_direct_browserstack_settings_do_not_trigger_sdk_module(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "src/test/java/Demo.java"
            java.parent.mkdir(parents=True)
            java.write_text(
                """class Demo {
                    void configure() {
                        SHAFT.Properties.browserStack.set().deviceName("Pixel 9");
                    }
                }""",
                encoding="utf-8",
            )

            evidence = upgrade.scan_optional_modules(root)

        self.assertFalse(evidence["shaft-browserstack"])

    def test_all_supported_native_stack_and_runner_combinations_are_detected(self):
        stacks = {
            "selenium": ("org.seleniumhq.selenium", "selenium-chrome-driver"),
            "appium": ("io.appium", "java-client"),
            "rest-assured": ("io.rest-assured", "rest-assured"),
        }
        runners = {
            "testng": ("org.testng", "testng"),
            "junit": ("org.junit.jupiter", "junit-jupiter"),
        }
        for stack_name, stack_coordinate in stacks.items():
            for runner_name, runner_coordinate in runners.items():
                with self.subTest(stack=stack_name, runner=runner_name):
                    with tempfile.TemporaryDirectory() as temp_dir:
                        root = Path(temp_dir)
                        (root / "pom.xml").write_text(
                            f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
                            <modelVersion>4.0.0</modelVersion>
                            <groupId>example</groupId><artifactId>native</artifactId><version>1</version>
                            <dependencies>
                              <dependency><groupId>{stack_coordinate[0]}</groupId>
                                <artifactId>{stack_coordinate[1]}</artifactId><version>1</version></dependency>
                              <dependency><groupId>{runner_coordinate[0]}</groupId>
                                <artifactId>{runner_coordinate[1]}</artifactId><version>1</version></dependency>
                            </dependencies></project>""",
                            encoding="utf-8",
                        )

                        analysis = upgrade.analyze_project(root)

                    self.assertFalse(analysis.legacy_project)
                    self.assertEqual(analysis.stacks, (stack_name,))
                    self.assertEqual(analysis.runners, (runner_name,))
                    self.assertEqual(len(analysis.candidate_poms), 1)

    def test_junit4_coordinate_is_supported(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>native</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>org.seleniumhq.selenium</groupId>
                    <artifactId>selenium-firefox-driver</artifactId><version>1</version></dependency>
                  <dependency><groupId>junit</groupId>
                    <artifactId>junit</artifactId><version>4.13.2</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(analysis.stacks, ("selenium",))
        self.assertEqual(analysis.runners, ("junit",))

    def test_rest_assured_family_coordinate_is_supported(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>api</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>io.rest-assured</groupId>
                    <artifactId>json-schema-validator</artifactId><version>1</version></dependency>
                  <dependency><groupId>org.testng</groupId>
                    <artifactId>testng</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(analysis.stacks, ("rest-assured",))
        self.assertEqual(analysis.runners, ("testng",))

    def test_bom_only_modular_project_is_supported(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>modular</artifactId><version>1</version>
                <dependencyManagement><dependencies>
                  <dependency><groupId>io.github.shafthq</groupId>
                    <artifactId>shaft-bom</artifactId><version>10.2.20260623</version>
                    <type>pom</type><scope>import</scope></dependency>
                </dependencies></dependencyManagement>
                </project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertTrue(analysis.existing_modular_project)

    def test_native_child_pom_inherits_runner_detection_from_parent(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>parent</artifactId><version>1</version>
                <packaging>pom</packaging>
                <modules><module>web-tests</module></modules>
                <dependencies>
                  <dependency><groupId>org.testng</groupId>
                    <artifactId>testng</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )
            child = root / "web-tests"
            child.mkdir()
            (child / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <parent><groupId>example</groupId><artifactId>parent</artifactId><version>1</version></parent>
                <artifactId>web-tests</artifactId>
                <dependencies>
                  <dependency><groupId>org.seleniumhq.selenium</groupId>
                    <artifactId>selenium-java</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(
            [pom.relative_to(root).as_posix() for pom in analysis.candidate_poms],
            ["web-tests/pom.xml"],
        )

    def test_native_child_with_junit_platform_coordinate_is_candidate(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>parent</artifactId><version>1</version>
                <packaging>pom</packaging>
                <modules><module>web-tests</module></modules></project>""",
                encoding="utf-8",
            )
            child = root / "web-tests"
            child.mkdir()
            (child / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <parent><groupId>example</groupId><artifactId>parent</artifactId><version>1</version></parent>
                <artifactId>web-tests</artifactId>
                <dependencies>
                  <dependency><groupId>org.seleniumhq.selenium</groupId>
                    <artifactId>selenium-java</artifactId><version>1</version></dependency>
                  <dependency><groupId>org.junit.platform</groupId>
                    <artifactId>junit-platform-suite-api</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(
            [pom.relative_to(root).as_posix() for pom in analysis.candidate_poms],
            ["web-tests/pom.xml"],
        )

    def test_native_rest_assured_family_child_inherits_runner_from_parent(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>parent</artifactId><version>1</version>
                <packaging>pom</packaging>
                <modules><module>api-tests</module></modules>
                <dependencies>
                  <dependency><groupId>org.testng</groupId>
                    <artifactId>testng</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )
            child = root / "api-tests"
            child.mkdir()
            (child / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <parent><groupId>example</groupId><artifactId>parent</artifactId><version>1</version></parent>
                <artifactId>api-tests</artifactId>
                <dependencies>
                  <dependency><groupId>io.rest-assured</groupId>
                    <artifactId>json-schema-validator</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(
            [pom.relative_to(root).as_posix() for pom in analysis.candidate_poms],
            ["api-tests/pom.xml"],
        )

    def test_cucumber_project_is_supported(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>bdd</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>io.cucumber</groupId>
                    <artifactId>cucumber-java</artifactId><version>7.0.0</version></dependency>
                  <dependency><groupId>io.cucumber</groupId>
                    <artifactId>cucumber-testng</artifactId><version>7.0.0</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(analysis.stacks, ("cucumber",))
        self.assertEqual(analysis.runners, ("cucumber",))
        self.assertEqual(len(analysis.candidate_poms), 1)

    def test_runner_only_project_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>unit-only</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>org.testng</groupId>
                    <artifactId>testng</artifactId><version>7.0.0</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(upgrade.UpgradeError, "not a supported"):
                upgrade.analyze_project(root)

    def test_native_third_party_integrations_do_not_add_shaft_optional_modules(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>example</groupId><artifactId>native</artifactId><version>1</version>
                <dependencies>
                  <dependency><groupId>org.seleniumhq.selenium</groupId>
                    <artifactId>selenium-java</artifactId><version>1</version></dependency>
                  <dependency><groupId>org.testng</groupId>
                    <artifactId>testng</artifactId><version>1</version></dependency>
                  <dependency><groupId>com.browserstack</groupId>
                    <artifactId>browserstack-java-sdk</artifactId><version>1</version></dependency>
                  <dependency><groupId>org.openpnp</groupId>
                    <artifactId>opencv</artifactId><version>1</version></dependency>
                </dependencies></project>""",
                encoding="utf-8",
            )

            analysis = upgrade.analyze_project(root)

        self.assertEqual(analysis.optional_modules, ())

    def test_transform_does_not_copy_bom_import_metadata_to_engine_dependency(self):
        pom = """<project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>example</groupId>
            <artifactId>demo</artifactId>
            <version>1.0.0</version>
            <dependencies>
                <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>shaft-bom</artifactId>
                    <version>10.2.20260623</version>
                    <type>pom</type>
                    <scope>import</scope>
                </dependency>
            </dependencies>
        </project>
        """

        transformed = upgrade.transform_pom_bytes(pom.encode(), "10.2.20260623", ())
        root = upgrade.parse_xml(transformed)
        dependencies = upgrade.direct_child(root, "dependencies")
        engine = next(
            dependency
            for dependency in dependencies
            if upgrade.child_text(dependency, "artifactId") == "shaft-engine"
        )

        self.assertEqual(upgrade.child_text(engine, "type"), "")
        self.assertEqual(upgrade.child_text(engine, "scope"), "")

    def test_failed_compile_restores_original_pom(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            pom = root / "pom.xml"
            pom.write_text(SIMPLE_POM, encoding="utf-8")
            analysis = upgrade.analyze_project(root)
            results = iter((command_result(0), command_result(1, "upgrade failure")))

            execution = upgrade.execute_upgrade_transaction(
                analysis,
                "10.2.20260609",
                ("mvn", "test-compile"),
                30,
                compile_runner=lambda *_: next(results),
            )

            self.assertFalse(execution.succeeded)
            self.assertTrue(execution.rolled_back)
            self.assertEqual(pom.read_text(encoding="utf-8"), SIMPLE_POM)

    def test_successful_compile_keeps_modular_pom(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            pom = root / "pom.xml"
            pom.write_text(SIMPLE_POM, encoding="utf-8")
            analysis = upgrade.analyze_project(root)
            results = iter((command_result(0), command_result(0)))

            execution = upgrade.execute_upgrade_transaction(
                analysis,
                "10.2.20260609",
                ("mvn", "test-compile"),
                30,
                compile_runner=lambda *_: next(results),
            )

            self.assertTrue(execution.succeeded)
            self.assertFalse(execution.rolled_back)
            upgraded = pom.read_text(encoding="utf-8")
            self.assertIn("<artifactId>shaft-bom</artifactId>", upgraded)
            self.assertIn("<artifactId>shaft-engine</artifactId>", upgraded)
            self.assertNotIn("<artifactId>SHAFT_ENGINE</artifactId>", upgraded)

    def test_three_failed_ai_attempts_roll_back_every_change(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            pom = root / "pom.xml"
            pom.write_text(SIMPLE_POM, encoding="utf-8")
            java = root / "src/test/java/Demo.java"
            java.parent.mkdir(parents=True)
            original_java = "class Demo { broken legacyCall; }\n"
            java.write_text(original_java, encoding="utf-8")
            analysis = upgrade.analyze_project(root)
            results = iter(
                (
                    command_result(0),
                    command_result(1, "Demo.java compile failure"),
                    command_result(1, "Demo.java compile failure"),
                    command_result(1, "Demo.java compile failure"),
                    command_result(1, "Demo.java compile failure"),
                )
            )
            repairs = [
                {
                    "summary": f"attempt {attempt}",
                    "changes": [
                        {
                            "path": "src/test/java/Demo.java",
                            "content": f"class Demo {{ int attempt = {attempt}; }}\n",
                            "reason": "compile",
                        }
                    ],
                }
                for attempt in range(1, 4)
            ]
            repair_client = FakeRepairClient(repairs)

            execution = upgrade.execute_upgrade_transaction(
                analysis,
                "10.2.20260609",
                ("mvn", "test-compile"),
                30,
                compile_runner=lambda *_: next(results),
                repair_client=repair_client,
            )

            self.assertFalse(execution.succeeded)
            self.assertEqual(execution.ai_attempts, 3)
            self.assertEqual(repair_client.calls, 3)
            self.assertEqual(pom.read_text(encoding="utf-8"), SIMPLE_POM)
            self.assertEqual(java.read_text(encoding="utf-8"), original_java)

    def test_contract_breaking_ai_response_is_reverted_before_successful_compile(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            pom = root / "pom.xml"
            pom.write_text(SIMPLE_POM, encoding="utf-8")
            analysis = upgrade.analyze_project(root)
            results = iter(
                (
                    command_result(0),
                    command_result(1, "pom.xml compile failure"),
                    command_result(0),
                )
            )
            invalid_pom = """<project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>example</groupId><artifactId>demo</artifactId><version>1</version>
            </project>"""
            repair_client = FakeRepairClient(
                [
                    {
                        "summary": "remove dependencies",
                        "changes": [
                            {
                                "path": "pom.xml",
                                "content": invalid_pom,
                                "reason": "incorrect repair",
                            }
                        ],
                    }
                ]
            )

            execution = upgrade.execute_upgrade_transaction(
                analysis,
                "10.2.20260609",
                ("mvn", "test-compile"),
                30,
                compile_runner=lambda *_: next(results),
                repair_client=repair_client,
            )

            self.assertTrue(execution.succeeded)
            upgraded = pom.read_text(encoding="utf-8")
            self.assertIn("<artifactId>shaft-bom</artifactId>", upgraded)
            self.assertIn("<artifactId>shaft-engine</artifactId>", upgraded)

    def test_ai_path_policy_rejects_files_outside_supplied_context(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            pom = root / "pom.xml"
            pom.write_text(SIMPLE_POM, encoding="utf-8")
            transaction = upgrade.FileTransaction()
            response = {
                "summary": "unsafe",
                "changes": [
                    {
                        "path": "../outside.java",
                        "content": "class Outside {}",
                        "reason": "unsafe",
                    }
                ],
            }
            with self.assertRaisesRegex(upgrade.UpgradeError, "unsafe path"):
                upgrade.apply_ai_changes(transaction, root, response, {"pom.xml"})

    def test_extract_response_output_text_reads_responses_api_shape(self):
        payload = {
            "output": [
                {
                    "type": "message",
                    "content": [
                        {
                            "type": "output_text",
                            "text": json.dumps({"summary": "ok", "changes": []}),
                        }
                    ],
                }
            ]
        }
        self.assertIn('"summary": "ok"', upgrade.extract_response_output_text(payload))

    def test_extract_response_output_text_reads_top_level_text(self):
        payload = {"output_text": json.dumps({"summary": "ok", "changes": []})}
        self.assertIn('"summary": "ok"', upgrade.extract_response_output_text(payload))

    def test_file_transaction_restores_created_and_existing_files(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            existing = root / "existing.txt"
            created = root / "created.txt"
            existing.write_bytes(b"before")
            transaction = upgrade.FileTransaction()
            transaction.write_bytes(existing, b"after")
            transaction.write_bytes(created, b"new")

            transaction.rollback()

            self.assertEqual(existing.read_bytes(), b"before")
            self.assertFalse(created.exists())

    def test_windows_custom_command_strips_surrounding_executable_quotes(self):
        project_root = Path.cwd()
        with mock.patch.object(upgrade.os, "name", "nt"):
            command = upgrade.parse_compile_command(
                '"C:\\Program Files\\Maven\\mvn.cmd" test-compile',
                project_root,
            )
        self.assertEqual(command[0], "C:\\Program Files\\Maven\\mvn.cmd")
        self.assertEqual(command[1], "test-compile")

    def test_default_compile_command_resolves_dependencies_before_compile(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            wrapper = root / "mvnw"
            wrapper.write_text("#!/bin/sh\n", encoding="utf-8")

            with mock.patch.object(upgrade.os, "name", "posix"):
                command = upgrade.default_compile_command(root)

        self.assertEqual(command[0], str(wrapper.resolve()))
        self.assertEqual(command[1:3], ["dependency:go-offline", "test-compile"])

    def test_windows_default_compile_command_prefers_cmd_wrapper(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            wrapper = root / "mvnw.cmd"
            wrapper.write_text("@echo off\n", encoding="utf-8")

            with mock.patch.object(upgrade.os, "name", "nt"):
                command = upgrade.default_compile_command(root)

        self.assertEqual(command[0], str(wrapper.resolve()))
        self.assertEqual(command[1:3], ["dependency:go-offline", "test-compile"])


if __name__ == "__main__":
    unittest.main()
