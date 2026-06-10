import json
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NAMESPACE = {"m": "http://maven.apache.org/POM/4.0.0"}
BROWSERSTACK_SDK = ("com.browserstack", "browserstack-java-sdk")


def dependencies(pom: Path) -> dict[tuple[str, str], ET.Element]:
    root = ET.parse(pom).getroot()
    return {
        (
            dependency.findtext("m:groupId", namespaces=NAMESPACE),
            dependency.findtext("m:artifactId", namespaces=NAMESPACE),
        ): dependency
        for dependency in root.findall("m:dependencies/m:dependency", NAMESPACE)
    }


class BrowserStackModuleBoundaryTest(unittest.TestCase):
    def test_engine_is_lean_and_retains_appium(self):
        engine_dependencies = dependencies(ROOT / "shaft-engine" / "pom.xml")

        self.assertNotIn(BROWSERSTACK_SDK, engine_dependencies)
        self.assertIn(("io.appium", "java-client"), engine_dependencies)

    def test_optional_module_provides_engine_and_browserstack_sdk(self):
        module_pom = ROOT / "shaft-browserstack" / "pom.xml"
        module = ET.parse(module_pom).getroot()
        module_dependencies = dependencies(module_pom)

        self.assertEqual(module.findtext("m:packaging", namespaces=NAMESPACE), "jar")
        self.assertEqual(
            module.findtext("m:properties/m:surefire.failIfNoSpecifiedTests", namespaces=NAMESPACE),
            "false",
        )
        self.assertIn(("${project.groupId}", "shaft-engine"), module_dependencies)
        self.assertIn(BROWSERSTACK_SDK, module_dependencies)

    def test_bom_and_consumer_fixture_use_optional_module(self):
        bom = ET.parse(ROOT / "shaft-bom" / "pom.xml").getroot()
        managed_artifacts = {
            dependency.findtext("m:artifactId", namespaces=NAMESPACE)
            for dependency in bom.findall(
                "m:dependencyManagement/m:dependencies/m:dependency", NAMESPACE
            )
        }
        fixture_dependencies = dependencies(
            ROOT / "tools/modularization/consumer-fixtures/browserstack-sdk/pom.xml"
        )

        self.assertIn("shaft-browserstack", managed_artifacts)
        self.assertIn(("io.github.shafthq", "shaft-browserstack"), fixture_dependencies)

    def test_workflows_resolve_sdk_only_for_browserstack_jobs(self):
        workflows = [
            ROOT / ".github/workflows/e2eTests.yml",
            ROOT / ".github/workflows/e2eLocalTests.yml",
            ROOT / ".github/workflows/e2eLambdaTestTests.yml",
            ROOT / ".github/workflows/e2eMoonTests.yml",
        ]
        commands = [
            line.strip()
            for workflow in workflows
            for line in workflow.read_text(encoding="utf-8").splitlines()
            if "mvn " in line
        ]

        self.assertTrue(commands)
        for command in commands:
            if "-DexecutionAddress=browserstack" in command:
                expected_module = "shaft-browserstack"
            elif (
                "ImageProcessingActionsUnitTest" in command
                or "-pl shaft-visual -am" in command
            ):
                expected_module = "shaft-visual"
            elif "DesktopVideoRecordingProviderRegistrationTest" in command:
                expected_module = "shaft-video"
            else:
                expected_module = "shaft-engine"
            self.assertIn(f"-pl {expected_module} -am", command)
            if expected_module != "shaft-browserstack":
                self.assertNotIn("shaft-browserstack", command)

    def test_direct_cold_cache_saving_meets_issue_threshold(self):
        manifests = ROOT / "docs/modularization/dependency-baseline/manifests"
        summary = json.loads((manifests / "browserstack-sdk.json").read_text(encoding="utf-8"))
        baseline = json.loads(
            (manifests / summary["artifactsRef"]).read_text(encoding="utf-8")
        )
        browserstack = next(
            artifact
            for artifact in baseline["artifacts"]
            if artifact["coordinate"]
            == "com.browserstack:browserstack-java-sdk:jar:1.59.8"
        )

        self.assertGreaterEqual(browserstack["compressedBytes"], 38_204_017)


if __name__ == "__main__":
    unittest.main()
