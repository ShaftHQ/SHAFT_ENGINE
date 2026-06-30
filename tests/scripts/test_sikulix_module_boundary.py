import unittest
import xml.etree.ElementTree as ET
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NAMESPACE = {"m": "http://maven.apache.org/POM/4.0.0"}
SIKULIX_API = ("com.sikulix", "sikulixapi")
FORBIDDEN_SHADOW_CLASSES = {
    "src/main/java/com/shaft/driver/SHAFT.java",
    "src/main/java/com/shaft/driver/DriverFactory.java",
    "src/main/java/com/shaft/driver/internal/FluentWebDriverAction.java",
    "src/main/java/com/shaft/gui/element/internal/ElementActionsHelper.java",
    "src/main/java/com/shaft/gui/internal/image/ScreenshotManager.java",
}


def dependencies(pom: Path) -> dict[tuple[str, str], ET.Element]:
    root = ET.parse(pom).getroot()
    return {
        (
            dependency.findtext("m:groupId", namespaces=NAMESPACE),
            dependency.findtext("m:artifactId", namespaces=NAMESPACE),
        ): dependency
        for dependency in root.findall("m:dependencies/m:dependency", NAMESPACE)
    }


class SikuliXModuleBoundaryTest(unittest.TestCase):
    def test_engine_is_lean_and_retains_appium_windows_support(self):
        engine_dependencies = dependencies(ROOT / "shaft-engine/pom.xml")

        self.assertNotIn(SIKULIX_API, engine_dependencies)
        self.assertIn(("io.appium", "java-client"), engine_dependencies)
        self.assertTrue(
            (ROOT / "shaft-engine/src/test/java/testPackage/appium/WindowsDesktopAppiumE2ETest.java").is_file()
        )

    def test_optional_module_owns_sikulix_dependency_without_shadowing_engine_classes(self):
        module_pom = ROOT / "shaft-sikulix/pom.xml"
        module = ET.parse(module_pom).getroot()
        module_dependencies = dependencies(module_pom)

        self.assertEqual(module.findtext("m:packaging", namespaces=NAMESPACE), "jar")
        self.assertEqual(
            module.findtext("m:properties/m:surefire.failIfNoSpecifiedTests", namespaces=NAMESPACE),
            "false",
        )
        self.assertIn(("${project.groupId}", "shaft-engine"), module_dependencies)
        self.assertIn(SIKULIX_API, module_dependencies)
        for relative_path in FORBIDDEN_SHADOW_CLASSES:
            with self.subTest(relative_path=relative_path):
                self.assertFalse((ROOT / "shaft-sikulix" / relative_path).exists())

    def test_bom_and_consumer_fixtures_use_optional_module(self):
        bom = ET.parse(ROOT / "shaft-bom/pom.xml").getroot()
        managed_artifacts = {
            dependency.findtext("m:artifactId", namespaces=NAMESPACE)
            for dependency in bom.findall(
                "m:dependencyManagement/m:dependencies/m:dependency", NAMESPACE
            )
        }
        fixture_dependencies = dependencies(
            ROOT / "tools/modularization/consumer-fixtures/sikulix/pom.xml"
        )
        combined_fixture_dependencies = dependencies(
            ROOT / "tools/modularization/consumer-fixtures/combined-modules/pom.xml"
        )

        self.assertIn("shaft-sikulix", managed_artifacts)
        self.assertIn(("io.github.shafthq", "shaft-sikulix"), fixture_dependencies)
        self.assertIn(("io.github.shafthq", "shaft-sikulix"), combined_fixture_dependencies)


if __name__ == "__main__":
    unittest.main()
