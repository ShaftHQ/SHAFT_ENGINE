"""The packaged shaft-engine jar must contain the Cucumber 8 adapter package."""

import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
POM = REPO_ROOT / "shaft-engine" / "pom.xml"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


class ShaftEngineJarIncludesAdapterTest(unittest.TestCase):
    def test_jar_includes_io_package(self):
        root = ET.parse(POM).getroot()
        jar = next(
            plugin
            for plugin in root.findall(".//m:plugin", NS)
            if plugin.findtext("m:artifactId", default="", namespaces=NS) == "maven-jar-plugin"
        )
        patterns = [node.text for node in jar.findall(".//m:include", NS)]
        self.assertIn("io/**/*", patterns)
        self.assertIn("com/**/*", patterns)
        packaged = sorted((REPO_ROOT / "shaft-engine" / "target").glob("shaft-engine-*.jar"))
        packaged = [path for path in packaged if not path.name.endswith(("-sources.jar", "-javadoc.jar"))]
        if not packaged:
            return
        import zipfile
        with zipfile.ZipFile(packaged[-1]) as archive:
            names = archive.namelist()
        self.assertIn(
            "io/qameta/allure/cucumber8jvm/AllureCucumber8Jvm.class",
            names,
        )


if __name__ == "__main__":
    unittest.main()
