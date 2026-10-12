"""The packaged shaft-engine jar must contain the Cucumber 8 adapter package."""

import unittest
import zipfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
POM = REPO_ROOT / "shaft-engine" / "pom.xml"
ADAPTER_CLASS = "io/qameta/allure/cucumber8jvm/AllureCucumber8Jvm.class"
VENDOR_SOURCE = (
    REPO_ROOT
    / "shaft-engine"
    / "src"
    / "main"
    / "java"
    / "io"
    / "qameta"
    / "allure"
    / "cucumber8jvm"
    / "AllureCucumber8Jvm.java"
)


class ShaftEngineJarIncludesAdapterTest(unittest.TestCase):
    def test_jar_includes_io_package(self):
        pom = POM.read_text(encoding="utf-8")
        self.assertIn("<artifactId>allure-cucumber8-jvm</artifactId>", pom)
        self.assertIn("<version>a1df39ff</version>", pom)
        self.assertIn("raw.githubusercontent.com/MohabMohie/allure-java/maven-repo/", pom)
        self.assertNotIn("jitpack.io", pom)
        self.assertFalse(VENDOR_SOURCE.exists())
        self.assertIn("<include>io/**/*</include>", pom)
        self.assertIn("<include>com/**/*</include>", pom)
        packaged = [
            path
            for path in sorted((REPO_ROOT / "shaft-engine" / "target").glob("shaft-engine-*.jar"))
            if not path.name.endswith(("-sources.jar", "-javadoc.jar"))
        ]
        if not packaged:
            return
        with zipfile.ZipFile(packaged[-1]) as archive:
            names = archive.namelist()
            self.assertIn(ADAPTER_CLASS, names)
            self.assertNotIn("module-info.class", names)
            self.assertNotIn("META-INF/aop-ajc.xml", names)


if __name__ == "__main__":
    unittest.main()
