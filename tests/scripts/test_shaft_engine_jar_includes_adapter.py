"""SHAFT consumes the official Cucumber 8 adapter jar and does not unpack it into shaft-engine."""

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
    def test_adapter_jar_is_consumed_not_unpacked(self):
        pom = POM.read_text(encoding="utf-8")
        parent = (REPO_ROOT / "pom.xml").read_text(encoding="utf-8")
        start = pom.index("<artifactId>allure-cucumber8-jvm</artifactId>")
        block_start = pom.rfind("<dependency>", 0, start)
        block_end = pom.index("</dependency>", start)
        adapter = pom[block_start:block_end]
        self.assertIn("<groupId>io.qameta.allure</groupId>", adapter)
        self.assertNotIn("<version>", adapter)
        self.assertNotIn("io.github.shafthq", adapter)
        self.assertNotIn("<optional>", adapter)
        self.assertNotIn("a1df39ff", pom)
        self.assertNotIn("unpack-cucumber8-adapter", pom)
        self.assertNotIn("<include>io/**/*</include>", pom)
        self.assertNotIn("jitpack.io", pom)
        self.assertNotIn("jitpack.io", parent)
        self.assertNotIn("<repositories>", parent)
        self.assertNotIn("<module>allure-cucumber8-jvm</module>", parent)
        self.assertNotIn("raw.githubusercontent.com/MohabMohie/allure-java/maven-repo/", parent)
        self.assertFalse((REPO_ROOT / "allure-cucumber8-jvm").exists())
        self.assertFalse(VENDOR_SOURCE.exists())
        packaged = [
            path
            for path in sorted((REPO_ROOT / "shaft-engine" / "target").glob("shaft-engine-*.jar"))
            if not path.name.endswith(("-sources.jar", "-javadoc.jar"))
        ]
        if not packaged:
            return
        with zipfile.ZipFile(packaged[-1]) as archive:
            names = archive.namelist()
            self.assertNotIn(ADAPTER_CLASS, names)
            self.assertNotIn("module-info.class", names)
            self.assertNotIn("META-INF/aop-ajc.xml", names)


if __name__ == "__main__":
    unittest.main()
