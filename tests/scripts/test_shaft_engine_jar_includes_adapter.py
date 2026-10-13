"""SHAFT consumes the Cucumber 8 adapter jar and does not unpack it into shaft-engine."""

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
        self.assertIn("<artifactId>allure-cucumber8-jvm</artifactId>", pom)
        adapter = pom.split("<artifactId>allure-cucumber8-jvm</artifactId>", 1)[1].split("</dependency>", 1)[0]
        self.assertIn("<version>${project.version}</version>", adapter)
        self.assertNotIn("<optional>", adapter)
        self.assertNotIn("a1df39ff", pom)
        self.assertNotIn("unpack-cucumber8-adapter", pom)
        self.assertNotIn("<include>io/**/*</include>", pom)
        self.assertNotIn("jitpack.io", pom)
        self.assertNotIn("jitpack.io", parent)
        self.assertNotIn("<repositories>", parent)
        self.assertNotIn("raw.githubusercontent.com/MohabMohie/allure-java/maven-repo/", parent)
        module = (REPO_ROOT / "allure-cucumber8-jvm" / "pom.xml").read_text(encoding="utf-8")
        self.assertIn("ace544ca18fa1839da535242a8150a0776198958f29ca242b75f3235ec34c36d", module)
        self.assertNotIn("<repositories>", module)
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
