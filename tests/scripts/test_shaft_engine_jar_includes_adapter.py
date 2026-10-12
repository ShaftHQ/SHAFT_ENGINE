"""The packaged shaft-engine jar must contain the Cucumber 8 adapter package."""

import unittest
import zipfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
POM = REPO_ROOT / "shaft-engine" / "pom.xml"
ADAPTER_CLASS = "io/qameta/allure/cucumber8jvm/AllureCucumber8Jvm.class"


class ShaftEngineJarIncludesAdapterTest(unittest.TestCase):
    def test_jar_includes_io_package(self):
        pom = POM.read_text(encoding="utf-8")
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
            self.assertIn(ADAPTER_CLASS, archive.namelist())


if __name__ == "__main__":
    unittest.main()
