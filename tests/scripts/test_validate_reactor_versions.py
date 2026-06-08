import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_reactor_versions import validate_reactor_versions


class ValidateReactorVersionsTest(unittest.TestCase):
    def write_pom(self, path: Path, contents: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents, encoding="utf-8")

    def test_accepts_aligned_parent_and_bom_versions(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            root_pom = root / "pom.xml"
            self.write_pom(root_pom, ROOT_POM)
            self.write_pom(root / "module" / "pom.xml", MODULE_POM.replace("VERSION", "1.2.3"))

            self.assertEqual(validate_reactor_versions(root_pom), [])

    def test_rejects_divergent_module_and_managed_versions(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            root_pom = root / "pom.xml"
            self.write_pom(root_pom, ROOT_POM)
            self.write_pom(root / "module" / "pom.xml", MODULE_POM.replace("VERSION", "9.9.9"))

            errors = validate_reactor_versions(root_pom)

            self.assertEqual(len(errors), 2)
            self.assertIn("parent version '9.9.9' does not match '1.2.3'", errors[0])
            self.assertIn("version '9.9.9' diverges from '1.2.3'", errors[1])


ROOT_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>io.github.shafthq</groupId><artifactId>shaft-parent</artifactId><version>1.2.3</version>
  <modules><module>module</module></modules>
</project>"""

MODULE_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <parent><groupId>io.github.shafthq</groupId><artifactId>shaft-parent</artifactId><version>VERSION</version></parent>
  <artifactId>shaft-bom</artifactId>
  <dependencyManagement><dependencies><dependency>
    <groupId>io.github.shafthq</groupId><artifactId>shaft-engine</artifactId><version>VERSION</version>
  </dependency></dependencies></dependencyManagement>
</project>"""


if __name__ == "__main__":
    unittest.main()
