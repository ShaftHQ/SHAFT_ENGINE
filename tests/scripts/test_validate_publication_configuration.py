import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_publication_configuration import validate_publication_configuration


class ValidatePublicationConfigurationTest(unittest.TestCase):
    def test_repository_publication_configuration_is_valid(self):
        self.assertEqual(validate_publication_configuration(), [])

    def test_reports_missing_public_module_and_release_ordering(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(ROOT_POM, encoding="utf-8")
            (root / "report-aggregate").mkdir()
            (root / "report-aggregate" / "pom.xml").write_text(AGGREGATE_POM, encoding="utf-8")
            (root / ".github" / "workflows").mkdir(parents=True)
            (root / ".github" / "workflows" / "mavenCentral_cd.yml").write_text(
                "- name: Create GitHub Release\n- name: Deploy to Maven Central\n",
                encoding="utf-8",
            )
            (root / "tools" / "modularization" / "publication-fixtures" / "all-modules").mkdir(parents=True)
            (root / "tools" / "modularization" / "publication-fixtures" / "all-modules" / "pom.xml").write_text(
                "<project xmlns=\"http://maven.apache.org/POM/4.0.0\"/>",
                encoding="utf-8",
            )

            errors = validate_publication_configuration(root)

            self.assertTrue(any("missing deployable modules" in error for error in errors))
            self.assertIn("Maven Central deployment must complete before GitHub release creation", errors)
            self.assertTrue(any("combined-module fixture" in error for error in errors))


ROOT_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>io.github.shafthq</groupId><artifactId>shaft-parent</artifactId><version>1.2.3</version>
  <modules><module>shaft-engine</module><module>report-aggregate</module></modules>
  <build><plugins/></build>
</project>"""

AGGREGATE_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <artifactId>report-aggregate</artifactId>
  <build><plugins><plugin><artifactId>maven-deploy-plugin</artifactId><configuration><skip>true</skip></configuration></plugin></plugins></build>
</project>"""


if __name__ == "__main__":
    unittest.main()
