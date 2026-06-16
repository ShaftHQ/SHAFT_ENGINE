import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_quality_configuration import (
    validate_maven_jvm_configuration,
    validate_quality_configuration,
    validate_surefire_jacoco_arg_lines,
    validate_workflow_coverage_policy,
)


class ValidateQualityConfigurationTest(unittest.TestCase):
    def test_repository_configuration_is_valid(self):
        self.assertEqual(validate_quality_configuration(), [])

    def test_rejects_java_25_only_maven_startup_option(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / ".mvn").mkdir()
            (root / ".mvn" / "jvm.config").write_text(
                "--sun-misc-unsafe-memory-access=allow\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_maven_jvm_configuration(root),
                [
                    "Maven JVM configuration must guard Java 25-only options "
                    "for the dependency submission Java 21 runtime"
                ],
            )

    def test_accepts_guarded_java_25_maven_startup_option(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / ".mvn").mkdir()
            (root / ".mvn" / "jvm.config").write_text(
                "-XX:+IgnoreUnrecognizedVMOptions\n"
                "--sun-misc-unsafe-memory-access=allow\n",
                encoding="utf-8",
            )

            self.assertEqual(validate_maven_jvm_configuration(root), [])

    def test_rejects_surefire_arg_line_that_drops_jacoco_injection(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            module = root / "shaft-visual"
            module.mkdir()
            (module / "pom.xml").write_text(
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <build>
        <plugins>
            <plugin>
                <artifactId>maven-surefire-plugin</artifactId>
                <configuration>
                    <argLine>${surefireArgLine} ${mockitoAgentArgLine}</argLine>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>""",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_surefire_jacoco_arg_lines(root),
                ["shaft-visual Surefire argLine must preserve JaCoCo's @{argLine} injection"],
            )

    def test_rejects_optional_jacoco_coverage_in_workflows(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "e2eTests.yml").write_text(
                "steps:\n"
                "  - uses: ./.github/actions/post-test-report\n"
                "    with:\n"
                "      require-coverage: false\n",
                encoding="utf-8",
            )

            self.assertEqual(
                validate_workflow_coverage_policy(root),
                [
                    ".github/workflows/e2eTests.yml must not mark JaCoCo coverage "
                    "optional with 'require-coverage: false'"
                ],
            )

    def test_reports_missing_aggregate_module(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                '<project xmlns="http://maven.apache.org/POM/4.0.0"><modules/></project>',
                encoding="utf-8",
            )
            (root / ".github" / "workflows").mkdir(parents=True)
            (root / ".github" / "actions").mkdir(parents=True)
            (root / "shaft-engine").mkdir()
            (root / ".github" / "dependabot.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "coverage-readiness.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "security.yml").write_text("", encoding="utf-8")
            (root / ".github" / "workflows" / "e2eTests.yml").write_text("", encoding="utf-8")
            (root / "shaft-engine" / "pom.xml").write_text("", encoding="utf-8")

            errors = validate_quality_configuration(root)

            self.assertIn("root pom.xml must include report-aggregate", errors)
            self.assertIn("report-aggregate/pom.xml is missing", errors)


if __name__ == "__main__":
    unittest.main()
