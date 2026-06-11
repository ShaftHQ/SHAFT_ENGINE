import tempfile
import unittest
from pathlib import Path
from unittest import mock

import scripts.ci.verify_maven_central_release as verify

FIXTURE_GOALS = verify.FIXTURE_GOALS
fixture_commands = verify.fixture_commands
publication_paths = verify.publication_paths
write_settings = verify.write_settings


class VerifyMavenCentralReleaseTest(unittest.TestCase):
    def test_maven_executable_prefers_windows_command_shim(self):
        with mock.patch.object(
            verify.shutil,
            "which",
            side_effect=lambda candidate: "C:/tools/mvn.cmd" if candidate == "mvn.cmd" else None,
        ):
            self.assertEqual(verify.maven_executable("Windows"), "C:/tools/mvn.cmd")

    def test_maven_executable_reports_missing_maven(self):
        with mock.patch.object(verify.shutil, "which", return_value=None):
            with self.assertRaisesRegex(RuntimeError, "Maven executable was not found"):
                verify.maven_executable("Linux")

    def test_publication_paths_cover_every_public_artifact_and_classifier(self):
        paths = publication_paths("1.2.3")

        self.assertIn("io/github/shafthq/shaft-engine/1.2.3/shaft-engine-1.2.3.jar", paths)
        self.assertIn("io/github/shafthq/shaft-pilot-core/1.2.3/shaft-pilot-core-1.2.3.jar", paths)
        self.assertIn("io/github/shafthq/shaft-ai/1.2.3/shaft-ai-1.2.3-javadoc.jar.asc", paths)
        self.assertIn("io/github/shafthq/shaft-visual/1.2.3/shaft-visual-1.2.3-javadoc.jar.asc", paths)
        self.assertIn("io/github/shafthq/shaft-bom/1.2.3/shaft-bom-1.2.3.pom.asc", paths)
        self.assertIn("io/github/shafthq/SHAFT_ENGINE/1.2.3/SHAFT_ENGINE-1.2.3.pom", paths)

    def test_fixture_commands_use_isolated_repositories_and_release_version(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = Path(temp_dir)
            settings = temp / "settings.xml"
            commands = fixture_commands("1.2.3", temp / "repository", settings)

            self.assertEqual(len(commands), len(FIXTURE_GOALS))
            self.assertTrue(all("-Dshaft.version=1.2.3" in command for command in commands))
            self.assertEqual(
                {Path(command[command.index("-f") + 1]).parent.name for command in commands},
                set(FIXTURE_GOALS),
            )
            repositories = {
                next(value for value in command if value.startswith("-Dmaven.repo.local="))
                for command in commands
            }
            self.assertEqual(len(repositories), len(FIXTURE_GOALS))

    def test_settings_force_the_requested_repository(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            settings = Path(temp_dir) / "settings.xml"
            write_settings(settings, "https://repo.example.test/maven2")

            contents = settings.read_text(encoding="utf-8")
            self.assertIn("https://repo.example.test/maven2", contents)
            self.assertIn("<mirrorOf>*</mirrorOf>", contents)


if __name__ == "__main__":
    unittest.main()
