import tempfile
import unittest
import urllib.error
from pathlib import Path
from unittest import mock

from scripts.ci.check_maven_release_version import main, publication_url, release_exists


class CheckMavenReleaseVersionTest(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.pom = Path(self.temp_dir.name) / "pom.xml"
        self.pom.write_text(
            '<project xmlns="http://maven.apache.org/POM/4.0.0">'
            "<modelVersion>4.0.0</modelVersion>"
            "<groupId>io.github.shafthq</groupId>"
            "<artifactId>shaft-parent</artifactId>"
            "<version>1.2.3</version>"
            "</project>",
            encoding="utf-8",
        )

    def tearDown(self):
        self.temp_dir.cleanup()

    def test_builds_parent_publication_url(self):
        coordinate, url = publication_url(self.pom, "https://repo.example/maven2/")

        self.assertEqual(coordinate, "io.github.shafthq:shaft-parent:1.2.3")
        self.assertEqual(
            url,
            "https://repo.example/maven2/io/github/shafthq/shaft-parent/"
            "1.2.3/shaft-parent-1.2.3.pom",
        )

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_existing_parent_coordinate_stops_release(self, urlopen):
        urlopen.return_value.__enter__.return_value = object()

        coordinate, exists = release_exists(self.pom)

        self.assertEqual(coordinate, "io.github.shafthq:shaft-parent:1.2.3")
        self.assertTrue(exists)

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_missing_parent_coordinate_allows_release(self, urlopen):
        urlopen.side_effect = urllib.error.HTTPError(
            "https://repo.example", 404, "Not Found", {}, None
        )

        _, exists = release_exists(self.pom)

        self.assertFalse(exists)

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_central_error_fails_closed(self, urlopen):
        urlopen.side_effect = urllib.error.HTTPError(
            "https://repo.example", 503, "Unavailable", {}, None
        )

        with self.assertRaisesRegex(RuntimeError, "HTTP 503"):
            release_exists(self.pom)

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_github_output_reports_already_released_without_failing(self, urlopen):
        urlopen.return_value.__enter__.return_value = object()
        output_path = Path(self.temp_dir.name) / "github_output.txt"

        exit_code = main([
            "--pom", str(self.pom),
            "--github-output", str(output_path),
        ])

        self.assertEqual(exit_code, 0)
        self.assertEqual(output_path.read_text(encoding="utf-8"), "already_released=true\n")

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_github_output_reports_available_version(self, urlopen):
        urlopen.side_effect = urllib.error.HTTPError(
            "https://repo.example", 404, "Not Found", {}, None
        )
        output_path = Path(self.temp_dir.name) / "github_output.txt"

        exit_code = main([
            "--pom", str(self.pom),
            "--github-output", str(output_path),
        ])

        self.assertEqual(exit_code, 0)
        self.assertEqual(output_path.read_text(encoding="utf-8"), "already_released=false\n")

    @mock.patch("scripts.ci.check_maven_release_version.urllib.request.urlopen")
    def test_github_output_still_raises_on_genuine_errors(self, urlopen):
        urlopen.side_effect = urllib.error.HTTPError(
            "https://repo.example", 503, "Unavailable", {}, None
        )
        output_path = Path(self.temp_dir.name) / "github_output.txt"

        with self.assertRaisesRegex(RuntimeError, "HTTP 503"):
            main(["--pom", str(self.pom), "--github-output", str(output_path)])

        self.assertFalse(output_path.exists())

    def test_without_pom_falls_back_to_missing_file_error(self):
        with self.assertRaises(FileNotFoundError):
            main(["--pom", str(Path(self.temp_dir.name) / "missing-pom.xml")])


if __name__ == "__main__":
    unittest.main()
