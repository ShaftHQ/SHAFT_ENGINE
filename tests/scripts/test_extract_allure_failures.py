import json
import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts.ci.extract_allure_failures import extract_failures, to_markdown


class ExtractAllureFailuresTests(unittest.TestCase):
    def test_extracts_failed_and_broken_results_from_directory(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "passed-result.json").write_text(json.dumps({"status": "passed", "name": "passes"}), encoding="utf-8")
            (root / "failed-result.json").write_text(
                json.dumps(
                    {
                        "status": "failed",
                        "name": "fallbackName",
                        "labels": [
                            {"name": "testClass", "value": "ExampleTest"},
                            {"name": "testMethod", "value": "fails"},
                        ],
                        "statusDetails": {"message": "expected true but was false", "trace": "AssertionError\n at ExampleTest.fails"},
                    }
                ),
                encoding="utf-8",
            )
            (root / "broken-result.json").write_text(
                json.dumps(
                    {
                        "status": "broken",
                        "fullName": "pkg.ExampleTest.breaks",
                        "statusDetails": {"trace": "RuntimeException: browser died\n at Driver.start"},
                    }
                ),
                encoding="utf-8",
            )

            failures = extract_failures([root])

        self.assertEqual([failure.status for failure in failures], ["broken", "failed"])
        self.assertEqual(failures[0].method, "pkg.ExampleTest.breaks")
        self.assertEqual(failures[0].reason, "RuntimeException: browser died")
        self.assertEqual(failures[1].method, "ExampleTest.fails")
        self.assertEqual(failures[1].reason, "expected true but was false")

    def test_extracts_results_from_zip_archive_and_renders_markdown(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            archive_path = Path(temp_dir) / "allure.zip"
            with zipfile.ZipFile(archive_path, "w") as archive:
                archive.writestr(
                    "allure-results/test-result.json",
                    json.dumps(
                        {
                            "status": "failed",
                            "fullName": "pkg.ZipTest.fails",
                            "statusDetails": {"message": "value | was invalid"},
                        }
                    ),
                )

            failures = extract_failures([archive_path])
            markdown = to_markdown(failures)

        self.assertIn("`pkg.ZipTest.fails`", markdown)
        self.assertIn("value \\| was invalid", markdown)
        self.assertIn("`allure-results/test-result.json`", markdown)


if __name__ == "__main__":
    unittest.main()
