import contextlib
import io
import json
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest.mock import patch

from scripts.ci.extract_allure_failures import (
    extract_failures,
    extract_surefire_failures,
    main,
    missing_surefire_failures,
    to_comparison_json,
    to_markdown,
    to_surefire_markdown,
)


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

    def test_ignores_non_result_json_payloads(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "array-attachment.json").write_text(json.dumps([{"id": 1}]), encoding="utf-8")
            (root / "object-attachment.json").write_text(json.dumps({"id": 1}), encoding="utf-8")
            archive_path = root / "allure.zip"
            with zipfile.ZipFile(archive_path, "w") as archive:
                archive.writestr("allure-results/array-attachment.json", json.dumps([{"id": 1}]))
                archive.writestr("allure-results/failed-result.json", json.dumps({"status": "failed", "name": "real failure"}))

            failures = extract_failures([root, archive_path])

        self.assertEqual([failure.method for failure in failures], ["real failure"])

    def test_extracts_surefire_failures_from_xml(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            reports = Path(temp_dir)
            (reports / "TEST-TestSuite.xml").write_text(
                """<?xml version="1.0" encoding="UTF-8"?>
<testsuite tests="2" failures="1" errors="1" skipped="0">
  <testcase classname="pkg.ExampleTest" name="fails">
    <failure message="expected true but was false">java.lang.AssertionError: expected true but was false
        at pkg.ExampleTest.fails(ExampleTest.java:10)</failure>
  </testcase>
  <testcase classname="pkg.ExampleTest" name="breaks">
    <error type="java.lang.RuntimeException">java.lang.RuntimeException: browser died
        at pkg.ExampleTest.breaks(ExampleTest.java:20)</error>
  </testcase>
</testsuite>
""",
                encoding="utf-8",
            )

            failures = extract_surefire_failures([reports])

        self.assertEqual([failure.status for failure in failures], ["failure", "error"])
        self.assertEqual(failures[0].method, "pkg.ExampleTest.fails")
        self.assertEqual(failures[0].reason, "expected true but was false")
        self.assertEqual(failures[1].method, "pkg.ExampleTest.breaks")
        self.assertEqual(failures[1].reason, "java.lang.RuntimeException")

    def test_detects_surefire_failures_missing_from_allure_results(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            allure = root / "allure-results"
            surefire = root / "surefire-reports"
            allure.mkdir()
            surefire.mkdir()
            (allure / "known-result.json").write_text(
                json.dumps(
                    {
                        "status": "failed",
                        "labels": [
                            {"name": "testClass", "value": "ExampleTest"},
                            {"name": "testMethod", "value": "recordedFailure"},
                        ],
                    }
                ),
                encoding="utf-8",
            )
            (surefire / "TEST-TestSuite.xml").write_text(
                """<?xml version="1.0" encoding="UTF-8"?>
<testsuite tests="2" failures="2" errors="0" skipped="0">
  <testcase classname="pkg.ExampleTest" name="recordedFailure">
    <failure message="recorded failure" />
  </testcase>
  <testcase classname="pkg.LightHouseGenerateReportCoverageUnitTest" name="missingFailure">
    <failure message="lighthouse artifact was not created" />
  </testcase>
</testsuite>
""",
                encoding="utf-8",
            )

            allure_failures = extract_failures([allure])
            surefire_failures = extract_surefire_failures([surefire])
            missing_failures = missing_surefire_failures(allure_failures, surefire_failures)
            markdown = to_surefire_markdown(missing_failures)
            comparison_json = json.loads(to_comparison_json(allure_failures, surefire_failures, missing_failures))

        self.assertEqual(
            [failure.method for failure in missing_failures], ["pkg.LightHouseGenerateReportCoverageUnitTest.missingFailure"]
        )
        self.assertIn("lighthouse artifact was not created", markdown)
        self.assertEqual(
            comparison_json["missingSurefireFailures"][0]["method"],
            "pkg.LightHouseGenerateReportCoverageUnitTest.missingFailure",
        )

    def test_cli_markdown_uses_allure_as_single_source_of_truth(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            allure = root / "allure-results"
            surefire = root / "surefire-reports"
            allure.mkdir()
            surefire.mkdir()
            (allure / "passed-result.json").write_text(
                json.dumps({"status": "passed", "name": "passes"}), encoding="utf-8"
            )
            (surefire / "TEST-TestSuite.xml").write_text(
                """<?xml version="1.0" encoding="UTF-8"?>
<testsuite tests="1" failures="1" errors="0" skipped="0">
  <testcase classname="pkg.ExampleTest" name="surefireOnlyFailure">
    <failure message="this should not be printed below the Allure summary" />
  </testcase>
</testsuite>
""",
                encoding="utf-8",
            )
            output = io.StringIO()
            with patch(
                "sys.argv",
                [
                    "extract_allure_failures.py",
                    str(allure),
                    "--surefire-reports",
                    str(surefire),
                ],
            ), contextlib.redirect_stdout(output):
                exit_code = main()

        self.assertEqual(exit_code, 0)
        self.assertIn("No failed or broken Allure test cases were found.", output.getvalue())
        self.assertNotIn("Surefire failures missing from Allure results", output.getvalue())
        self.assertNotIn("surefireOnlyFailure", output.getvalue())


if __name__ == "__main__":
    unittest.main()
