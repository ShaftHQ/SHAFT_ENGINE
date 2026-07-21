import tempfile
import unittest
from pathlib import Path

from scripts.ci.assert_tests_executed import main, summarize


JUNIT_XML = """<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="{name}" tests="{tests}" failures="{failures}" errors="{errors}" skipped="{skipped}">
</testsuite>
"""


class AssertTestsExecutedTest(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)

    def write_report(self, filename, **counts):
        defaults = {"name": "com.example.Test", "tests": 0, "failures": 0, "errors": 0, "skipped": 0}
        defaults.update(counts)
        path = Path(self.temp_dir.name) / filename
        path.write_text(JUNIT_XML.format(**defaults), encoding="utf-8")
        return path

    def test_summarize_reads_junit_style_counts(self):
        # Real shape confirmed against shaft-cli/target/surefire-reports/TEST-*.xml
        # (Maven Surefire) and Gradle's Test task, which both emit this schema.
        report = self.write_report("all-skipped.xml", tests=6, failures=0, errors=0, skipped=6)

        tests, failures, errors, skipped = summarize(report)

        self.assertEqual((tests, failures, errors, skipped), (6, 0, 0, 6))

    def test_all_tests_skipped_fails_the_gate(self):
        # Guards against a gate-property typo silently producing a false-green nightly.
        report = self.write_report("all-skipped.xml", tests=6, failures=0, errors=0, skipped=6)

        exit_code = main([str(report)])

        self.assertEqual(exit_code, 1)

    def test_some_tests_executed_passes_the_gate(self):
        report = self.write_report("partial.xml", tests=6, failures=0, errors=0, skipped=3)

        exit_code = main([str(report)])

        self.assertEqual(exit_code, 0)

    def test_real_failure_still_fails_the_gate_even_if_executed(self):
        report = self.write_report("failed.xml", tests=6, failures=1, errors=0, skipped=0)

        exit_code = main([str(report)])

        self.assertEqual(exit_code, 1)

    def test_aggregates_multiple_report_files(self):
        # Nightly jobs pass one XML per test class; a typo affecting only one class
        # must still fail the gate even though the other classes executed fine.
        executed = self.write_report("executed.xml", tests=3, failures=0, errors=0, skipped=0)
        all_skipped = self.write_report("skipped.xml", tests=3, failures=0, errors=0, skipped=3)

        exit_code = main([str(executed), str(all_skipped), "--min-executed", "4"])

        self.assertEqual(exit_code, 1)

    def test_min_executed_threshold_is_configurable(self):
        report = self.write_report("partial.xml", tests=6, failures=0, errors=0, skipped=5)

        exit_code = main([str(report), "--min-executed", "1"])

        self.assertEqual(exit_code, 0)


if __name__ == "__main__":
    unittest.main()
