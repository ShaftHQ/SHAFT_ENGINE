"""Tests for scripts/ci/flaky_report.py (#7005)."""
from __future__ import annotations

import importlib.util
import io
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("flaky_report", ROOT / "scripts/ci/flaky_report.py")
flaky = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(flaky)

REPORT = """<testsuite>
<testcase classname="a.T" name="stable"/>
<testcase classname="a.T" name="flaky"><flakyFailure message="x"/></testcase>
<testcase classname="a.T" name="flakyErr"><flakyError message="y"/></testcase>
<testcase classname="a.T" name="broken"><failure message="z"/></testcase>
</testsuite>"""


class FlakyReportTest(unittest.TestCase):
    def test_reports_only_tests_that_passed_on_retry(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            reports = Path(tmp, "m", "target", "surefire-reports")
            reports.mkdir(parents=True)
            (reports / "TEST-a.T.xml").write_text(REPORT, encoding="utf-8")
            self.assertEqual(["a.T#flaky", "a.T#flakyErr"], flaky.flaky_tests(reports))
            out = io.StringIO()
            with redirect_stdout(out):
                self.assertEqual(0, flaky.main(["x", str(Path(tmp, "m"))]))
        self.assertIn("::warning title=Flaky test::a.T#flaky passed only on retry", out.getvalue())
        self.assertIn("2 passed only on retry", out.getvalue())

    def test_no_reports_means_no_flakes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual([], flaky.flaky_tests(Path(tmp)))
        self.assertIn("None: no test passed only on retry.", flaky.render("m", []))


if __name__ == "__main__":
    unittest.main()
