"""Unit tests for Surefire/TestNG green assertion used by N-run proof scripts."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts.ci.assert_surefire_green import (
    assert_surefire_green,
    main,
    summarize_surefire_reports,
    summarize_testng_results,
)


SUREFIRE_XML = """<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="{name}" tests="{tests}" failures="{failures}" errors="{errors}" skipped="0">
</testsuite>
"""

TESTNG_XML = """<?xml version="1.0" encoding="UTF-8"?>
<testng-results ignored="0" total="{total}" passed="{passed}" failed="{failed}" skipped="0">
</testng-results>
"""


class AssertSurefireGreenTest(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)

    def write_surefire(self, filename: str, *, failures: int = 0, errors: int = 0) -> Path:
        path = self.root / filename
        path.write_text(
            SUREFIRE_XML.format(
                name=filename, tests=1, failures=failures, errors=errors,
            ),
            encoding="utf-8",
        )
        return path

    def write_testng(self, *, failed: int = 0) -> Path:
        path = self.root / "testng-results.xml"
        path.write_text(
            TESTNG_XML.format(total=1, passed=0 if failed else 1, failed=failed),
            encoding="utf-8",
        )
        return path

    def test_summarize_surefire_aggregates_failures_and_errors(self):
        self.write_surefire("TEST-One.xml", failures=1, errors=0)
        self.write_surefire("TEST-Two.xml", failures=0, errors=2)
        self.assertEqual((1, 2), summarize_surefire_reports(self.root))

    def test_green_surefire_passes(self):
        self.write_surefire("TEST-Ok.xml", failures=0, errors=0)
        assert_surefire_green(self.root, label="unit run 1/3")
        self.assertEqual(0, main([str(self.root), "--label", "unit run 1/3"]))

    def test_failed_surefire_exits_nonzero(self):
        self.write_surefire("TEST-Bad.xml", failures=1, errors=0)
        with self.assertRaises(SystemExit) as raised:
            assert_surefire_green(self.root, label="unit run 2/3")
        self.assertEqual(1, raised.exception.code)
        self.assertEqual(1, main([str(self.root), "--label", "unit run 2/3"]))

    def test_testng_fallback_when_no_surefire_xml(self):
        self.write_testng(failed=0)
        assert_surefire_green(self.root, label="testng green")
        self.write_testng(failed=3)
        with self.assertRaises(SystemExit) as raised:
            assert_surefire_green(self.root, label="testng red")
        self.assertEqual(1, raised.exception.code)

    def test_missing_reports_fail_closed(self):
        with self.assertRaises(SystemExit) as raised:
            assert_surefire_green(self.root, label="empty")
        self.assertEqual(1, raised.exception.code)
        self.assertIsNone(summarize_testng_results(self.root))

    def test_wave_proof_script_uses_shared_helper(self):
        root = Path(__file__).resolve().parents[2]
        script = (root / "scripts/ci/run_click_type_wave_f_proof.sh").read_text(encoding="utf-8")
        self.assertIn("scripts/ci/assert_surefire_green.py", script)
        self.assertIn("maven.test.failure.ignore=false", script)
        playbook = (root / "chaos-engine/references/work-github-playbook.md").read_text(
            encoding="utf-8",
        )
        self.assertIn("Wave PR open checklist", playbook)
        self.assertIn("assert_surefire_green.py", playbook)


if __name__ == "__main__":
    unittest.main()
