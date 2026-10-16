"""
Failures-only rerun script shipped with shaft-test-execution (#7059).

A stub `mvn` writes Allure results and TestNG's testng-failed.xml, so the test
proves attempt 2 reruns only the failure, keeps the passed and the
named-blocked results, and falls back to the whole suite without
testng-failed.xml.
"""
from __future__ import annotations

import json
import os
import shutil
import stat
import subprocess  # nosec B404 - runs the repository's own script with fixed argv
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "shaft-skills/shaft-test-execution/scripts/rerun-failures.sh"

SUITE = """<!DOCTYPE suite SYSTEM "https://testng.org/testng-1.0.dtd">
<suite name="S"><test name="T"><classes><class name="a.C"/></classes></test></suite>
"""

# Attempt 1: t1 passes, t2 fails, t3 is a named-blocked skip. Later attempts
# (a rerun suite) make t2 pass. The stub records each suite file it ran.
STUB = r"""#!/usr/bin/env python3
import json, os, re, sys, uuid
suite = next(a.split("=", 1)[1] for a in sys.argv if a.startswith("-Dsurefire.suiteXmlFiles="))
open("ran.log", "a").write(suite + "\n")
os.makedirs("allure-results", exist_ok=True)
os.makedirs("target/surefire-reports", exist_ok=True)
rerun = "rerun-" in suite
def write(name, status, message=""):
    data = {"name": name, "status": status, "statusDetails": {"message": message},
            "labels": [{"name": "parentSuite", "value": "S"}]}
    json.dump(data, open(f"allure-results/{uuid.uuid4()}-result.json", "w"))
if rerun:
    write("t2", "passed")
else:
    write("t1", "passed"); write("t2", "failed", "boom"); write("t3", "skipped", "BLOCKED BUG-12")
    if os.environ.get("NO_FAILED_XML") != "1":
        open("target/surefire-reports/testng-failed.xml", "w").write(
            '<suite name="Failed suite [S]"><test name="T(failed)"><classes><class name="a.C">'
            '<methods><include name="t2"/></methods></class></classes></test></suite>')
"""


class RerunFailuresScriptTest(unittest.TestCase):
    def setUp(self) -> None:
        self.bash = shutil.which("bash")
        if self.bash is None:
            self.skipTest("bash not available")
        self.tmp = tempfile.TemporaryDirectory()
        self.work = Path(self.tmp.name)
        (self.work / "suite.xml").write_text(SUITE, encoding="utf-8")
        bin_dir = self.work / "bin"
        bin_dir.mkdir()
        stub = bin_dir / "mvn"
        stub.write_text(STUB, encoding="utf-8")
        stub.chmod(stub.stat().st_mode | stat.S_IEXEC)
        self.env = dict(os.environ, PATH=f"{bin_dir}{os.pathsep}{os.environ['PATH']}")

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def run_script(self, *extra: str, env: dict | None = None) -> subprocess.CompletedProcess:
        return subprocess.run([self.bash, str(SCRIPT), "--attempts", "3", *extra, "suite.xml"],  # nosec B603
                              cwd=self.work, env=env or self.env, capture_output=True, text=True, timeout=60)

    def statuses(self) -> dict[str, str]:
        found = {}
        for path in (self.work / "allure-results").glob("*-result.json"):
            data = json.loads(path.read_text(encoding="utf-8"))
            found[data["name"]] = data["status"]
        return found

    def test_second_attempt_reruns_only_the_failure_and_keeps_passed_and_blocked(self) -> None:
        result = self.run_script()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        ran = (self.work / "ran.log").read_text(encoding="utf-8").split()
        self.assertEqual(["suite.xml", "target/rerun-suite.xml"], ran)
        rerun = (self.work / "target/rerun-suite.xml").read_text(encoding="utf-8")
        self.assertIn('<suite name="S"', rerun)
        self.assertIn('<test name="T">', rerun)
        self.assertEqual({"t1": "passed", "t2": "passed", "t3": "skipped"}, self.statuses())
        self.assertTrue((self.work / "target/attempts/suite-1").is_dir())

    def test_without_testng_failed_xml_it_falls_back_to_the_whole_suite(self) -> None:
        result = self.run_script(env=dict(self.env, NO_FAILED_XML="1"))
        self.assertNotEqual(0, result.returncode)
        ran = (self.work / "ran.log").read_text(encoding="utf-8").split()
        self.assertEqual(["suite.xml"] * 3, ran)
        self.assertIn("still has failures after 3 attempts", result.stdout)


if __name__ == "__main__":
    unittest.main()
