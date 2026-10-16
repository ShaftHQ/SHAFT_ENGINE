"""`install --commit HEAD` names the real problem instead of 'source record is invalid'."""

from __future__ import annotations

import subprocess  # nosec B404 - runs the repository's own installer with fixed arguments
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class InstallCommitRefTest(unittest.TestCase):
    def run_install(self, commit: str) -> subprocess.CompletedProcess[str]:
        with tempfile.TemporaryDirectory() as project:
            return subprocess.run(  # nosec B603 - fixed argv
                [sys.executable, str(ROOT / "chaos-engine/install.py"), "install", "--project", project,
                 "--source", str(ROOT / "chaos-engine"), "--commit", commit, "--skip-tools"],
                capture_output=True, text=True, timeout=120, check=False)

    def test_symbolic_ref_is_refused_with_the_fix(self):
        result = self.run_install("HEAD")
        output = result.stdout + result.stderr
        self.assertNotEqual(0, result.returncode)
        self.assertIn("full lowercase 40-hex sha", output)
        self.assertIn("git rev-parse HEAD", output)
        self.assertNotIn("source record is invalid", output)

    def test_short_sha_is_refused_too(self):
        result = self.run_install("1465f10")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("40-hex", result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
