"""#8078: the harness fetch refuses a dangling symlink and does not keep a mismatched dest."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "fetch_harness_overlay.sh"


class FetchHarnessOverlayTests(unittest.TestCase):
    def test_script_checks_the_fetch_and_removes_a_mismatched_dest(self):
        text = SCRIPT.read_text(encoding="utf-8")
        self.assertIn("fetch.fsckObjects=true", text)
        self.assertIn("core.hooksPath=/dev/null", text)
        self.assertIn("GIT_TERMINAL_PROMPT=0", text)
        self.assertIn('rm -rf "$dest"', text)

    def test_dangling_symlink_is_refused_before_init(self):
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary) / "repo"
            repo.mkdir()
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            pin = repo / "scripts" / "ci"
            pin.mkdir(parents=True)
            (pin / "harness_pin.txt").write_text("a" * 40 + "\n", encoding="utf-8")
            dest = repo / ".chaos-engine"
            dest.symlink_to(repo / "missing-target")
            completed = subprocess.run(
                ["bash", str(SCRIPT)], cwd=repo, capture_output=True, text=True, check=False
            )
            self.assertEqual(1, completed.returncode, completed.stderr)
            self.assertIn("already exists", completed.stderr)
            self.assertTrue(dest.is_symlink())
            self.assertFalse(dest.exists())


if __name__ == "__main__":
    unittest.main()
