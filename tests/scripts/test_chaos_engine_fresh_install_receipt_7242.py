"""#7242 chunk 7: a fresh install into an empty workspace, no add-ons, prints the full receipt."""

from __future__ import annotations

import os
import re
import subprocess  # nosec B404 - runs the repository's own installer
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RECEIPT = re.compile(
    r"^entry: caveman=ultra ponytail=ultra ethics=loaded persona=none index=\d+ "
    r"retrieve=mempalace:missing-fresh-install graphify:missing-fresh-install$",
    re.M,
)


def git_head() -> str:
    return subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True,  # nosec B603 B607
                          text=True, check=True).stdout.strip()


class FreshInstallReceiptTest(unittest.TestCase):
    def test_fresh_install_without_add_ons_prints_the_full_receipt(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            subprocess.run(["git", "init", "-q"], cwd=project, check=True)  # nosec B603 B607
            env = {k: v for k, v in os.environ.items()
                   if not k.startswith(("CHAOS_ENGINE_MEMPALACE", "CHAOS_ENGINE_GRAPHIFY", "CHAOS_ENGINE_ADDONS"))}
            install = subprocess.run(  # nosec B603 - fixed argv
                [sys.executable, str(ROOT / "chaos-engine/install.py"), "install", "--project", str(project),
                 "--source", str(ROOT / "chaos-engine"), "--commit", git_head(), "--skip-tools"],
                capture_output=True, text=True, timeout=300, env=env, check=False)
            self.assertEqual(0, install.returncode, install.stderr[-2000:])
            entry = subprocess.run(  # nosec B603 - fixed argv
                [sys.executable, str(project / ".chaos-engine/tool.py"), "entry", "--full"],
                cwd=project, capture_output=True, text=True, timeout=60, env=env, check=True).stdout
            self.assertRegex(entry, RECEIPT)
            self.assertFalse((project / ".chaos-engine/addons").exists())


if __name__ == "__main__":
    unittest.main()
