"""#6543: POSIX build wrappers stay executable and workflows never depend on the bit."""
from __future__ import annotations

import re
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WRAPPERS = ("shaft-intellij/gradlew", "shaft-mcp/mvnw")


class WrapperScriptsTest(unittest.TestCase):
    def test_posix_wrappers_are_tracked_executable(self):
        listing = subprocess.run(
            ["git", "ls-files", "-s", "--", *WRAPPERS],
            cwd=ROOT, capture_output=True, text=True, check=True,
        ).stdout.splitlines()
        modes = {line.split("\t")[1]: line.split()[0] for line in listing}
        for wrapper in WRAPPERS:
            self.assertEqual("100755", modes.get(wrapper), wrapper)

    def test_workflows_run_wrappers_through_bash(self):
        bare = re.compile(r"^\s*(?:-\s*)?run:\s*\./(?:gradlew|mvnw)\b", re.M)
        offenders = [
            path.name for path in (ROOT / ".github/workflows").glob("*.y*ml")
            if bare.search(path.read_text(encoding="utf-8"))
        ]
        self.assertEqual([], offenders)


if __name__ == "__main__":
    unittest.main()
