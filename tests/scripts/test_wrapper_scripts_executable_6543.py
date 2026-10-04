"""#6543: POSIX build wrappers stay executable and workflows never depend on the bit."""
from __future__ import annotations

import re
import shutil
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WRAPPERS = ("shaft-intellij/gradlew", "shaft-mcp/mvnw")
WORKFLOWS = ROOT / ".github/workflows"
BARE_WRAPPER = re.compile(r"^\s*(?:-\s*)?run:\s*\./(?:gradlew|mvnw)\b", re.M)


def _git_ls_files_modes(*paths: str) -> dict[str, str]:
    git = shutil.which("git")
    if git is None:
        raise unittest.SkipTest("git not on PATH")
    listing = subprocess.run(  # nosec B603 - resolved git, fixed argv, no shell.
        [git, "ls-files", "-s", "--", *paths],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.splitlines()
    return {line.split("\t")[1]: line.split()[0] for line in listing}


class WrapperScriptsTest(unittest.TestCase):
    def test_posix_wrappers_are_tracked_executable(self):
        modes = _git_ls_files_modes(*WRAPPERS)
        for wrapper in WRAPPERS:
            self.assertEqual("100755", modes.get(wrapper), wrapper)

    def test_workflows_run_wrappers_through_bash(self):
        offenders = [
            path.name
            for path in WORKFLOWS.glob("*.y*ml")
            if BARE_WRAPPER.search(path.read_text(encoding="utf-8"))
        ]
        self.assertEqual([], offenders)


if __name__ == "__main__":
    unittest.main()
