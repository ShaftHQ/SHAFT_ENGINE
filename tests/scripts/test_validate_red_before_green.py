"""Regression tests for the parent-code RED validator (#4567 item 1)."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/validate_red_before_green.py"


class ValidateRedBeforeGreenTest(unittest.TestCase):
    def repository(
        self,
        *,
        parent_passes: bool = False,
        trailer: str = "",
        new_test_module: bool = False,
        dependency: bool = False,
        test_dependency: bool = False,
    ) -> tuple[Path, str]:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        (root / "scripts/agents").mkdir(parents=True)
        (root / "tests/scripts").mkdir(parents=True)
        (root / "scripts/__init__.py").write_text("", encoding="utf-8")
        guard_source = (
            "from scripts.agents.helper import enabled\n" if dependency else "def enabled():\n    return False\n"
        )
        (root / "scripts/agents/guard.py").write_text(guard_source, encoding="utf-8")
        if dependency:
            (root / "scripts/agents/helper.py").write_text(
                "def enabled():\n    return False\n", encoding="utf-8"
            )
        if test_dependency:
            (root / "tests/support.py").write_text("ENABLED = False\n", encoding="utf-8")
        if not new_test_module:
            (root / "tests/scripts/test_guard.py").write_text(
                "from scripts.agents import guard\n"
                "import unittest\n\n"
                "class GuardTest(unittest.TestCase):\n"
                "    def test_existing(self):\n"
                "        self.assertFalse(guard.enabled())\n",
                encoding="utf-8",
            )
        self.git(root, "init")
        self.git(root, "config", "user.email", "test@example.com")
        self.git(root, "config", "user.name", "Test")
        self.git(root, "add", ".")
        self.git(root, "commit", "-m", "parent")
        if not dependency:
            child_value = "False" if parent_passes else "True"
            (root / "scripts/agents/guard.py").write_text(
                f"def enabled():\n    return {child_value}\n", encoding="utf-8"
            )
        existing = (
            "    def test_existing(self):\n"
            "        self.assertFalse(guard.enabled())\n\n"
            if not new_test_module
            else ""
        )
        subject = "ENABLED" if test_dependency else "guard.enabled()"
        assertion = f"self.assertFalse({subject})" if parent_passes else f"self.assertTrue({subject})"
        (root / "tests/scripts/test_guard.py").write_text(
            "from scripts.agents import guard\n"
            + ("from tests.support import ENABLED\n" if test_dependency else "")
            + "import unittest\n\n"
            "class GuardTest(unittest.TestCase):\n"
            f"{existing}"
            "    def test_added(self):\n"
            f"        {assertion}\n",
            encoding="utf-8",
        )
        self.git(root, "add", ".")
        self.git(root, "commit", "-m", f"child{trailer}")
        return root, self.git(root, "rev-parse", "HEAD").strip()

    @staticmethod
    def git(root: Path, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=root, text=True)

    def run_validator(self, root: Path, revision: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--root",
                str(root),
                revision,
                "scripts/agents/guard.py",
                "tests/scripts/test_guard.py",
            ],
            capture_output=True,
            text=True,
        )

    def test_new_test_that_fails_against_parent_is_accepted(self):
        self.assertTrue(SCRIPT.is_file(), "the parent-code RED validator must exist")
        root, revision = self.repository()
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_new_test_that_passes_against_parent_is_reported(self):
        self.assertTrue(SCRIPT.is_file(), "the parent-code RED validator must exist")
        root, revision = self.repository(parent_passes=True)
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 1)
        self.assertIn("GuardTest.test_added", result.stderr)

    def test_a_substantive_no_red_trailer_allows_an_entangled_commit(self):
        self.assertTrue(SCRIPT.is_file(), "the parent-code RED validator must exist")
        root, revision = self.repository(
            parent_passes=True,
            trailer="\n\nno-red: refactor moves shared setup across packages so parent imports cannot run safely",
        )
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_a_new_test_module_is_checked_against_parent_code(self):
        root, revision = self.repository(parent_passes=True, new_test_module=True)
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("GuardTest.test_added", result.stderr)

    def test_parent_dependencies_are_available_during_the_red_run(self):
        root, revision = self.repository(parent_passes=True, dependency=True)
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)

    def test_parent_test_helpers_are_available_during_the_red_run(self):
        root, revision = self.repository(parent_passes=True, test_dependency=True)
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)

    def test_a_no_red_body_line_is_not_an_opt_out(self):
        root, revision = self.repository(
            parent_passes=True,
            trailer=(
                "\n\nno-red: refactor moves shared setup across packages so parent imports cannot run safely"
                "\n\nThis ordinary body paragraph means the line is not a trailer."
            ),
        )
        result = self.run_validator(root, revision)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
