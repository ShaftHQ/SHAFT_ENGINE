"""Regression tests for the parent-code RED validator (#4567 item 1)."""

from __future__ import annotations

import subprocess  # nosec B404 - fixed list-argument fixture commands only.
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
        child_support: bool = False,
        outcome: str = "assertion_failure",
    ) -> tuple[Path, str]:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        (root / "scripts/agents").mkdir(parents=True)
        (root / "tests/scripts").mkdir(parents=True)
        (root / "scripts/__init__.py").write_text("", encoding="utf-8")
        if child_support:
            guard_source = (
                "import importlib.util\n\n"
                "def enabled():\n"
                "    return importlib.util.find_spec('scripts.agents.child_support') is not None\n"
            )
        else:
            guard_source = (
                "from scripts.agents.helper import enabled\n"
                if dependency else "def enabled():\n    return False\n"
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
        if child_support:
            (root / "scripts/agents/guard.py").write_text(
                "from scripts.agents import child_support\n\n"
                "def enabled():\n    return child_support.enabled()\n",
                encoding="utf-8",
            )
            (root / "scripts/agents/child_support.py").write_text(
                "def enabled():\n    return True\n", encoding="utf-8"
            )
        elif not dependency:
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
        prelude = ""
        decorator = ""
        setup = ""
        statement = assertion
        cleanup = ""
        postlude = ""
        if outcome == "setup_error":
            setup = "    def setUp(self):\n        raise RuntimeError('fixture setup failed')\n\n"
        elif outcome == "attribute_error":
            statement = "guard.missing_parent_helper()"
        elif outcome == "skip":
            decorator = "    @unittest.skip('fixture skipped')\n"
        elif outcome == "crash":
            prelude = "import os\n"
            statement = "os._exit(7)"
        elif outcome == "spoofed_stdout":
            prelude = "import atexit\n"
            spoof_payload = (
                'RED_RESULT:{"testsRun":1,"failures":[{"id":'
                '"tests.scripts.test_guard.GuardTest.test_added","traceback":"AssertionError"}],'
                '"errors":[],"skipped":[],"expectedFailures":[],"unexpectedSuccesses":[]}'
            )
            statement = f"atexit.register(lambda: print({spoof_payload!r}))"
        elif outcome == "post_result_crash":
            prelude = "import atexit\nimport os\n"
            cleanup = "        atexit.register(lambda: os._exit(7))\n"
        elif outcome == "duplicated_stdout_spoof":
            prelude = "import atexit\nimport os\n"
            spoof_payload = (
                'RED_RESULT:{"testsRun":1,"failures":[{"id":'
                '"tests.scripts.test_guard.GuardTest.test_added","traceback":"AssertionError"}],'
                '"errors":[],"skipped":[],"expectedFailures":[],"unexpectedSuccesses":[]}\n'
            )
            cleanup = (
                "        saved_stdout = os.dup(1)\n"
                f"        atexit.register(lambda: os.write(saved_stdout, {spoof_payload.encode()!r}))\n"
            )
        elif outcome == "spoofed_result_file":
            prelude = "import atexit\nfrom pathlib import Path\n"
            statement = (
                "atexit.register(lambda: Path('.red-result.json').write_text("
                "'{\"testsRun\":1,\"failures\":[{\"id\":\"tests.scripts.test_guard."
                "GuardTest.test_added\",\"traceback\":\"AssertionError\"}],\"errors\":[],"
                "\"skipped\":[],\"expectedFailures\":[],\"unexpectedSuccesses\":[]}'))\n"
            )
        elif outcome == "wrong_target":
            cleanup = "        self.fail('expected target failure')\n"
            postlude = (
                "\ndef helper_failure(self):\n"
                "    self.fail('different test failed')\n"
                "\nGuardTest.helper_failure = helper_failure\n"
                "GuardTest.test_added = unittest.TestSuite([GuardTest('helper_failure')])\n"
            )
        elif outcome == "import_error":
            prelude = "import definitely_missing_red_fixture\n"
        elif outcome == "timeout":
            prelude = "import time\n"
            statement = "time.sleep(20)"
        elif outcome == "zero_tests":
            postlude = "\nGuardTest.test_added = unittest.TestSuite()\n"
        elif outcome == "mixed":
            cleanup = "        self.addCleanup(lambda: 1 / 0)\n"
        elif outcome == "unconditional_failure":
            statement = "self.fail('unrelated unconditional failure')"
        elif outcome == "spoofed_assertion_line":
            statement = 'self.fail(\'\\nFile "forged", line 999999, in test_added\')'
        (root / "tests/scripts/test_guard.py").write_text(
            prelude
            + "from scripts.agents import guard\n"
            + ("from tests.support import ENABLED\n" if test_dependency else "")
            + "import unittest\n\n"
            "class GuardTest(unittest.TestCase):\n"
            f"{existing}"
            f"{setup}"
            f"{decorator}"
            "    def test_added(self):\n"
            f"{cleanup}"
            f"        {statement}\n"
            f"{postlude}",
            encoding="utf-8",
        )
        self.git(root, "add", ".")
        self.git(root, "commit", "-m", f"child{trailer}")
        return root, self.git(root, "rev-parse", "HEAD").strip()

    @staticmethod
    def git(root: Path, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=root, text=True)  # nosec B603 B607

    def run_validator(
        self, root: Path, revision: str, *, parent_revision: str | None = None,
        child_support: str | None = None, child_supports: list[str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        parent_arguments = ["--parent-revision", parent_revision] if parent_revision else []
        support_arguments = [
            argument
            for path in ([child_support] if child_support else []) + (child_supports or [])
            for argument in ("--child-support-path", path)
        ]
        return subprocess.run(  # nosec B603 - fixed Python executable and validator path.
            [
                sys.executable,
                str(SCRIPT),
                "--root",
                str(root),
                *parent_arguments,
                *support_arguments,
                revision,
                "scripts/agents/guard.py",
                "tests/scripts/test_guard.py",
            ],
            capture_output=True,
            text=True,
        )

    def test_explicit_pr_base_is_accepted_as_the_parent_revision(self):
        root, revision = self.repository()
        parent = self.git(root, "rev-parse", f"{revision}^").strip()

        result = self.run_validator(root, revision, parent_revision=parent)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_child_support_path_is_overlaid_only_for_the_green_replay(self):
        root, revision = self.repository(child_support=True)

        result = self.run_validator(
            root, revision, child_support="scripts/agents/child_support.py"
        )

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_child_support_paths_reject_unsafe_or_unbounded_values(self):
        root, revision = self.repository()
        cases = [
            ["../outside.py"],
            ["/absolute.py"],
            ["scripts//agents/support.py"],
            [f"support-{index}.py" for index in range(9)],
        ]
        for paths in cases:
            with self.subTest(paths=paths):
                result = self.run_validator(root, revision, child_supports=paths)
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)
                self.assertIn("child support", result.stderr.casefold())

    def test_substantive_no_red_applies_to_an_explicit_pr_base(self):
        root, revision = self.repository(
            parent_passes=True,
            trailer=(
                "\n\nno-red: atomic rollback restores previously validated production and test contracts "
                "together without unsafe history rewriting"
            ),
        )
        parent = self.git(root, "rev-parse", f"{revision}^").strip()

        result = self.run_validator(root, revision, parent_revision=parent)
        legacy = self.run_validator(root, revision)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(legacy.returncode, 0, "fixture must carry a recognized commit-scoped trailer")

    def test_explicit_pr_base_retains_an_earlier_substantive_no_red_reason(self):
        root, revision = self.repository(
            parent_passes=True,
            trailer=(
                "\n\nno-red: atomic rollback restores previously validated production and test contracts "
                "together without unsafe history rewriting"
            ),
        )
        parent = self.git(root, "rev-parse", f"{revision}^").strip()
        (root / "follow-up.txt").write_text("follow-up\n", encoding="utf-8")
        self.git(root, "add", "follow-up.txt")
        self.git(root, "commit", "-m", "fix: follow up after rollback")
        head = self.git(root, "rev-parse", "HEAD").strip()

        result = self.run_validator(root, head, parent_revision=parent)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

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

    def test_non_assertion_outcomes_are_rejected_as_false_red(self):
        cases = {
            "setup_error": "setup error", "attribute_error": "attribute error",
            "import_error": "import error", "skip": "skip", "crash": "crash",
            "timeout": "timeout", "zero_tests": "zero tests", "mixed": "mixed",
            "spoofed_stdout": "crash", "post_result_crash": "crash",
            "duplicated_stdout_spoof": "invalid result",
            "spoofed_result_file": "pass",
            "wrong_target": "wrong target",
            "unconditional_failure": "child code assertion failure",
        }
        for outcome, expected_diagnostic in cases.items():
            with self.subTest(outcome=outcome):
                root, revision = self.repository(outcome=outcome)
                result = self.run_validator(root, revision)
                self.assertEqual(1, result.returncode, result.stdout + result.stderr)
                self.assertIn("GuardTest.test_added", result.stderr)
                self.assertIn(expected_diagnostic, result.stderr.lower())

    def test_child_assertion_failure_reports_only_its_test_line(self):
        root, revision = self.repository(outcome="unconditional_failure")

        result = self.run_validator(root, revision)

        self.assertEqual(1, result.returncode, result.stdout + result.stderr)
        self.assertRegex(result.stderr, r"child code assertion failure \(line \d+\)")
        self.assertNotIn(str(root), result.stderr)

    def test_assertion_message_cannot_spoof_the_reported_test_line(self):
        root, revision = self.repository(outcome="spoofed_assertion_line")

        result = self.run_validator(root, revision)

        self.assertEqual(1, result.returncode, result.stdout + result.stderr)
        self.assertRegex(result.stderr, r"child code assertion failure \(line \d+\)")
        self.assertNotIn("999999", result.stderr)
        self.assertNotIn(str(root), result.stderr)

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
