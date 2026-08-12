"""Repository-aware PR watcher contract tests (#4726)."""

import io
import json
import subprocess  # nosec B404 - fixed test doubles only.
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path

from scripts.agents import watch_pr_checks


class WatchPrChecksRepositoryContextTest(unittest.TestCase):
    def test_root_defaults_to_the_callers_current_working_directory(self):
        caller = Path("unrelated-consumer-repository")

        with unittest.mock.patch.object(watch_pr_checks.Path, "cwd", return_value=caller):
            arguments = watch_pr_checks.build_parser().parse_args([])

        self.assertEqual(caller, arguments.root)

    def test_numeric_pr_warns_reports_inferred_repo_and_uses_foreign_cwd(self):
        temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(temporary_directory.cleanup)
        caller = Path(temporary_directory.name).resolve()
        calls: list[tuple[list[str], Path]] = []

        def run_gh(_executable: str, arguments: list[str], root: Path):
            calls.append((arguments, root))
            if arguments[:3] == ["repo", "view", "--json"]:
                return subprocess.CompletedProcess(
                    arguments, 0, stdout=json.dumps({"nameWithOwner": "consumer/project"}), stderr=""
                )
            return subprocess.CompletedProcess(
                arguments,
                0,
                stdout=json.dumps([{"name": "gate", "state": "FAILURE", "link": "https://checks/1"}]),
                stderr="",
            )

        stdout = io.StringIO()
        stderr = io.StringIO()
        with (
            unittest.mock.patch.object(sys, "argv", ["watch_pr_checks.py", "--pr", "42", "--poll-once"]),
            unittest.mock.patch.object(watch_pr_checks.Path, "cwd", return_value=caller),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch.object(watch_pr_checks, "run_gh", side_effect=run_gh),
            unittest.mock.patch(
                "scripts.agents.repository_context.infer_repository",
                return_value="consumer/project",
            ),
            unittest.mock.patch("sys.stdout", stdout),
            unittest.mock.patch("sys.stderr", stderr),
        ):
            exit_code = watch_pr_checks.main()

        self.assertEqual(1, exit_code)
        self.assertIn("numeric --pr 42", stderr.getvalue())
        self.assertIn("consumer/project", stderr.getvalue())
        self.assertEqual(
            {"failingJobs": [{"name": "gate", "runUrl": "https://checks/1"}]},
            json.loads(stdout.getvalue()),
        )
        self.assertTrue(all(root == caller for _, root in calls))
        self.assertIn(
            ["pr", "checks", "42", "--repo", "consumer/project", "--json", "name,state,link"],
            [arguments for arguments, _ in calls],
        )

    def test_exit_contract_stays_green_red_pending_environment_error(self):
        self.assertEqual(("GREEN", []), watch_pr_checks.classify_checks([
            {"name": "gate", "state": "SUCCESS", "link": "https://checks/green"}
        ]))
        bucket, failing = watch_pr_checks.classify_checks([
            {"name": "gate", "state": "FAILURE", "link": "https://checks/red"}
        ])
        self.assertEqual("RED", bucket)
        self.assertEqual("gate", failing[0]["name"])
        self.assertEqual(("PENDING", []), watch_pr_checks.classify_checks([]))

        error_stdout = io.StringIO()
        error_stderr = io.StringIO()
        with (
            unittest.mock.patch.object(sys, "argv", ["watch_pr_checks.py", "--poll-once"]),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", side_effect=watch_pr_checks.CheckWatchError("missing")),
            unittest.mock.patch("sys.stdout", error_stdout),
            unittest.mock.patch("sys.stderr", error_stderr),
        ):
            self.assertEqual(3, watch_pr_checks.main())
        self.assertEqual("", error_stdout.getvalue())
        self.assertIn("missing", error_stderr.getvalue())

        pending_stdout = io.StringIO()
        pending_stderr = io.StringIO()
        with (
            unittest.mock.patch.object(
                sys,
                "argv",
                ["watch_pr_checks.py", "--pr", "https://github.com/owner/project/pull/9", "--poll-once"],
            ),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch.object(
                watch_pr_checks,
                "run_gh",
                return_value=subprocess.CompletedProcess([], 0, stdout="[]", stderr=""),
            ),
            unittest.mock.patch("sys.stdout", pending_stdout),
            unittest.mock.patch("sys.stderr", pending_stderr),
        ):
            self.assertEqual(2, watch_pr_checks.main())
        self.assertEqual("", pending_stdout.getvalue())
        self.assertIn("timed out waiting", pending_stderr.getvalue())

    def test_ci_entrypoint_is_only_a_relative_canonical_adapter(self):
        adapter = Path(__file__).resolve().parents[2] / "scripts/ci/watch_pr_checks.py"
        source = adapter.read_text(encoding="utf-8")

        self.assertIn("from scripts.agents.watch_pr_checks import main", source)
        self.assertNotIn("def poll_once", source)
        self.assertNotIn("def resolve_repo", source)

    def test_invalid_root_is_exit_three_without_a_traceback(self):
        missing = Path("missing-consumer-repository").resolve()
        self.assertFalse(missing.exists())
        stdout = io.StringIO()
        stderr = io.StringIO()
        caught = None
        with (
            unittest.mock.patch.object(
                sys,
                "argv",
                ["watch_pr_checks.py", "--root", str(missing), "--repo", "owner/project", "--poll-once"],
            ),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch("sys.stdout", stdout),
            unittest.mock.patch("sys.stderr", stderr),
        ):
            try:
                exit_code = watch_pr_checks.main()
            except Exception as error:  # The RED observation converts an escaping traceback to an assertion.
                caught = error
                exit_code = None

        self.assertIsNone(caught, f"repository context error escaped main: {caught}")
        self.assertEqual(3, exit_code)
        self.assertEqual("", stdout.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())

    def test_malformed_or_unknown_check_payloads_are_exit_three_without_traceback(self):
        payloads = ({"state": "SUCCESS"}, ["SUCCESS"], [1], [{"name": "gate", "state": "BOGUS", "link": ""}])
        for payload in payloads:
            with self.subTest(payload=payload):
                stdout = io.StringIO()
                stderr = io.StringIO()
                caught = None
                with (
                    unittest.mock.patch.object(
                        sys,
                        "argv",
                        ["watch_pr_checks.py", "--pr", "https://github.com/owner/project/pull/9", "--poll-once"],
                    ),
                    unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
                    unittest.mock.patch.object(
                        watch_pr_checks,
                        "run_gh",
                        return_value=subprocess.CompletedProcess(
                            [], 0, stdout=json.dumps(payload), stderr=""
                        ),
                    ),
                    unittest.mock.patch("sys.stdout", stdout),
                    unittest.mock.patch("sys.stderr", stderr),
                ):
                    try:
                        exit_code = watch_pr_checks.main()
                    except Exception as error:
                        caught = error
                        exit_code = None
                self.assertIsNone(caught, f"malformed check payload escaped main: {caught}")
                self.assertEqual(3, exit_code)
                self.assertEqual("", stdout.getvalue())
                self.assertNotIn("Traceback", stderr.getvalue())

    def test_pr_gate_runs_every_repository_runtime_test(self):
        workflow = (
            Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"
        ).read_text(encoding="utf-8")
        for module in (
            "tests.scripts.test_repository_context",
            "tests.scripts.test_watch_pr_checks",
            "tests.scripts.test_act_as_mohab_runtime",
        ):
            self.assertIn(module, workflow)
        for path in (
            "tests/scripts/test_repository_context.py",
            "tests/scripts/test_watch_pr_checks.py",
            "tests/scripts/test_act_as_mohab_runtime.py",
        ):
            self.assertIn(path, workflow)


if __name__ == "__main__":
    unittest.main()
