"""Repository-aware PR watcher contract tests (#4726)."""

import io
import json
import re
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

    def test_transient_github_http_errors_retry_until_green(self):
        green = [{"name": "gate", "state": "SUCCESS", "link": "https://checks/green"}]
        for status in ("503", "429"):
            with self.subTest(status=status):
                polls = iter(
                    (
                        watch_pr_checks.CheckWatchError(
                            f"gh pr checks failed: HTTP {status}"
                        ),
                        green,
                    )
                )

                def poll_once(*_args, **_kwargs):
                    item = next(polls)
                    if isinstance(item, Exception):
                        raise item
                    return item

                stdout = io.StringIO()
                stderr = io.StringIO()
                slept: list[int] = []
                with (
                    unittest.mock.patch.object(
                        sys,
                        "argv",
                        [
                            "watch_pr_checks.py",
                            "--pr",
                            "https://github.com/owner/project/pull/9",
                            "--max-polls",
                            "3",
                            "--interval",
                            "10",
                        ],
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks, "resolve_gh", return_value="gh"
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks, "poll_once", side_effect=poll_once
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks.time, "sleep", side_effect=slept.append
                    ),
                    unittest.mock.patch("sys.stdout", stdout),
                    unittest.mock.patch("sys.stderr", stderr),
                ):
                    exit_code = watch_pr_checks.main()

                self.assertNotEqual(3, exit_code)
                self.assertEqual(0, exit_code)
                self.assertEqual([10], slept)
                self.assertIn("all checks green", stdout.getvalue())

    def test_exhausted_transient_github_http_errors_still_exit_three(self):
        for status in ("503", "429"):
            with self.subTest(status=status):
                stdout = io.StringIO()
                stderr = io.StringIO()
                slept: list[int] = []
                with (
                    unittest.mock.patch.object(
                        sys,
                        "argv",
                        [
                            "watch_pr_checks.py",
                            "--pr",
                            "https://github.com/owner/project/pull/9",
                            "--max-polls",
                            "2",
                            "--interval",
                            "10",
                        ],
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks, "resolve_gh", return_value="gh"
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks,
                        "poll_once",
                        side_effect=watch_pr_checks.CheckWatchError(
                            f"gh pr checks failed: HTTP {status}"
                        ),
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks.time, "sleep", side_effect=slept.append
                    ),
                    unittest.mock.patch("sys.stdout", stdout),
                    unittest.mock.patch("sys.stderr", stderr),
                ):
                    exit_code = watch_pr_checks.main()

                self.assertEqual(3, exit_code)
                self.assertEqual([10], slept)
                self.assertEqual("", stdout.getvalue())
                self.assertIn(status, stderr.getvalue())

    def test_malformed_json_with_char_or_line_429_does_not_retry(self):
        cases = (
            (
                "char 429",
                "gh pr checks returned unparseable JSON: Expecting value: line 1 column 430 (char 429)",
            ),
            (
                "line 429",
                "gh pr checks returned unparseable JSON: Expecting value: line 429 column 1 (char 0)",
            ),
        )
        for label, message in cases:
            with self.subTest(label=label):
                stdout = io.StringIO()
                stderr = io.StringIO()
                slept: list[int] = []
                with (
                    unittest.mock.patch.object(
                        sys,
                        "argv",
                        [
                            "watch_pr_checks.py",
                            "--pr",
                            "https://github.com/owner/project/pull/9",
                            "--max-polls",
                            "3",
                            "--interval",
                            "10",
                        ],
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks, "resolve_gh", return_value="gh"
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks,
                        "poll_once",
                        side_effect=watch_pr_checks.CheckWatchError(message),
                    ),
                    unittest.mock.patch.object(
                        watch_pr_checks.time, "sleep", side_effect=slept.append
                    ),
                    unittest.mock.patch("sys.stdout", stdout),
                    unittest.mock.patch("sys.stderr", stderr),
                ):
                    exit_code = watch_pr_checks.main()

                self.assertEqual(3, exit_code)
                self.assertEqual([], slept)
                self.assertEqual("", stdout.getvalue())
                self.assertIn("unparseable JSON", stderr.getvalue())

    def test_scheduled_acceptance_runs_every_repository_runtime_test(self):
        workflow = (
            Path(__file__).resolve().parents[2]
            / ".github/workflows/agent-plugin-acceptance.yml"
        ).read_text(encoding="utf-8")
        for module in (
            "tests.scripts.test_repository_context",
            "tests.scripts.test_watch_pr_checks",
            "tests.scripts.test_chaos_engine_runtime",
        ):
            self.assertIn(module, workflow)




class WatchPrChecksSupersededCancelledTest(unittest.TestCase):
    """#5753: cancelled PR Gate summaries must not false-stop delivery."""

    def test_superseded_cancelled_same_name_is_not_red(self):
        bucket, failing = watch_pr_checks.classify_checks(
            [
                {"name": "PR Gate Summary", "state": "CANCELLED", "link": "https://checks/old"},
                {"name": "PR Gate Summary", "state": "IN_PROGRESS", "link": "https://checks/new"},
            ]
        )
        self.assertEqual("PENDING", bucket)
        self.assertEqual([], failing)

        bucket, failing = watch_pr_checks.classify_checks(
            [
                {"name": "PR Gate Summary", "state": "CANCELLED", "link": "https://checks/old"},
                {"name": "PR Gate Summary", "state": "SUCCESS", "link": "https://checks/new"},
                {"name": "Unit Tests", "state": "SUCCESS", "link": "https://checks/u"},
            ]
        )
        self.assertEqual("GREEN", bucket)
        self.assertEqual([], failing)

    def test_lone_cancelled_without_successor_stays_red(self):
        bucket, failing = watch_pr_checks.classify_checks(
            [{"name": "PR Gate Summary", "state": "CANCELLED", "link": "https://checks/only"}]
        )
        self.assertEqual("RED", bucket)
        self.assertEqual("PR Gate Summary", failing[0]["name"])

    def test_collapse_prefers_pending_over_success_and_red(self):
        effective = watch_pr_checks.collapse_checks_by_name(
            [
                {"name": "gate", "state": "FAILURE", "link": "https://a"},
                {"name": "gate", "state": "SUCCESS", "link": "https://b"},
                {"name": "gate", "state": "QUEUED", "link": "https://c"},
            ]
        )
        self.assertEqual(1, len(effective))
        self.assertEqual("QUEUED", effective[0]["state"])


class QuietUnattendedWatchTest(unittest.TestCase):
    """#6149: one blocking watch, one line, no second status table."""

    def test_superseded_summary_stays_pending_while_another_check_runs(self):
        bucket, failing = watch_pr_checks.classify_checks(
            [
                {"name": "PR Gate Summary", "state": "FAILURE", "link": "https://checks/old"},
                {
                    "name": "ChaosEngine fresh installer (${{ matrix.os }})",
                    "state": "CANCELLED",
                    "link": "https://checks/matrix",
                },
                {"name": "Agent Guidance Gate", "state": "IN_PROGRESS", "link": "https://checks/live"},
            ]
        )
        self.assertEqual("PENDING", bucket)
        self.assertEqual([], failing)

    def test_a_real_failure_stays_red_while_another_check_runs(self):
        bucket, failing = watch_pr_checks.classify_checks(
            [
                {"name": "Agent Guidance Gate", "state": "FAILURE", "link": "https://checks/red"},
                {"name": "CodeQL", "state": "IN_PROGRESS", "link": "https://checks/live"},
            ]
        )
        self.assertEqual("RED", bucket)
        self.assertEqual("Agent Guidance Gate", failing[0]["name"])

    def test_in_progress_with_auto_merge_armed_stays_pending(self):
        checks = [
            {"name": "Agent Guidance Gate", "state": "IN_PROGRESS", "link": "https://checks/1"}
        ]
        pull = {"state": "OPEN", "mergedAt": None, "autoMergeRequest": {"enabledAt": "t"}}
        bucket, failing = watch_pr_checks.classify_unattended(checks, pull)
        self.assertEqual("PENDING", bucket)
        self.assertEqual([], failing)

    def test_unattended_command_is_one_blocking_watch(self):
        command = watch_pr_checks.unattended_watch_command(9, repo="ShaftHQ/SHAFT_ENGINE")
        self.assertIn("scripts/agents/watch_pr_checks.py", command)
        self.assertIn("--until-merged", command)
        self.assertNotIn("--poll-once", command)
        self.assertNotIn("--admin", command)
        self.assertNotIn("gh pr view", command)
        self.assertNotIn("gh pr checks", command)
        playbook = re.sub(
            r"\s+",
            " ",
            (Path(__file__).resolve().parents[2] / "chaos-engine/references/work-github-playbook.md").read_text(
                encoding="utf-8"
            ),
        )
        self.assertIn("gh run view", playbook)
        self.assertIn("second watch while one task is pending", playbook)
        self.assertNotIn("gh pr checks <n> --watch --fail-fast", playbook)

    def test_pending_polls_print_one_green_line_and_no_table(self):
        polls = iter(
            (
                [{"name": "gate", "state": "IN_PROGRESS", "link": "https://checks/1"}],
                [{"name": "gate", "state": "IN_PROGRESS", "link": "https://checks/1"}],
                [{"name": "gate", "state": "SUCCESS", "link": "https://checks/1"}],
            )
        )
        stdout = io.StringIO()
        stderr = io.StringIO()
        with (
            unittest.mock.patch.object(
                sys,
                "argv",
                [
                    "watch_pr_checks.py",
                    "--pr",
                    "https://github.com/owner/project/pull/9",
                    "--max-polls",
                    "3",
                    "--interval",
                    "10",
                ],
            ),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch.object(
                watch_pr_checks, "poll_once", side_effect=lambda *_a, **_k: next(polls)
            ),
            unittest.mock.patch.object(watch_pr_checks.time, "sleep"),
            unittest.mock.patch("sys.stdout", stdout),
            unittest.mock.patch("sys.stderr", stderr),
        ):
            exit_code = watch_pr_checks.main()
        self.assertEqual(0, exit_code)
        self.assertEqual("all checks green\n", stdout.getvalue())
        self.assertEqual("", stderr.getvalue())
        self.assertEqual(0, len(watch_pr_checks._STATUS_TABLE.findall(stdout.getvalue())))

    def test_second_status_table_in_one_watch_fails(self):
        table = "| job | state |\n| --- | --- |\n| gate | pending |\n"
        with self.assertRaises(watch_pr_checks.CheckWatchError):
            watch_pr_checks.reject_repeated_status_table(table + table)

    def test_until_merged_keeps_green_checks_pending_and_prints_merged_once(self):
        states = iter(
            (
                {
                    "state": "OPEN",
                    "mergedAt": None,
                    "autoMergeRequest": {"enabledAt": "t"},
                    "mergeStateStatus": "CLEAN",
                },
                {
                    "state": "MERGED",
                    "mergedAt": "2026-09-23T00:00:00Z",
                    "mergeStateStatus": "CLEAN",
                },
            )
        )
        green = [{"name": "gate", "state": "SUCCESS", "link": "https://checks/1"}]
        stdout = io.StringIO()
        with (
            unittest.mock.patch.object(
                sys,
                "argv",
                [
                    "watch_pr_checks.py",
                    "--pr",
                    "https://github.com/owner/project/pull/9",
                    "--until-merged",
                    "--max-polls",
                    "2",
                    "--interval",
                    "10",
                ],
            ),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch.object(watch_pr_checks, "poll_once", return_value=green),
            unittest.mock.patch.object(
                watch_pr_checks, "fetch_pull", side_effect=lambda *_a, **_k: next(states)
            ),
            unittest.mock.patch.object(watch_pr_checks.time, "sleep"),
            unittest.mock.patch("sys.stdout", stdout),
            unittest.mock.patch("sys.stderr", io.StringIO()),
        ):
            exit_code = watch_pr_checks.main()
        self.assertEqual(0, exit_code)
        self.assertEqual("MERGED\n", stdout.getvalue())

    def test_red_line_is_job_name_and_log_url_only(self):
        stdout = io.StringIO()
        with (
            unittest.mock.patch.object(
                sys,
                "argv",
                ["watch_pr_checks.py", "--pr", "https://github.com/owner/project/pull/9", "--poll-once"],
            ),
            unittest.mock.patch.object(watch_pr_checks, "resolve_gh", return_value="gh"),
            unittest.mock.patch.object(
                watch_pr_checks,
                "poll_once",
                return_value=[{"name": "gate", "state": "FAILURE", "link": "https://checks/red"}],
            ),
            unittest.mock.patch("sys.stdout", stdout),
            unittest.mock.patch("sys.stderr", io.StringIO()),
        ):
            self.assertEqual(1, watch_pr_checks.main())
        payload = json.loads(stdout.getvalue())
        self.assertEqual([{"name": "gate", "runUrl": "https://checks/red"}], payload["failingJobs"])
        self.assertEqual(set(payload), {"failingJobs"})

    def test_scripts_do_not_force_merge(self):
        root = Path(__file__).resolve().parents[2]
        offenders: list[str] = []
        for base in (root / "scripts", root / "chaos-engine", root / "tests" / "scripts"):
            for path in base.rglob("*"):
                if path.suffix not in {".py", ".md"} or not path.is_file():
                    continue
                merge = "gh pr " + "merge"
                admin = "--" + "admin"
                for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
                    if merge in line and admin in line:
                        offenders.append(f"{path.relative_to(root)}:{number}")
        self.assertEqual([], offenders)
        watcher = (root / "scripts/agents/watch_pr_checks.py").read_text(encoding="utf-8")
        self.assertNotIn("pr merge", watcher)


if __name__ == "__main__":
    unittest.main()
