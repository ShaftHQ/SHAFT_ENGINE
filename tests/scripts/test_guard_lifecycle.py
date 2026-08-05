"""SessionStart and Stop share one portable lifecycle contract."""

from __future__ import annotations

import inspect
import io
import json
import re
import subprocess  # nosec B404 - tests drive the tracked hook command locally.
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

from scripts.agents import guard

# Every Stop rule `run_stop` calls. All of them are patched off in the classes
# whose subject is something else.
#
# R18 reads the real repository, and silently made five of those tests depend
# on whether a push happened to be pending -- green on a clean checkout and on
# CI, red exactly when the harness was working as designed. R17 then repeated
# it: it shells out to `gh`, so an independent review posted on the open pull
# request turned the same five red again. Two rules, one defect, because the
# first fix named R18 rather than the class R18 belonged to.
#
# Hence: isolate every Stop rule, not the subset known today to read outside
# the process. A rule that needs no isolation loses nothing by being listed --
# its own test class calls it directly, and the collection test re-patches
# whatever it asserts on -- while a rule that does need it can no longer be
# overlooked. `StopRuleIsolationIsCompleteTest` fails until a newly added rule
# is named here, which is the part that prevents a third repeat.
ISOLATED_STOP_RULES = (
    "check_r16_learning_loop",
    "check_r17_unarmed_pull_request",
    "check_r18_unpushed_work",
    # R20 shells out to the sync helper, so leaving it live would make every
    # test below depend on whether this machine's deployed harness happens to
    # match the tracked one -- the third instance of the defect this list
    # exists for, caught by the equality pin in the commit that added it.
    "check_r20_user_harness_drift",
)


def isolate_stop_rules(case: unittest.TestCase, except_for: tuple[str, ...] = ()) -> None:
    """Patch every Stop rule off for `case`, undone when the test finishes.

    `except_for` leaves one rule live so a test can exercise it through
    `run_stop` rather than by calling it directly. That distinction matters:
    calling the check proves the function works, and only going through
    `run_stop` proves the hook can reach it, which is the difference
    `gotcha.a-guards-tests-passing-proves-the-function-works-never-that-the-
    hook-can-reach-it` records. Every other rule stays patched, so the test is
    still deterministic.
    """
    for name in ISOLATED_STOP_RULES:
        if name in except_for:
            continue
        patcher = patch(f"scripts.agents.guard.{name}", return_value=None)
        patcher.start()
        case.addCleanup(patcher.stop)


class GuardLifecycleTest(unittest.TestCase):
    def setUp(self):
        """Isolate these tests from every Stop rule that reads live state.

        R18 asks the real repository whether the branch has unpushed commits
        and R17 asks `gh` whether an open pull request has a review nobody
        armed. Without this, these tests pass or fail on whether a push
        happens to be pending or a reviewer happens to have replied -- green
        on a clean checkout and on CI, red exactly when the harness is doing
        its job. A result that depends on the environment rather than on the
        subject is not a test of the subject.
        """
        isolate_stop_rules(self)

    def output(self, function, payload: dict) -> dict | None:
        stream = io.StringIO()
        with redirect_stdout(stream):
            self.assertEqual(function(payload), 0)
        text = stream.getvalue().strip()
        return json.loads(text) if text else None

    @patch("scripts.agents.guard._sync_advisory", return_value="user harness drift")
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": ["dirty sibling"]},
    )
    def test_session_start_injects_hygiene_and_sync_context(self, _report, _sync):
        output = self.output(guard.run_session_start, {"cwd": "."})

        specific = output["hookSpecificOutput"]
        self.assertEqual(specific["hookEventName"], "SessionStart")
        self.assertIn("dirty sibling", specific["additionalContext"])
        self.assertIn("user harness drift", specific["additionalContext"])
        self.assertIn("act-as-mohab", specific["additionalContext"])

    @patch("scripts.agents.guard._sync_advisory", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_session_start_delivers_the_standing_constraints_rather_than_asking_for_them(
        self, _report, _sync
    ):
        """#4540: the rule that must not depend on an agent remembering it.

        `routing.md` already carries the retrieval table, and it is reached
        only when the entrypoint routes a deliverable to a surface -- so the
        duty to query the stores *before* discovery sits behind a load it is
        supposed to precede, and fires too late by construction. Measured, not
        supposed: the session that added this had already run `gh issue list`,
        `git ls-files` and `rg` before any store was queried.

        Restating it harder is the mitigation the literature measures and
        finds wanting (arXiv 2604.20911: templating, restating and detection
        recover only partial compliance as context grows; arXiv 2607.25398:
        the best model honours a long binding policy document 36.2% of the
        time under strict grading, and failures persist at maximum reasoning
        effort). So this does not ask. The hook carries the constraints in,
        which costs no adherence and cannot decay.

        Titles only, and that is deliberate: 12 objects are 949 bytes of
        title against several tens of kilobytes of body. The title is enough
        to know a constraint exists and to go read it, which is the job an
        always-injected index has to do. Bodies belong behind `memory
        inspect`.
        """
        output = self.output(guard.run_session_start, {"cwd": "."})
        context = output["hookSpecificOutput"]["additionalContext"]
        self.assertIn("Standing constraints", context)
        self.assertIn("closing keywords", context, "a real constraint title must appear")
        self.assertIn(
            "memory load", context, "the deeper query must be named where it is needed"
        )

    @patch("scripts.agents.guard._sync_advisory", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_session_start_still_confirms_the_entrypoint(self, _report, _sync):
        output = self.output(guard.run_session_start, {"cwd": "."})
        self.assertIn(
            "act-as-mohab", output["hookSpecificOutput"]["additionalContext"]
        )

    @patch("scripts.agents.guard._open_pull_request_count", return_value=1)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [
                {"is_current": True, "state": "pending", "branch": "ChaosEngine/x"}
            ],
            "advisories": [],
        },
    )
    def test_stop_accepts_pending_commits_that_an_open_pull_request_covers(
        self, _report, _count
    ):
        """#4542: the hook blocked on a state it never did the work to evaluate.

        `pending` is returned on commit count alone. The branch that found this
        was clean, pushed, and covered by an open pull request carrying nine
        `Closes` lines -- and blocked on every turn, because
        `open_pull_requests` was `None`, meaning *the lookup never ran*, not
        *no pull request exists*.

        With no reachable green state the only exits were to merge a draft to
        silence a hook or to delete the guard, which is
        `gotcha.a-check-whose-healthy-end-state-is-unreachable-is-a-check-that-
        will-be-weakened` exactly, and iron law 4 forbids the second.
        """
        self.assertIsNone(self.output(guard.run_stop, {"cwd": "."}))

    @patch("scripts.agents.guard._open_pull_request_count", return_value=0)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [
                {"is_current": True, "state": "pending", "branch": "ChaosEngine/x"}
            ],
            "advisories": [],
        },
    )
    def test_stop_still_blocks_pending_commits_with_no_pull_request(
        self, _report, _count
    ):
        """The case the check exists for must survive the fix.

        Commits ahead with a *confirmed* zero open pull requests is genuinely
        unfinished work: it lives on one machine and nobody else can see it.
        Fixing #4542 must not buy a reachable green state by making the hook
        toothless.
        """
        output = self.output(guard.run_stop, {"cwd": "."})
        self.assertEqual(output["decision"], "block")
        self.assertIn("pending work", output["reason"])

    @patch("scripts.agents.guard._open_pull_request_count", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [
                {"is_current": True, "state": "pending", "branch": "ChaosEngine/x"}
            ],
            "advisories": [],
        },
    )
    def test_stop_fails_open_when_the_pull_request_lookup_cannot_answer(
        self, _report, _count
    ):
        """`None` and `0` must not collapse into the same branch.

        No `gh`, no auth, no network, or a rate limit yields `None`. Treating
        that as "no pull request" is what produced #4542, and on a machine
        without `gh` it would strand every session permanently rather than
        once. A hook that cannot verify must not hold the session hostage --
        the requirement is any agent on any machine, and most machines have no
        GitHub credentials at all.
        """
        self.assertIsNone(self.output(guard.run_stop, {"cwd": "."}))

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [{"is_current": True, "state": "uncommitted"}],
            "advisories": [],
        },
    )
    def test_stop_blocks_once_when_current_work_is_uncommitted(self, _report):
        output = self.output(guard.run_stop, {"cwd": ".", "stop_hook_active": False})
        self.assertEqual(output["decision"], "block")
        self.assertIn("uncommitted", output["reason"])
        self.assertIn("act-as-mohab", output["reason"])
        self.assertNotIn("push", output["reason"].lower())

        repeated = self.output(
            guard.run_stop, {"cwd": ".", "stop_hook_active": True}
        )
        self.assertIsNone(repeated)

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [{"is_current": True, "state": "pending"}],
            "advisories": [],
        },
    )
    def test_stop_routes_pending_work_to_authorization_aware_completion(self, _report):
        output = self.output(guard.run_stop, {"cwd": "."})
        self.assertEqual(output["decision"], "block")
        reason = output["reason"].lower()
        self.assertIn("act-as-mohab", reason)
        self.assertNotIn("pull request", reason)
        self.assertNotIn("merge", reason)

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [{"is_current": True, "state": "abandoned"}],
            "advisories": [],
        },
    )
    def test_stop_routes_abandoned_work_without_expanding_authority(self, _report):
        output = self.output(guard.run_stop, {"cwd": "."})
        reason = output["reason"].lower()
        self.assertIn("act-as-mohab", reason)
        self.assertNotIn("pull request", reason)
        self.assertNotIn("merge", reason)

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={
            "worktrees": [{"is_current": True, "state": "clean"}],
            "advisories": [],
        },
    )
    def test_stop_allows_a_clean_current_worktree(self, _report):
        self.assertIsNone(self.output(guard.run_stop, {"cwd": "."}))

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_stop_fails_closed_when_current_worktree_is_missing(self, _report):
        output = self.output(guard.run_stop, {"cwd": "."})
        self.assertEqual(output["decision"], "block")
        self.assertIn("could not be identified", output["reason"])

    @patch("scripts.agents.guard.subprocess.run")
    def test_lifecycle_helpers_fit_the_hook_budget_and_sync_uses_default_mode(self, run):
        run.side_effect = (
            subprocess.CompletedProcess([], 0, '{"worktrees": [], "advisories": []}', ""),
            subprocess.CompletedProcess([], 0, "", ""),
        )

        guard._worktree_report(".")
        guard._sync_advisory()

        first, second = run.call_args_list
        self.assertLessEqual(first.kwargs["timeout"] + second.kwargs["timeout"], 20)
        self.assertNotIn("--check", second.args[0])


if __name__ == "__main__":
    unittest.main()


class SessionLedgerTest(unittest.TestCase):
    """#4541: the session-scoped state nine of the twelve rules need.

    `guard.py` is stateless -- every PreToolUse call is a fresh process that
    sees only the current tool call. "Has a test run since the last production
    edit", "how long since the last push", "was a store queried before this
    discovery" are all questions about the session, and none of them can be
    asked without somewhere to write down what has already happened.

    The ledger is deliberately dumb: an append-only list of event strings. It
    holds no judgement, so a rule that changes its mind does not invalidate
    history, and a corrupted ledger costs nothing but a re-observation.
    """

    def payload(self, session: str, directory: str) -> dict:
        return {"session_id": session, "cwd": directory}

    def test_an_event_recorded_is_an_event_read_back(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-a", directory)
                self.assertEqual(guard.ledger_events(payload), [])
                self.assertTrue(guard.ledger_record(payload, "test-run"))
                self.assertTrue(guard.ledger_record(payload, "push"))
                self.assertEqual(guard.ledger_events(payload), ["test-run", "push"])

    def test_one_session_cannot_see_another_sessions_events(self):
        """A shared ledger would let a sibling agent satisfy this agent's gate.

        Concurrent agents each own a worktree and run their own hooks. If the
        ledger were keyed by repository rather than by session, one delegate
        running a test would unlock a production write for a different one --
        a gate that can be satisfied by somebody else is not a gate.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.ledger_record(self.payload("session-a", directory), "test-run")
                self.assertEqual(guard.ledger_events(self.payload("session-b", directory)), [])

    def test_the_ledger_fails_open_when_it_cannot_be_written(self):
        """A hook that cannot record must never block. It runs before every call."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-a", directory)
                with patch("scripts.agents.guard._ledger_path", return_value=None):
                    self.assertFalse(guard.ledger_record(payload, "test-run"))
                    self.assertEqual(guard.ledger_events(payload), [])

    def test_a_corrupt_ledger_reads_as_empty_rather_than_raising(self):
        """Worst case is re-observing an event, never a session that cannot start."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-a", directory)
                guard.ledger_record(payload, "test-run")
                path = guard._ledger_path(payload)
                self.assertIsNotNone(path)
                with open(path, "wb") as handle:
                    handle.write(b"\x00\xff not json at all")
                self.assertEqual(guard.ledger_events(payload), [])


class ProductionBeforeTestGateTest(unittest.TestCase):
    """#4541 / iron law 3: RED before GREEN, enforced rather than remembered.

    This is the highest-value row in the registry. "No production code before
    an observed failing test" has been the law since the entrypoint was
    written and has never had a mechanism -- `test_agent_router_contract.py`
    pins that the *sentence* exists, which cannot observe whether any
    production code was written first.

    Scope is deliberately narrow. Only compiled source under a module's
    `src/main/` counts, because the entrypoint itself exempts the rest:
    documentation, guidance, configuration and generated code "may skip
    test-first; validate their structure or affected flow instead". A gate
    that fired on a README edit would be argued away within a day.
    """

    def payload(self, session: str, path: str) -> dict:
        return {
            "session_id": session,
            "cwd": ".",
            "tool_name": "Write",
            "tool_input": {"file_path": path},
        }

    def test_a_test_command_is_recognised_as_one(self):
        for command in (
            "py -3 -m unittest tests.scripts.test_guard_lifecycle",
            "python3 -m pytest tests/",
            "mvn -Dtest=SomeTest test",
        ):
            with self.subTest(command=command):
                self.assertTrue(guard.looks_like_a_test_run(command))
        for command in ("git status", "ls -la", "echo testing the waters"):
            with self.subTest(command=command):
                self.assertFalse(guard.looks_like_a_test_run(command))

    def test_production_source_is_blocked_when_no_test_has_run(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("s1", "shaft-engine/src/main/java/Thing.java")
                reason = guard.check_r12_test_before_production(payload, "Write")
        self.assertIsNotNone(reason)
        self.assertIn("failing test", reason)

    def test_production_source_is_allowed_once_a_test_run_is_on_the_ledger(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("s2", "shaft-engine/src/main/java/Thing.java")
                guard.ledger_record(payload, "test-run")
                self.assertIsNone(guard.check_r12_test_before_production(payload, "Write"))

    def test_the_test_that_creates_red_is_never_blocked(self):
        """Writing the failing test must not require having already run one.

        Blocking the RED step would make the law unsatisfiable: the only way
        to observe a failing test is to write it first.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                for path in (
                    "shaft-engine/src/test/java/ThingTest.java",
                    "tests/scripts/test_guard_lifecycle.py",
                    "AGENTS.md",
                    "scripts/ci/validate_agent_setup.py",
                    ".agents/skills/act-as-mohab/SKILL.md",
                ):
                    with self.subTest(path=path):
                        payload = self.payload("s3", path)
                        self.assertIsNone(
                            guard.check_r12_test_before_production(payload, "Write")
                        )

    def test_the_gate_fails_open_without_a_session(self):
        """No session id means no ledger, and an unanswerable question never blocks."""
        payload = {"cwd": ".", "tool_input": {"file_path": "a/src/main/java/T.java"}}
        self.assertIsNone(guard.check_r12_test_before_production(payload, "Write"))


class PushBeforeDeleteGateTest(unittest.TestCase):
    """#4541: the entrypoint's cleanup order, as a refusal rather than a step.

    Task isolation says push any branch a remote has never seen *first*, then
    delete. The order is not interchangeable: reversed, the only copy of that
    work is gone and no later step recovers it.

    Only `-D` is guarded. `git branch -d` already refuses an unmerged branch,
    so git enforces the safe form itself; restating it would add noise without
    safety, which is how a guard earns its own deletion.
    """

    def test_force_deleting_a_branch_with_unpushed_commits_is_refused(self):
        with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=3):
            reason = guard.check_r13_push_before_delete("git branch -D feature/x", "Bash")
        self.assertIsNotNone(reason)
        self.assertIn("push", reason.lower())
        self.assertIn("feature/x", reason)

    def test_force_deleting_a_fully_pushed_branch_is_allowed(self):
        with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=0):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -D feature/x", "Bash"))

    def test_the_safe_delete_form_is_never_touched(self):
        with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=5):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -d feature/x", "Bash"))

    def test_it_fails_open_when_the_commit_count_cannot_be_answered(self):
        """Unknown is not zero and is not many -- #4542's lesson, applied here."""
        with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=None):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -D feature/x", "Bash"))

    def test_prose_naming_the_command_is_not_the_command(self):
        with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=3):
            self.assertIsNone(
                guard.check_r13_push_before_delete(
                    'git commit -m "explain why git branch -D is guarded"', "Bash"
                )
            )


class RemediesAreNotBlockedByAnotherRuleTest(unittest.TestCase):
    """A rule may only name an escape that the rest of the file permits.

    R14 offered three ways out of a blocked `git reset --hard`, and one of
    them -- "stash deliberately" -- is refused outright by R8 in this
    repository. Not a deadlock, since the other two are legal, but it is the
    near miss that `decision.check-every-new-guard-pairwise-against-the-
    guards-already-shipped` was written after: when the last remedy standing
    is one another rule forbids, deleting a guard becomes the cheapest exit,
    and iron law 4 forbids that exit.

    Explicit rows rather than prose-scraping the file. Several rules quote
    commands precisely to say they are *not* affected -- R13 on `git branch
    -d`, R14 on `git reset --hard` itself -- so a scan for backticked
    commands would flag the documentation as the defect.
    """

    REMEDIES = (
        ("R13", "git push -u origin feature"),
        ("R14", "git add -A && git commit -m x"),
        ("R14", "git reset --soft HEAD~1"),
        ("R14", "git reset HEAD~1"),
    )

    def test_no_rule_offers_an_escape_another_rule_refuses(self):
        for rule, command in self.REMEDIES:
            with self.subTest(rule=rule, command=command):
                self.assertIsNone(
                    guard.check_r8_git_stash(command),
                    f"{rule} names a remedy R8 refuses",
                )
                with patch(
                    "scripts.agents.guard._uncommitted_file_count", return_value=4
                ):
                    self.assertIsNone(
                        guard.check_r14_hard_reset(command, "Bash", "."),
                        f"{rule} names a remedy R14 refuses",
                    )

    def test_the_forbidden_remedy_is_still_forbidden(self):
        """Guards the check above against passing because R8 stopped working."""
        self.assertIsNotNone(guard.check_r8_git_stash("git stash"))

    def test_r14_no_longer_recommends_the_command_r8_refuses(self):
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=4):
            reason = guard.check_r14_hard_reset("git reset --hard HEAD~1", "Bash", ".")
        self.assertIsNotNone(reason)
        self.assertNotIn("stash deliberately", reason)


class HookBudgetTest(unittest.TestCase):
    """One invocation gets one window, and the entry point must open it.

    The comment above `SUBPROCESS_TIMEOUT` claimed every helper query shared a
    budget. Nothing implemented it -- adversarial review measured a single
    PreToolUse invocation issuing 7 subprocesses at 4s each, 28s against a 10s
    hook timeout. A killed PreToolUse hook fails *open*, so overrunning does
    not produce a slow decision, it produces no decision and silently skips
    every rule in the file.

    `test_main_opens_the_window` is the load-bearing one. A budget the entry
    point never starts is exactly
    `gotcha.a-guards-tests-passing-proves-the-function-works-never-that-the-
    hook-can-reach-it`: the arithmetic would test green while nothing enforced
    it.
    """

    def tearDown(self):
        guard.clear_hook_budget()

    def test_the_per_call_ceiling_applies_outside_a_hook(self):
        guard.clear_hook_budget()
        self.assertEqual(guard._subprocess_timeout(), float(guard.SUBPROCESS_TIMEOUT))

    def test_an_open_window_never_exceeds_the_per_call_ceiling(self):
        guard.start_hook_budget(60.0)
        self.assertEqual(guard._subprocess_timeout(), float(guard.SUBPROCESS_TIMEOUT))

    def test_a_nearly_spent_window_shortens_the_next_call(self):
        guard.start_hook_budget(1.0)
        self.assertLessEqual(guard._subprocess_timeout(), 1.0)
        self.assertGreater(guard._subprocess_timeout(), 0)

    def test_a_spent_window_still_returns_a_positive_timeout(self):
        """Zero would be ambiguous; the caller must take its existing timeout path."""
        guard.start_hook_budget(-1.0)
        self.assertGreater(guard._subprocess_timeout(), 0)
        self.assertLess(guard._subprocess_timeout(), 0.01)

    def test_the_window_bounds_the_total_not_just_each_call(self):
        """The property the old comment asserted and nothing enforced."""
        guard.start_hook_budget(5.0)
        self.assertLessEqual(sum(guard._subprocess_timeout() for _ in range(7)), 28.0)
        guard.start_hook_budget(-1.0)
        self.assertLess(sum(guard._subprocess_timeout() for _ in range(7)), 1.0)

    def test_main_opens_the_window(self):
        guard.clear_hook_budget()
        payload = json.dumps({"hook_event_name": "Stop", "stop_hook_active": True})
        with patch("sys.stdin", io.StringIO(payload)):
            self.assertEqual(guard.main([]), 0)
        self.assertIsNotNone(
            guard._hook_deadline, "main must open the shared window before dispatching"
        )


class DeliveredBranchIsNotUnrecoverableTest(unittest.TestCase):
    """A squash-merged branch is delivered, however its commit count reads.

    Found by adversarial review of this pull request. This repository
    squash-merges and GitHub deletes the head branch on merge, so every
    original commit of a *fully delivered* branch reports as existing on no
    remote. R13 therefore refused `git branch -D` -- the entrypoint's own
    cleanup step 2 -- on correct work, and the remedy it named,
    `git push -u origin <branch>`, would have re-created the remote branch for
    already merged work. R18 nagged for the same push on every turn.

    Reproduced in a fixture repository before the fix: pushed, squash-merged,
    remote branch deleted, and `git rev-list --count feature --not --remotes`
    still answered 2.

    `git cherry` cannot substitute. It compares patch ids, and a squash of two
    commits matches neither of them -- verified in the same fixture, where
    both commits came back marked `+`. Only a content comparison answers the
    question the rules are actually asking.
    """

    def test_a_delivered_branch_reports_nothing_unrecoverable(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=2):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch", return_value=True
            ):
                self.assertEqual(guard._unrecoverable_commit_count("feature"), 0)

    def test_an_undelivered_branch_still_reports_its_commits(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=2):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch", return_value=False
            ):
                self.assertEqual(guard._unrecoverable_commit_count("feature"), 2)

    def test_unanswerable_stays_unanswerable(self):
        """None and 0 are opposite facts and must not collapse into each other."""
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=None):
            self.assertIsNone(guard._unrecoverable_commit_count("feature"))

    def test_delivery_is_not_queried_when_nothing_is_unpushed(self):
        """The common case must not pay for the git calls the rare case needs.

        A PreToolUse hook has a 10s budget and fails *open* when killed, so
        every avoidable subprocess in the common path is a chance to silently
        bypass every rule in this file.
        """
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=0):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch"
            ) as delivery:
                self.assertEqual(guard._unrecoverable_commit_count("feature"), 0)
                delivery.assert_not_called()

    def test_r13_permits_deleting_a_delivered_branch(self):
        """End to end: the cleanup the entrypoint mandates must be reachable."""
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=2):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch", return_value=True
            ):
                self.assertIsNone(
                    guard.check_r13_push_before_delete("git branch -D feature", "Bash")
                )

    def test_r13_still_blocks_a_branch_that_exists_nowhere_else(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=2):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch", return_value=False
            ):
                reason = guard.check_r13_push_before_delete("git branch -D feature", "Bash")
        self.assertIsNotNone(reason)
        self.assertIn("R13 blocked", reason)


class HardResetGateTest(unittest.TestCase):
    """R14, and the incident that produced it.

    While building R13 this guard's own author ran `git reset --hard HEAD~1`
    to set up a probe branch, with R13's implementation and tests uncommitted.
    Both were destroyed instantly. Nothing in the file caught it: R8 guards
    `git stash`, R9 guards `git worktree add`, R13 guards `git branch -D` --
    and the most destructive command of the set was unguarded.

    The failure was compounded by a stale `.pyc`: the suite went green against
    bytecode for source that no longer existed, so a passing run proved
    nothing. That is the vacuous-green shape the harness polices elsewhere.

    `--hard` alone is the trigger. A soft or mixed reset leaves the working
    tree intact, and `--hard` on a clean tree destroys nothing.
    """

    def test_a_hard_reset_with_uncommitted_work_is_refused(self):
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=4):
            reason = guard.check_r14_hard_reset("git reset --hard HEAD~1", "Bash", ".")
        self.assertIsNotNone(reason)
        self.assertIn("uncommitted", reason.lower())

    def test_a_hard_reset_on_a_clean_tree_is_allowed(self):
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=0):
            self.assertIsNone(guard.check_r14_hard_reset("git reset --hard HEAD~1", "Bash", "."))

    def test_soft_and_mixed_resets_are_never_touched(self):
        """They do not touch the working tree, so there is nothing to protect."""
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=4):
            for command in ("git reset --soft HEAD~1", "git reset HEAD~1", "git reset --mixed HEAD~1"):
                with self.subTest(command=command):
                    self.assertIsNone(guard.check_r14_hard_reset(command, "Bash", "."))

    def test_it_fails_open_when_the_tree_state_cannot_be_answered(self):
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=None):
            self.assertIsNone(guard.check_r14_hard_reset("git reset --hard HEAD~1", "Bash", "."))

    def test_prose_naming_the_command_is_not_the_command(self):
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=4):
            self.assertIsNone(
                guard.check_r14_hard_reset(
                    'git commit -m "never run git reset --hard with work in flight"', "Bash", "."
                )
            )


class ReviewBeforeArmingGateTest(unittest.TestCase):
    """R15 / iron law 6: no arming before an independent adversarial review.

    The entrypoint requires a separate instance to review every
    behaviour-changing step before the next one starts, and the Ownership
    section requires arming auto-merge only once that gate passes. Neither had
    a mechanism, so the one irreversible step in the whole workflow -- handing
    a diff to auto-merge -- rested on remembering.

    `constraint.always-address-pr-review-comments-not-just-ci-checks-and-merge-conflicts`
    is why a review by the PR's own author does not count: the point is an
    independent reader, and self-review is the shape the constraint was
    written against.
    """

    def test_arming_without_any_review_is_refused(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=0):
            reason = guard.check_r15_review_before_arming("gh pr merge 4539 --auto --squash", "Bash")
        self.assertIsNotNone(reason)
        self.assertIn("review", reason.lower())

    def test_arming_after_an_independent_review_is_allowed(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=1):
            self.assertIsNone(
                guard.check_r15_review_before_arming("gh pr merge 4539 --auto --squash", "Bash")
            )

    def test_it_fails_open_when_the_review_state_cannot_be_answered(self):
        """No gh, no auth, no network: unknown is not zero (#4542)."""
        with patch("scripts.agents.guard._independent_review_count", return_value=None):
            self.assertIsNone(
                guard.check_r15_review_before_arming("gh pr merge 4539 --auto", "Bash")
            )

    def test_unrelated_gh_commands_are_untouched(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=0):
            for command in ("gh pr view 4539", "gh pr list --state open", "gh issue create --title x"):
                with self.subTest(command=command):
                    self.assertIsNone(guard.check_r15_review_before_arming(command, "Bash"))

    def test_prose_naming_the_command_is_not_the_command(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=0):
            self.assertIsNone(
                guard.check_r15_review_before_arming(
                    'git commit -m "explain why gh pr merge --auto is guarded"', "Bash"
                )
            )


class LearningLoopStopGateTest(unittest.TestCase):
    """R16: the learning loop, which the entrypoint requires before reporting done.

    "Before reporting done, run the learned-lessons workflow: route every
    learning exactly once." It had no mechanism, and this session is the
    evidence: an iteration reported done having skipped the mandatory
    retrieval entirely, and the owner caught it rather than any check.

    Deliberately a block-once reminder, not a hard gate. `run_stop` already
    returns 0 when `stop_hook_active` is set, so the agent is interrupted a
    single time and may then end the turn. That matters more here than
    anywhere else in the file: "nothing durable surfaced" is a legitimate and
    common outcome that the entrypoint explicitly endorses -- "Nothing durable
    is a valid result" -- so a rule that could not be satisfied by saying so
    would be a rule that forces invented memory objects.

    It must also never strand a delegate in a linked worktree. R11 refuses a
    memory write from one by design, so demanding a memory write there would
    leave no legal state -- the deadlock shape recorded in
    `gotcha.a-check-whose-healthy-end-state-is-unreachable-is-a-check-that-
    will-be-weakened`. Blocking once and allowing the second attempt is what
    keeps that impossible.
    """

    def test_committing_without_routing_a_learning_is_interrupted_once(self):
        with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
            self.assertIsNotNone(guard.check_r16_learning_loop({"session_id": "s"}))

    def test_a_recorded_memory_write_satisfies_it(self):
        with patch(
            "scripts.agents.guard.ledger_events", return_value=["commit", "memory-write"]
        ):
            self.assertIsNone(guard.check_r16_learning_loop({"session_id": "s"}))

    def test_a_session_that_changed_nothing_is_never_interrupted(self):
        """A read-only session owes no learning; asking would train the block away."""
        with patch("scripts.agents.guard.ledger_events", return_value=["test-run"]):
            self.assertIsNone(guard.check_r16_learning_loop({"session_id": "s"}))

    def test_the_second_stop_attempt_is_always_allowed(self):
        """Block once. `run_stop` returns 0 on stop_hook_active, so this cannot loop.

        A linked-worktree delegate cannot write memory at all (R11), so a hard
        gate here would strand it permanently. Interrupting once and yielding
        is what makes "nothing durable is a valid result" a reachable state.
        """
        with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
            output = io.StringIO()
            with redirect_stdout(output):
                self.assertEqual(
                    guard.run_stop({"cwd": ".", "session_id": "s", "stop_hook_active": True}), 0
                )
            self.assertEqual(output.getvalue().strip(), "")


class UnarmedPullRequestStopGateTest(unittest.TestCase):
    """R17: opening a pull request does not end the duty; arming it is the duty.

    The entrypoint is explicit that a PR is not the outcome -- arm auto-merge
    once the review gate passes, then watch until the remote confirms merged.
    A reviewed pull request left unarmed is the exact silence that rule exists
    to prevent: nobody is waiting on anything, and nothing will merge.

    **Fires only when a review already exists, and that is not a nicety.**
    Blocking on any unarmed PR would deadlock against R15, which refuses `gh
    pr merge --auto` without an independent review: on a fresh PR the Stop
    hook would demand arming while R15 refused it, leaving no legal state and
    making deletion of one guard the cheapest exit -- forbidden by iron law 4.
    No review yet is simply an earlier point in the same pipeline, with
    somewhere legal to go.
    """

    def test_a_reviewed_but_unarmed_pull_request_is_reported(self):
        with patch(
            "scripts.agents.guard._unarmed_reviewed_pull_request", return_value="4539"
        ):
            reason = guard.check_r17_unarmed_pull_request({"cwd": "."})
        self.assertIsNotNone(reason)
        self.assertIn("4539", reason)
        self.assertIn("auto-merge", reason.lower())

    def test_an_armed_pull_request_is_silent(self):
        with patch("scripts.agents.guard._unarmed_reviewed_pull_request", return_value=None):
            self.assertIsNone(guard.check_r17_unarmed_pull_request({"cwd": "."}))

    def test_an_unreviewed_pull_request_is_silent_so_r15_is_not_deadlocked(self):
        """The pairwise check that matters. R15 refuses to arm without a review.

        `_unarmed_reviewed_pull_request` returns None for an unreviewed PR by
        construction, so this asserts the contract rather than the plumbing:
        Stop must never demand an action another gate forbids.
        """
        with patch("scripts.agents.guard._unarmed_reviewed_pull_request", return_value=None):
            self.assertIsNone(guard.check_r17_unarmed_pull_request({"cwd": "."}))

    def test_the_r15_and_r17_pair_has_a_legal_state_for_every_review_count(self):
        """Walk the pipeline and assert no combination leaves the agent stuck."""
        for reviews, armed in ((0, False), (1, False), (1, True)):
            with self.subTest(reviews=reviews, armed=armed):
                arming_blocked = False
                with patch(
                    "scripts.agents.guard._independent_review_count", return_value=reviews
                ):
                    arming_blocked = (
                        guard.check_r15_review_before_arming("gh pr merge 4539 --auto", "Bash")
                        is not None
                    )
                pending = "4539" if (reviews > 0 and not armed) else None
                with patch(
                    "scripts.agents.guard._unarmed_reviewed_pull_request", return_value=pending
                ):
                    stop_blocked = guard.check_r17_unarmed_pull_request({"cwd": "."}) is not None
                self.assertFalse(
                    arming_blocked and stop_blocked,
                    "no reachable state may block arming and block stopping at once",
                )


class StopReasonsAreCollectedTest(unittest.TestCase):
    """Every Stop rule must be reachable, not just the first one listed.

    `run_stop` returned after the first reason it found, and `stop_hook_active`
    makes the *second* Stop attempt return 0 immediately. Together those mean
    exactly one Stop rule can ever fire per session: when R16 blocked, R17 and
    the uncommitted-work check were never evaluated at all.

    That is the same family as the unbound-check defects recorded in
    `gotcha.a-guards-tests-passing-proves-the-function-works-never-that-the-
    hook-can-reach-it` -- a rule that exists, tests green, and cannot fire --
    and it got worse with every Stop rule added, since each new one starved
    the ones below it. Found by pairwise-checking the Stop rules against each
    other before adding a third, which is what
    `decision.check-every-new-guard-pairwise-against-the-guards-already-shipped`
    asks for.

    Collecting them into one block is also better for the reader: an agent
    ending its turn learns everything it owes at once rather than discovering
    the next duty only after satisfying the previous one.
    """

    def setUp(self):
        """Isolate these tests from every Stop rule that reads live state.

        R18 asks the real repository whether the branch has unpushed commits
        and R17 asks `gh` whether an open pull request has a review nobody
        armed. Without this, these tests pass or fail on whether a push
        happens to be pending or a reviewer happens to have replied -- green
        on a clean checkout and on CI, red exactly when the harness is doing
        its job. A result that depends on the environment rather than on the
        subject is not a test of the subject.
        """
        isolate_stop_rules(self)

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_a_learning_and_an_unarmed_pull_request_are_reported_together(self, _report):
        with patch("scripts.agents.guard.check_r16_learning_loop", return_value="LEARNING"):
            with patch(
                "scripts.agents.guard.check_r17_unarmed_pull_request", return_value="UNARMED"
            ):
                output = io.StringIO()
                with redirect_stdout(output):
                    guard.run_stop({"cwd": ".", "session_id": "s"})
        payload = json.loads(output.getvalue())
        self.assertEqual(payload["decision"], "block")
        self.assertIn("LEARNING", payload["reason"])
        self.assertIn("UNARMED", payload["reason"])

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_a_clean_session_still_produces_no_block(self, _report):
        with patch("scripts.agents.guard.check_r16_learning_loop", return_value=None):
            with patch("scripts.agents.guard.check_r17_unarmed_pull_request", return_value=None):
                output = io.StringIO()
                with redirect_stdout(output):
                    self.assertEqual(guard.run_stop({"cwd": ".", "session_id": "s"}), 0)
        self.assertEqual(output.getvalue().strip(), "")


class UserHarnessDriftStopGateTest(unittest.TestCase):
    """R20 / #4547: the harness noticed it disagreed with itself and said nothing.

    `AGENTS.md` states that user harness drift deploys through
    `scripts/agents/sync_user_harness.py`. `_sync_advisory` detects the drift
    and had exactly one call site -- `run_session_start` -- so the finding was
    printed once at session start and consumed by nothing. The drift reported
    at the start of the session that wrote this rule was still there at the
    end of it.

    Drift means the *deployed* harness on this machine no longer matches the
    *tracked* harness in the repository. Every conclusion drawn by reading
    `.agents/skills/**` is then a conclusion about a copy that is not the one
    the host loads. It is the one inconsistency the harness cannot detect from
    inside a single file read, and the only advisory in the set whose remedy
    is a single deterministic command with no judgement in it.

    Reports rather than refuses, like R16 and R18: `run_stop` returns 0 once
    `stop_hook_active` is set, so this interrupts a turn and never traps one.
    Deploying the fix from the hook was considered and left to the issue --
    it would have a hook writing outside the repository, which is more
    authority than any rule here takes, and that is the owner's call to make
    rather than a side effect of closing a ticket.
    """

    def setUp(self):
        isolate_stop_rules(self, except_for=("check_r20_user_harness_drift",))

    def stop(self, payload: dict) -> dict | None:
        stream = io.StringIO()
        with redirect_stdout(stream):
            self.assertEqual(guard.run_stop(payload), 0)
        text = stream.getvalue().strip()
        return json.loads(text) if text else None

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_stop_reports_drift_through_the_hook(self, _report):
        """Through `run_stop`, not by calling the check: reachability is the point."""
        with patch(
            "scripts.agents.guard._sync_advisory", return_value="User harness drift detected."
        ):
            output = self.stop({"cwd": "."})
        self.assertIsNotNone(output, "drift must reach the Stop payload")
        self.assertEqual(output["decision"], "block")
        self.assertIn("sync_user_harness.py --apply", output["reason"])

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_a_synced_harness_does_not_block(self, _report):
        with patch("scripts.agents.guard._sync_advisory", return_value=None):
            self.assertIsNone(self.stop({"cwd": "."}))

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_it_interrupts_once_and_never_traps_the_turn(self, _report):
        with patch(
            "scripts.agents.guard._sync_advisory", return_value="User harness drift detected."
        ):
            self.assertIsNotNone(self.stop({"cwd": "."}))
            self.assertIsNone(self.stop({"cwd": ".", "stop_hook_active": True}))

    def test_the_remedy_it_names_is_not_refused_by_another_rule(self):
        """Pairwise, as a test: a gate whose only exit another rule blocks is a deadlock."""
        with patch(
            "scripts.agents.guard._sync_advisory", return_value="User harness drift detected."
        ):
            reason = guard.check_r20_user_harness_drift({"cwd": "."})
        self.assertIsNotNone(reason)
        remedy = "py -3 scripts/agents/sync_user_harness.py --apply"
        self.assertIn(remedy, reason, "a gate must name the command that satisfies it")
        self.assertIsNone(guard.check_r8_git_stash(remedy))
        self.assertIsNone(guard.check_r13_push_before_delete(remedy, "Bash"))
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=4):
            self.assertIsNone(guard.check_r14_hard_reset(remedy, "Bash", "."))


class StopRuleIsolationIsCompleteTest(unittest.TestCase):
    """`ISOLATED_STOP_RULES` must name every rule `run_stop` calls.

    This is the check that makes the isolation survive the next rule rather
    than the current one. The defect it guards has now shipped twice in this
    branch, identically: R18 read live git state and quietly made five Stop
    tests depend on whether a push was pending, and once that was fixed by
    name, R17 did the same thing through `gh` and turned the same five red
    again.

    Neither could be caught by CI. A fresh checkout has nothing unpushed and
    no credentials to ask about reviews, so the environment that runs the
    suite is precisely the one where the bug is invisible. Only an equality
    check on the rule list can fail early, in the commit that adds the rule.

    Equality, not containment, in both directions: an unlisted new rule is the
    defect itself, and a listed rule that `run_stop` no longer calls is a
    patch aimed at nothing, which reads like coverage and is not.
    """

    def stop_rules(self) -> set[str]:
        source = inspect.getsource(guard.run_stop)
        return set(re.findall(r"check_r\d+_[a-z_]+", source))

    def test_every_stop_rule_is_isolated(self):
        self.assertEqual(self.stop_rules(), set(ISOLATED_STOP_RULES))

    def test_every_isolated_rule_exists_on_the_guard(self):
        for name in ISOLATED_STOP_RULES:
            self.assertTrue(
                callable(getattr(guard, name, None)),
                f"{name} is patched by name but is not a callable on the guard",
            )

    def test_the_rule_list_is_not_empty(self):
        """A regex that matched nothing would make both checks above vacuous."""
        self.assertTrue(self.stop_rules())


class UnpushedWorkStopGateTest(unittest.TestCase):
    """R18 / #4538, #4530: the recoverability half of the cadence rules.

    The owner standard was set after a delegate ran 25 minutes with nothing
    pushed: everything it had completed existed in one worktree on one
    machine. "A branch is recoverable only by whoever can see the machine it
    lives on. A pushed draft is recoverable by anyone." Told to push, five of
    eight issues became visible and recoverable within a minute.

    This gates the property that actually matters -- commits existing on no
    remote at the end of a turn -- rather than the five-minute interval, which
    no hook can observe without a wall clock the agent does not share. The
    interval is the practice; unpushed-at-turn-end is the failure it prevents,
    and it is the half a mechanism can hold.

    Pairwise: aligned with R13, which refuses to force-delete an unpushed
    branch. Both are satisfied by the same `git push`, so no state blocks one
    while the other forbids the remedy. It fires only when a branch exists,
    because a detached HEAD has nothing to push -- the same
    inapplicable-is-not-unknown distinction #4542 was filed for.
    """

    def test_commits_on_no_remote_are_reported(self):
        with patch("scripts.agents.guard._current_branch", return_value="ChaosEngine/x"):
            with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=4):
                reason = guard.check_r18_unpushed_work({"cwd": "."})
        self.assertIsNotNone(reason)
        self.assertIn("push", reason.lower())
        self.assertIn("ChaosEngine/x", reason)

    def test_a_fully_pushed_branch_is_silent(self):
        with patch("scripts.agents.guard._current_branch", return_value="ChaosEngine/x"):
            with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=0):
                self.assertIsNone(guard.check_r18_unpushed_work({"cwd": "."}))

    def test_a_detached_head_is_silent_because_it_has_nothing_to_push(self):
        """Inapplicable is not unknown and is not unpushed (#4542's lesson)."""
        with patch("scripts.agents.guard._current_branch", return_value=None):
            with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=9):
                self.assertIsNone(guard.check_r18_unpushed_work({"cwd": "."}))

    def test_it_fails_open_when_the_count_cannot_be_answered(self):
        with patch("scripts.agents.guard._current_branch", return_value="ChaosEngine/x"):
            with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=None):
                self.assertIsNone(guard.check_r18_unpushed_work({"cwd": "."}))

    def test_r13_and_r18_are_satisfied_by_the_same_remedy(self):
        """The pairwise property as a test: one `git push` clears both."""
        with patch("scripts.agents.guard._current_branch", return_value="ChaosEngine/x"):
            with patch("scripts.agents.guard._unrecoverable_commit_count", return_value=0):
                self.assertIsNone(guard.check_r18_unpushed_work({"cwd": "."}))
                self.assertIsNone(
                    guard.check_r13_push_before_delete("git branch -D ChaosEngine/x", "Bash")
                )


class FreshBaseGateTest(unittest.TestCase):
    """R19: never edit on the default branch.

    Task isolation requires a fresh `ChaosEngine/*` branch cut from fetched
    `origin/main` before task-specific edits. Only part of that is
    mechanisable, and the scope here is deliberately the part that is.

    Editing while HEAD is `main` is unambiguous and always wrong: the work has
    no branch of its own, cannot be opened as a pull request without a later
    rescue, and collides with anything else using the shared checkout.

    Whether an existing `ChaosEngine/*` branch is "fresh enough" is judgement
    -- the entrypoint explicitly allows reusing one for dependent work in the
    same task -- so a hook that guessed would block legitimate continuation
    every time. A gate that fires on correct work is the gate that gets
    deleted, so this one does not guess.
    """

    def payload(self, path: str) -> dict:
        return {"cwd": ".", "tool_name": "Write", "tool_input": {"file_path": path}}

    def test_editing_on_the_default_branch_is_refused(self):
        for branch in ("main", "master"):
            with self.subTest(branch=branch):
                with patch("scripts.agents.guard._current_branch", return_value=branch):
                    reason = guard.check_r19_fresh_base(self.payload("scripts/x.py"), "Write")
                self.assertIsNotNone(reason)
                self.assertIn("ChaosEngine/", reason)

    def test_editing_on_a_task_branch_is_allowed(self):
        with patch("scripts.agents.guard._current_branch", return_value="ChaosEngine/thing-1"):
            self.assertIsNone(guard.check_r19_fresh_base(self.payload("scripts/x.py"), "Write"))

    def test_a_detached_head_is_not_the_default_branch(self):
        with patch("scripts.agents.guard._current_branch", return_value=None):
            self.assertIsNone(guard.check_r19_fresh_base(self.payload("scripts/x.py"), "Write"))

    def test_non_write_tools_are_untouched(self):
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            self.assertIsNone(guard.check_r19_fresh_base({"cwd": "."}, "Bash"))

    def test_r19_and_r14_do_not_trap_an_agent_on_main_with_uncommitted_work(self):
        """Pairwise: the remedy for R19 must not be something R14 forbids.

        R19's remedy is `git checkout -b ChaosEngine/...`, which carries
        uncommitted changes onto the new branch and touches nothing. R14 only
        refuses `git reset --hard`. So the escape from R19 is always open,
        which is what keeps the pair free of the deadlock recorded in
        decision.check-every-new-guard-pairwise-against-the-guards-already-shipped.
        """
        with patch("scripts.agents.guard._uncommitted_file_count", return_value=7):
            self.assertIsNone(
                guard.check_r14_hard_reset("git checkout -b ChaosEngine/new", "Bash", ".")
            )
