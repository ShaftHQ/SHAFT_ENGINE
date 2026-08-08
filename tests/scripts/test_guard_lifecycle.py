"""SessionStart and Stop share one portable lifecycle contract."""

from __future__ import annotations

import ast
import inspect
import io
import json
import os
import re
import shutil
import subprocess  # nosec B404 - tests drive the tracked hook command locally.
import sys
import tempfile
import time
import unittest
from contextlib import redirect_stdout
from unittest import mock
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
    "check_r21_run_state_not_recorded",
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


class DelegatePreflightRedTest(unittest.TestCase):
    """#4570's missing delegate and learning-loop behavior."""

    def test_unadapted_dispatch_is_denied_before_it_can_run(self):
        output = io.StringIO()
        payload = {
            "tool_name": "Task",
            "tool_input": {"subagent_type": "general-purpose"},
            "session_id": "red-r22",
            "cwd": ".",
        }
        with patch("scripts.agents.guard.ledger_record"):
            with redirect_stdout(output):
                self.assertEqual(guard.run_pretooluse(payload), 0)
        self.assertIn("R22 blocked", output.getvalue())

    def test_committed_work_without_a_learning_route_cannot_arm_auto_merge(self):
        with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
            with patch("scripts.agents.guard._independent_review_count", return_value=1):
                self.assertIsNotNone(
                    guard.check_r15_review_before_arming(
                        "gh pr merge 1 --auto --squash", "Bash", {"session_id": "red-r15"}
                    )
                )

    def test_session_preflight_stays_within_the_cross_host_byte_limit(self):
        output = io.StringIO()
        with patch("scripts.agents.guard._standing_constraints", return_value="x" * 9000):
            with patch(
                "scripts.agents.guard._worktree_report",
                return_value={"worktrees": [], "advisories": []},
            ):
                with patch("scripts.agents.guard._mempalace_wake_up", return_value=None):
                    with patch("scripts.agents.guard._sync_advisory", return_value=None):
                        with redirect_stdout(output):
                            self.assertEqual(guard.run_session_start({"cwd": "."}), 0)
        payload = json.loads(output.getvalue())
        context = payload["hookSpecificOutput"]["additionalContext"]
        self.assertLessEqual(len(context.encode("utf-8")), 8192)

    def test_substantive_learning_none_reason_is_recorded(self):
        events: list[str] = []
        payload = {
            "tool_name": "Bash",
            "tool_input": {
                "command": 'py -3 scripts/agents/guard.py --learning-none "No durable learning surfaced"'
            },
            "session_id": "red-learning-none",
            "cwd": ".",
        }
        with patch(
            "scripts.agents.guard.ledger_record", side_effect=lambda _payload, event: events.append(event)
        ):
            self.assertEqual(guard.run_pretooluse(payload), 0)
        self.assertTrue(any(event.startswith("learning-none:") for event in events))


class GuardLifecycleTest(unittest.TestCase):
    """The SessionStart and Stop contract every host shares.

    Not a single rule: this covers what `run_session_start` injects and how
    `run_stop` routes worktree state, which is the frame R16, R17, R18 and R20
    all report through. A rule can be correct and still never reach an agent if
    this layer drops it, so these tests pin the envelope rather than any one
    rule's logic.

    Every Stop rule is patched off in `setUp` (#4555): their own classes test
    them, and leaving them live made these tests depend on the machine.
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


class PreflightPackTest(unittest.TestCase):
    """#4570 A4: bounded retrieval augments, but never blocks, SessionStart."""

    def test_mempalace_wake_up_is_injected_and_the_full_pack_is_byte_capped(self):
        self.assertTrue(hasattr(guard, "_mempalace_wake_up"), "A4 wake-up helper is missing")
        with patch("scripts.agents.guard._worktree_report", return_value={"worktrees": [], "advisories": []}):
            with patch("scripts.agents.guard._sync_advisory", return_value=None):
                with patch(
                    "scripts.agents.guard._mempalace_wake_up",
                    return_value="MemPalace wake-up\n" + "x" * 9000,
                ):
                    stream = io.StringIO()
                    with redirect_stdout(stream):
                        self.assertEqual(guard.run_session_start({"cwd": "."}), 0)
        context = json.loads(stream.getvalue())["hookSpecificOutput"]["additionalContext"]
        self.assertIn("MemPalace wake-up", context)
        self.assertLessEqual(len(context.encode("utf-8")), 8192)

    def test_mempalace_wake_up_fails_open_when_the_binary_cannot_run(self):
        self.assertTrue(hasattr(guard, "_mempalace_wake_up"), "A4 wake-up helper is missing")
        with patch("scripts.agents.guard.subprocess.run", side_effect=OSError):
            self.assertIsNone(guard._mempalace_wake_up("."))

    def test_mempalace_wake_up_fails_open_on_an_invalid_working_directory(self):
        with patch("scripts.agents.guard.subprocess.run", side_effect=ValueError):
            self.assertIsNone(guard._mempalace_wake_up("."))

    def test_session_start_injects_native_memory_do_not_lines(self):
        with tempfile.TemporaryDirectory() as directory:
            constraints = os.path.join(directory, ".memory", "memory", "constraints")
            gotchas = os.path.join(directory, ".memory", "memory", "gotchas")
            os.makedirs(constraints)
            os.makedirs(gotchas)
            with open(os.path.join(constraints, "one.json"), "w", encoding="utf-8") as handle:
                json.dump({"title": "A stored constraint"}, handle)
            reminder = "Do not discard a squash-merged branch before diffing it."
            with open(os.path.join(gotchas, "squash-merge.md"), "w", encoding="utf-8") as handle:
                handle.write(reminder)
            with patch("scripts.agents.guard._worktree_report", return_value={"worktrees": [], "advisories": []}):
                with patch("scripts.agents.guard._sync_advisory", return_value=None):
                    with patch("scripts.agents.guard._mempalace_wake_up", return_value=None):
                        stream = io.StringIO()
                        with redirect_stdout(stream):
                            self.assertEqual(guard.run_session_start({"cwd": directory}), 0)
        context = json.loads(stream.getvalue())["hookSpecificOutput"]["additionalContext"]
        self.assertIn(reminder, context)

    def test_native_memory_reminders_fail_open_when_the_store_cannot_be_read(self):
        with patch("scripts.agents.guard.os.listdir", side_effect=OSError):
            self.assertIsNone(guard._memory_do_not_lines("."))

    def test_native_memory_constraints_fail_open_when_the_store_cannot_be_read(self):
        with patch("scripts.agents.guard.os.listdir", side_effect=OSError):
            self.assertIsNone(guard._standing_constraints("."))


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

    Defends every PreToolUse rule at once -- R1, R2, R3, R8, R9, R10, R11,
    R13, R14, R15 and R19 -- because a hook killed for exceeding its timeout
    fails open and skips all of them for that call. Found by the adversarial
    review of #4539.

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


class OnlyAddedLinesAreBranchUniqueTest(unittest.TestCase):
    """The delivery predicate reads a diff direction it was getting backwards (#4569).

    `_content_exists_on_default_branch` asks whether deleting a branch would
    lose content the default branch lacks, and answered it with `git diff
    --quiet`, which reports *any* difference. In `git diff origin/main
    <branch>` a `-` line is content on `main` and not on the branch: the
    default branch moved on after the squash landed. Only a `+` line is
    content the branch holds and `main` does not, and only that can be lost.

    Measured, not reasoned: of the nine branches #4569 left undeletable, three
    carry zero branch-only added lines and were still refused. The stored
    gotcha `squash-merge-makes-landed-branches-look-unmerged-by-rev-list-count`
    had already recorded the same trap in the same words -- `git diff
    origin/main <branch>` "renders main's newer commits as deletions on the
    branch side, which reads like the branch is about to delete live work".

    `--numstat` rather than counting `+` lines out of a patch. It is one
    structured field per file, it reports a binary difference as `-` instead of
    a number -- which must never read as delivered -- and it exits 0 whether or
    not anything differs, so it goes through `_git_output` like every other
    query here instead of needing its own `subprocess.run` to read an exit
    code as data.

    What this deliberately does NOT decide: whether a branch whose own work was
    a *deletion* has landed. Such a branch is a content subset of `main` and is
    read as delivered. That is the predicate this function is named for and
    documents -- "loses no content the default branch lacks" -- and telling it
    apart from a landed branch needs main read for meaning, which is the shape
    section 6.1 of #4567 records this repository losing at every time.
    """

    REFERENCE = "abc123\n"
    BRANCH = "ChaosEngine/probe-that-does-not-exist"

    def delivery(self, numstat: str | None, listing: str = "one.py\ntwo.py\n"):
        """Run the predicate with git's three answers scripted."""
        git = mock.Mock(side_effect=[self.REFERENCE, listing, numstat])
        with patch("scripts.agents.guard._git_output", git):
            answer = guard._content_exists_on_default_branch(self.BRANCH)
        return answer, git

    def test_an_unmerged_deletion_only_branch_is_not_delivered(self):
        """A deletion is branch work until the default branch proves it landed."""
        answer, git = self.delivery("0\t12\tone.py\n0\t3\ttwo.py\n")
        self.assertFalse(answer, "R13 must not discard unpushed deletion-only work")
        self.assertEqual(git.call_count, 3, "the delivery question must reach git")
        self.assertIn(
            "--numstat",
            git.call_args_list[2].args[0],
            "added lines have to be counted, and --quiet cannot count them",
        )

    def test_an_identical_branch_is_delivered(self):
        answer, _ = self.delivery("")
        self.assertTrue(answer)

    def test_a_branch_holding_added_lines_is_not_delivered(self):
        """The protection, unweakened: one `+` line is one line only this branch has."""
        answer, _ = self.delivery("7\t0\tone.py\n0\t3\ttwo.py\n")
        self.assertFalse(answer)

    def test_a_binary_difference_is_never_read_as_delivered(self):
        """`--numstat` writes `-` for binary, and `-` is not zero."""
        answer, _ = self.delivery("-\t-\tone.bin\n")
        self.assertFalse(answer)

    def test_an_unanswerable_diff_is_not_read_as_delivered(self):
        """False on uncertainty, because only one of the two answers permits a deletion."""
        answer, _ = self.delivery(None)
        self.assertFalse(answer)

    def test_a_branch_that_introduces_no_files_is_delivered(self):
        """Unchanged behaviour, pinned so the rewrite cannot drop it."""
        answer, _ = self.delivery("unused", listing="\n")
        self.assertTrue(answer)

    def test_r13_refuses_deleting_a_branch_with_unproved_deletions(self):
        """End to end through the rule, not only the predicate.

        `gotcha.a-guards-tests-passing-proves-the-function-works-never-that-the-
        hook-can-reach-it` is why this exists: every other test in this class
        would pass identically if `_unrecoverable_commit_count` stopped calling
        the predicate at all.
        """
        git = mock.Mock(side_effect=[self.REFERENCE, "one.py\n", "0\t9\tone.py\n"])
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
            with patch("scripts.agents.guard._git_output", git):
                self.assertIn(
                    "R13 blocked",
                    guard.check_r13_push_before_delete(f"git branch -D {self.BRANCH}", "Bash"),
                )

    def test_r13_still_refuses_a_branch_holding_unlanded_additions(self):
        """The over-application guard: the fix must not clear a branch with real work."""
        git = mock.Mock(side_effect=[self.REFERENCE, "one.py\n", "42\t0\tone.py\n"])
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
            with patch("scripts.agents.guard._git_output", git):
                reason = guard.check_r13_push_before_delete(
                    f"git branch -D {self.BRANCH}", "Bash"
                )
        self.assertIsNotNone(reason)
        self.assertIn("R13 blocked", reason)

    def test_r13_and_r18_agree_an_unproved_branch_needs_a_push(self):
        """Pairwise: both guards retain work whose delivery is unproved.

        `decision.check-every-new-guard-pairwise-against-the-guards-already-
        shipped` lists "the push-cadence rule versus R13 (do not push a branch
        about to be deleted)" under still to check. This is that pair: R13's
        refusal is what makes a squash-landed branch look unpushed, and R18 is
        the rule whose remedy is to push it. Both read
        `_unrecoverable_commit_count`, so the safety predicate must drive both
        guards consistently.
        """
        git = mock.Mock(side_effect=[self.REFERENCE, "one.py\n", "0\t9\tone.py\n"])
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
            with patch("scripts.agents.guard._git_output", git):
                self.assertIsNotNone(
                    guard.check_r13_push_before_delete(f"git branch -D {self.BRANCH}", "Bash")
                )
        git = mock.Mock(side_effect=[self.REFERENCE, "one.py\n", "0\t9\tone.py\n"])
        with patch("scripts.agents.guard._current_branch", return_value=self.BRANCH):
            with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
                with patch("scripts.agents.guard._git_output", git):
                    self.assertIsNotNone(
                        guard.check_r18_unpushed_work({"cwd": "."}),
                        "R18 must demand a push for work R13 cannot safely delete",
                    )

    def test_the_unpushed_count_asks_git_with_an_explicit_revision(self):
        """`--not --remotes` with no positive revision reports nothing, ever.

        Not a regression this branch introduced -- `_unpushed_commit_count`
        already names the branch -- and pinned here because #4569 watched the
        revisionless form hand a clean bill of health to all ten branches while
        `git branch -r --contains` said the opposite. The next agent editing
        this query will reach for the shape that reads more naturally, and it
        is the one that can never fail.
        """
        completed = mock.Mock(returncode=0, stdout="2\n")
        with patch("scripts.agents.guard.subprocess.run", return_value=completed) as run:
            guard._unpushed_commit_count("feature")
        argv = run.call_args.args[0]
        self.assertIn("feature", argv, "the query must name a positive revision")
        self.assertLess(
            argv.index("feature"),
            argv.index("--not"),
            "git does not default to HEAD; the revision has to precede --not",
        )


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

    def test_arming_after_a_commit_requires_a_learning_route(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=1):
            with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
                reason = guard.check_r15_review_before_arming(
                    "gh pr merge 4539 --auto --squash", "Bash", {"session_id": "s"}
                )
        self.assertIsNotNone(reason)
        self.assertIn("learning", reason.lower())

    def test_auto_equals_true_cannot_bypass_the_learning_route(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=1):
            with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
                for auto in ("true", "1", "t", "T"):
                    with self.subTest(auto=auto):
                        self.assertIsNotNone(
                            guard.check_r15_review_before_arming(
                                f"gh pr merge 4539 --auto={auto} --squash",
                                "Bash",
                                {"session_id": "s"},
                            )
                        )

    def test_a_learning_write_keeps_reviewed_arming_available(self):
        with patch("scripts.agents.guard._independent_review_count", return_value=1):
            with patch("scripts.agents.guard.ledger_events", return_value=["commit", "memory-write"]):
                self.assertIsNone(
                    guard.check_r15_review_before_arming(
                        "gh pr merge 4539 --auto --squash", "Bash", {"session_id": "s"}
                    )
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


class DelegateStopHookTest(unittest.TestCase):
    """A committed delegate must reach R16 through the host stop event (#4570 A8)."""

    def setUp(self):
        isolate_stop_rules(self, except_for=("check_r16_learning_loop",))

    def test_subagent_stop_registration_reaches_r16(self):
        root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        for name in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(host=name):
                with open(os.path.join(root, name), encoding="utf-8") as handle:
                    hooks = json.load(handle)["hooks"]
                self.assertIn("SubagentStop", hooks)

        output = io.StringIO()
        payload = json.dumps({"hook_event_name": "SubagentStop", "session_id": "delegate"})
        with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
            with patch("sys.stdin", io.StringIO(payload)):
                with redirect_stdout(output):
                    self.assertEqual(guard.main([]), 0)
        self.assertIn("Learning loop", output.getvalue())


class LearningWriteObservationTest(unittest.TestCase):
    """R16 must observe every supported route that writes a learning (#4570 A9)."""

    def events_after(
        self, tool_name: str, command: str = "", tool_input: dict | None = None
    ) -> list[str]:
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = {
                    "session_id": f"learning-{tool_name}-{command}",
                    "cwd": directory,
                    "tool_name": tool_name,
                    "tool_input": {"command": command, **(tool_input or {})},
                }
                self.assertEqual(guard.run_pretooluse(payload), 0)
                return guard.ledger_events(payload)

    def test_mcp_and_cli_learning_writes_reach_the_ledger(self):
        root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        for name in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(host=name):
                with open(os.path.join(root, name), encoding="utf-8") as handle:
                    matcher = json.load(handle)["hooks"]["PreToolUse"][0]["matcher"]
                self.assertIn("mcp__mempalace__", matcher)

        for tool_name, command in (
            ("mcp__mempalace__mempalace_mine", ""),
            ("Bash", "memory remember --stdin"),
            ("PowerShell", "mempalace sweep"),
        ):
            with self.subTest(tool_name=tool_name, command=command):
                self.assertIn("memory-write", self.events_after(tool_name, command))

    def test_reading_or_mentioning_a_write_does_not_count(self):
        self.assertNotIn("memory-write", self.events_after("mcp__shaft-memory__search_memory"))
        self.assertNotIn(
            "memory-write", self.events_after("mcp__mempalace__mempalace_get_aaak_spec")
        )
        self.assertNotIn(
            "memory-write", self.events_after("mcp__mempalace__mempalace_delete_by_source")
        )
        self.assertNotIn("memory-write", self.events_after("mcp__mempalace__mempalace_sync"))
        self.assertNotIn("memory-write", self.events_after("Bash", 'echo "memory remember"'))

    def test_a_non_dry_memories_deletion_counts_as_a_write(self):
        self.assertIn(
            "memory-write",
            self.events_after(
                "mcp__mempalace__mempalace_delete_by_source", tool_input={"dry_run": False}
            ),
        )
        self.assertIn(
            "memory-write",
            self.events_after("mcp__mempalace__mempalace_sync", tool_input={"apply": True}),
        )


class LearningNoneEscapeTest(unittest.TestCase):
    """R16 / #4570: the legitimate no-learning outcome reaches the closing gate."""

    def events_after(self, command: str) -> list[str]:
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = {
                    "session_id": command,
                    "cwd": directory,
                    "tool_name": "Bash",
                    "tool_input": {"command": command},
                }
                self.assertEqual(guard.run_pretooluse(payload), 0)
                return guard.ledger_events(payload)

    def test_substantive_learning_none_reason_is_recorded(self):
        events = self.events_after(
            'py -3 scripts/agents/guard.py --learning-none "No durable learning surfaced"'
        )
        self.assertIn("learning-none:No durable learning surfaced", events)

    def test_empty_or_placeholder_reason_does_not_count(self):
        for reason in ('""', '"n/a"'):
            with self.subTest(reason=reason):
                events = self.events_after(
                    f"py -3 scripts/agents/guard.py --learning-none {reason}"
                )
                self.assertFalse(any(event.startswith("learning-none:") for event in events))


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
        # R20 reads live git state through this helper, so without pinning it
        # these tests pass or fail on whether the branch running them happens
        # to edit harness sources -- and the branch that added R20 does. That
        # is the fourth instance of the defect ISOLATED_STOP_RULES exists for,
        # and the first the equality pin could not catch: the pin covers Stop
        # rules, and this is a helper one of them calls. Filed as its own gap.
        # The tests that are *about* the suppression override this locally.
        patcher = patch(
            "scripts.agents.guard._branch_edits_harness_sources", return_value=False
        )
        patcher.start()
        self.addCleanup(patcher.stop)

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

    def test_a_branch_editing_harness_sources_is_not_drift(self):
        """The rule fired on its own author's next commit, and was wrong.

        R20 shipped, then the very next commit edited `delegation.md` and R20
        reported it as drift. It is not: while a branch edits harness sources
        the deployment is *supposed* to lag. The remedy it named was worse than
        the false positive -- `--apply` would have deployed an unmerged,
        unreviewed branch edit onto the host harness.
        """
        with patch(
            "scripts.agents.guard._sync_advisory", return_value="User harness drift detected."
        ):
            with patch(
                "scripts.agents.guard._branch_edits_harness_sources", return_value=True
            ):
                self.assertIsNone(guard.check_r20_user_harness_drift({"cwd": "."}))
            with patch(
                "scripts.agents.guard._branch_edits_harness_sources", return_value=False
            ):
                self.assertIsNotNone(guard.check_r20_user_harness_drift({"cwd": "."}))

    def test_harness_source_paths_are_recognised_and_others_are_not(self):
        """The suppression must be no wider than the files the sync deploys."""
        for path in (
            ".agents/skills/act-as-mohab/references/delegation.md",
            ".claude/skills/retrieve-first/SKILL.md",
            ".claude/user-harness/CLAUDE.md",
            ".claude/agents/coder.md",
            ".codex/agents/reviewer.toml",
        ):
            with self.subTest(path=path):
                self.assertTrue(guard._HARNESS_SOURCE.match(path))
        for path in (
            "scripts/agents/guard.py",
            "tests/scripts/test_guard_lifecycle.py",
            "AGENTS.md",
            "docs/.agents/skills/x.md",
            ".memory/memory/decisions/x.json",
        ):
            with self.subTest(path=path):
                self.assertIsNone(guard._HARNESS_SOURCE.match(path))

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


class LedgerIsAppendOnlyAndReapedTest(unittest.TestCase):
    """#4552: a read-modify-write loses whole events, and files lived forever.

    `ledger_record` read the ledger, appended, and wrote it back. This host
    issues tool calls in parallel, so two hooks could interleave and one
    event vanished. Not free: R12 refuses a production write until a test run
    is recorded, so a dropped `test-run` blocks work that did satisfy the
    rule -- a gate firing on correct work.

    The old docstring defended the design by saying an append-only file would
    force the reader to tolerate a partial line. That inverts the trade. A
    tolerant reader is a few lines and loses at most the torn line; the
    whole-document format lost *every* event in the file on any corruption.
    """

    def payload(self, session: str, directory: str) -> dict:
        return {"session_id": session, "cwd": directory}

    def test_recording_does_not_read_the_ledger_first(self):
        """The property, asserted directly rather than raced for.

        The defect was a read followed by a write, with a window between them.
        A thread race would only sometimes hit that window; proving the read
        is gone holds every time.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-append", directory)
                with patch(
                    "scripts.agents.guard.ledger_events",
                    side_effect=AssertionError("record must not read first"),
                ):
                    self.assertTrue(guard.ledger_record(payload, "test-run"))
                self.assertEqual(guard.ledger_events(payload), ["test-run"])

    def test_many_appends_all_survive(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-many", directory)
                for index in range(50):
                    self.assertTrue(guard.ledger_record(payload, f"event-{index}"))
                self.assertEqual(len(guard.ledger_events(payload)), 50)

    def test_a_torn_line_costs_only_that_line(self):
        """The whole point of the format change: partial loss, not total loss."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-torn", directory)
                guard.ledger_record(payload, "before")
                path = guard._ledger_path(payload)
                with open(path, "a", encoding="utf-8") as handle:
                    handle.write('{"partial\n')
                guard.ledger_record(payload, "after")
                self.assertEqual(guard.ledger_events(payload), ["before", "after"])

    def test_a_legacy_whole_document_ledger_survives_the_first_append(self):
        """Found in live data an hour after the append-only change shipped.

        The previous format wrote one array with no trailing newline, so the
        first append landed on the same line:
        `["test-run", "commit"]"test-run"`. Read as a single value that line is
        unparsable, and skipping it dropped the entire pre-upgrade history --
        which would leave R12 blocking a production write whose test run really
        had been observed. On the real ledger this session was using, 140
        events were being read as 10.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-legacy", directory)
                path = guard._ledger_path(payload)
                with open(path, "w", encoding="utf-8") as handle:
                    handle.write('["test-run", "commit"]')  # legacy, no newline
                guard.ledger_record(payload, "memory-write")

                self.assertEqual(
                    guard.ledger_events(payload), ["test-run", "commit", "memory-write"]
                )

    def test_two_appends_sharing_a_line_both_survive(self):
        """The concurrency shape of the same defect."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-shared-line", directory)
                path = guard._ledger_path(payload)
                with open(path, "w", encoding="utf-8") as handle:
                    handle.write('"first""second"\n')
                self.assertEqual(guard.ledger_events(payload), ["first", "second"])

    def test_an_undecodable_value_costs_only_the_rest_of_its_line(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-partial", directory)
                path = guard._ledger_path(payload)
                with open(path, "w", encoding="utf-8") as handle:
                    handle.write('"kept"\n{"torn\n"also-kept"\n')
                self.assertEqual(guard.ledger_events(payload), ["kept", "also-kept"])

    def test_stale_ledgers_are_reaped_after_two_dormant_windows(self):
        """Reaping is two-phase (#4548 finding 11): mark on the first sighting
        past retention, delete only once the mark is itself past retention
        with no write to the ledger in between. See
        `DormantSessionLedgerIsNotReapedByAnotherSessionTest` for the
        regression this timing exists to satisfy: a ledger dormant for only
        one window must survive, and one dormant for two must not."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-keep", directory)
                guard.ledger_record(payload, "fresh")
                current = guard._ledger_path(payload)
                folder = guard.os.path.dirname(current)
                stale = guard.os.path.join(folder, "stale.json")
                with open(stale, "w", encoding="utf-8") as handle:
                    handle.write('"old"\n')
                old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
                guard.os.utime(stale, (old, old))

                guard.ledger_record(payload, "again")
                self.assertTrue(
                    guard.os.path.exists(stale),
                    "one dormant window must only mark a ledger, not delete it",
                )
                mark = stale + guard._REAP_MARK_SUFFIX
                self.assertTrue(guard.os.path.exists(mark), "the sweep must leave a mark")
                guard.os.utime(mark, (old, old))

                guard.ledger_record(payload, "once-more")

                self.assertFalse(
                    guard.os.path.exists(stale), "two dormant windows must reap it"
                )
                self.assertFalse(guard.os.path.exists(mark), "the mark is reaped with it")
                self.assertTrue(guard.os.path.exists(current), "the live ledger must survive")
                self.assertEqual(
                    guard.ledger_events(payload), ["fresh", "again", "once-more"]
                )

    def test_reaping_never_raises(self):
        """Housekeeping inside a hook must not be able to block a tool call."""
        guard._reap_stale_ledgers(os.path.join("does", "not", "exist"))

    def test_the_ledger_directory_follows_the_environment(self):
        """Isolation the tests only appeared to have.

        `tempfile.gettempdir()` caches its answer, so every ledger test's
        `TMPDIR` patch was inert and they all shared the real temp directory.
        Whole-file writes hid it by overwriting whatever the last run left.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                path = guard._ledger_path(self.payload("session-env", directory))
        self.assertIsNotNone(path)
        self.assertTrue(
            os.path.realpath(path).startswith(os.path.realpath(directory)),
            f"ledger landed outside the patched temp directory: {path}",
        )


class DormantSessionLedgerIsNotReapedByAnotherSessionTest(unittest.TestCase):
    """A session dormant for one retention window survives (#4548 finding 11).

    R15/R17/R21 trust whatever a session's ledger already recorded, and this
    harness explicitly supports resuming a session after a long pause. The
    old `_reap_stale_ledgers` judged staleness by raw mtime and deleted on
    first sight, so any *other* session's routine `ledger_record` call,
    arriving once `LEDGER_RETENTION_SECONDS` had passed, could delete a
    dormant-but-still-relevant session's ledger -- silently turning "this
    session dispatched a reviewer" into "it did not" and failing a gate a
    correct agent had already satisfied. This is the mutation the test below
    kills: reverting `_reap_stale_ledgers` to delete on the first sighting
    (instead of marking first) turns this red.
    """

    def payload(self, session: str, directory: str) -> dict:
        return {"session_id": session, "cwd": directory}

    def test_a_ledger_stale_by_one_window_survives_another_sessions_sweep(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                dormant = self.payload("session-dormant", directory)
                guard.ledger_record(dormant, "review:ChaosEngine/some-branch")
                dormant_path = guard._ledger_path(dormant)
                old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
                guard.os.utime(dormant_path, (old, old))

                # An unrelated session's own routine write triggers the sweep
                # that scans the whole shared ledger directory.
                other = self.payload("session-other", directory)
                guard.ledger_record(other, "test-run")

                self.assertTrue(
                    guard.os.path.exists(dormant_path),
                    "a session dormant for exactly one retention window must "
                    "survive another session's reap sweep",
                )
                self.assertEqual(
                    guard.ledger_events(dormant),
                    ["review:ChaosEngine/some-branch"],
                    "the dormant session's recorded evidence must still be readable",
                )

    def test_a_ledger_dormant_for_two_windows_is_eventually_reaped(self):
        """The other half of the property: reaping must still happen."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                dormant = self.payload("session-abandoned", directory)
                guard.ledger_record(dormant, "test-run")
                dormant_path = guard._ledger_path(dormant)
                old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
                guard.os.utime(dormant_path, (old, old))

                other = self.payload("session-other", directory)
                guard.ledger_record(other, "test-run")  # marks it
                mark = dormant_path + guard._REAP_MARK_SUFFIX
                guard.os.utime(mark, (old, old))

                guard.ledger_record(other, "test-run")  # reaps it

                self.assertFalse(
                    guard.os.path.exists(dormant_path),
                    "a session dormant for two full retention windows must be reaped",
                )


class SelfTestCoversEveryRuleTest(unittest.TestCase):
    """`--self-test` must exercise every rule, and `main` must run it (#4551).

    The command an agent runs to sanity-check the guard, and a job the PR gate
    runs, covered R1, R9, R10 and R11 and exercised none of R12 through R20 --
    eight rules, two thirds of the file -- while printing a passing summary.
    Reassuring output is what an agent acts on, so a green that means nothing
    is worse than no check at all.
    """

    def test_main_runs_both_new_self_tests(self):
        """A self-test the entry point never calls is the same defect one level up."""
        source = inspect.getsource(guard.main)
        self.assertIn("run_required_action_self_test()", source)
        self.assertIn("run_rule_coverage_self_test()", source)

    def test_every_defined_rule_is_claimed_by_the_coverage_table(self):
        self.assertEqual(guard._defined_rules(), set(guard._SELF_TEST_COVERAGE))

    def test_the_stub_helper_restores_what_it_replaced(self):
        """A leaked stub would silently disarm every later case in the run."""
        original = guard._uncommitted_file_count
        guard._with_stubs({"_uncommitted_file_count": lambda cwd: 99}, lambda: None)
        self.assertIs(guard._uncommitted_file_count, original)

    def test_the_stub_helper_restores_even_when_the_action_raises(self):
        original = guard._uncommitted_file_count

        def boom():
            raise RuntimeError("self-test case failed hard")

        with self.assertRaises(RuntimeError):
            guard._with_stubs({"_uncommitted_file_count": lambda cwd: 99}, boom)
        self.assertIs(guard._uncommitted_file_count, original)


class HookWorkingDirectoryIsReadOneWayTest(unittest.TestCase):
    """Every rule asks for the working directory through the same helper (#4553).

    `_hook_working_directory` exists to normalise the payload's `cwd` and fall
    back to the process directory when a host omits it. R10, R11 and R14 used
    it; R17, R18 and R19 read `hook_input.get("cwd")` directly.

    Harmless on the day it was found, because `subprocess(cwd=None)` inherits
    the process directory and lands on the same answer. It stops being harmless
    the moment a value goes anywhere other than a subprocess `cwd`, or a host
    sends a payload the normaliser would have corrected. What it costs
    immediately is a reader with no way to tell which of the two forms is
    intended, and two forms that disagree only in the rare case are the ones
    that survive review.

    Asserted over the source rather than by calling each rule, because the
    defect is which expression is written, and a behavioural test would pass
    for as long as the two forms happen to agree -- which is the entire period
    in which the bug is invisible.
    """

    def rules(self) -> dict[str, object]:
        return {
            name: value
            for name, value in vars(guard).items()
            if name.startswith("check_r") and callable(value)
        }

    def test_no_rule_reads_cwd_off_the_payload(self):
        for name, function in self.rules().items():
            with self.subTest(rule=name):
                self.assertNotIn(
                    'hook_input.get("cwd")',
                    inspect.getsource(function),
                    f"{name} must go through _hook_working_directory",
                )

    def test_the_normaliser_is_the_one_place_that_reads_it(self):
        """The helper itself must keep reading the raw field, or it normalises nothing."""
        self.assertIn(
            'hook_input.get("cwd")', inspect.getsource(guard._hook_working_directory)
        )

    def test_the_rule_set_is_not_empty(self):
        """A name filter that matched nothing would make the check above vacuous."""
        self.assertGreaterEqual(len(self.rules()), 10)

    def test_subprocess_helpers_receive_the_hook_working_directory(self):
        """GitHub and git queries must resolve in the hook's worktree (#4564)."""
        cwd = "C:/hook-worktree"
        completed = mock.Mock(returncode=0, stdout="0\n")
        with patch("scripts.agents.guard.subprocess.run", return_value=completed) as run:
            guard._unpushed_commit_count("feature", cwd)
        self.assertEqual(run.call_args.kwargs["cwd"], cwd)

        with patch("scripts.agents.guard._git_output", return_value=None) as git:
            guard._content_exists_on_default_branch("feature", cwd)
        self.assertTrue(git.call_args_list)
        self.assertTrue(all(call.kwargs["cwd"] == cwd for call in git.call_args_list))

        completed.stdout = '{"author":{"login":"author"},"reviews":[]}'
        with patch("scripts.agents.guard.subprocess.run", return_value=completed) as run:
            guard._independent_review_count("42", cwd)
        self.assertEqual(run.call_args.kwargs["cwd"], cwd)

        completed.stdout = "[]"
        with patch("scripts.agents.guard.subprocess.run", return_value=completed) as run:
            guard._open_pull_request_count("feature", cwd)
        self.assertEqual(run.call_args.kwargs["cwd"], cwd)


class _NoSubprocess:
    """Stand-in for the `subprocess` module in which nothing can be run.

    Keeps the real exception classes, because `guard` catches them by name and
    a bare mock in an `except` clause raises `TypeError` instead of being
    caught -- which would report as an error in the subject tests and look
    exactly like the defect being searched for.
    """

    SubprocessError = subprocess.SubprocessError
    TimeoutExpired = subprocess.TimeoutExpired
    CalledProcessError = subprocess.CalledProcessError

    @staticmethod
    def run(*args, **kwargs):
        raise OSError("subprocess disabled for the determinism check")


class StopTestsAreIndependentOfLiveStateTest(unittest.TestCase):
    """#4555: assert determinism directly instead of enumerating the readers.

    The same defect has now shipped four times. R18 read git and made five Stop
    tests depend on whether a push was pending. R17 did it through `gh`, so the
    same five went red once this pull request received a review. R20's helper
    `_branch_edits_harness_sources` did it again, and that one the equality pin
    could not see at all -- `ISOLATED_STOP_RULES` names Stop *rules*, and this
    was a helper one of them calls.

    Enumeration keeps losing to the next thing nobody enumerated, so this
    stops enumerating. It runs the Stop-facing test classes twice -- once
    normally, once with every subprocess refused -- and requires the same
    result. A test that reaches outside the process for its answer changes
    behaviour between those two runs; one that does not, cannot. It does not
    matter whether the reader is a rule, a helper, or something added later.

    CI structurally cannot catch any instance of this: a fresh checkout has
    nothing unpushed, no credentials to ask about reviews, and no branch
    mid-edit, so the environment that runs the suite is exactly the one where
    the defect is invisible. This check does not depend on the environment at
    all, which is the point.
    """

    SUBJECT_CLASSES = (
        "GuardLifecycleTest",
        "StopReasonsAreCollectedTest",
        "UserHarnessDriftStopGateTest",
        "UnarmedPullRequestStopGateTest",
        "UnpushedWorkStopGateTest",
        "LearningLoopStopGateTest",
        "RunStateStopGateTest",
    )

    def subjects(self) -> unittest.TestSuite:
        suite = unittest.TestSuite()
        loader = unittest.TestLoader()
        module = sys.modules[__name__]
        for name in self.SUBJECT_CLASSES:
            suite.addTests(loader.loadTestsFromTestCase(getattr(module, name)))
        return suite

    def outcome(self) -> tuple[int, int, int]:
        result = unittest.TextTestRunner(stream=io.StringIO(), verbosity=0).run(
            self.subjects()
        )
        return result.testsRun, len(result.failures), len(result.errors)

    def test_the_subject_classes_all_exist(self):
        """A misspelled class name would silently shrink what is being checked."""
        module = sys.modules[__name__]
        stop_gate_classes = {
            name
            for name, subject in vars(module).items()
            if (
                name.endswith("StopGateTest")
                and isinstance(subject, type)
                and issubclass(subject, unittest.TestCase)
            )
        }
        self.assertTrue(
            stop_gate_classes.issubset(self.SUBJECT_CLASSES),
            f"Stop-gate classes missing from SUBJECT_CLASSES: "
            f"{sorted(stop_gate_classes - set(self.SUBJECT_CLASSES))}",
        )
        for name in self.SUBJECT_CLASSES:
            with self.subTest(cls=name):
                self.assertTrue(hasattr(module, name), f"{name} is not defined here")
        self.assertGreaterEqual(self.outcome()[0], 20, "the subject set is suspiciously small")

    def test_refusing_every_subprocess_changes_nothing(self):
        baseline = self.outcome()
        with patch("scripts.agents.guard.subprocess", _NoSubprocess):
            forced = self.outcome()
        self.assertEqual(
            baseline,
            forced,
            "a Stop test's outcome changed when subprocesses were refused, so it "
            "reads live state -- patch that reader off in the class whose subject "
            "is something else",
        )

    def test_the_check_can_fail(self):
        """Proof the comparison is live, not two identical no-ops.

        Without this, a subject list that loaded nothing would compare (0,0,0)
        to (0,0,0) and report determinism it never examined.
        """
        with patch("scripts.agents.guard.subprocess", _NoSubprocess):
            self.assertRaises(OSError, guard.subprocess.run, ["git", "status"])


class GuardTestClassesNameTheRuleTheyDefendTest(unittest.TestCase):
    """Every guard test class says which rule it protects (#4550).

    When one of these fails, the only thing the failure line carries is
    `ClassName.test_name`. If the class does not say which rule it defends,
    the reader learns that a function broke and not that a policy stopped
    being enforced -- and the guidance file holding that policy is not
    referenced from the test at all.

    **Deliberately per class, not per test**, which is a departure from how
    #4550 was filed. That issue came from 48 Codacy "missing docstring"
    findings, and satisfying it literally means writing 124 method docstrings
    across three files. Most would restate the assertion on the line below
    them, which is the docstring Codacy also accepts and nobody should write.
    The rule name pays for itself exactly once per class, which is also the
    granularity the failure output shows.

    An `R<number>` or an issue reference counts. Both point somewhere a reader
    can follow: the rule in `guard.py`, or the ticket that argued for it.
    """

    FILES = (
        "test_guard_lifecycle.py",
        "test_guard_nul_corruption.py",
        "test_guard_memory_worktree.py",
    )
    NAMES_A_RULE = re.compile(r"\bR\d+\b|#\d{3,}")

    def guard_test_classes(self):
        """Yield (file, class name, docstring) for every TestCase in the guard suites."""
        found = []
        directory = os.path.dirname(os.path.abspath(__file__))
        for name in self.FILES:
            path = os.path.join(directory, name)
            tree = ast.parse(open(path, encoding="utf-8").read())
            for node in tree.body:
                if not isinstance(node, ast.ClassDef):
                    continue
                # Only unittest classes: helpers and stand-ins defend no rule.
                bases = {getattr(base, "attr", getattr(base, "id", "")) for base in node.bases}
                if "TestCase" not in bases:
                    continue
                found.append((name, node.name, ast.get_docstring(node) or ""))
        return found

    def test_every_guard_test_class_names_its_rule(self):
        for filename, class_name, docstring in self.guard_test_classes():
            with self.subTest(cls=f"{filename}:{class_name}"):
                self.assertTrue(
                    docstring.strip(), f"{class_name} has no docstring naming the rule it pins"
                )
                self.assertRegex(
                    docstring,
                    self.NAMES_A_RULE,
                    f"{class_name} must name the rule it defends, as R<n> or an issue number",
                )

    def test_the_scan_finds_the_classes_it_claims_to_check(self):
        """A path or base-class filter that matched nothing would pass vacuously."""
        self.assertGreaterEqual(len(self.guard_test_classes()), 15)


class WhatCountsAsAReviewTest(unittest.TestCase):
    """R15 and R17: a bot comment is not a review, and a draft is not ready.

    Both defects were observed live on #4554, when R17 told this session to arm
    a draft pull request carrying four unimplemented tickets. Obeying it would
    have merged unfinished work as soon as CI went green.

    The second defect is the dangerous one. `github-code-quality` had left a
    `COMMENTED` review, and the shared predicate counted any review by a
    distinct account -- so **R15, the gate whose entire purpose is that
    somebody independent read the diff, was satisfiable by a bot posting a
    comment.** A reviewer who reads and finds nothing approves; one who finds
    something requests changes. Neither leaves a bare comment.

    One predicate for both rules, because they must agree. If R17 counted a
    review R15 did not, Stop would demand arming while R15 refused it, leaving
    no legal state -- the deadlock `_unarmed_reviewed_pull_request` already
    warns about, reproduced one rule over.
    """

    AUTHOR = "the-author"

    def reviews(self, login: str, state: str) -> list:
        return [{"author": {"login": login}, "state": state}]

    def test_a_bot_comment_is_not_an_independent_review(self):
        self.assertEqual(
            guard._independent_reviews(
                self.reviews("github-code-quality", "COMMENTED"), self.AUTHOR
            ),
            [],
        )

    def test_a_verdict_from_another_account_is(self):
        for state in ("APPROVED", "CHANGES_REQUESTED"):
            with self.subTest(state=state):
                self.assertEqual(
                    len(guard._independent_reviews(self.reviews("someone", state), self.AUTHOR)),
                    1,
                )

    def test_the_authors_own_approval_is_not_independent(self):
        self.assertEqual(
            guard._independent_reviews(self.reviews(self.AUTHOR, "APPROVED"), self.AUTHOR), []
        )

    def test_malformed_review_data_is_not_a_review(self):
        """Unknown must not read as reviewed; that direction unlocks arming."""
        for payload in (None, "APPROVED", [{"author": None, "state": "APPROVED"}], [{}]):
            with self.subTest(payload=repr(payload)[:30]):
                self.assertEqual(guard._independent_reviews(payload, self.AUTHOR), [])

    def test_both_rules_use_the_one_predicate(self):
        """Divergence is what creates the deadlock, so it is asserted away."""
        for name in ("_independent_review_count", "_unarmed_reviewed_pull_request"):
            with self.subTest(helper=name):
                source = inspect.getsource(getattr(guard, name))
                self.assertIn("_independent_reviews(", source)

    def test_a_draft_pull_request_is_never_reported_as_unarmed(self):
        """A draft is the author saying it is not ready; arming it merges that."""
        payload = {
            "number": 4554,
            "autoMergeRequest": None,
            "isDraft": True,
            "author": {"login": self.AUTHOR},
            "reviews": self.reviews("someone", "APPROVED"),
        }
        completed = subprocess.CompletedProcess([], 0, json.dumps(payload), "")
        with patch("scripts.agents.guard.subprocess.run", return_value=completed):
            self.assertIsNone(guard._unarmed_reviewed_pull_request("."))

        ready = dict(payload, isDraft=False)
        completed = subprocess.CompletedProcess([], 0, json.dumps(ready), "")
        with patch("scripts.agents.guard.subprocess.run", return_value=completed):
            self.assertEqual(guard._unarmed_reviewed_pull_request("."), "4554")

    def test_the_draft_field_is_actually_requested(self):
        """A field the query never asks for is always absent, so never a draft."""
        self.assertIn("isDraft", inspect.getsource(guard._unarmed_reviewed_pull_request))


class ObservedReviewDispatchTest(unittest.TestCase):
    """R15 / #4545 option C: a dispatch the hook watched counts as a review.

    R15 was unsatisfiable by the agent it governs. Its own message says to get
    a separate instance to review the diff, and doing exactly that leaves
    `gh pr view --json reviews` empty, so the only satisfying action belonged
    to a different account. #4539 is the receipt: a subagent review found two
    confirmed blockers, R15 refused anyway, and the owner armed by hand. A gate
    routinely bypassed has been deleted in everything but name.

    **Observed, never asserted**, which is the entire basis for allowing it. No
    command, flag or instruction writes a review event -- only the hook seeing
    the dispatch. That reduces "an agent that reviewed nothing" to "an agent
    that dispatched a reviewer and ignored its findings", which is strictly
    smaller and the same threat model R12 already rests on.

    The accepted cost is real and recorded rather than hidden: R15 is no longer
    unforgeable.
    """

    def payload(self, subagent: str, tool: str = "Task") -> dict:
        return {
            "tool_name": tool,
            "tool_input": {"subagent_type": subagent},
            "session_id": "s",
            "cwd": ".",
        }

    def test_a_reviewer_dispatch_produces_a_review_event(self):
        with patch("scripts.agents.guard._current_branch", return_value="feature"):
            self.assertEqual(
                guard._reviewer_dispatch_event(self.payload("reviewer"), "Task"),
                "review:feature",
            )

    def test_any_other_subagent_produces_nothing(self):
        for subagent in ("coder", "tester", "general-purpose", ""):
            with self.subTest(subagent=subagent):
                self.assertIsNone(
                    guard._reviewer_dispatch_event(self.payload(subagent), "Task")
                )

    def test_a_non_dispatch_tool_produces_nothing(self):
        self.assertIsNone(guard._reviewer_dispatch_event(self.payload("reviewer", "Bash"), "Bash"))

    def test_an_unanswerable_branch_records_nothing(self):
        """This test previously asserted the opposite, and was wrong.

        The first version recorded a keyless `review` when git could not name
        the branch, calling it "fail open, matching what R15 does when `gh`
        cannot answer". Adversarial review reproduced the consequence: a bare
        `review` matched every branch, so dispatching a reviewer from a
        detached-HEAD worktree armed an unrelated pull request from a
        different directory. The "keyed to the branch so a review of one
        branch cannot silently clear another" guarantee was void in exactly
        the case it was written for.

        Failing open is right for a rule that *refuses*; it is wrong for the
        evidence that *satisfies* one. Recording nothing leaves R15 refusing,
        and refusing is a state an agent can leave by dispatching from a
        branch. Being wrongly cleared is a state nobody can detect.
        """
        with patch("scripts.agents.guard._current_branch", return_value=None):
            self.assertIsNone(
                guard._reviewer_dispatch_event(self.payload("reviewer"), "Agent")
            )

    def test_no_recorded_event_clears_a_branch_it_does_not_name(self):
        for events in (["review"], ["review:other"], []):
            with self.subTest(events=events):
                with patch("scripts.agents.guard.ledger_events", return_value=events):
                    self.assertFalse(
                        guard._ledger_records_a_review({"session_id": "s"}, "feature")
                    )

    def test_an_unknown_branch_is_never_cleared(self):
        """`None` must not match a recorded review, however the ledger looks."""
        with patch("scripts.agents.guard.ledger_events", return_value=["review:feature"]):
            self.assertFalse(guard._ledger_records_a_review({"session_id": "s"}, None))

    def test_both_rules_read_the_branch_the_same_way(self):
        """Two sources for one question is how the R15/R17 deadlock arrived.

        R15 read local git, R17 read GitHub's `headRefName`. On a detached
        HEAD those disagree: GitHub still names a head ref while local git
        names none, so Stop demanded an arming R15 refused. No legal state --
        the deadlock the rule claimed to guard against.
        """
        expression = "_current_branch(_hook_working_directory(hook_input or {}))"
        for name in ("check_r15_review_before_arming", "_unarmed_reviewed_pull_request"):
            with self.subTest(rule=name):
                self.assertIn(expression, inspect.getsource(getattr(guard, name)))
        self.assertNotIn(
            'payload.get("headRefName")',
            inspect.getsource(guard._unarmed_reviewed_pull_request),
        )


class DispatchAdapterGateTest(unittest.TestCase):
    """R22 refuses dispatches that cannot receive a role adapter (#4570 A2)."""

    @staticmethod
    def payload(subagent: object) -> dict:
        return {
            "tool_name": "Task",
            "tool_input": {"subagent_type": subagent},
            "session_id": "r22",
            "cwd": ".",
        }

    def test_run_pretooluse_denies_an_unadapted_dispatch(self):
        """The hook path, not merely a helper, must refuse general-purpose."""
        output = io.StringIO()
        with patch("scripts.agents.guard.ledger_record"):
            with redirect_stdout(output):
                self.assertEqual(guard.run_pretooluse(self.payload("general-purpose")), 0)
        self.assertIn("R22 blocked", output.getvalue())

    def test_a_denied_dispatch_is_not_recorded_as_a_delegate(self):
        events: list[str] = []
        with patch(
            "scripts.agents.guard.ledger_record", side_effect=lambda _payload, event: events.append(event)
        ):
            guard.run_pretooluse(self.payload("general-purpose"))
        self.assertNotIn("delegate-dispatch", events)

    def test_run_pretooluse_allows_every_adapted_dispatch(self):
        for subagent in ("coder", "reviewer", "tester", "helper", "chaos-engine"):
            with self.subTest(subagent=subagent):
                output = io.StringIO()
                with patch("scripts.agents.guard.ledger_record"):
                    with redirect_stdout(output):
                        self.assertEqual(guard.run_pretooluse(self.payload(subagent)), 0)
                self.assertNotIn("R22 blocked", output.getvalue())

    def test_r22_records_the_learning_loop_arming_escape(self):
        source = inspect.getsource(guard.check_r22_dispatch_adapter).lower()
        self.assertIn("learning-loop", source)
        self.assertIn("escape", source)

    def test_the_recorder_is_wired_into_the_hook(self):
        """A recorder the hook never calls is the defect this batch keeps finding."""
        self.assertIn(
            "_reviewer_dispatch_event(hook_input, tool_name)",
            inspect.getsource(guard.run_pretooluse),
        )

    def test_both_hosts_intercept_a_dispatch(self):
        """Neither matcher listed Task or Agent, so the rule would be dead on arrival."""
        root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        for name in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(host=name):
                text = open(os.path.join(root, name), encoding="utf-8").read()
                self.assertIn("Task|Agent", text)


class HistoricalDispatchReplayTest(unittest.TestCase):
    """R22 / #4570 A12: replay the 18-dispatch audit without local transcript state."""

    # Transcribed from session 834edc78-c25b-4c2c-8b65-9bf3bed8aa2b and its
    # subagent logs. The original files are host runtime state, so the small,
    # source-controlled fixture is what makes this regression portable to CI.
    DISPATCHES = (
        ("Adversarial review PR 4554", "reviewer", True),
        ("Fix review findings 1 and 2 on PR 4554", "coder", True),
        ("File review findings 3 and 4 as issues", "general-purpose", False),
        ("Scoped review of PR 4554 new commits", "reviewer", True),
        ("Analyze review-loop cost and propose caps", "general-purpose", False),
        ("Correct R21 known-limits prose and log F1", "general-purpose", False),
        ("Purge stale local worktrees safely", "coder", True),
        ("Lane A: RED-before-GREEN validator", "coder", True),
        ("Lane B: setUp pin, R19 scope, R12 honesty", "coder", True),
        ("Lane C: credit scan, docstring sibling scan", "coder", True),
        ("Lane D: stopping rule and design-ruling gate", "coder", True),
        ("Audit and design dispatch-time enforcement", "reviewer", True),
        ("List vacuous-risk tests", "general-purpose", False),
        ("Extract prior-art issues", "general-purpose", False),
        ("Read harness guidance files", "general-purpose", False),
        ("Measure batch size vs review cycles", "general-purpose", False),
        ("Inventory guard.py rules and tests", "general-purpose", False),
        ("Audit ticket acceptance criteria vs diff", "general-purpose", False),
    )

    def test_correct_historical_dispatches_and_all_four_lanes_are_allowed(self):
        self.assertEqual(len(self.DISPATCHES), 18)
        lanes = [entry for entry in self.DISPATCHES if entry[0].startswith("Lane ")]
        self.assertEqual(len(lanes), 4)
        for description, subagent_type, allowed in self.DISPATCHES:
            with self.subTest(description=description):
                output = io.StringIO()
                payload = {
                    "tool_name": "Agent",
                    "tool_input": {"subagent_type": subagent_type},
                    "session_id": "historic-4570",
                    "cwd": ".",
                }
                with patch("scripts.agents.guard.ledger_record"):
                    with redirect_stdout(output):
                        self.assertEqual(guard.run_pretooluse(payload), 0)
                self.assertEqual("R22 blocked" not in output.getvalue(), allowed)

    def test_r15_accepts_an_observed_dispatch(self):
        arming = "gh pr merge 1 --auto --squash"
        with patch("scripts.agents.guard._independent_review_count", return_value=0):
            with patch("scripts.agents.guard._current_branch", return_value="feature"):
                with patch(
                    "scripts.agents.guard.ledger_events", return_value=["review:feature"]
                ):
                    self.assertIsNone(
                        guard.check_r15_review_before_arming(arming, "Bash", {"session_id": "s"})
                    )
                with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
                    self.assertIsNotNone(
                        guard.check_r15_review_before_arming(arming, "Bash", {"session_id": "s"})
                    )

    def test_a_review_of_another_branch_does_not_count(self):
        arming = "gh pr merge 1 --auto --squash"
        with patch("scripts.agents.guard._independent_review_count", return_value=0):
            with patch("scripts.agents.guard._current_branch", return_value="feature"):
                with patch(
                    "scripts.agents.guard.ledger_events", return_value=["review:other-branch"]
                ):
                    self.assertIsNotNone(
                        guard.check_r15_review_before_arming(arming, "Bash", {"session_id": "s"})
                    )

    def test_r17_uses_the_same_union(self):
        """Divergence here is the deadlock: Stop demanding what R15 refuses."""
        self.assertIn(
            "_ledger_records_a_review", inspect.getsource(guard._unarmed_reviewed_pull_request)
        )
        self.assertIn(
            "_ledger_records_a_review", inspect.getsource(guard.check_r15_review_before_arming)
        )


class RunStateStopGateTest(unittest.TestCase):
    """R21 / #4536: a delegating session that leaves no state behind.

    The owner requirement is that enough state lives on GitHub for a second
    agent to pick the work up when the first runs out of tokens. Findings
    already have a rule and it is kept; decisions and in-flight state have no
    home. Measured on #4504: zero comments while an agent was implementing the
    owner's choice, which existed only in a dispatch prompt and a conversation.

    Partial by construction, and the partiality is the honest part. Of the four
    triggers #4536 lists, one is a tool call this hook sees. "An owner decided
    something" is not an event, so it stays prose and the issue stays open.

    Both halves are observed, never asserted: the dispatch, and a `gh issue
    comment` that answers it. Whether the comment said anything useful is not a
    question a hook can answer, and one that tried would be satisfied by noise.
    """

    def test_a_delegating_session_that_changed_things_and_posts_nothing_is_reported(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=["delegate-dispatch", "commit"],
        ):
            self.assertIsNotNone(guard.check_r21_run_state_not_recorded({"session_id": "s"}))

    def test_a_read_only_session_that_only_dispatched_owes_nothing(self):
        """The fourth fires-on-correct-work defect, caught in review.

        R21 shipped keyed on `delegate-dispatch` alone, and that event is
        recorded for every Task/Agent call. So it fired on a session whose only
        dispatch was the `reviewer` iron law 6 mandates, and on one that ran an
        `Explore` search -- demanding a tracker comment for asking a question.
        It would have fired on the very session that ordered this rule's own
        adversarial review.

        R16 has carried the same precondition since it shipped: a read-only
        session owes no learning. R21 needed it and did not have it.
        """
        for events in (
            ["delegate-dispatch"],
            ["delegate-dispatch", "test-run"],
            ["delegate-dispatch", "review:feature"],
        ):
            with self.subTest(events=events):
                with patch("scripts.agents.guard.ledger_events", return_value=events):
                    self.assertIsNone(guard.check_r21_run_state_not_recorded({"session_id": "s"}))

    def test_posting_state_satisfies_it(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=["delegate-dispatch", "commit", "issue-update"],
        ):
            self.assertIsNone(guard.check_r21_run_state_not_recorded({"session_id": "s"}))

    def test_a_session_that_delegated_nothing_owes_nothing(self):
        """It must not fire on a solo session, which is most of them."""
        for events in ([], ["commit"], ["test-run", "commit", "memory-write"]):
            with self.subTest(events=events):
                with patch("scripts.agents.guard.ledger_events", return_value=events):
                    self.assertIsNone(guard.check_r21_run_state_not_recorded({"session_id": "s"}))

    def test_the_commands_that_count_as_recording_state(self):
        for command in (
            "gh issue comment 4536 --body x",
            "gh pr comment 4554 --body x",
            "gh issue edit 4536 --body x",
            # Opening a pull request that carries the run state is what the
            # draft-PR-first rule asks for, and did not count until review.
            "gh pr create --draft --title t --body x",
            "gh pr edit 4554 --body x",
        ):
            with self.subTest(command=command):
                self.assertTrue(guard._updates_a_tracked_issue(command))

    def test_targeting_a_repository_explicitly_still_counts(self):
        """#4548, second review: gh's own `-R`/`--repo` was not recognised.

        `gh -R owner/repo issue comment ...` is the standard way to post from
        a linked worktree or any cwd that is not the tracked repo's own
        checkout -- exactly the post R21 demands. The mutation this test
        kills: delete `_split_gh_global_flags` (or its call site) and these
        commands stop counting, so R21 fires on a session that already did
        the required work.

        `_git_output` is patched so this asserts the repository comparison
        added in the third review rather than its fail-open branch, which is
        what an unpatched run would have exercised wherever `origin` is
        unset.
        """
        with patch(
            "scripts.agents.guard._git_output",
            return_value="git@github.com:ShaftHQ/SHAFT_ENGINE.git\n",
        ):
            for command in (
                "gh -R ShaftHQ/SHAFT_ENGINE issue comment 4536 --body x",
                "gh --repo ShaftHQ/SHAFT_ENGINE pr edit 4554 --body x",
                "gh --repo=ShaftHQ/SHAFT_ENGINE issue edit 4536 --body x",
                # A fork's own `origin` is `someone/SHAFT_ENGINE`; the name
                # after the slash is what carries the signal.
                "gh -R someone/SHAFT_ENGINE pr comment 4554 --body x",
            ):
                with self.subTest(command=command):
                    self.assertTrue(guard._updates_a_tracked_issue(command))

    def test_writing_to_another_repository_does_not_count(self):
        """#4554, third review: `-R` also names repositories that are not this one.

        The two halves of the previous fix contradicted each other.
        `_skip_gh_global_flags` strips exactly the flag that says "not this
        repository", and `pr create` counts because it is bound to the
        current branch -- a binding `-R other/repo` removes. `AGENTS.md`
        sends companion docs changes to a separate pull request in
        `../shafthq.github.io`, so opening that one cleared R21 for the
        SHAFT_ENGINE session it had posted nothing to.

        The mutation this test kills: drop the repository comparison and
        `-R` goes back to satisfying R21 from anywhere. `_git_output` is
        patched because git is the boundary here, not the behavior: the
        assertion is about which repository the command names.
        """
        with patch(
            "scripts.agents.guard._git_output",
            return_value="https://github.com/ShaftHQ/SHAFT_ENGINE.git\n",
        ):
            for command in (
                "gh -R ShaftHQ/shafthq.github.io pr create --title docs --body x",
                "gh --repo ShaftHQ/shafthq.github.io pr comment 12 --body x",
                "gh --repo=someone/unrelated issue comment 1 --body x",
            ):
                with self.subTest(command=command):
                    self.assertFalse(guard._updates_a_tracked_issue(command))

    def test_changing_to_the_companion_repository_before_a_write_does_not_count(self):
        """#4566: a same-command `cd` is an implicit repository target."""
        session_root = os.path.abspath(os.path.join(tempfile.gettempdir(), "SHAFT_ENGINE"))
        companion_root = os.path.normpath(os.path.join(session_root, "..", "shafthq.github.io"))

        def remote(_arguments, command_cwd):
            if command_cwd == companion_root:
                return "git@github.com:ShaftHQ/shafthq.github.io.git\n"
            if command_cwd == session_root:
                return "git@github.com:ShaftHQ/SHAFT_ENGINE.git\n"
            self.fail(f"unexpected repository lookup: {command_cwd!r}")

        with patch("scripts.agents.guard._git_output", side_effect=remote):
            self.assertFalse(
                guard._updates_a_tracked_issue(
                    "cd ../shafthq.github.io && gh pr create --title docs --body x", session_root
                )
            )

    def test_lowercase_set_location_path_options_do_not_count(self):
        """PowerShell parameters are case-insensitive (#4566 review)."""
        session_root = os.path.abspath(os.path.join(tempfile.gettempdir(), "SHAFT_ENGINE"))
        companion_root = os.path.normpath(os.path.join(session_root, "..", "shafthq.github.io"))

        def remote(_arguments, command_cwd):
            if command_cwd == companion_root:
                return "git@github.com:ShaftHQ/shafthq.github.io.git\n"
            if command_cwd == session_root:
                return "git@github.com:ShaftHQ/SHAFT_ENGINE.git\n"
            self.fail(f"unexpected repository lookup: {command_cwd!r}")

        with patch("scripts.agents.guard._git_output", side_effect=remote):
            for option in ("-path", "-literalpath"):
                with self.subTest(option=option):
                    self.assertFalse(
                        guard._updates_a_tracked_issue(
                            f"Set-Location {option} ../shafthq.github.io; "
                            "gh pr create --title docs --body x",
                            session_root,
                        )
                    )

    def test_colon_set_location_path_options_do_not_count(self):
        """PowerShell also permits `-Path:<value>` (#4566 final review)."""
        session_root = os.path.abspath(os.path.join(tempfile.gettempdir(), "SHAFT_ENGINE"))
        companion_root = os.path.normpath(os.path.join(session_root, "..", "shafthq.github.io"))

        def remote(_arguments, command_cwd):
            if command_cwd == companion_root:
                return "git@github.com:ShaftHQ/shafthq.github.io.git\n"
            if command_cwd == session_root:
                return "git@github.com:ShaftHQ/SHAFT_ENGINE.git\n"
            self.fail(f"unexpected repository lookup: {command_cwd!r}")

        with patch("scripts.agents.guard._git_output", side_effect=remote):
            for option in ("-path:../shafthq.github.io", "-literalpath:../shafthq.github.io"):
                with self.subTest(option=option):
                    self.assertFalse(
                        guard._updates_a_tracked_issue(
                            f"Set-Location {option}; gh pr create --title docs --body x", session_root
                        )
                    )

    def test_trailing_repository_flags_for_another_repository_do_not_count(self):
        """#4566: `gh` accepts `--repo` after the subcommand too."""
        with patch(
            "scripts.agents.guard._git_output",
            return_value="https://github.com/ShaftHQ/SHAFT_ENGINE.git\n",
        ):
            for command in (
                "gh pr create --repo ShaftHQ/shafthq.github.io --title docs --body x",
                "gh issue comment 12 -R ShaftHQ/shafthq.github.io --body x",
            ):
                with self.subTest(command=command):
                    self.assertFalse(guard._updates_a_tracked_issue(command))

    def test_reading_an_issue_is_not_recording_state(self):
        """The boundary, held by what it refuses rather than what it accepts."""
        for command in (
            "gh issue view 4536",
            "gh issue list --state open",
            "gh pr view 4554 --json body",
            "git commit -m 'comment on the issue'",
            "",
        ):
            with self.subTest(command=command):
                self.assertFalse(guard._updates_a_tracked_issue(command))

    def test_creating_an_unrelated_issue_does_not_count(self):
        """#4548, second review: `issue create` names no existing issue.

        `gh issue create` for a brand-new, unrelated ticket used to clear
        R21 for whatever this session was actually supposed to report --
        real, and reproduced by the reviewer running unrelated
        `gh issue create` calls in the same session this rule governs.
        `pr create` is unaffected: it is bound to the current branch, so it
        cannot name someone else's work the way `issue create` can. The
        mutation this test kills: match `issue create` the same way `pr
        create` is matched, and this goes back to asserting the opposite.
        """
        for command in ("gh issue create --title t --body x",):
            with self.subTest(command=command):
                self.assertFalse(guard._updates_a_tracked_issue(command))

    def test_pr_create_without_draft_still_counts(self):
        """`pr create` is matched regardless of `--draft` (unlike `issue create`).

        Guards the asymmetry in the fix above: dropping `issue create` must
        not have accidentally narrowed `pr create` to only its `--draft`
        form, which the pre-existing case in
        `test_the_commands_that_count_as_recording_state` does not cover on
        its own.
        """
        self.assertTrue(guard._updates_a_tracked_issue("gh pr create --title t --body x"))

    def test_both_recorders_are_wired_into_the_hook(self):
        source = inspect.getsource(guard.run_pretooluse)
        self.assertIn('ledger_record(hook_input, "delegate-dispatch")', source)
        self.assertIn('ledger_record(hook_input, "issue-update")', source)



def _bare_interruption_promises(messages: list[str]) -> list[str]:
    """Every rendered message with a bare "interrupts once" promise."""
    violations = []
    for message in messages:
        collapsed = re.sub(r"\s+", " ", message.lower())
        for match in re.finditer(r"interrupts once(?! per turn\b)", collapsed):
            start = max(0, match.start() - 20)
            violations.append(collapsed[start : match.start() + 40])
    return violations


class InterruptsOncePromiseIsHonestTest(unittest.TestCase):
    """Every Stop message says what it actually does (#4558, #4548, and review).

    "This interrupts once" was true only inside one Stop cycle:
    `stop_hook_active` makes the immediate retry proceed, and the next turn
    starts with it False again, so an unsatisfied rule fires every turn until
    it is satisfied. #4558 records the same over-claim for R20's remedy. A rule
    that promises to interrupt once and then interrupts every turn teaches an
    agent to distrust the messages, which is how a guard gets deleted.

    The previous version of this test asserted
    `assertNotIn("This interrupts once.", source)` against the raw source
    text, and it passed while two messages still made the bare promise. R16's
    said "...interrupts once and will not ask again" -- missed on casing and
    on trailing words the exact-substring check never matched. R17's ended
    with two adjacent string literals split across source lines:
    `"...ask for it. This "` then `"interrupts once."` -- valid Python that
    concatenates to the exact literal promise at runtime, but the substring
    never appears in the *source text* because a closing quote, a newline and
    indentation sit between "This " and "interrupts once.". The check
    defended one literal spelling instead of the invariant it was named for,
    and both false promises shipped anyway, one commit after this exact
    lesson (pin the rule, not a spelling) was reaffirmed elsewhere in this
    file.

    Written now as the invariant itself, via `_bare_interruption_promises`:
    no guard message may promise a single interruption, in any phrasing,
    any casing, or split across any number of adjacent literals. A future
    rule that adds a third bare-promise variant fails this the same way the
    first two would have.
    """

    def test_no_message_promises_a_single_interruption(self):
        violations = _bare_interruption_promises(guard._rendered_stop_reasons())
        self.assertEqual(
            violations,
            [],
            "a guard message promises a single interruption instead of naming "
            f"the per-turn mechanism: {violations!r}",
        )

    def test_every_stop_rule_has_a_rendered_message_probe(self):
        source = inspect.getsource(guard.run_stop)
        dispatched = set(re.findall(r"check_r\d+_[a-z_]+", source))
        self.assertEqual(dispatched, set(guard._STOP_RULE_RENDERERS))

    def test_the_stop_rules_still_say_what_makes_the_retry_proceed(self):
        """The honest version has to name the mechanism, not just drop the claim."""
        self.assertIn("interrupts once per turn", inspect.getsource(guard))

    def test_a_split_literal_bare_promise_is_still_caught(self):
        """The mutation this class exists to kill: R17's exact former shape.

        Two string literals, adjacent in source, concatenated by Python at
        parse time into one value that ends the bare promise -- exactly how
        the defect this test replaces slipped past a raw-text substring
        check. `_bare_interruption_promises` must catch it from the value,
        not the text.
        """
        message = "ask for it. This " "interrupts once."
        violations = _bare_interruption_promises([message])
        self.assertTrue(
            violations, "a split-literal bare promise must be caught, not missed"
        )

    def test_a_runtime_assembled_bare_promise_is_caught(self):
        message = f"This interrupts {'once'}."
        reasons = guard._rendered_stop_reasons({"synthetic": lambda: message})
        self.assertTrue(_bare_interruption_promises(reasons))

    def test_the_honest_phrasing_raises_no_violation(self):
        """The check must not flag the wording it is meant to require."""
        message = "This interrupts once per turn: retry proceeds."
        self.assertEqual(_bare_interruption_promises([message]), [])


class HarnessDriftSuppressionTest(unittest.TestCase):
    """R20's suppression, tested against the real helper (#4547).

    Deliberately a separate class. `UserHarnessDriftStopGateTest` patches
    `_branch_edits_harness_sources` off in `setUp` so its own subject stays
    deterministic -- which means a test of the helper placed there would
    exercise the mock and pass no matter what the helper did. That is a
    vacuous pass of exactly the kind this batch keeps finding, and it happened
    here on the first attempt.
    """

    def test_an_unanswerable_git_suppresses_rather_than_reports(self):
        """This assertion is the reverse of the one it replaces, on evidence.

        The first version returned False -- do not suppress -- when git could
        not answer, reasoning that unknown should let the advisory speak.
        Adversarial review reproduced three ordinary ways to reach it: no local
        `origin/main`, a fetched `origin/main` with no merge base, and a hook
        cwd outside any repository. In each, R20 fired on a harness-editing
        branch and named `--apply`, which would deploy unmerged guidance to the
        host -- the exact defect this helper exists to prevent.

        So it fails closed. The cost is a missed staleness report; the
        alternative is a remedy that damages the machine, and those are not
        comparable.
        """
        for committed, working in ((None, ""), ("", None), (None, None)):
            with self.subTest(committed=committed, working=working):
                with patch(
                    "scripts.agents.guard._git_output",
                    side_effect=[committed, working],
                ):
                    self.assertTrue(guard._branch_edits_harness_sources("."))

    def test_the_suppression_asks_git_in_the_hook_working_directory(self):
        """A helper that always reads the process directory is #4553, one over."""
        self.assertIn(
            "_branch_edits_harness_sources(_hook_working_directory(hook_input))",
            inspect.getsource(guard.check_r20_user_harness_drift),
        )
        with patch("scripts.agents.guard._git_output", return_value="") as git:
            guard._branch_edits_harness_sources("/somewhere")
        for call in git.call_args_list:
            self.assertEqual(call.args[1], "/somewhere")

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

    def payload(self, path: str, cwd: str = ".", tool_name: str = "Write") -> dict:
        path_key = "notebook_path" if tool_name == "NotebookEdit" else "file_path"
        return {"cwd": cwd, "tool_name": tool_name, "tool_input": {path_key: path}}

    def repository(self):
        """A throwaway repository root and a sibling directory outside it.

        Both are realpath'd because the temp directory is a symlink on some
        platforms, and a check that compares one resolved path against one
        unresolved prefix reports every write as outside -- which would make
        the out-of-repo cases below pass for the wrong reason.
        """
        base = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, base, ignore_errors=True)
        root = os.path.join(os.path.realpath(base), "repo")
        outside = os.path.join(os.path.realpath(base), "elsewhere")
        os.makedirs(root, exist_ok=True)
        os.makedirs(outside, exist_ok=True)
        return root, outside

    def test_a_write_outside_the_repository_is_not_refused(self):
        """R19 governs where this repository's work lands, and nothing else.

        Reproduced three times before it was scoped: an analysis agent, a
        read-only agent writing its own scratch file, and the orchestrator
        opening the pull request for the batch that fixed it. Each was refused
        for writing to a path with no relation to this checkout, and the remedy
        the rule names -- `git checkout -b` -- would have switched a working
        tree shared with other live agents. A gate that fires on correct work
        is the gate that gets deleted.
        """
        root, outside = self.repository()
        target = os.path.join(outside, "scratch.txt")
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                self.assertIsNone(
                    guard.check_r19_fresh_base(self.payload(target, cwd=root), "Write")
                )

    def test_a_notebook_edit_outside_the_repository_is_not_refused(self):
        root, outside = self.repository()
        target = os.path.join(outside, "scratch.ipynb")
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                self.assertIsNone(
                    guard.check_r19_fresh_base(
                        self.payload(target, cwd=root, tool_name="NotebookEdit"), "NotebookEdit"
                    )
                )

    def test_a_write_inside_the_repository_is_still_refused(self):
        """The other half, asserted in the same shape: the rule still binds."""
        root, _ = self.repository()
        target = os.path.join(root, "scripts", "x.py")
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                reason = guard.check_r19_fresh_base(self.payload(target, cwd=root), "Write")
        self.assertIsNotNone(reason)
        self.assertIn("ChaosEngine/", reason)

    def test_a_relative_path_is_resolved_against_the_hook_directory(self):
        """A bare `scripts/x.py` is in-repo, and reads as out-of-repo unresolved.

        This is the case that decides whether the scoping is a fix or a hole:
        agents write relative paths, and a check comparing the raw string to an
        absolute root would let every one of them through.
        """
        root, _ = self.repository()
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                reason = guard.check_r19_fresh_base(
                    self.payload("scripts/x.py", cwd=root), "Write"
                )
        self.assertIsNotNone(reason, "a relative path is relative to the checkout")

    def test_a_relative_path_that_escapes_the_repository_is_not_refused(self):
        """`..` leaves the checkout, so the rule has nothing to say about it."""
        root, _ = self.repository()
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                self.assertIsNone(
                    guard.check_r19_fresh_base(
                        self.payload(os.path.join("..", "elsewhere", "note.txt"), cwd=root),
                        "Write",
                    )
                )

    def test_a_symlink_out_of_the_repository_is_followed(self):
        """The write lands outside, so the name it was reached by is not the fact.

        Skipped rather than faked where the platform will not create one:
        Windows needs developer mode or elevation for `os.symlink`, and a test
        that silently degraded to a plain directory would assert the case it
        was written to cover was never exercised.
        """
        root, outside = self.repository()
        link = os.path.join(root, "scratch-link")
        try:
            os.symlink(outside, link, target_is_directory=True)
        except (OSError, NotImplementedError, AttributeError) as error:
            self.skipTest(f"symlinks unavailable here: {error}")
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                self.assertIsNone(
                    guard.check_r19_fresh_base(
                        self.payload(os.path.join(link, "note.txt"), cwd=root), "Write"
                    )
                )

    def test_an_unanswerable_repository_root_still_refuses(self):
        """Fails closed, unlike R18, and for a reason that is not symmetry.

        `_current_branch` has already answered from this same directory, so a
        root that will not answer means git is behaving inconsistently rather
        than absently. Guessing "outside" there would disable the rule for the
        whole session on one flaky query, and the cost of guessing "inside" is
        one refusal with a remedy already in the message.
        """
        root, outside = self.repository()
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=None):
                self.assertIsNotNone(
                    guard.check_r19_fresh_base(
                        self.payload(os.path.join(outside, "scratch.txt"), cwd=root), "Write"
                    )
                )

    def test_a_write_with_no_path_is_still_refused(self):
        """No path is not evidence of an outside path, and the self-test relies on it."""
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            self.assertIsNotNone(guard.check_r19_fresh_base({"cwd": "."}, "Write"))

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

    def test_the_hook_itself_lets_an_out_of_repo_write_through(self):
        """Wiring, not just the function.

        `gotcha.a-guards-tests-passing-proves-the-function-works-never-that-the-
        hook-can-reach-it`: every other test in this class calls
        `check_r19_fresh_base` directly and would pass identically if
        `run_pretooluse` denied the call for some other reason. The scoping is
        only worth anything if the deny never reaches the agent, so this drives
        the entry point and asserts nothing was printed -- a deny is stdout.
        """
        root, outside = self.repository()
        payload = {
            "cwd": root,
            "tool_name": "Write",
            "tool_input": {"file_path": os.path.join(outside, "scratch.txt")},
        }
        stream = io.StringIO()
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._git_output", return_value=root + "\n"):
                with redirect_stdout(stream):
                    self.assertEqual(guard.run_pretooluse(payload), 0)
        self.assertEqual(stream.getvalue().strip(), "", "the hook still denied the write")

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
