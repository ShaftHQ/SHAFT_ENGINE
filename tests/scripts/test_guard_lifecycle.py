"""SessionStart and Stop share one portable lifecycle contract."""

from __future__ import annotations

import io
import json
import subprocess  # nosec B404 - tests drive the tracked hook command locally.
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

from scripts.agents import guard


class GuardLifecycleTest(unittest.TestCase):
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
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
            reason = guard.check_r13_push_before_delete("git branch -D feature/x", "Bash")
        self.assertIsNotNone(reason)
        self.assertIn("push", reason.lower())
        self.assertIn("feature/x", reason)

    def test_force_deleting_a_fully_pushed_branch_is_allowed(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=0):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -D feature/x", "Bash"))

    def test_the_safe_delete_form_is_never_touched(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=5):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -d feature/x", "Bash"))

    def test_it_fails_open_when_the_commit_count_cannot_be_answered(self):
        """Unknown is not zero and is not many -- #4542's lesson, applied here."""
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=None):
            self.assertIsNone(guard.check_r13_push_before_delete("git branch -D feature/x", "Bash"))

    def test_prose_naming_the_command_is_not_the_command(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=3):
            self.assertIsNone(
                guard.check_r13_push_before_delete(
                    'git commit -m "explain why git branch -D is guarded"', "Bash"
                )
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
