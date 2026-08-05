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
