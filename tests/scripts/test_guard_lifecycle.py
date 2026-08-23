"""SessionStart and Stop share one portable lifecycle contract."""

from __future__ import annotations

import ast
import hashlib
import importlib
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
from concurrent.futures import ThreadPoolExecutor
from contextlib import redirect_stdout
from datetime import UTC, datetime
from pathlib import Path
from unittest import mock
from unittest.mock import patch

from scripts.agents import guard
try:
    from scripts.agents import learning_session
except ImportError:  # RED replay runs this test file against pre-rename production.
    from scripts.agents import learning_loop as learning_session

try:
    reflection = importlib.import_module("scripts.agents.reflection")
except ModuleNotFoundError as error:
    if error.name != "scripts.agents.reflection":
        raise

    def _missing_reflection(*_args, **_kwargs):
        raise AssertionError("reflection support is unavailable")

    class _MissingReflection:
        """Make the pre-feature fixture fail as a RED assertion, not at import."""

        active_entries = staticmethod(_missing_reflection)
        append_entry = staticmethod(_missing_reflection)
        entries = staticmethod(_missing_reflection)
        has_valid_terminal_receipt = staticmethod(_missing_reflection)
        ledger_path = staticmethod(_missing_reflection)
        main = staticmethod(_missing_reflection)
        mark_non_attempt = staticmethod(_missing_reflection)
        pending_checkpoint = staticmethod(_missing_reflection)
        record_activity = staticmethod(_missing_reflection)
        record_failure = staticmethod(_missing_reflection)
        record_platform_outcome = staticmethod(_missing_reflection)
        record_receipt = staticmethod(_missing_reflection)
        record_session_start = staticmethod(_missing_reflection)
        record_trigger = staticmethod(_missing_reflection)

    reflection = _MissingReflection()

LEARNING_CONTROLLER = str(Path(learning_session.__file__))


def preflight_context_bytes(context: str) -> int:
    """Measure source preflight without mandatory verbatim companion bodies."""
    root = Path(__file__).resolve().parents[2]
    for name in ("caveman", "ponytail"):
        body = root.joinpath(
            f"chaos-engine/vendor/{name}/skills/{name}/SKILL.md"
        ).read_text(encoding="utf-8")
        context = context.replace(body, "", 1)
    return len(context.encode("utf-8"))

# Every Stop rule `run_stop` calls. All of them are patched off in the classes
# whose subject is something else.
#
# R18 reads the real repository and once made five tests depend on whether a
# push happened to be pending. Isolating every Stop rule keeps these tests
# deterministic when another rule later gains an external reader.
#
# Hence: isolate every Stop rule, not the subset known today to read outside
# the process. A rule that needs no isolation loses nothing by being listed --
# its own test class calls it directly, and the collection test re-patches
# whatever it asserts on -- while a rule that does need it can no longer be
# overlooked. `StopRuleIsolationIsCompleteTest` fails until a newly added rule
# is named here, which is the part that prevents a third repeat.
ISOLATED_STOP_RULES = (
    "check_r16_learning_session",
    "check_r18_unpushed_work",
    # R20 shells out to the sync helper, so leaving it live would make every
    # test below depend on whether this machine's deployed harness happens to
    # match the tracked one -- the third instance of the defect this list
    # exists for, caught by the equality pin in the commit that added it.
    "check_r20_user_harness_drift",
    "check_r21_run_state_not_recorded",
    "check_r24_foreign_worktree_left_behind",
    "check_r29_delivery_complete",
)


def isolate_stop_rules(
    case: unittest.TestCase, except_for: tuple[str, ...] = ()
) -> None:
    """Patch retained Stop rules off except for the rule under test."""
    for name in ISOLATED_STOP_RULES:
        if name in except_for:
            continue
        patcher = patch(f"scripts.agents.guard.{name}", return_value=None)
        patcher.start()
        case.addCleanup(patcher.stop)


class ReflectionCheckpointContractTest(unittest.TestCase):
    """#5001: the second attempt failure opens a reflection checkpoint."""

    def test_second_failure_in_one_task_requires_reflection(self):
        with tempfile.TemporaryDirectory() as temporary:
            payload = {
                "hook_event_name": "PostToolUse",
                "tool_name": "PowerShell",
                "tool_input": {
                    "command": "py -3 -m unittest tests.scripts.test_guard_lifecycle"
                },
                "tool_response": {"status": "failed", "exit_code": 1},
                "session_id": "reflection-second-failure",
                "cwd": ".",
            }
            with patch.dict(os.environ, {"TMPDIR": temporary, "TEMP": temporary}):
                with redirect_stdout(io.StringIO()):
                    guard.run_posttooluse(payload)
                second_output = io.StringIO()
                with redirect_stdout(second_output):
                    guard.run_posttooluse(payload)
                ledger = Path(guard._ledger_path(payload))

                self.assertTrue(ledger.is_file(), "failed outcomes must reach the task ledger")
                self.assertIn('"kind":"task-failure"', ledger.read_text(encoding="utf-8"))
                self.assertIn("reflection", second_output.getvalue().casefold())
                fingerprint = reflection.pending_checkpoint(
                    "reflection-second-failure"
                )["failureFingerprints"][0]
                self.assertIn(fingerprint, second_output.getvalue())

    def test_first_failure_remains_normal(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_posttooluse(self._failure("first-only"))
            self.assertEqual("", output.getvalue())
            self.assertIsNone(reflection.pending_checkpoint("first-only"))

    def test_duplicate_host_observation_does_not_advance_the_threshold(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = {**self._failure("deduplicated"), "tool_use_id": "call-1"}
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            self.assertIsNone(reflection.pending_checkpoint("deduplicated"))
            failures = [
                item for item in reflection.entries("deduplicated")
                if item.get("kind") == "task-failure"
            ]
            self.assertEqual(1, len(failures))

    def _failure(self, session_id, *, command="py -3 -m unittest focused", event="PostToolUse"):
        return {
            "hook_event_name": event,
            "tool_name": "PowerShell",
            "tool_input": {"command": command},
            "tool_response": {"status": "failed", "exit_code": 1},
            "session_id": session_id,
            "cwd": ".",
        }

    def _receipt(self, checkpoint, **overrides):
        receipt = {
            "schemaVersion": 1,
            "taskId": "issue-5000",
            "trigger": checkpoint["trigger"],
            "failureFingerprints": checkpoint["failureFingerprints"],
            "failedAssumption": "The unchanged attempt would produce new evidence.",
            "approachesCompared": ["Inspect the bounded state", "Change one diagnostic input"],
            "chosenExperiment": "Change one input and run the focused check.",
            "changedApproach": "Stopped the unchanged retry and isolated the invariant.",
            "proofCommandOrCheck": "focused unittest",
            "proofOutcome": "The focused check passed.",
            "durableDisposition": "nothing-durable",
        }
        receipt.update(overrides)
        return receipt

    def test_distinct_second_failure_requires_task_reflection(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            first = self._failure("distinct", command="py -3 -m unittest first")
            second = self._failure("distinct", command="mvn -pl shaft-engine test")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(first)
                guard.run_posttooluse(second)
            checkpoint = reflection.pending_checkpoint("distinct")
            self.assertEqual("task", checkpoint["depth"])
            self.assertEqual("second-failure", checkpoint["trigger"])

    def test_same_second_failure_requires_deep_reflection_and_blocks_third_attempt(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("same")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            checkpoint = reflection.pending_checkpoint("same")
            self.assertEqual("deep", checkpoint["depth"])
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_pretooluse(
                    {**payload, "hook_event_name": "PreToolUse", "tool_response": {}},
                    "portable",
                )
            self.assertIn("permissionDecision", output.getvalue())
            self.assertIn("Reflection required", output.getvalue())

    def test_read_only_diagnosis_and_non_attempt_are_allowed_without_clearing_checkpoint(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("diagnosis")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_pretooluse(
                    {"tool_name": "Read", "session_id": "diagnosis", "tool_input": {}},
                    "portable",
                )
            self.assertEqual("", output.getvalue())
            non_attempt = {**payload}
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(non_attempt)
            newest = [
                item for item in reflection.entries("diagnosis")
                if item.get("kind") == "task-failure"
            ][-1]
            reflection.mark_non_attempt("diagnosis", newest["failureId"], "capability-probe")
            self.assertEqual(2, reflection.pending_checkpoint("diagnosis")["attemptCount"])

    def test_pending_checkpoint_denial_does_not_create_another_failure(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("non-recursive")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            before = [
                item
                for item in reflection.active_entries("non-recursive")
            ]
            denied = {
                "hook_event_name": "PreToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "scripts/agents/probe.py"},
                "session_id": "non-recursive",
                "cwd": ".",
            }
            with redirect_stdout(io.StringIO()):
                guard.run_pretooluse(denied, "portable")
                guard.run_pretooluse(denied, "portable")
            after = [
                item
                for item in reflection.active_entries("non-recursive")
            ]

            self.assertEqual(before, after)
            self.assertEqual(2, reflection.pending_checkpoint("non-recursive")["attemptCount"])

    def test_guard_refusals_are_non_attempt_observations(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = {"session_id": "guard-scope", "tool_use_id": "first"}
            with redirect_stdout(io.StringIO()):
                guard._record_guard_block_and_deny(payload, "same refusal", "portable")
                guard._record_guard_block_and_deny(
                    {**payload, "tool_use_id": "second"}, "same refusal", "portable"
                )
            self.assertIsNone(reflection.pending_checkpoint("guard-scope"))
            failures = [
                item
                for item in reflection.entries("guard-scope")
                if item.get("kind") == "task-failure"
            ]
            self.assertEqual(2, len(failures))
            self.assertTrue(all(item["attempted"] is False for item in failures))

    def test_agent_ids_isolate_reflection_checkpoints(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("root-session")
            payload["agent_id"] = "audit-a"
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)

            agent_session = reflection.scope_session_id("root-session", "audit-a")
            other_session = reflection.scope_session_id("root-session", "audit-b")
            self.assertIsNotNone(reflection.pending_checkpoint(agent_session))
            self.assertIsNone(reflection.pending_checkpoint(other_session))
            self.assertIsNone(reflection.pending_checkpoint("root-session"))
            self.assertEqual(
                "audit-a",
                guard.normalize_hook_input({"agentId": "audit-a"})["agent_id"],
            )

    def test_failed_read_only_subagent_diagnostics_never_open_a_checkpoint(self):
        for command in (
            "rg -n needle scripts",
            "Get-Content scripts/agents/guard.py",
            "git branch --show-current",
        ):
            with self.subTest(command=command), tempfile.TemporaryDirectory() as temporary, patch.dict(
                os.environ, {"TMPDIR": temporary, "TEMP": temporary}
            ):
                payload = self._failure("root-readonly", command=command)
                payload["agent_id"] = "audit-readonly"
                with redirect_stdout(io.StringIO()):
                    guard.run_posttooluse(payload)
                    guard.run_posttooluse(payload)

                scoped = reflection.scope_session_id("root-readonly", "audit-readonly")
                self.assertIsNone(reflection.pending_checkpoint(scoped))
                failures = [
                    item
                    for item in reflection.entries(scoped)
                    if item.get("kind") == "task-failure"
                ]
                self.assertTrue(failures)
                self.assertTrue(all(item["attempted"] is False for item in failures))

    def test_valid_receipt_resets_checkpoint_and_replay_reopens_it(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("receipt")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            checkpoint = reflection.pending_checkpoint("receipt")
            token = reflection.record_session_start("receipt")
            recorded = reflection.record_receipt("receipt", self._receipt(checkpoint), token)
            self.assertIsNone(reflection.pending_checkpoint("receipt"))
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            self.assertEqual("deep", reflection.pending_checkpoint("receipt")["depth"])
            reflection.append_entry("receipt", recorded)
            self.assertEqual(
                "deep",
                reflection.pending_checkpoint("receipt")["depth"],
                "a valid old receipt must not clear a new occurrence",
            )

    def test_tampered_receipt_does_not_clear_checkpoint(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("tampered")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            checkpoint = reflection.pending_checkpoint("tampered")
            token = reflection.record_session_start("tampered")
            receipt = reflection.record_receipt("tampered", self._receipt(checkpoint), token)
            receipt["proofOutcome"] = "tampered after validation"
            reflection.append_entry("tampered", receipt)
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            self.assertEqual("deep", reflection.pending_checkpoint("tampered")["depth"])

    def test_receipt_rejects_stale_fingerprint_secret_and_user_path(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("unsafe")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            checkpoint = reflection.pending_checkpoint("unsafe")
            token = reflection.record_session_start("unsafe")
            for overrides in (
                {"failureFingerprints": ["stale"]},
                {"failedAssumption": "api_key=do-not-store"},
                {"proofOutcome": "see C:\\Users\\person\\trace.log"},
            ):
                with self.assertRaises(ValueError):
                    reflection.record_receipt("unsafe", self._receipt(checkpoint, **overrides), token)

    def test_claude_failure_event_and_codex_failed_result_have_equivalent_state(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            codex = self._failure("codex")
            claude = self._failure("claude", event="PostToolUseFailure")
            claude.pop("tool_response")
            claude["error"] = "raw host text must not be persisted"
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(codex)
                guard.run_posttooluse(claude)
            codex_entry = next(item for item in reflection.entries("codex") if item.get("kind") == "task-failure")
            claude_entry = next(item for item in reflection.entries("claude") if item.get("kind") == "task-failure")
            for field in ("failureClass", "target", "invariant", "phase", "fingerprint"):
                self.assertEqual(codex_entry[field], claude_entry[field])
            self.assertNotIn("raw host text", json.dumps(claude_entry))

    def test_long_session_blocks_even_recursive_stop_until_receipt_and_summary(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            session = "long"
            token = reflection.record_session_start(session, "2020-01-01T00:00:00+00:00")
            payload = {"session_id": session, "stop_hook_active": True}
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_stop(payload)
            self.assertIn("Terminal reflection required", output.getvalue())
            receipt = self._receipt(
                {"trigger": "long-session-completion", "failureFingerprints": []},
                trigger="long-session-completion",
            )
            reflection.record_receipt(session, receipt, token)
            payload["last_assistant_message"] = "\n".join(
                f"{label}: recorded" for label in guard._TERMINAL_REFLECTION_LABELS
            )
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_stop(payload)
            self.assertEqual("", output.getvalue())

    def test_changed_diagnostic_test_is_allowed_but_unchanged_rerun_is_not(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            payload = self._failure("changed-diagnostic")
            with redirect_stdout(io.StringIO()):
                guard.run_posttooluse(payload)
                guard.run_posttooluse(payload)
            self.assertIsNotNone(reflection.pending_checkpoint("changed-diagnostic"))
            changed = {
                **payload,
                "hook_event_name": "PreToolUse",
                "tool_input": {"command": "py -3 -m unittest different.probe"},
            }
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_pretooluse(changed, "portable")
            self.assertEqual("", output.getvalue())

    def test_second_local_ci_disagreement_opens_checkpoint(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            for platform, outcome in (
                ("local", "failed"),
                ("ci", "passed"),
                ("local", "failed"),
                ("ci", "passed"),
            ):
                reflection.record_platform_outcome(
                    "platform", target="focused-test", platform=platform, outcome=outcome
                )
            checkpoint = reflection.pending_checkpoint("platform")
            self.assertEqual("platform-disagreement", checkpoint["trigger"])

    def test_host_flow_correlates_different_local_ci_commands_by_logical_target(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            outcomes = (
                ("local", "failed", "py -3 -m unittest package.case", 1),
                ("ci", "passed", "mvn -Dtest=Case test", 0),
                ("local", "failed", "py -3 -m unittest package.case", 1),
                ("ci", "passed", "mvn -Dtest=Case test", 0),
            )
            for platform, status, command, code in outcomes:
                with redirect_stdout(io.StringIO()):
                    guard.run_posttooluse(
                        {
                            "hook_event_name": "PostToolUse",
                            "tool_name": "PowerShell",
                            "tool_input": {"command": command},
                            "tool_response": {"status": status, "exit_code": code},
                            "session_id": "host-platform",
                            "platform": platform,
                            "target": "Case",
                        }
                    )
            self.assertEqual(
                "platform-disagreement",
                reflection.pending_checkpoint("host-platform")["trigger"],
            )

    def test_explicit_semantic_trigger_opens_checkpoint_without_store_or_github(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            reflection.record_trigger("semantic", "premise-invalidated", "premise")
            checkpoint = reflection.pending_checkpoint("semantic")
            self.assertEqual("premise-invalidated", checkpoint["trigger"])


class ReflectionReceiptPrivacyTest(unittest.TestCase):
    """#5000: reflection recovery is private, bound, and non-bypassable."""
    def _pending(self, session):
        for _ in range(2):
            reflection.record_failure(
                session,
                phase="tool-outcome",
                target="focused-test",
                failure_class="tool-failure",
                platform="win32",
            )
        return reflection.pending_checkpoint(session)

    def _receipt(self, checkpoint, **overrides):
        receipt = {
            "schemaVersion": 1,
            "taskId": "issue-5000",
            "trigger": checkpoint["trigger"],
            "failureFingerprints": checkpoint["failureFingerprints"],
            "failedAssumption": "The direct test represented the installed flow.",
            "approachesCompared": ["Patch the copy", "Use one portable source"],
            "chosenExperiment": "Execute a real temporary install.",
            "changedApproach": "Test through the installed hook boundary.",
            "proofCommandOrCheck": "portable install unittest",
            "proofOutcome": "The installed hook completed successfully.",
            "durableDisposition": "guidance-fixed",
        }
        receipt.update(overrides)
        return receipt

    def test_session_token_and_closed_schema_reject_forged_receipts(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            checkpoint = self._pending("forged")
            token = reflection.record_session_start("forged")
            with self.assertRaises(ValueError):
                reflection.record_receipt("forged", self._receipt(checkpoint), "forged-token")
            with self.assertRaises(ValueError):
                reflection.record_receipt(
                    "forged", self._receipt(checkpoint, rawLog="forbidden"), token
                )
            reflection.append_entry(
                "forged",
                {
                    "schemaVersion": 1,
                    "kind": "reflection-receipt",
                    "sessionHash": "forged",
                    "checkpointDigest": "forged",
                    "receiptHash": hashlib.sha256(b"caller-controlled").hexdigest(),
                },
            )
            self.assertIsNotNone(reflection.pending_checkpoint("forged"))

    def test_failure_classifications_never_persist_secret_or_user_path(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            reflection.record_failure(
                "privacy",
                phase="password=hunter2",
                target=r"C:\Users\alice\private",
                failure_class="tool-failure",
                platform="/home/alice/machine",
                invariant="api_key=forbidden",
            )
            rendered = reflection.ledger_path("privacy").read_text(encoding="utf-8").casefold()
            for forbidden in ("hunter2", "alice", "forbidden", "c:\\users", "/home/"):
                self.assertNotIn(forbidden, rendered)
            checkpoint = self._pending("receipt-privacy")
            token = reflection.record_session_start("receipt-privacy")
            for unsafe in (
                "access_token=abc123",
                "bearer abc12345",
                r"\\server\alice\share",
                "workspace-alice-private",
            ):
                with self.assertRaises(ValueError):
                    reflection.record_receipt(
                        "receipt-privacy",
                        self._receipt(checkpoint, failedAssumption=unsafe),
                        token,
                    )

    def test_canonical_issue_url_and_harmless_token_prose_are_accepted(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            checkpoint = self._pending("issue-url")
            token = reflection.record_session_start("issue-url")
            recorded = reflection.record_receipt(
                "issue-url",
                self._receipt(
                    checkpoint,
                    issue="https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5296",
                    failedAssumption="The session token label implied shared recovery state.",
                ),
                token,
            )
            self.assertEqual(
                "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5296",
                recorded["issue"],
            )

            near_miss = self._pending("issue-near-miss")
            near_miss_token = reflection.record_session_start("issue-near-miss")
            with self.assertRaises(ValueError):
                reflection.record_receipt(
                    "issue-near-miss",
                    self._receipt(
                        near_miss,
                        issue="https://github.com/ShaftHQ/SHAFT_ENGINE/pull/5296",
                    ),
                    near_miss_token,
                )

    def test_exact_recovery_command_allowed_and_substring_bypass_blocked(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            self._pending("recovery")
            failure_ids = [
                item["failureId"] for item in reflection.active_entries("recovery")
            ]
            exact = (
                "py -3 scripts/agents/reflection.py non-attempt --session-id recovery "
                f"--failure-id {failure_ids[-1]} --reason capability-probe"
            )
            bypass = (
                "Write-Output scripts/agents/reflection.py --session-id recovery; "
                "git commit -am bypass"
            )
            tracker = "gh issue comment 5000 --body reflection-plan"
            self.assertEqual("non-attempt", guard._reflection_recovery_operation(exact))
            self.assertIsNone(guard._reflection_recovery_operation(bypass))
            for python_bypass in (
                'py -3 -c "print(1)" scripts/agents/reflection.py receipt --session-id recovery',
                "python -m malicious scripts/agents/reflection.py receipt --session-id recovery",
                "py -3 scripts/agents/reflection.py receipt --session-id recovery --session-token t --json '{}';git commit -am x",
            ):
                self.assertIsNone(guard._reflection_recovery_operation(python_bypass))
            allowed = io.StringIO()
            with redirect_stdout(allowed):
                guard.run_pretooluse(
                    {"tool_name": "PowerShell", "tool_input": {"command": exact}, "session_id": "recovery"}
                )
            self.assertEqual("", allowed.getvalue())
            blocked = io.StringIO()
            with redirect_stdout(blocked):
                guard.run_pretooluse(
                    {"tool_name": "PowerShell", "tool_input": {"command": bypass}, "session_id": "recovery"}
                )
            self.assertIn("permissionDecision", blocked.getvalue())
            tracker_output = io.StringIO()
            with redirect_stdout(tracker_output):
                guard.run_pretooluse(
                    {"tool_name": "PowerShell", "tool_input": {"command": tracker}, "session_id": "recovery"}
                )
            self.assertEqual("", tracker_output.getvalue())

    def test_cli_non_attempt_disposes_only_the_exact_failure(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            checkpoint = self._pending("cli-non-attempt")
            self.assertIsNotNone(checkpoint)
            failure_ids = [
                item["failureId"] for item in reflection.active_entries("cli-non-attempt")
            ]
            with redirect_stdout(io.StringIO()):
                self.assertEqual(
                    0,
                    reflection.main(
                        [
                            "non-attempt", "--session-id", "cli-non-attempt",
                            "--failure-id", failure_ids[-1], "--reason", "syntax-error",
                        ]
                    ),
                )
            self.assertIsNone(reflection.pending_checkpoint("cli-non-attempt"))
            remaining = reflection.active_entries("cli-non-attempt")
            self.assertEqual([failure_ids[0]], [item["failureId"] for item in remaining])

    def test_cli_recovery_resolves_agent_scope_from_root_identity(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            scoped = reflection.scope_session_id("root-cli", "audit-a")
            checkpoint = self._pending(scoped)
            self.assertIsNotNone(checkpoint)
            failure_id = [
                item["failureId"] for item in reflection.active_entries(scoped)
            ][-1]
            with redirect_stdout(io.StringIO()):
                result = reflection.main(
                    [
                        "non-attempt",
                        "--session-id",
                        "root-cli",
                        "--agent-id",
                        "audit-a",
                        "--failure-id",
                        failure_id,
                        "--reason",
                        "capability-probe",
                    ]
                )

            self.assertEqual(0, result)
            self.assertIsNone(reflection.pending_checkpoint(scoped))

    def test_cli_json_receipt_completes_without_an_intermediate_file(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            checkpoint = self._pending("cli-receipt")
            token = reflection.record_session_start("cli-receipt")
            with redirect_stdout(io.StringIO()):
                result = reflection.main(
                    [
                        "receipt", "--session-id", "cli-receipt",
                        "--session-token", token,
                        "--json", json.dumps(self._receipt(checkpoint)),
                    ]
                )
            self.assertEqual(0, result)
            self.assertIsNone(reflection.pending_checkpoint("cli-receipt"))

    def test_sessions_are_isolated(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            self._pending("one")
            reflection.record_failure(
                "two", phase="tool-outcome", target="focused", failure_class="tool-failure"
            )
            self.assertIsNotNone(reflection.pending_checkpoint("one"))
            self.assertIsNone(reflection.pending_checkpoint("two"))

    def test_concurrent_failures_have_distinct_exact_ids(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            def write_failure(index):
                return reflection.record_failure(
                    "concurrent",
                    phase="tool-outcome",
                    target=f"target-{index}",
                    failure_class="tool-failure",
                )["failureId"]

            with ThreadPoolExecutor(max_workers=8) as pool:
                identifiers = list(pool.map(write_failure, range(24)))
            self.assertEqual(len(identifiers), len(set(identifiers)))


class TerminalReflectionContractTest(unittest.TestCase):
    """#5000: sessions over one hour owe a final bound receipt/report."""
    def _receipt(self):
        return {
            "schemaVersion": 1,
            "taskId": "issue-5000",
            "trigger": "long-session-completion",
            "failureFingerprints": [],
            "failedAssumption": "Direct unit proof represented the installed flow.",
            "approachesCompared": ["Patch copies", "Use one portable source"],
            "chosenExperiment": "Run a real installed hook.",
            "changedApproach": "Verify the installed boundary first.",
            "proofCommandOrCheck": "installed hook probe",
            "proofOutcome": "The installed hook passed.",
            "durableDisposition": "guidance-fixed",
        }

    def test_under_one_hour_cannot_prerecord_terminal_receipt_and_does_not_block_stop(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            token = reflection.record_session_start("short")
            with self.assertRaises(ValueError):
                reflection.record_receipt("short", self._receipt(), token)
            output = io.StringIO()
            with redirect_stdout(output):
                guard.run_stop({"session_id": "short", "stop_hook_active": True})
            self.assertEqual("", output.getvalue())

    def test_restart_preserves_earliest_start_and_every_summary_label_is_required(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            token = reflection.record_session_start("restart", "2020-01-01T00:00:00+00:00")
            reflection.record_session_start("restart")
            reflection.record_receipt("restart", self._receipt(), token)
            complete = "\n".join(
                f"{label}: recorded" for label in guard._TERMINAL_REFLECTION_LABELS
            )
            self.assertIsNone(
                guard._terminal_reflection_reason(
                    {"session_id": "restart", "last_assistant_message": complete}
                )
            )
            for label in guard._TERMINAL_REFLECTION_LABELS:
                missing = complete.replace(f"{label}: recorded", "")
                reason = guard._terminal_reflection_reason(
                    {"session_id": "restart", "last_assistant_message": missing}
                )
                self.assertIn(label, reason)

    def test_activity_after_terminal_receipt_reopens_terminal_duty(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            token = reflection.record_session_start("late", "2020-01-01T00:00:00+00:00")
            reflection.record_receipt("late", self._receipt(), token)
            self.assertTrue(reflection.has_valid_terminal_receipt("late"))
            reflection.record_activity("late", "mutation-or-delivery")
            self.assertFalse(reflection.has_valid_terminal_receipt("late"))

    def test_failure_after_terminal_receipt_reopens_terminal_duty(self):
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            token = reflection.record_session_start(
                "late-failure", "2020-01-01T00:00:00+00:00"
            )
            reflection.record_receipt("late-failure", self._receipt(), token)
            reflection.record_failure(
                "late-failure",
                phase="tool-outcome",
                target="proof",
                failure_class="tool-failure",
            )
            self.assertFalse(reflection.has_valid_terminal_receipt("late-failure"))


class CommitObservationTest(unittest.TestCase):
    """R29 records only successful retained commits as delivery evidence."""

    def test_failed_commit_attempt_does_not_create_a_commit_event(self):
        events: list[str] = []
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "git commit -m final-scope"},
            "tool_response": {"status": "failed", "exit_code": 1},
        }
        with patch(
            "scripts.agents.guard.ledger_record",
            side_effect=lambda _payload, event: events.append(event) or True,
        ):
            guard.run_posttooluse(payload)
        self.assertNotIn("commit", events)

    def test_interrupted_posttooluse_is_not_a_successful_commit(self):
        events: list[str] = []
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "git commit -m final-scope"},
            "tool_response": {"status": "success", "exit_code": 0},
        }
        with patch(
            "scripts.agents.guard.ledger_record",
            side_effect=lambda _payload, event: events.append(event) or True,
        ):
            guard.run_posttooluse(payload)
            self.assertIn("commit", events)
            events.clear()
            guard.run_posttooluse({**payload, "tool_response": {"interrupted": True}})
        self.assertNotIn("commit", events)

    def test_cross_host_result_alias_is_normalized_before_failure_check(self):
        normalized = guard.normalize_hook_input(
            {
                "toolName": "shell_command",
                "toolInput": {"command": "git commit -m final-scope"},
                "toolResponse": {"status": "failed", "exitCode": 1},
            }
        )
        self.assertEqual(normalized["tool_response"]["status"], "failed")

    def test_git_executable_and_global_options_are_commit_invocations(self):
        for command in (
            "git.exe commit -m final-scope",
            "git -C . commit -m final-scope",
            "git -c user.name=delivery commit -m final-scope",
        ):
            with self.subTest(command=command):
                self.assertTrue(guard._is_git_commit_command(command))

    def test_shared_repository_resolution_is_capped_by_the_hook_budget(self):
        with patch("scripts.agents.guard._subprocess_timeout", return_value=0.125):
            with patch("scripts.agents.guard.subprocess.run", return_value=mock.Mock()) as runner:
                guard._bounded_repository_context_runner(
                    ["gh", "repo", "view"], capture_output=True, text=True, check=False
                )
        self.assertEqual(runner.call_args.kwargs["timeout"], 0.125)


class ResearchPreflightAdvisoryStoreTest(unittest.TestCase):
    """R25 store policy is independent of the broad Stop-rule fixture."""

    def test_store_events_are_advisory_for_a_complete_non_store_receipt(self):
        advisory = {"query-native-memory", "query-mempalace", "query-graphify"}
        required = [
            event for event in guard.RESEARCH_PREFLIGHT_EVENTS if event not in advisory
        ]
        payload = {
            "cwd": ".",
            "session_id": "research-first-test",
            "tool_name": "Write",
            "tool_input": {},
        }
        with patch("scripts.agents.guard.ledger_events", return_value=required):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(payload, "Write")
            )


class DelegatePreflightRedTest(unittest.TestCase):
    """#4570's missing delegate and learning-session behavior."""

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

    def test_session_preflight_stays_within_the_cross_host_byte_limit(self):
        output = io.StringIO()
        with patch(
            "scripts.agents.guard._worktree_report",
            return_value={"worktrees": [], "advisories": ["x" * 9000]},
        ):
            with patch("scripts.agents.guard._sync_advisory", return_value=None):
                with redirect_stdout(output):
                    self.assertEqual(guard.run_session_start({"cwd": "."}), 0)
        payload = json.loads(output.getvalue())
        context = payload["hookSpecificOutput"]["additionalContext"]
        self.assertLessEqual(len(output.getvalue().encode("utf-8")), 4096)
        self.assertLessEqual(preflight_context_bytes(context), 4096)

    def test_structured_learning_none_reason_is_recorded_after_success(self):
        events: list[str] = []
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            payload = {
                "tool_name": "Bash",
                "tool_input": {
                    "command": f'py -3 "{LEARNING_CONTROLLER}" attest-none '
                    "--session-id red-learning-none --operation-id red-none-op "
                    "--reason-code no_new_evidence"
                },
                "session_id": "red-learning-none",
                "cwd": directory,
            }
            learning_session.attest_no_learning(state, "red-learning-none", "no_new_evidence")
            learning_session.record_completion(
                state, "red-learning-none", "red-none-op", "attest-none"
            )
            with patch.object(learning_session, "default_state_dir", return_value=state):
                with patch(
                    "scripts.agents.guard.ledger_record",
                    side_effect=lambda _payload, event: events.append(event),
                ):
                    self.assertEqual(guard.run_posttooluse(payload), 0)
        self.assertTrue(any(event.startswith("learning-none:") for event in events))


class GuardLifecycleTest(unittest.TestCase):
    """The SessionStart and Stop contract every host shares.

    Not a single rule: this covers what `run_session_start` injects and how
    `run_stop` routes worktree state, which is the frame R16, R18, R20, and R29
    all report through. A rule can be correct and still never reach an agent if
    this layer drops it, so these tests pin the envelope rather than any one
    rule's logic.

    Every Stop rule is patched off in `setUp` (#4555): their own classes test
    them, and leaving them live made these tests depend on the machine.
    """

    def setUp(self):
        """Isolate these tests from every Stop rule that reads live state.

        R18 asks the real repository whether the branch has unpushed commits.
        Without isolation, these tests pass or fail on whether a push happens
        to be pending instead of on their subject.
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
        self.assertIn("chaos-engine", specific["additionalContext"])

    @patch("scripts.agents.guard._sync_advisory", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_session_start_delivers_tracked_policy_not_unscoped_memory_prose(
        self, _report, _sync
    ):
        """Tracked policy is ambient authority; retrieved prose is task-scoped evidence."""
        output = self.output(guard.run_session_start, {"cwd": "."})
        context = output["hookSpecificOutput"]["additionalContext"]
        self.assertIn("chaos-engine/SKILL.md", context)
        self.assertIn("untrusted evidence", context)
        self.assertNotIn("Standing constraints", context)
        self.assertNotIn("closing keywords", context)

    @patch("scripts.agents.guard._sync_advisory", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_session_start_still_confirms_the_entrypoint(self, _report, _sync):
        output = self.output(guard.run_session_start, {"cwd": "."})
        self.assertIn(
            "chaos-engine", output["hookSpecificOutput"]["additionalContext"]
        )

    @patch("scripts.agents.guard._sync_advisory", return_value=None)
    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [], "advisories": []},
    )
    def test_session_start_injects_the_complete_implementation_preflight(
        self, _report, _sync
    ):
        context = self.output(guard.run_session_start, {"cwd": "."})[
            "hookSpecificOutput"
        ]["additionalContext"].lower()
        for required in (
            "implementation preflight",
            "live files",
            "routed skill",
            "native memory",
            "mempalace",
            "graphify",
            "authoritative online research",
            "proven approaches",
            "concrete plan",
        ):
            self.assertIn(required, context)

    def payload(self) -> dict:
        return {
            "cwd": ".",
            "session_id": "research-first-test",
            "tool_name": "Write",
            "tool_input": {"file_path": ".agents/skills/act-as-mohab/SKILL.md"},
        }

    def test_write_without_a_receipt_is_blocked(self):
        with patch("scripts.agents.guard.ledger_events", return_value=[]):
            reason = guard.check_r25_research_before_implementation(self.payload(), "Write")
        self.assertIn("research-first", reason.lower())
        self.assertIn("read-live-files", reason)

    def test_each_missing_or_late_receipt_event_blocks_the_mutation(self):
        required = guard.IMPLEMENTATION_PREFLIGHT_EVENTS
        for missing in required:
            with self.subTest(missing=missing):
                events = [event for event in required if event != missing]
                with patch("scripts.agents.guard.ledger_events", return_value=events):
                    self.assertIsNotNone(
                        guard.check_r25_research_before_implementation(self.payload(), "Write")
                    )
        late = [*required[1:], required[0]]
        with patch("scripts.agents.guard.ledger_events", return_value=late):
            self.assertIsNotNone(
                guard.check_r25_research_before_implementation(self.payload(), "Write")
            )

    def test_complete_ordered_receipt_allows_the_mutation(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=list(guard.RESEARCH_PREFLIGHT_EVENTS),
        ):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(self.payload(), "Write")
            )

    def test_analysis_tools_are_not_blocked(self):
        with patch("scripts.agents.guard.ledger_events", return_value=[]):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(self.payload(), "Read")
            )

    def test_live_tool_events_map_to_the_receipt_vocabulary(self):
        fixtures = (
            ("Read", {"file_path": "src/Main.java"}, None, "read-live-files"),
            ("Read", {"file_path": ".agents/skills/act-as-mohab/SKILL.md"}, None, "load-routed-skill"),
            ("PowerShell", {"command": "memory search harness"}, None, "query-native-memory"),
            ("PowerShell", {"command": "mempalace search harness"}, None, "query-mempalace"),
            ("PowerShell", {"command": "graphify query guard"}, None, "query-graphify"),
            ("WebSearch", {"query": "official hook documentation"}, {"url": "https://docs.github.com/en/actions"}, "authoritative-online-research"),
            ("update_plan", {"explanation": "Compare proven approaches", "plan": []}, None, "compare-proven-approaches"),
            ("update_plan", {"explanation": "Compare proven approaches", "plan": [{"step": "Implement", "status": "pending"}]}, None, "record-plan"),
            ("enter_plan_mode", {}, None, "compare-proven-approaches"),
            ("EnterPlanMode", {}, None, "compare-proven-approaches"),
            ("exit_plan_mode", {}, None, "record-plan"),
            ("ExitPlanMode", {}, None, "record-plan"),
            (
                "todo_write",
                {
                    "todos": [
                        {
                            "id": "1",
                            "content": "Compare proven approaches then implement",
                            "status": "in_progress",
                        }
                    ]
                },
                None,
                "compare-proven-approaches",
            ),
            (
                "todo_write",
                {
                    "todos": [
                        {"id": "1", "content": "Implement mapping", "status": "pending"}
                    ]
                },
                None,
                "record-plan",
            ),
        )
        for tool_name, tool_input, tool_result, expected in fixtures:
            with self.subTest(tool_name=tool_name, expected=expected):
                self.assertIn(
                    expected,
                    guard._research_preflight_events(tool_name, tool_input, tool_result),
                )
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command", {"command": "memory.txt search guard"}
            ),
            (),
        )
        for command in (
            "memory.cmd.exe query guard",
            "memory.ps1.exe search guard",
            "graphify.bat.exe query guard",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    guard._research_preflight_events(
                        "shell_command", {"command": command}
                    ),
                    (),
                )

    def test_todo_write_without_compare_marker_does_not_emit_compare(self):
        events = guard._research_preflight_events(
            "todo_write",
            {"todos": [{"id": "1", "content": "Implement mapping", "status": "pending"}]},
        )
        self.assertIn("record-plan", events)
        self.assertNotIn("compare-proven-approaches", events)
        self.assertEqual(guard._research_preflight_events("todo_write", {"todos": []}), ())
        self.assertEqual(guard._research_preflight_events("todo_write", {}), ())

    def test_wrapped_update_plan_maps_plan_receipt_events(self):
        source = (
            'const result = await tools.update_plan({explanation:"Compare proven approaches",'
            'plan:[{step:"Implement",status:"in_progress"}]}); text(result);'
        )

        self.assertEqual(
            guard._research_preflight_events("functions.exec", source, {}),
            ("compare-proven-approaches", "record-plan"),
        )
        self.assertEqual(
            guard._research_preflight_events(
                "functions.exec", f'const example = {json.dumps(source)}; text(example);', {}
            ),
            (),
        )
        invalid_sources = (
            'await tools.update_plan({/* explanation:"Compare proven approaches" */'
            'plan:[{step:"Implement",status:"in_progress"}]});',
            'await tools.update_plan({explanation:"Compare proven approaches",'
            'plan:[buildStep()]});',
            'await tools.update_plan({...payload,explanation:"Compare proven approaches",'
            'plan:[{step:"Implement",status:"in_progress"}]});',
            'await tools.update_plan({metadata:{explanation:"Compare proven approaches"},'
            'plan:[{step:"Implement",status:"in_progress"}]});',
        )
        required_prefix = list(guard.IMPLEMENTATION_PREFLIGHT_EVENTS[:3])
        for invalid_source in invalid_sources:
            with self.subTest(invalid_source=invalid_source):
                events = guard._research_preflight_events("functions.exec", invalid_source, {})
                self.assertEqual(events, ())
                with patch.object(
                    guard,
                    "ledger_events",
                    return_value=[*required_prefix, *events],
                ), patch.object(
                    guard, "_act_as_mohab_root", return_value=str(Path.cwd())
                ), patch.object(guard, "_path_is_inside", return_value=True):
                    self.assertIsNotNone(
                        guard.check_r25_research_before_implementation(
                            self.payload(), "Write"
                        )
                    )

    def test_shell_command_maps_research_clis_in_command_order(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command",
                {"command": "memory search guard; mempalace status; graphify query guard"},
            ),
            ("query-native-memory", "query-mempalace", "query-graphify"),
        )

    def test_full_path_memory_exe_maps_native_memory(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command", {"command": r'& "C:\\tools\\memory.exe" search guard'}
            ),
            ("query-native-memory",),
        )

    def test_pinned_memory_cmd_query_maps_native_memory(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command",
                {"command": r'& "C:\\project\\.chaos-engine\\memory.cmd" query guard'},
            ),
            ("query-native-memory",),
        )

    def test_full_path_mempalace_exe_maps_mempalace(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command", {"command": r'& "C:\\tools\\mempalace.exe" status'}
            ),
            ("query-mempalace",),
        )

    def test_pinned_mempalace_ps1_maps_mempalace(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command",
                {"command": r'& "C:\\project\\.chaos-engine\\mempalace.ps1" search guard'},
            ),
            ("query-mempalace",),
        )

    def test_full_path_graphify_exe_maps_graphify(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command", {"command": r'& "C:\\tools\\graphify.exe" query guard'}
            ),
            ("query-graphify",),
        )

    def test_pinned_graphify_bat_maps_graphify(self):
        self.assertEqual(
            guard._research_preflight_events(
                "shell_command",
                {"command": r'& "C:\\project\\.chaos-engine\\graphify.bat" query guard'},
            ),
            ("query-graphify",),
        )

    def test_underscore_memory_mcp_name_maps_native_memory(self):
        self.assertEqual(
            guard._research_preflight_events(
                "mcp__shaft_memory__search_memory", {"query": "guard"}
            ),
            ("query-native-memory",),
        )

    def test_web_run_structural_open_maps_allowlisted_official_source(self):
        self.assertEqual(
            guard._research_preflight_events(
                "web__run",
                {"open": [{"ref_id": "https://docs.python.org/3/using/cmdline.html"}]},
                {"content": [{"type": "text", "text": "official source opened"}]},
            ),
            ("authoritative-online-research",),
        )

    def test_exec_command_maps_cmd_research_in_command_order(self):
        self.assertEqual(
            guard._research_preflight_events(
                "exec_command",
                {
                    "cmd": "memory search guard; mempalace search guard --wing x; "
                    "graphify query guard"
                },
            ),
            (
                "query-native-memory",
                "query-mempalace",
                "query-graphify",
            ),
        )
        self.assertEqual(
            guard.normalize_hook_input(
                {"tool_name": "exec_command", "tool_input": {"cmd": "Set-Content x y"}}
            )["tool_name"],
            "PowerShell",
        )
        output = io.StringIO()
        wrapped = {
            "tool_name": "functions.exec",
            "tool_input": 'await tools.exec_command({cmd:"git reset --hard HEAD~1"});',
            "cwd": ".",
            "session_id": "wrapped-command-safety",
        }
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=list(guard.RESEARCH_PREFLIGHT_EVENTS),
        ):
            with patch("scripts.agents.guard._reflection.pending_checkpoint", return_value=None):
                with patch("scripts.agents.guard.check_r19_fresh_base", return_value=None):
                    with patch(
                        "scripts.agents.guard._uncommitted_file_count",
                        return_value=1,
                    ):
                        with redirect_stdout(output):
                            guard.run_pretooluse(wrapped)
        self.assertIn("R14", output.getvalue())
        dynamic_output = io.StringIO()
        dynamic = {
            "tool_name": "functions.exec",
            "tool_input": "const bad = 'git reset --hard HEAD~1'; "
            "await tools.exec_command({cmd: bad});",
            "cwd": ".",
            "session_id": "wrapped-dynamic-command",
        }
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=list(guard.RESEARCH_PREFLIGHT_EVENTS),
        ):
            with patch("scripts.agents.guard._reflection.pending_checkpoint", return_value=None):
                with patch("scripts.agents.guard.check_r19_fresh_base", return_value=None):
                    with redirect_stdout(dynamic_output):
                        guard.run_pretooluse(dynamic)
        self.assertIn("cannot inspect", dynamic_output.getvalue())
        mixed_output = io.StringIO()
        mixed = {
            **dynamic,
            "tool_input": 'await tools.exec_command({cmd:"echo safe"}); '
            "const bad = 'git reset --hard HEAD~1'; "
            "await tools.exec_command({cmd: bad});",
        }
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=list(guard.RESEARCH_PREFLIGHT_EVENTS),
        ):
            with patch("scripts.agents.guard._reflection.pending_checkpoint", return_value=None):
                with patch("scripts.agents.guard.check_r19_fresh_base", return_value=None):
                    with redirect_stdout(mixed_output):
                        guard.run_pretooluse(mixed)
        self.assertIn("cannot inspect", mixed_output.getvalue())

    def test_wrapped_functions_exec_command_maps_official_source(self):
        self.assertEqual(
            guard._research_preflight_events(
                "functions.exec",
                'const result = await tools.exec_command({cmd:"curl.exe --url '
                'https://docs.python.org/3/library/json.html -o NUL"}); text(result);',
                {"exit_code": 0},
            ),
            ("authoritative-online-research",),
        )

    def test_exec_command_maps_isolated_primary_source_fetch(self):
        self.assertEqual(
            guard._research_preflight_events(
                "exec_command",
                {
                    "cmd": "curl.exe --url "
                    "https://docs.python.org/3/library/json.html -o NUL"
                },
            ),
            ("authoritative-online-research",),
        )

    def test_failed_current_host_research_calls_do_not_certify_success(self):
        fixtures = (
            (
                "PowerShell",
                {"command": "memory search guard"},
                {"status": "failed", "exit_code": 1},
            ),
            (
                "mcp__shaft-memory__search_memory",
                {"query": "guard"},
                {"isError": True},
            ),
            (
                "web__run",
                {"open": [{"ref_id": "https://docs.python.org/3/using/cmdline.html"}]},
                {"status": "failed", "url": "https://docs.python.org/3/using/cmdline.html"},
            ),
        )
        observed = []
        with patch(
            "scripts.agents.guard.ledger_record",
            side_effect=lambda _payload, event: observed.append(event),
        ):
            for tool_name, tool_input, tool_response in fixtures:
                guard.run_posttooluse(
                    {
                        "tool_name": tool_name,
                        "tool_input": tool_input,
                        "tool_response": tool_response,
                    }
                )
        self.assertEqual(observed, [])

    def test_portable_hook_matchers_skip_read_only_pre_calls_but_observe_outcomes(self):
        plan_surfaces = (
            "update_plan",
            "enter_plan_mode",
            "EnterPlanMode",
            "exit_plan_mode",
            "ExitPlanMode",
            "todo_write",
            "TodoWrite",
        )
        read_only_surfaces = ("Read", "Grep", "WebSearch", "WebFetch", "web__run", *plan_surfaces)
        for relative in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(relative=relative):
                hooks = json.loads(
                    (Path(__file__).resolve().parents[2] / relative).read_text(encoding="utf-8")
                )["hooks"]
                pre = hooks["PreToolUse"][0]["matcher"]
                post = hooks["PostToolUse"][0]["matcher"]
                for tool in read_only_surfaces:
                    self.assertIsNone(re.fullmatch(pre, tool), tool)
                    self.assertIsNotNone(re.fullmatch(post, tool), tool)
                for tool in ("apply_patch", "PowerShell"):
                    self.assertIsNotNone(re.fullmatch(pre, tool), tool)
                for tool in ("Task", "Agent", "spawn_agent"):
                    self.assertIsNotNone(re.fullmatch(pre, tool), tool)
                    self.assertIsNone(re.fullmatch(post, tool), tool)

    def test_redirect_detection_is_quote_aware(self):
        quoted = 'rg -n "Map<String, List<Integer>>" shaft-engine'
        redirected = 'rg -n "Map<String, List<Integer>>" shaft-engine > matches.txt'

        self.assertFalse(guard._shell_is_mutation(quoted))
        self.assertEqual((), guard._shell_mutation_targets(quoted))
        self.assertTrue(guard._shell_is_mutation(redirected))
        self.assertEqual(("matches.txt",), guard._shell_mutation_targets(redirected))

    def test_powershell_named_paths_and_literal_variables_are_static_targets(self):
        self.assertEqual(
            ("scripts/out.txt",),
            guard._shell_mutation_targets("Set-Content -LiteralPath scripts/out.txt -Value x"),
        )
        self.assertEqual(
            ("scripts/out.txt",),
            guard._shell_mutation_targets("$receipt = 'scripts/out.txt'; Out-File -FilePath $receipt"),
        )
        context = guard._invocation_context(
            {
                "cwd": ".",
                "tool_input": {"command": "Set-Content -Path $dynamic -Value x"},
            },
            "PowerShell",
        )
        self.assertTrue(context.unresolved)

    def test_generate_notes_preview_is_the_only_read_only_gh_post(self):
        preview = "gh api --method POST repos/ShaftHQ/SHAFT_ENGINE/releases/generate-notes -f tag_name=x"
        release = "gh api --method POST repos/ShaftHQ/SHAFT_ENGINE/releases -f tag_name=x"

        self.assertFalse(guard._shell_is_mutation(preview))
        self.assertTrue(guard._shell_is_mutation(release))
        self.assertTrue(guard._shell_is_mutation("gh api --method PATCH repos/ShaftHQ/SHAFT_ENGINE/releases/1"))

    def test_one_successful_forwarded_wrapped_web_result_certifies_research(self):
        source = (
            'const result = await tools.web__run({open:[{ref_id:"https://docs.github.com/en/rest/releases/releases"}]}); '
            "text(result);"
        )
        result = {
            "status": "completed",
            "content": [{"type": "text", "text": "https://docs.github.com/en/rest/releases/releases"}],
        }
        self.assertIn(
            "authoritative-online-research",
            guard._research_preflight_events("functions.exec", source, result),
        )
        counterexamples = (
            'await tools.web__run({open:[]}); text("https://docs.github.com/en/rest/releases/releases");',
            'const result = await tools.web__run(dynamic); text(result);',
            'const first = await tools.web__run({open:[]}); const second = await tools.web__run({open:[]}); text(first);',
            'const result = await tools.web__run({open:[]}); text({url:"https://docs.github.com/en/rest/releases/releases"});',
        )
        for candidate in counterexamples:
            with self.subTest(candidate=candidate):
                self.assertNotIn(
                    "authoritative-online-research",
                    guard._research_preflight_events("functions.exec", candidate, result),
                )

    def test_observational_post_event_avoids_effect_journal(self):
        payload = {
            "hook_event_name": "PostToolUse",
            "session_id": "pure-post",
            "tool_name": "Read",
            "tool_input": {"file_path": "AGENTS.md"},
        }
        with patch("scripts.agents.guard._kernel_core") as kernel:
            kernel = kernel.return_value
            normalized = mock.Mock(target_phase="")
            normalized.name = "PostToolUse"
            kernel.normalize_event.return_value = normalized
            kernel.evaluate.return_value = mock.Mock(decision="allow")
            kernel.EffectJournal.side_effect = AssertionError("journal should not be opened")

            report = guard._evaluate_kernel_event(payload, "codex")

        self.assertEqual(kernel.evaluate.return_value, report)
        kernel.evaluate.assert_called_once_with(normalized)

    def test_apply_patch_string_targets_the_named_worktree_not_hook_cwd(self):
        patch_input = "*** Update File: C:/task-worktree/scripts/x.py\n"
        self.assertEqual(
            ("C:/task-worktree/scripts/x.py",),
            guard._implementation_targets("apply_patch", patch_input),
        )

    def test_every_live_mutation_lane_requires_the_receipt(self):
        fixtures = (
            ("PowerShell", {"command": "Set-Content scripts/x.py changed"}),
            ("exec_command", {"cmd": "Set-Content scripts/x.py changed"}),
            ("functions.exec", 'const p = "x"; await tools.apply_patch(p);'),
            (
                "functions.exec",
                'await tools.exec_command({cmd:"echo safe"}); '
                "const bad = 'git reset --hard HEAD~1'; "
                "await tools.exec_command({cmd: bad});",
            ),
            (
                "mcp__shaft_memory__remember_memory",
                {"content": "durable"},
            ),
            ("PowerShell", {"command": "Clear-Content scripts/x.py"}),
            ("PowerShell", {"command": "Copy-Item source.txt scripts/x.py"}),
            ("PowerShell", {"command": "Rename-Item old.txt scripts/x.py"}),
            ("Bash", {"command": "printf changed > scripts/x.py"}),
            ("mcp__shaft-memory__remember_memory", {"content": "durable"}),
            ("mcp__mempalace__mempalace_delete_drawer", {"drawer_id": "x"}),
            ("PowerShell", {"command": "gh api --method PATCH repos/o/r -f x=y"}),
        )
        for tool_name, tool_input in fixtures:
            with self.subTest(tool_name=tool_name, tool_input=tool_input):
                payload = {"cwd": ".", "tool_input": tool_input}
                with patch("scripts.agents.guard.ledger_events", return_value=[]):
                    self.assertIsNotNone(
                        guard.check_r25_research_before_implementation(payload, tool_name)
                    )

    def test_apply_patch_shares_default_branch_and_outside_target_scoping(self):
        inside = {
            "cwd": ".",
            "tool_input": {"patch": "*** Update File: scripts/x.py\n"},
        }
        outside = {
            "cwd": ".",
            "tool_input": {"patch": "*** Update File: ../scratch.txt\n"},
        }
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch(
                "scripts.agents.guard._repository_root",
                return_value=os.path.realpath("."),
            ):
                self.assertIsNotNone(guard.check_r19_fresh_base(inside, "apply_patch"))
                self.assertIsNone(guard.check_r19_fresh_base(outside, "apply_patch"))
        with patch("scripts.agents.guard.ledger_events", return_value=[]):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(outside, "apply_patch")
            )
        wrapped = {
            "cwd": ".",
            "tool_input": 'const p = "patch"; await tools.apply_patch(p);',
        }
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch(
                "scripts.agents.guard._repository_root",
                return_value=os.path.realpath("."),
            ):
                self.assertIsNotNone(
                    guard.check_r19_fresh_base(wrapped, "functions.exec")
                )

    def test_only_successful_post_tool_events_certify_research(self):
        payload = {
            "cwd": ".",
            "session_id": "post-tool-receipt",
            "tool_name": "Read",
            "tool_input": {"file_path": ".agents/skills/act-as-mohab/SKILL.md"},
        }
        observed = []
        with patch("scripts.agents.guard.ledger_record", side_effect=lambda _payload, event: observed.append(event)):
            self.assertEqual(guard.run_pretooluse(payload), 0)
        self.assertEqual(observed, [], "attempted PreToolUse calls must not certify success")

        with patch("scripts.agents.guard.ledger_record", side_effect=lambda _payload, event: observed.append(event)):
            self.assertEqual(guard.run_posttooluse(payload), 0)
        self.assertEqual(observed, ["read-live-files", "load-routed-skill"])

    def test_successful_shell_research_is_recorded_in_command_order(self):
        payload = {
            "cwd": ".",
            "session_id": "ordered-shell-receipt",
            "tool_name": "PowerShell",
            "tool_input": {
                "command": "graphify query x; mempalace search x; memory search x; "
                "Get-Content .agents/skills/act-as-mohab/SKILL.md"
            },
        }
        observed = []
        with patch("scripts.agents.guard.ledger_record", side_effect=lambda _payload, event: observed.append(event)):
            guard.run_posttooluse(payload)
        self.assertEqual(
            observed,
            [
                "query-graphify",
                "query-mempalace",
                "query-native-memory",
                "read-live-files",
                "load-routed-skill",
            ],
        )

    def test_only_explicit_primary_source_research_counts_as_authoritative(self):
        generic = guard._research_preflight_events(
            "WebSearch",
            {"query": "unofficial blog, no standard documentation"},
            {"results": [{"url": "https://example.com/opinion"}]},
        )
        primary = guard._research_preflight_events(
            "WebSearch",
            {"query": "official GitHub hooks documentation"},
            {"results": [{"url": "https://docs.github.com/en/actions"}]},
        )
        self.assertNotIn("authoritative-online-research", generic)
        self.assertIn("authoritative-online-research", primary)
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "web__run",
                {"open": [{"ref_id": "https://example.com/opinion"}]},
                {"content": [{"type": "text", "text": "opened"}]},
            ),
        )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "web__run",
                {
                    "search_query": [
                        {"q": "discuss https://docs.python.org on example.com"}
                    ]
                },
                {"results": [{"url": "https://example.com/opinion"}]},
            ),
        )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "functions.exec",
                "text('https://docs.python.org/3/library/json.html')",
                {"output": "https://docs.python.org/3/library/json.html"},
            ),
        )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "exec_command",
                {
                    "cmd": "curl.exe https://example.com -H "
                    "'Referer: https://docs.python.org/3/library/json.html'"
                },
            ),
        )
        self.assertNotIn(
            "query-native-memory",
            guard._research_preflight_events(
                "exec_command", {"cmd": "echo memory search guard"}
            ),
        )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "functions.exec",
                'await tools.exec_command({cmd:"curl.exe '
                'https://docs.python.org/3/library/json.html"}); '
                'await tools.exec_command({cmd:"echo complete"});',
                {"exit_code": 0},
            ),
        )
        for command in (
            "curl.exe https://example.com -o https://docs.python.org/result",
            "curl.exe https://example.com --proxy https://docs.python.org/proxy",
            "curl.exe --url https://docs.python.org/3/library/json.html || true",
        ):
            with self.subTest(command=command):
                self.assertNotIn(
                    "authoritative-online-research",
                    guard._research_preflight_events(
                        "exec_command", {"cmd": command}
                    ),
                )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "functions.exec",
                "const result = await tools.web__run({open: [{ref_id: "
                "'https://example.com/opinion'}]}); text(result);",
                {"output": "https://example.com/opinion"},
            ),
        )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "functions.exec",
                "const result = await tools.web__run({open: [{ref_id: "
                "'https://example.com/opinion'}]}); "
                "text('https://docs.python.org/3/library/json.html');",
                {"output": "https://docs.python.org/3/library/json.html"},
            ),
        )

    def test_shell_file_targets_share_main_and_outside_scoping(self):
        inside = {"cwd": ".", "tool_input": {"command": "Set-Content scripts/x.py x"}}
        outside_path = os.path.join(tempfile.gettempdir(), "research-scratch.txt")
        outside = {
            "cwd": ".",
            "tool_input": {"command": f'Set-Content "{outside_path}" x'},
        }
        root = os.path.realpath(".")
        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._repository_root", return_value=root):
                self.assertIsNotNone(guard.check_r19_fresh_base(inside, "PowerShell"))
                self.assertIsNone(guard.check_r19_fresh_base(outside, "PowerShell"))
        with patch("scripts.agents.guard.ledger_events", return_value=[]):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(outside, "PowerShell")
            )

    def test_issue_backed_plan_comment_completes_its_own_receipt_after_success(self):
        command = (
            "gh issue comment 4666 --body 'Implementation plan and executable specification'"
        )
        payload = {"cwd": ".", "tool_input": {"command": command}}
        first_seven = list(guard.RESEARCH_PREFLIGHT_EVENTS[:-1])
        with patch("scripts.agents.guard.ledger_events", return_value=first_seven):
            self.assertIsNone(
                guard.check_r25_research_before_implementation(payload, "PowerShell")
            )
        observed = []
        with patch(
            "scripts.agents.guard.ledger_record",
            side_effect=lambda _payload, event: observed.append(event),
        ):
            guard.run_posttooluse(
                {**payload, "tool_name": "PowerShell", "session_id": "issue-plan"}
            )
        self.assertIn("record-plan", observed)

        unrelated = {"cwd": ".", "tool_input": {"command": "gh issue comment 4666 --body status"}}
        with patch("scripts.agents.guard.ledger_events", return_value=first_seven):
            self.assertIsNotNone(
                guard.check_r25_research_before_implementation(unrelated, "PowerShell")
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


class SessionStartRetrievalIsolationTest(unittest.TestCase):
    """#5304: SessionStart emits locators and never invokes optional retrieval stores."""

    def test_session_start_does_not_start_memory_mempalace_or_graphify(self):
        with patch(
            "scripts.agents.guard._worktree_report",
            return_value={"worktrees": [], "advisories": []},
        ), patch("scripts.agents.guard._sync_advisory", return_value=None), patch(
            "scripts.agents.guard.subprocess.run"
        ) as run:
            stream = io.StringIO()
            with redirect_stdout(stream):
                self.assertEqual(guard.run_session_start({"cwd": "."}), 0)

        run.assert_not_called()
        rendered = stream.getvalue()
        context = json.loads(rendered)["hookSpecificOutput"]["additionalContext"]
        self.assertLessEqual(len(rendered.encode("utf-8")), 4096)
        self.assertNotIn("memory load", context.casefold())
        self.assertNotIn("wake-up", context.casefold())
        self.assertIn("untrusted evidence", context)

    def test_cold_guard_import_defers_learning_and_repository_context(self):
        completed = subprocess.run(
            [
                sys.executable,
                "-c",
                "import sys; import scripts.agents.guard; "
                "print('scripts.agents.learning_session' in sys.modules); "
                "print('scripts.agents.repository_context' in sys.modules)",
            ],
            cwd=Path(__file__).resolve().parents[2],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual(["False", "False"], completed.stdout.splitlines())



if __name__ == "__main__":
    unittest.main()


class SessionLedgerTest(unittest.TestCase):
    """#4541: session-scoped facts used by retained lifecycle rules.

    `guard.py` is stateless -- every PreToolUse call is a fresh process that
    sees only the current tool call. Delivery, reflection, and retrieval rules
    need a bounded record of what the current session already observed.

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
                self.assertTrue(guard.ledger_record(payload, "observation"))
                self.assertTrue(guard.ledger_record(payload, "push"))
                self.assertEqual(guard.ledger_events(payload), ["observation", "push"])

    def test_one_session_cannot_see_another_sessions_events(self):
        """A shared ledger would let a sibling agent satisfy this agent's gate.

        Concurrent agents each own a worktree and run their own hooks. If the
        ledger were keyed by repository rather than by session, one session's
        delivery observations could be mistaken for a different session's.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.ledger_record(self.payload("session-a", directory), "observation")
                self.assertEqual(guard.ledger_events(self.payload("session-b", directory)), [])

    def test_the_ledger_fails_open_when_it_cannot_be_written(self):
        """A hook that cannot record must never block. It runs before every call."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-a", directory)
                with patch("scripts.agents.guard._ledger_path", return_value=None):
                    self.assertFalse(guard.ledger_record(payload, "observation"))
                    self.assertEqual(guard.ledger_events(payload), [])

    def test_a_corrupt_ledger_reads_as_empty_rather_than_raising(self):
        """Worst case is re-observing an event, never a session that cannot start."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-a", directory)
                guard.ledger_record(payload, "observation")
                path = guard._ledger_path(payload)
                self.assertIsNotNone(path)
                with open(path, "wb") as handle:
                    handle.write(b"\x00\xff not json at all")
                self.assertEqual(guard.ledger_events(payload), [])




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


class HookJsonProtocolTest(unittest.TestCase):
    """#4993: the host wire contract is one JSON object per hook invocation."""

    @staticmethod
    def invoke(payload: object) -> str:
        raw = payload if isinstance(payload, str) else json.dumps(payload)
        output = io.StringIO()
        with patch("sys.stdin", io.StringIO(raw)):
            with redirect_stdout(output):
                try:
                    guard.main([])
                except Exception as error:
                    return f"EXCEPTION:{type(error).__name__}\n"
        return output.getvalue()

    def test_functions_exec_accepts_object_wrapped_freeform_input(self):
        output = self.invoke(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "functions.exec",
                "tool_input": {
                    "input": (
                        'const result = await tools.exec_command({cmd:"git status"}); '
                        "text(result.output);"
                    )
                },
                "cwd": str(Path(__file__).resolve().parents[2]),
                "session_id": "object-wrapped-functions-exec",
            }
        )

        self.assertNotIn("Lifecycle hook produced invalid JSON output", output)
        self.assertIsInstance(json.loads(output), dict)

    def test_repository_hook_dispatch_reaches_the_portable_kernel(self):
        payload = {
            "hook_event_name": "SessionStart",
            "session_id": "repository-kernel-reachability",
        }
        with patch.object(
            guard,
            "_evaluate_kernel_event",
            wraps=guard._evaluate_kernel_event,
        ) as evaluate_kernel:
            output = self.invoke(payload)

        evaluate_kernel.assert_called_once()
        self.assertIsInstance(json.loads(output), dict)

    def test_functions_exec_cmd_wrapped_shell_is_inspected(self):
        output = self.invoke(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "functions.exec",
                "tool_input": {"cmd": "git reset --hard HEAD~1"},
                "cwd": str(Path(__file__).resolve().parents[2]),
                "session_id": "cmd-wrapped-functions-exec",
            }
        )

        decision = json.loads(output)["hookSpecificOutput"]
        self.assertEqual("deny", decision["permissionDecision"])
        self.assertNotIn(
            "Lifecycle hook produced invalid JSON output",
            decision["permissionDecisionReason"],
        )

    def test_main_emits_one_json_object_for_every_lifecycle_event(self):
        callbacks = {
            "SessionStart": "run_session_start",
            "UserPromptSubmit": "run_user_prompt_submit",
            "Stop": "run_stop",
            "SubagentStop": "run_stop",
            "PreToolUse": "run_pretooluse",
            "PostToolUse": "run_posttooluse",
            "PreCompact": "run_observational_event",
            "SessionEnd": "run_observational_event",
        }
        observed = []
        for event, callback in callbacks.items():
            with patch.object(guard, callback, return_value=0):
                observed.append(
                    (
                        event,
                        self.invoke(
                            {
                                "hook_event_name": event,
                                "session_id": f"protocol-{event}",
                            }
                        ),
                    )
                )
        decisions = (
            (
                "PreToolUse",
                "run_pretooluse",
                {"hookSpecificOutput": {"permissionDecision": "deny"}},
            ),
            (
                "PreToolUse",
                "run_pretooluse",
                {"hookSpecificOutput": {"permissionDecision": "allow"}},
            ),
            ("Stop", "run_stop", {"decision": "block", "reason": "unfinished"}),
            (
                "SessionStart",
                "run_session_start",
                {"hookSpecificOutput": {"additionalContext": "ready"}},
            ),
        )
        for event, callback, decision in decisions:
            with patch.object(
                guard, callback, side_effect=lambda *_args, value=decision: print(json.dumps(value))
            ):
                observed.append(
                    (
                        event,
                        self.invoke(
                            {
                                "hook_event_name": event,
                                "session_id": f"decision-{event}",
                            }
                        ),
                    )
                )
        expected = [(event, "{}\n") for event in callbacks]
        expected.extend(
            (event, json.dumps(decision, separators=(",", ":")) + "\n")
            for event, _callback, decision in decisions
        )
        self.assertEqual(observed, expected)

    def test_main_contains_invalid_callback_stdout_with_a_deny(self):
        observed = []
        for rendered in ("junk", "{}\n{}", "[]", '{"value":NaN}', '{"value":Infinity}'):
            with patch.object(guard, "run_pretooluse", side_effect=lambda: None) as callback:
                callback.side_effect = lambda *_args, value=rendered: print(value)
                observed.append(
                    self.invoke(
                        {
                            "hook_event_name": "PreToolUse",
                            "tool_name": "Write",
                            "session_id": "invalid-callback",
                        }
                    )
                )
        with patch.object(guard, "run_pretooluse", side_effect=RuntimeError("crash")):
            observed.append(
                self.invoke(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "Write",
                        "session_id": "crashing-callback",
                    }
                )
            )
        expected = json.dumps(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": "Lifecycle hook produced invalid JSON output.",
                }
            },
            separators=(",", ":"),
        ) + "\n"
        stop_outputs = []
        for event in ("Stop", "SubagentStop"):
            with patch.object(guard, "run_stop", side_effect=lambda *_args: print("junk")):
                stop_outputs.append(self.invoke({"hook_event_name": event}))
        observational_outputs = []
        for event, callback in (
            ("SessionStart", "run_session_start"),
            ("PostToolUse", "run_posttooluse"),
            ("UserPromptSubmit", "run_user_prompt_submit"),
        ):
            with patch.object(guard, callback, side_effect=lambda *_args: print("junk")):
                observational_outputs.append(self.invoke({"hook_event_name": event}))
        with patch.object(guard, "run_pretooluse", side_effect=lambda *_args: print("junk")):
            grok = self.invoke(
                {
                    "hookEventName": "PreToolUse",
                    "toolName": "Write",
                    "sessionId": "grok-invalid-callback",
                }
            )
        block = (
            json.dumps(
                {"decision": "block", "reason": "Lifecycle hook produced invalid JSON output."},
                separators=(",", ":"),
            )
            + "\n"
        )
        grok_deny = (
            json.dumps(
                {"decision": "deny", "reason": "Lifecycle hook produced invalid JSON output."},
                separators=(",", ":"),
            )
            + "\n"
        )
        self.assertEqual(
            observed + stop_outputs + observational_outputs + [grok],
            [expected] * 6 + [block] * 2 + ["{}\n"] * 3 + [grok_deny],
        )

    def test_main_contains_system_exit_with_a_deny(self):
        with patch.object(guard, "run_pretooluse", side_effect=SystemExit(7)):
            output = self.invoke(
                {
                    "hook_event_name": "PreToolUse",
                    "tool_name": "Write",
                    "session_id": "system-exit-callback",
                }
            )

        decision = json.loads(output)["hookSpecificOutput"]
        self.assertEqual("deny", decision["permissionDecision"])
        self.assertEqual(
            "Lifecycle hook produced invalid JSON output.",
            decision["permissionDecisionReason"],
        )

    def test_main_frames_empty_malformed_nonobject_and_unknown_input(self):
        recursive = self.invoke("[" * 20_000 + "0" + "]" * 20_000)
        observed = [
            self.invoke(""),
            self.invoke("{"),
            self.invoke("[]"),
            self.invoke({"hook_event_name": "Unknown"}),
            self.invoke({"hook_event_name": {"invalid": True}}),
            recursive,
        ]
        self.assertEqual(observed, ["{}\n"] * 6)


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


class MergeCommitBranchReachabilityTest(unittest.TestCase):
    """R13/R18: merge commits make remote reachability the deletion-safety invariant."""

    def test_unrecoverable_count_is_the_remote_reachability_count(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=2):
            with patch(
                "scripts.agents.guard._content_exists_on_default_branch",
                return_value=True,
                create=True,
            ) as removed_bypass:
                self.assertEqual(guard._unrecoverable_commit_count("feature"), 2)
        removed_bypass.assert_not_called()

    def test_unanswerable_reachability_stays_unanswerable(self):
        with patch("scripts.agents.guard._unpushed_commit_count", return_value=None):
            self.assertIsNone(guard._unrecoverable_commit_count("feature"))
        self.assertNotIn(
            "_content_exists_on_default_branch",
            inspect.getsource(guard._unrecoverable_commit_count),
        )

    def test_squash_content_bypass_is_removed(self):
        self.assertFalse(hasattr(guard, "_content_exists_on_default_branch"))


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


class PullRequestAuditBeforeArmingGateTest(unittest.TestCase):
    """R28: auto-merge requires a fresh complete exact-head feedback audit."""
    def setUp(self):
        self.assertTrue(
            callable(getattr(guard, "check_r28_pr_audit_before_arming", None)),
            "auto-merge audit gate is missing",
        )

    def test_auto_merge_requires_a_clean_head_bound_audit_receipt(self):
        rule = getattr(guard, "check_r28_pr_audit_before_arming", None)
        self.assertIsNotNone(rule, "auto-merge audit gate is missing")
        command = "gh pr merge 17 --auto --merge"
        with mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=False):
            reason = rule(command, "PowerShell", {"cwd": "."})
        self.assertIn("pr-audit", reason)
        self.assertIn("#17", reason)

        with mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=True):
            self.assertIsNone(
                rule(command, "PowerShell", {"cwd": "."})
            )

    def test_non_merge_commands_and_non_auto_merge_do_not_require_receipt(self):
        rule = getattr(guard, "check_r28_pr_audit_before_arming", None)
        self.assertIsNotNone(rule, "auto-merge audit gate is missing")
        for command in ("gh pr view 17", "gh pr merge 17 --merge"):
            self.assertIsNone(rule(command, "PowerShell", {}))

    def test_ready_and_cross_repository_merge_require_matching_audit(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=False):
            self.assertIn("audit", guard.check_r28_pr_audit_before_arming("gh pr ready 17", "PowerShell", {}))
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=True):
            self.assertIn("repository", guard.check_r28_pr_audit_before_arming("gh pr merge 17 --repo other/project --auto --merge", "PowerShell", {}))

    def test_powershell_backslash_quote_cannot_hide_ready_or_merge(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        command = r'py -3 -c "probe=\"; gh pr ready 17 --repo consumer/project; #\""'
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=True):
            reason = guard.check_r28_pr_audit_before_arming(command, "PowerShell", {})

        self.assertIsNotNone(reason)
        self.assertIn("backslash-escaped", reason)


class MergeAuthorityBeforeArmingGateTest(unittest.TestCase):
    """R30: no PR merge mutation without recorded exact-head user authority."""

    def setUp(self):
        for name in (
            "check_r30_merge_authority_before_arming",
            "run_user_prompt_submit",
            "_trusted_executable_token",
        ):
            self.assertTrue(callable(getattr(guard, name, None)), f"{name} is missing")

    def test_merge_requires_exact_head_authority_event(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        command = "gh pr merge 17 --auto --merge"
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "ledger_events", return_value=[]):
            self.assertIn("authority", guard.check_r30_merge_authority_before_arming(command, "PowerShell", {}))
        event = f"merge-authority:{identity[0]}:17:{identity[2]}:digest"
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "ledger_events", return_value=[event]):
            self.assertIsNone(guard.check_r30_merge_authority_before_arming(command, "PowerShell", {}))

        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "ledger_events", return_value=[event]):
            self.assertIn("repository", guard.check_r30_merge_authority_before_arming("gh pr merge 17 --repo other/project --auto --merge", "PowerShell", {}))

    def test_gh_repo_environment_cannot_redirect_a_merge(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        events = [f"merge-authority:{identity[0]}:17:{identity[2]}:digest"]
        with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "ledger_events", return_value=events), mock.patch.object(guard, "_validated_pr_audit_receipt", return_value=True):
            for command in ("GH_REPO=other/project gh pr merge 17 --auto --merge", "$env:GH_REPO='other/project'; gh pr merge 17 --auto --merge"):
                self.assertIn("repository", guard.check_r28_pr_audit_before_arming(command, "PowerShell", {}))
                self.assertIn("repository", guard.check_r30_merge_authority_before_arming(command, "PowerShell", {}))

    def test_user_prompt_hook_persists_allow_deny_and_neutral_precedence(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary) / "authority.json"
            with mock.patch.object(guard, "_checkpoint_identity", return_value=identity), mock.patch.object(guard, "_hook_working_directory", return_value=temporary), mock.patch.object(guard, "_git_output", return_value=str(target)):
                for prompt, decision in (("merge this PR", "allow"), ("do not merge this PR", "deny")):
                    guard.run_user_prompt_submit({"prompt": prompt})
                    self.assertEqual(decision, json.loads(target.read_text(encoding="utf-8"))["decision"])
                for prompt in ("fix the test", "fix the merge conflicts"):
                    guard.run_user_prompt_submit({"prompt": prompt})
                    self.assertEqual("deny", json.loads(target.read_text(encoding="utf-8"))["decision"])
                guard.run_user_prompt_submit({"prompt": "no merge until tomorrow"})
                self.assertEqual("deny", json.loads(target.read_text(encoding="utf-8"))["decision"])
                for prompt in ("do not arm auto-merge this PR", "please do not enable auto-merge for the PR", "should I merge this PR?"):
                    guard.run_user_prompt_submit({"prompt": prompt})
                    self.assertEqual("deny", json.loads(target.read_text(encoding="utf-8"))["decision"])

    def test_executable_token_must_match_the_path_resolved_interpreter(self):
        with mock.patch.object(guard.shutil, "which", side_effect=lambda value: "C:/Windows/py.exe" if Path(value).name.lower() == "py.exe" else None):
            self.assertTrue(guard._trusted_executable_token("py.exe"))
            self.assertFalse(guard._trusted_executable_token("C:/evil/py.exe"))


class MergeModeGateTest(unittest.TestCase):
    """R15 preserves merge-commit ancestry without a review-counting gate."""

    def test_merge_commit_arming_is_allowed(self):
        for command in (
            "gh pr merge 4539 --auto --merge",
            "gh --repo ShaftHQ/SHAFT_ENGINE pr merge 4539 --merge",
            "gh pr merge 4539 --auto=false --merge",
        ):
            with self.subTest(command=command):
                self.assertIsNone(guard.check_r15_merge_mode(command, "Bash"))

    def test_squash_and_rebase_modes_are_rejected(self):
        for command in (
            "gh pr merge 4539 --squash",
            "gh pr merge 4539 --rebase",
            "gh pr merge 4539 -s",
            "gh pr merge 4539 -r",
            "gh pr merge 4539 --squash=true",
        ):
            with self.subTest(command=command):
                self.assertIn("merge commits", guard.check_r15_merge_mode(command, "Bash"))

    def test_false_merge_mode_flags_do_not_trigger(self):
        for command in (
            "gh pr merge 4539 --squash=false --merge",
            "gh pr merge 4539 --rebase=0 --merge",
        ):
            with self.subTest(command=command):
                self.assertIsNone(guard.check_r15_merge_mode(command, "PowerShell"))

    def test_non_merge_commands_are_ignored(self):
        self.assertIsNone(guard.check_r15_merge_mode("gh pr view 4539", "Bash"))


class LearningSessionStopGateTest(unittest.TestCase):
    """R16: exactly one learning session after delivery, before final report.

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

    def test_commit_or_guard_refusal_never_starts_learning_before_delivery(self):
        with patch("scripts.agents.guard.ledger_events", return_value=["commit"]):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))
        with patch("scripts.agents.guard.ledger_events", return_value=["guard-block"]):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

    def test_fresh_delivery_requires_one_terminal_learning_session(self):
        events = ["commit", 'delivery:{"repository":"ShaftHQ/SHAFT_ENGINE"}']
        with patch("scripts.agents.guard.ledger_events", return_value=events), patch(
            "scripts.agents.guard.check_r29_delivery_complete", return_value=None
        ):
            reason = guard.check_r16_learning_session({"session_id": "s"})
        self.assertIsNotNone(reason)
        self.assertIn("Learning Session", reason)

    def test_one_completion_receipt_permanently_satisfies_terminal_gate(self):
        events = [
            "commit",
            'delivery:{"repository":"ShaftHQ/SHAFT_ENGINE"}',
            "learning-session-complete:" + "a" * 64,
        ]
        with patch("scripts.agents.guard.ledger_events", return_value=events), patch(
            "scripts.agents.guard.check_r29_delivery_complete", return_value=None
        ):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

    def test_a_recorded_memory_write_satisfies_it(self):
        with patch(
            "scripts.agents.guard.ledger_events", return_value=["commit", "memory-write"]
        ):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=["commit", "learning-none:store_degraded"],
        ):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

    def test_a_created_issue_satisfies_it(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=["commit", "issue-created:4995"],
        ):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

    def test_a_successful_existing_issue_reference_satisfies_it(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=["commit", "learning-issue:4995"],
        ):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

    def test_a_session_that_changed_nothing_is_never_interrupted(self):
        """A read-only session owes no learning; asking would train the block away."""
        with patch("scripts.agents.guard.ledger_events", return_value=["read-observation"]):
            self.assertIsNone(guard.check_r16_learning_session({"session_id": "s"}))

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

    def test_the_second_stop_attempt_skips_terminal_reflection_too(self):
        output = io.StringIO()
        with patch(
            "scripts.agents.guard._terminal_reflection_reason",
            side_effect=AssertionError("recursive Stop must short-circuit first"),
        ):
            with redirect_stdout(output):
                self.assertEqual(
                    guard.run_stop(
                        {"cwd": ".", "session_id": "s", "stop_hook_active": True}
                    ),
                    0,
                )
        self.assertEqual(output.getvalue(), "")


class DelegateStopHookTest(unittest.TestCase):
    """R16: a delegate stop cannot start the root terminal Learning Session."""

    def setUp(self):
        isolate_stop_rules(self, except_for=("check_r16_learning_session",))

    def test_subagent_stop_registration_does_not_start_terminal_learning(self):
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
        self.assertNotIn("Learning", output.getvalue())


class LearningWriteObservationTest(unittest.TestCase):
    """R16 observes successful durable learning routes, never attempts or housekeeping."""

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
                self.assertEqual(guard.run_posttooluse(payload), 0)
                return guard.ledger_events(payload)

    def test_mcp_and_cli_learning_writes_reach_the_ledger(self):
        root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        for name in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(host=name):
                with open(os.path.join(root, name), encoding="utf-8") as handle:
                    matcher = json.load(handle)["hooks"]["PreToolUse"][0]["matcher"]
                self.assertIn("mcp__mempalace__", matcher)

        for tool_name, command in (
            ("mcp__mempalace__mempalace_add_drawer", ""),
            ("Bash", "memory remember --stdin"),
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

    def test_a_denied_command_does_not_create_a_learning_trigger(self):
        self.assertNotIn("guard-block", self.events_after("Bash", "git stash pop"))

    def test_standalone_issue_creation_is_credited_only_after_a_successful_result(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "action-issue",
                "cwd": directory,
                "tool_name": "PowerShell",
                "tool_input": {"command": "gh issue create --title fix --body receipt"},
                "tool_response": {
                    "exit_code": 0,
                    "stdout": "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731",
                },
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.run_pretooluse(payload)
                self.assertFalse(
                    any(event.startswith("issue-created:") for event in guard.ledger_events(payload))
                )
                guard.run_posttooluse(payload)
                self.assertIn("issue-created:4731", guard.ledger_events(payload))

                failed = {**payload, "session_id": "failed-action-issue"}
                failed["tool_response"] = {"exit_code": 1, "stderr": "not created"}
                guard.run_posttooluse(failed)
                self.assertFalse(
                    any(event.startswith("issue-created:") for event in guard.ledger_events(failed))
                )

                url = "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"
                rejected = (
                    ("stderr-url", "gh issue create --title fix --body receipt", {"exit_code": 0, "stderr": f"request failed; prior {url}"}),
                    ("cancelled", "gh issue create --title fix --body receipt", {"status": "cancelled", "stdout": url}),
                    ("denied", "gh issue create --title fix --body receipt", {"status": "denied", "stdout": url}),
                    ("timed-out", "gh issue create --title fix --body receipt", {"status": "timed_out", "stdout": url}),
                    ("structured-error", "gh issue create --title fix --body receipt", {"status": "success", "stdout": url, "error": {"message": "failed"}}),
                    ("structured-stderr", "gh issue create --title fix --body receipt", {"status": "success", "stdout": url, "stderr": ["failed"]}),
                    ("wrapper", f"gh issue create --title fix --body receipt; echo {url}", {"exit_code": 0, "stdout": url}),
                    ("cross-repo", "gh -R ShaftHQ/other issue create --title fix --body receipt", {"exit_code": 0, "stdout": url}),
                )
                for session_id, command, response in rejected:
                    with self.subTest(session_id=session_id):
                        candidate = {
                            **payload,
                            "session_id": session_id,
                            "tool_input": {"command": command},
                            "tool_response": response,
                        }
                        guard.run_posttooluse(candidate)
                        self.assertFalse(
                            any(
                                event.startswith("issue-created:")
                                for event in guard.ledger_events(candidate)
                            )
                        )

    @patch(
        "scripts.agents.guard._git_output",
        return_value="https://github.com/ShaftHQ/SHAFT_ENGINE.git",
    )
    def test_existing_issue_reference_is_bound_to_number_and_success(self, _git_output):
        repository = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        with tempfile.TemporaryDirectory() as directory, patch.dict(
            guard.os.environ, {"TMPDIR": directory, "TEMP": directory}
        ):
            payload = {
                "session_id": "existing-learning-issue",
                "cwd": repository,
                "tool_name": "PowerShell",
                "tool_input": {
                    "command": "gh issue comment 4995 --repo ShaftHQ/SHAFT_ENGINE --body evidence"
                },
                "tool_response": {"exit_code": 0},
            }
            guard.run_posttooluse(payload)
            self.assertIn("learning-issue:4995", guard.ledger_events(payload))

            scoped = {
                **payload,
                "session_id": "same-repo-environment",
                "tool_input": {
                    "command": "GH_REPO=ShaftHQ/SHAFT_ENGINE gh issue edit 4995 --add-label ready"
                },
            }
            guard.run_posttooluse(scoped)
            self.assertIn("learning-issue:4995", guard.ledger_events(scoped))

            quoted = {
                **payload,
                "session_id": "quoted-punctuation",
                "tool_input": {
                    "command": 'gh issue comment 4995 --body "evidence & more; still | one command"'
                },
            }
            guard.run_posttooluse(quoted)
            self.assertIn("learning-issue:4995", guard.ledger_events(quoted))

            for session_id, command in (
                ("bash-escaped-quotes", 'gh issue comment 4995 --body "evidence \\"quoted; still one\\""'),
                ("powershell-escaped-quotes", 'gh issue comment 4995 --body "evidence `"quoted; still one`""'),
            ):
                escaped = {
                    **payload,
                    "session_id": session_id,
                    "tool_input": {"command": command},
                }
                guard.run_posttooluse(escaped)
                self.assertIn("learning-issue:4995", guard.ledger_events(escaped))

            ambient_spoof = {
                **payload,
                "session_id": "ambient-repository-spoof",
                "tool_input": {
                    "command": "gh issue comment 4995 --body GH_REPO=ShaftHQ/SHAFT_ENGINE"
                },
            }
            with patch.dict(guard.os.environ, {"GH_REPO": "evil/other"}):
                guard.run_posttooluse(ambient_spoof)
            self.assertFalse(
                any(
                    event.startswith("learning-issue:")
                    for event in guard.ledger_events(ambient_spoof)
                )
            )

            for session_id, command, response in (
                ("failed-reference", payload["tool_input"]["command"], {"exit_code": 1}),
                ("generic-update", "gh issue edit --add-label ready", {"exit_code": 0}),
                ("pr-reference", "gh pr comment 4995 --body evidence", {"exit_code": 0}),
                ("other-repo", "gh issue comment 4995 --repo ShaftHQ/other --body evidence", {"exit_code": 0}),
                ("help", "gh issue comment 4995 --help", {"exit_code": 0}),
                ("dry-run", "gh issue edit 4995 --dry-run", {"exit_code": 0}),
                ("compound", "gh issue comment 4995 --body evidence; exit 0", {"exit_code": 0}),
                ("environment-other-repo", "GH_REPO=ShaftHQ/other gh issue comment 4995 --body evidence", {"exit_code": 0}),
                ("same-name-other-owner", "gh issue comment 4995 --repo evil/SHAFT_ENGINE --body evidence", {"exit_code": 0}),
                ("environment-same-name-other-owner", "GH_REPO=evil/SHAFT_ENGINE gh issue edit 4995 --add-label ready", {"exit_code": 0}),
                ("other-host", "gh issue comment 4995 --repo evil.example/ShaftHQ/SHAFT_ENGINE --body evidence", {"exit_code": 0}),
                ("environment-other-host", "GH_REPO=evil.example/ShaftHQ/SHAFT_ENGINE gh issue edit 4995 --add-label ready", {"exit_code": 0}),
                ("web", "gh issue comment 4995 --web", {"exit_code": 0}),
                ("delete", "gh issue comment 4995 --delete-last --yes", {"exit_code": 0}),
                ("help-value", "gh issue comment 4995 --help=true", {"exit_code": 0}),
                ("web-value", "gh issue comment 4995 --web=true", {"exit_code": 0}),
                ("delete-value", "gh issue comment 4995 --delete-last=true", {"exit_code": 0}),
                ("multiple-targets", "gh issue edit 4995 4996 --add-label ready", {"exit_code": 0}),
            ):
                with self.subTest(session_id=session_id):
                    candidate = {
                        **payload,
                        "session_id": session_id,
                        "tool_input": {"command": command},
                        "tool_response": response,
                    }
                    guard.run_posttooluse(candidate)
                    self.assertFalse(
                        any(
                            event.startswith("learning-issue:")
                            for event in guard.ledger_events(candidate)
                        )
                    )

            for session_id, response in (
                ("missing-result", None),
                ("empty-result", {}),
                ("cancelled", {"status": "cancelled"}),
                ("denied", {"status": "denied"}),
                ("timed-out", {"status": "timed_out"}),
                ("structured-error", {"status": "success", "error": {"message": "failed"}}),
                ("stderr", {"exit_code": 0, "stderr": "request failed"}),
            ):
                with self.subTest(session_id=session_id):
                    candidate = {
                        **payload,
                        "session_id": session_id,
                        "tool_response": response,
                    }
                    guard.run_posttooluse(candidate)
                    self.assertFalse(
                        any(
                            event.startswith("learning-issue:")
                            for event in guard.ledger_events(candidate)
                        )
                    )

    def test_housekeeping_mutations_do_not_masquerade_as_learning(self):
        for tool_name, command, tool_input in (
            ("mcp__mempalace__mempalace_mine", "", {}),
            ("PowerShell", "mempalace sweep", {}),
            ("mcp__mempalace__mempalace_delete_by_source", "", {"dry_run": False}),
            ("mcp__mempalace__mempalace_sync", "", {"apply": True}),
        ):
            with self.subTest(tool_name=tool_name):
                self.assertNotIn(
                    "memory-write", self.events_after(tool_name, command, tool_input)
                )


class LearningNoneEscapeTest(unittest.TestCase):
    """R16 accepts only the controller's enumerated no-learning attestation."""

    def events_after(self, command: str) -> list[str]:
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = {
                    "session_id": "learning-none-session",
                    "cwd": directory,
                    "tool_name": "Bash",
                    "tool_input": {"command": command},
                }
                if "--reason-code no_new_evidence" in command:
                    learning_session.attest_no_learning(
                        state, "learning-none-session", "no_new_evidence"
                    )
                    learning_session.record_completion(
                        state,
                        "learning-none-session",
                        "learning-none-op",
                        "attest-none",
                    )
                with patch.object(learning_session, "default_state_dir", return_value=state):
                    self.assertEqual(guard.run_pretooluse(payload), 0)
                    self.assertEqual(guard.run_posttooluse(payload), 0)
                return guard.ledger_events(payload)

    def test_structured_learning_none_reason_is_recorded(self):
        events = self.events_after(
            f'py -3 "{LEARNING_CONTROLLER}" attest-none '
            "--session-id learning-none-session --operation-id learning-none-op "
            "--reason-code no_new_evidence"
        )
        self.assertIn("learning-none:no_new_evidence", events)

    def test_empty_or_placeholder_reason_does_not_count(self):
        for reason in ('""', '"n/a"'):
            with self.subTest(reason=reason):
                events = self.events_after(
                    f'py -3 "{LEARNING_CONTROLLER}" attest-none '
                    f"--session-id learning-none-session --operation-id learning-none-op "
                    f"--reason-code {reason}"
                )
                self.assertFalse(any(event.startswith("learning-none:") for event in events))


class DeliveryCompleteStopGateTest(unittest.TestCase):
    """R29: Stop requires live mergedAt and scoped-cleanup delivery proof."""
    def setUp(self):
        for name in ("check_r29_delivery_complete", "_checkpoint_json_event"):
            self.assertTrue(callable(getattr(guard, name, None)), f"{name} is missing")

    def test_commit_cannot_complete_without_fresh_live_delivery_receipt(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        with mock.patch.object(guard, "ledger_events", return_value=["commit"]), mock.patch.object(
            guard, "_checkpoint_identity", return_value=identity
        ):
            reason = guard.check_r29_delivery_complete({"session_id": "s"})
        self.assertIn("delivery-status", reason)
        checkpoint = guard._checkpoint_json_event("checkpoint", *identity)
        event = guard._checkpoint_json_event("delivery", identity[0], identity[1], identity[2], observedAt=int(time.time()), taskHeads=[{"repository": identity[0], "head": identity[2]}])
        with mock.patch.object(guard, "ledger_events", return_value=["commit", checkpoint, event]), mock.patch.object(
            guard, "_checkpoint_identity", return_value=identity
        ):
            self.assertIsNone(guard.check_r29_delivery_complete({"session_id": "s"}))

    def test_legacy_commits_require_one_manifest_bound_identity_proof_each(self):
        identity = ("consumer/project", "ChaosEngine/task", "a" * 40)
        proof = {"repository": identity[0], "head": identity[2]}
        event = guard._checkpoint_json_event(
            "delivery",
            identity[0],
            identity[1],
            identity[2],
            observedAt=int(time.time()),
            taskHeads=[proof],
            legacyCommitProofs=[proof, proof],
        )
        with mock.patch.object(
            guard, "ledger_events", return_value=["commit", "commit", event]
        ):
            self.assertIsNone(guard.check_r29_delivery_complete({"session_id": "legacy"}))

        for malformed in (
            [proof],
            [proof, {"repository": "other/project", "head": "b" * 40}],
        ):
            bad_event = guard._checkpoint_json_event(
                "delivery",
                identity[0],
                identity[1],
                identity[2],
                observedAt=int(time.time()),
                taskHeads=[proof],
                legacyCommitProofs=malformed,
            )
            with self.subTest(proofs=malformed), mock.patch.object(
                guard, "ledger_events", return_value=["commit", "commit", bad_event]
            ):
                self.assertIsNotNone(
                    guard.check_r29_delivery_complete({"session_id": "legacy"})
                )

    def test_cleanup_receipt_can_be_recorded_from_primary_after_task_worktree_removal(self):
        task = ("consumer/project", "ChaosEngine/task", "a" * 40)
        primary = ("consumer/project", "main", "b" * 40)
        checkpoint = guard._checkpoint_json_event("checkpoint", *task)
        event = guard._checkpoint_json_event("delivery", primary[0], "main", primary[2], observedAt=int(time.time()), taskHeads=[{"repository": task[0], "head": task[2]}])
        with mock.patch.object(guard, "ledger_events", return_value=["commit", checkpoint, event]), mock.patch.object(
            guard, "_checkpoint_identity", return_value=task
        ):
            self.assertIsNone(guard.check_r29_delivery_complete({"session_id": "s"}))

    def test_degraded_cleanup_receipt_records_successful_delivery(self):
        identity = ("consumer/project", "main", "b" * 40)
        root = Path(guard.__file__).resolve().parents[2]
        runtime = root / "scripts/agents/act_as_mohab_cli.py"
        with tempfile.TemporaryDirectory() as temporary:
            receipt_path = Path(temporary) / "delivery.json"
            receipt_path.write_text(json.dumps({
                "schemaVersion": 1,
                "kind": "delivery-status",
                "repository": identity[0],
                "headOid": identity[2],
                "decision": "allow",
                "deliveryDecision": "allow",
                "cleanupDecision": "degraded",
                "reasons": [],
                "mergedCount": 1,
                "observedAt": datetime.now(UTC).isoformat(),
                "pullRequests": [{
                    "repository": identity[0],
                    "number": 7,
                    "headOid": "a" * 40,
                    "mergedAt": datetime.now(UTC).isoformat(),
                }],
                "cleanup": {
                    "outcome": "degraded",
                    "primarySynced": True,
                    "taskWorktreesAbsent": False,
                    "taskBranchesAbsent": False,
                    "unrelatedDirtyPreserved": True,
                    "residueSafe": True,
                    "residues": [{
                        "repository": identity[0],
                        "pullRequest": 7,
                        "worktree": "task-worktree",
                        "branch": "ChaosEngine/task",
                        "reasonCode": "removal-denied",
                    }],
                    "warnings": ["cleanup-residue-remains"],
                },
            }), encoding="utf-8")
            command = f'py -3 "{runtime}" delivery-status --receipt-out "{receipt_path}"'
            with mock.patch.object(
                guard, "_checkpoint_identity", return_value=identity
            ), mock.patch.object(
                guard, "_trusted_executable_token", return_value=True
            ), mock.patch.object(
                guard, "_harness_root", return_value=str(root)
            ), mock.patch.object(guard.shutil, "which", return_value=str(root / "py")):
                event = guard._successful_delivery_event({"cwd": str(root)}, command)
                receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
                for residue_update in (
                    {"repository": "token=secret"},
                    {"pullRequest": True},
                    {"worktree": ""},
                    {"branch": ""},
                    {"pullRequest": 8},
                    {"worktree": "api_key=abcdefgh"},
                    {"branch": "Bearer abcdefgh"},
                ):
                    malformed = json.loads(json.dumps(receipt))
                    malformed["cleanup"]["residues"][0].update(residue_update)
                    receipt_path.write_text(json.dumps(malformed), encoding="utf-8")
                    self.assertIsNone(
                        guard._successful_delivery_event({"cwd": str(root)}, command),
                        residue_update,
                    )
                for field in ("schemaVersion", "mergedCount"):
                    malformed = json.loads(json.dumps(receipt))
                    malformed[field] = True
                    receipt_path.write_text(json.dumps(malformed), encoding="utf-8")
                    self.assertIsNone(
                        guard._successful_delivery_event({"cwd": str(root)}, command),
                        field,
                    )
                malformed = json.loads(json.dumps(receipt))
                malformed["legacyCommitProofs"] = [
                    {"repository": "other/project", "head": "b" * 40}
                ]
                receipt_path.write_text(json.dumps(malformed), encoding="utf-8")
                self.assertIsNone(
                    guard._successful_delivery_event({"cwd": str(root)}, command)
                )
        self.assertIsNotNone(event)

    def test_unrelated_delivery_event_cannot_complete_this_task(self):
        task = ("consumer/project", "ChaosEngine/task", "a" * 40)
        event = guard._checkpoint_json_event("delivery", "other/project", "main", "b" * 40, observedAt=int(time.time()), taskHeads=[{"repository": "other/project", "head": "c" * 40}])
        checkpoint = guard._checkpoint_json_event("checkpoint", *task)
        with mock.patch.object(guard, "ledger_events", return_value=["commit", checkpoint, event]), mock.patch.object(guard, "_checkpoint_identity", return_value=task):
            self.assertIn("delivery", guard.check_r29_delivery_complete({"session_id": "s"}))

    def test_read_only_session_owes_no_delivery_receipt(self):
        with mock.patch.object(guard, "ledger_events", return_value=[]):
            self.assertIsNone(guard.check_r29_delivery_complete({}))




class StopReasonsAreCollectedTest(unittest.TestCase):
    """Every Stop rule must be reachable, not just the first one listed.

    `run_stop` returned after the first reason it found, and `stop_hook_active`
    makes the *second* Stop attempt return 0 immediately. Together those mean
    exactly one Stop rule can ever fire per session: when R16 blocked, R18 and
    later delivery checks were never evaluated at all.

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

        R18 asks the real repository whether the branch has unpushed commits.
        Without this, these tests depend on whether a push happens to be
        pending instead of on the subject.
        """
        isolate_stop_rules(self)

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_learning_and_unpushed_work_are_reported_together(self, _report):
        with patch("scripts.agents.guard.check_r16_learning_session", return_value="LEARNING"):
            with patch(
                "scripts.agents.guard.check_r18_unpushed_work", return_value="UNPUSHED"
            ):
                output = io.StringIO()
                with redirect_stdout(output):
                    guard.run_stop({"cwd": ".", "session_id": "s"})
        payload = json.loads(output.getvalue())
        self.assertEqual(payload["decision"], "block")
        self.assertIn("LEARNING", payload["reason"])
        self.assertIn("UNPUSHED", payload["reason"])

    @patch(
        "scripts.agents.guard._worktree_report",
        return_value={"worktrees": [{"is_current": True, "state": "clean"}], "advisories": []},
    )
    def test_a_clean_session_still_produces_no_block(self, _report):
        with patch("scripts.agents.guard.check_r16_learning_session", return_value=None):
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
            ".claude/user-harness/CLAUDE.md",
            ".claude/user-harness/settings.json",
        ):
            with self.subTest(path=path):
                self.assertTrue(guard._HARNESS_SOURCE.match(path))
        for path in (
            "scripts/agents/guard.py",
            "tests/scripts/test_guard_lifecycle.py",
            "AGENTS.md",
            ".claude/agents/coder.md",
            ".codex/agents/reviewer.toml",
            ".agents/skills/act-as-mohab/references/delegation.md",
            ".claude/skills/act-as-mohab/SKILL.md",
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

    @patch(
        "scripts.agents.guard.subprocess.run",
        return_value=subprocess.CompletedProcess([], 2, "", ""),
    )
    def test_hard_sync_failure_does_not_recommend_apply(self, _run):
        advisory = guard._sync_advisory()
        self.assertIsNotNone(advisory)
        self.assertIn("hard failure", advisory)
        self.assertNotIn("--apply", advisory)


class LedgerIsAppendOnlyAndReapedTest(unittest.TestCase):
    """#4552: a read-modify-write loses whole events, and files lived forever.

    `ledger_record` read the ledger, appended, and wrote it back. This host
    issues tool calls in parallel, so two hooks could interleave and one
    event vanished. Delivery and reflection consumers can make the wrong
    terminal decision when a real observation disappears.

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
                    self.assertTrue(guard.ledger_record(payload, "observation"))
                self.assertEqual(guard.ledger_events(payload), ["observation"])

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
        `["observation", "commit"]"observation"`. Read as a single value that
        line is unparsable, and skipping it dropped the entire pre-upgrade
        history. On the real ledger this session was using, 140 events were
        being read as 10.
        """
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                payload = self.payload("session-legacy", directory)
                path = guard._ledger_path(payload)
                with open(path, "w", encoding="utf-8") as handle:
                    handle.write('["observation", "commit"]')  # legacy, no newline
                guard.ledger_record(payload, "memory-write")

                self.assertEqual(
                    guard.ledger_events(payload), ["observation", "commit", "memory-write"]
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

    Delivery and learning checks trust session evidence, and this harness
    supports resuming a session after a long pause. The
    old `_reap_stale_ledgers` judged staleness by raw mtime and deleted on
    first sight, so any *other* session's routine `ledger_record` call,
    arriving once `LEDGER_RETENTION_SECONDS` had passed, could delete a
    dormant-but-still-relevant session's ledger. This is the mutation the test below
    kills: reverting `_reap_stale_ledgers` to delete on the first sighting
    (instead of marking first) turns this red.
    """

    def payload(self, session: str, directory: str) -> dict:
        return {"session_id": session, "cwd": directory}

    def test_a_ledger_stale_by_one_window_survives_another_sessions_sweep(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                dormant = self.payload("session-dormant", directory)
                guard.ledger_record(dormant, "delivery:evidence")
                dormant_path = guard._ledger_path(dormant)
                old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
                guard.os.utime(dormant_path, (old, old))

                # An unrelated session's own routine write triggers the sweep
                # that scans the whole shared ledger directory.
                other = self.payload("session-other", directory)
                guard.ledger_record(other, "observation")

                self.assertTrue(
                    guard.os.path.exists(dormant_path),
                    "a session dormant for exactly one retention window must "
                    "survive another session's reap sweep",
                )
                self.assertEqual(
                    guard.ledger_events(dormant),
                    ["delivery:evidence"],
                    "the dormant session's recorded evidence must still be readable",
                )

    def test_a_ledger_dormant_for_two_windows_is_eventually_reaped(self):
        """The other half of the property: reaping must still happen."""
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                dormant = self.payload("session-abandoned", directory)
                guard.ledger_record(dormant, "observation")
                dormant_path = guard._ledger_path(dormant)
                old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
                guard.os.utime(dormant_path, (old, old))

                other = self.payload("session-other", directory)
                guard.ledger_record(other, "observation")  # marks it
                mark = dormant_path + guard._REAP_MARK_SUFFIX
                guard.os.utime(mark, (old, old))

                guard.ledger_record(other, "observation")  # reaps it

                self.assertFalse(
                    guard.os.path.exists(dormant_path),
                    "a session dormant for two full retention windows must be reaped",
                )

    def test_a_write_during_sweep_prevents_the_ledger_from_being_deleted(self):
        with tempfile.TemporaryDirectory() as directory:
            ledger = os.path.join(directory, "concurrent.json")
            mark = ledger + guard._REAP_MARK_SUFFIX
            with open(ledger, "w", encoding="utf-8") as handle:
                handle.write('"old"\n')
            with open(mark, "w", encoding="utf-8"):
                pass
            old = time.time() - guard.LEDGER_RETENTION_SECONDS - 60
            os.utime(ledger, (old, old))
            os.utime(mark, (old, old))

            original_getmtime = os.path.getmtime

            def mtime(path):
                if path == mark:
                    with open(ledger, "a", encoding="utf-8") as handle:
                        handle.write('"resumed"\n')
                return original_getmtime(path)

            with patch("scripts.agents.guard.os.path.getmtime", side_effect=mtime):
                guard._reap_stale_ledgers(directory)

            self.assertTrue(os.path.exists(ledger))
            with open(ledger, encoding="utf-8") as handle:
                self.assertIn("resumed", handle.read())

    def test_an_orphaned_reap_mark_is_removed(self):
        with tempfile.TemporaryDirectory() as directory:
            mark = os.path.join(directory, "orphan.json" + guard._REAP_MARK_SUFFIX)
            with open(mark, "w", encoding="utf-8"):
                pass

            guard._reap_stale_ledgers(directory)

            self.assertFalse(os.path.exists(mark))


class SelfTestCoversEveryRuleTest(unittest.TestCase):
    """`--self-test` must exercise every rule, and `main` must run it (#4551).

    The command an agent runs to sanity-check the guard, and the PR gate that
    invokes it, must cover every retained rule. Reassuring output is what an
    agent acts on, so a green that means nothing is worse than no check at all.
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
    it; R18 and R19 once read `hook_input.get("cwd")` directly.

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


class ForeignWorktreeStopGateTest(unittest.TestCase):
    """R24 / #4546: surface stale foreign work without refusing the Stop retry."""

    def setUp(self):
        isolate_stop_rules(self, except_for=("check_r24_foreign_worktree_left_behind",))

    def stop(self, payload: dict) -> dict | None:
        stream = io.StringIO()
        with redirect_stdout(stream):
            self.assertEqual(guard.run_stop(payload), 0)
        text = stream.getvalue().strip()
        return json.loads(text) if text else None

    def test_stale_foreign_uncommitted_work_reaches_stop(self):
        """Reachability through `run_stop` prevents a defined but inert report rule."""
        report = {
            "foreign_worktree_stale_hours": 12,
            "worktrees": [
                {"path": "C:/current", "is_current": True, "state": "clean"},
                {
                    "path": "C:/foreign/still-working",
                    "is_current": False,
                    "is_remote_only": False,
                    "state": "uncommitted",
                    "age_hours": 24,
                },
            ]
        }
        with patch("scripts.agents.guard._worktree_report", return_value=report):
            output = self.stop({"cwd": "."})

        self.assertIsNotNone(output)
        self.assertIn("C:/foreign/still-working", output["reason"])
        self.assertIn("worktree_hygiene.py --check-pull-requests", output["reason"])

    def test_threshold_is_inclusive_and_fresh_work_is_silent(self):
        """The 12-hour boundary is exact; a one-second drift changes the safety outcome."""
        for age, expected in ((12, True), (12 - (1 / 3600), False)):
            with self.subTest(age=age):
                reason = guard.check_r24_foreign_worktree_left_behind(
                    {},
                    {
                        "foreign_worktree_stale_hours": 12,
                        "worktrees": [
                            {"path": "C:/foreign", "state": "uncommitted", "age_hours": age}
                        ]
                    },
                )
                self.assertEqual(reason is not None, expected)

    def test_reporter_owned_threshold_controls_foreign_age_selection(self):
        """#4546: Stop must follow the reporter's one threshold source, not a literal."""
        worktree = {"path": "C:/foreign", "state": "uncommitted", "age_hours": 24}
        self.assertIsNone(
            guard.check_r24_foreign_worktree_left_behind(
                {}, {"foreign_worktree_stale_hours": 25, "worktrees": [worktree]}
            )
        )
        self.assertIsNotNone(
            guard.check_r24_foreign_worktree_left_behind(
                {}, {"foreign_worktree_stale_hours": 24, "worktrees": [worktree]}
            )
        )

    def test_unknown_age_and_urgent_states_are_reported_without_waiting(self):
        """Unknown, corrupt, and unanswerable states cannot be safely aged away."""
        cases = (
            {"path": "C:/foreign/unknown-age", "state": "uncommitted", "age_hours": None},
            {"path": "C:/foreign/corrupt", "state": "corrupt", "age_hours": 0.01},
            {"path": "C:/foreign/unknown", "state": "unknown", "age_hours": 0.01},
        )
        for entry in cases:
            with self.subTest(entry=entry):
                reason = guard.check_r24_foreign_worktree_left_behind({}, {"worktrees": [entry]})
                self.assertIsNotNone(reason)
        unknown_age = guard.check_r24_foreign_worktree_left_behind(
            {}, {"worktrees": [cases[0]]}
        )
        self.assertIn("age could not be determined", unknown_age)

    def test_current_remote_only_and_nonpreserving_states_stay_silent(self):
        """R24 does not duplicate current-state gates or surface deletion-oriented advice."""
        worktrees = [
            {
                "path": "C:/current",
                "is_current": True,
                "state": "uncommitted",
                "age_hours": 30,
            },
            {
                "path": "origin/foreign",
                "is_remote_only": True,
                "state": "unknown",
                "age_hours": None,
            },
            *[
                {"path": f"C:/foreign/{state}", "state": state, "age_hours": 30}
                for state in ("pending", "superseded", "clean", "orphaned")
            ],
        ]
        self.assertIsNone(guard.check_r24_foreign_worktree_left_behind({}, {"worktrees": worktrees}))
        with patch("scripts.agents.guard._worktree_report", return_value={"worktrees": [worktrees[0]]}):
            output = self.stop({"cwd": "."})
        self.assertIsNotNone(output)
        self.assertIn("Current worktree has uncommitted work", output["reason"])
        self.assertNotIn("Foreign worktree report", output["reason"])

    def test_message_caps_paths_names_locks_and_avoids_unconfirmed_cleanup_commands(self):
        """A long report remains actionable without suggesting another agent's work be destroyed."""
        worktrees = [
            {
                "path": f"C:/foreign/{index}",
                "state": "uncommitted",
                "age_hours": 24,
                "locked": index == 0,
                "lock_reason": "agent session" if index == 0 else None,
            }
            for index in range(20)
        ]
        reason = guard.check_r24_foreign_worktree_left_behind(
            {}, {"foreign_worktree_stale_hours": 12, "worktrees": worktrees}
        )
        self.assertIsNotNone(reason)
        self.assertIn("C:/foreign/0", reason)
        self.assertIn("C:/foreign/1", reason)
        self.assertIn("C:/foreign/2", reason)
        self.assertNotIn("C:/foreign/3", reason)
        self.assertIn("Showing 3 of 20 worktrees", reason)
        self.assertIn("locked: agent session", reason)
        self.assertIn("worktree_hygiene.py --check-pull-requests", reason)
        self.assertNotIn("git worktree remove", reason)
        self.assertNotIn("git branch -D", reason)
        self.assertIn("do not commit on its behalf", reason)
        self.assertIn("gh issue comment <tracker>", reason)

    def test_malformed_reports_are_silent_rather_than_raising(self):
        """A corrupt helper payload cannot kill the hook process."""
        for report in (None, {}, {"worktrees": {}}, {"worktrees": [{"state": "uncommitted"}]}):
            with self.subTest(report=report):
                self.assertIsNone(guard.check_r24_foreign_worktree_left_behind({}, report))


class StopTestsAreIndependentOfLiveStateTest(unittest.TestCase):
    """#4555: assert determinism directly instead of enumerating the readers.

    External git state once made five Stop tests depend on whether a push was
    pending. R20's helper `_branch_edits_harness_sources` repeated the defect,
    and an equality pin on Stop rules could not see a helper call.

    Enumeration keeps losing to the next thing nobody enumerated, so this
    stops enumerating. It runs the Stop-facing test classes twice -- once
    normally, once with every subprocess refused -- and requires the same
    result. A test that reaches outside the process for its answer changes
    behaviour between those two runs; one that does not, cannot. It does not
    matter whether the reader is a rule, a helper, or something added later.

    CI structurally cannot catch any instance of this: a fresh checkout has
    nothing unpushed and no branch mid-edit, so CI is exactly where
    the defect is invisible. This check does not depend on the environment at
    all, which is the point.
    """

    SUBJECT_CLASSES = (
        "GuardLifecycleTest",
        "StopReasonsAreCollectedTest",
        "UserHarnessDriftStopGateTest",
        "UnpushedWorkStopGateTest",
        "LearningSessionStopGateTest",
        "RunStateStopGateTest",
        "ForeignWorktreeStopGateTest",
        "DeliveryCompleteStopGateTest",
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
            tree = ast.parse(Path(path).read_text(encoding="utf-8"))
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

    def test_collaboration_dispatch_reads_the_agent_type_adapter(self):
        payload = {
            "tool_name": "collaboration.spawn_agent",
            "tool_input": {"agent_type": "reviewer"},
            "session_id": "r22",
            "cwd": ".",
        }
        self.assertIsNone(
            guard.check_r22_dispatch_adapter(payload, "collaboration.spawn_agent")
        )
        payload["tool_input"]["agent_type"] = "general-purpose"
        self.assertIsNotNone(
            guard.check_r22_dispatch_adapter(payload, "collaboration.spawn_agent")
        )

    def test_r22_records_the_learning_session_arming_escape(self):
        source = inspect.getsource(guard.check_r22_dispatch_adapter).lower()
        self.assertIn("learning-session", source)
        self.assertIn("escape", source)

    def test_both_hosts_intercept_a_dispatch(self):
        """Neither matcher listed Task or Agent, so the rule would be dead on arrival."""
        root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        for name in (".claude/settings.json", ".codex/hooks.json"):
            with self.subTest(host=name):
                text = Path(root, name).read_text(encoding="utf-8")
                self.assertIn("Task|Agent", text)



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
            ["delegate-dispatch", "read-observation"],
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
        for events in ([], ["commit"], ["read-observation", "commit", "memory-write"]):
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
                "gh -RShaftHQ/SHAFT_ENGINE issue comment 4536 --body x",
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

    A raw-source substring check missed casing, trailing words, and adjacent
    string literals. This test inspects rendered values instead.

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
        """Adjacent literals still form one runtime promise.

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

    This check makes isolation survive the next rule. R18 once read live git
    state and made five Stop tests depend on whether a push was pending.

    A fresh checkout has nothing unpushed, so CI is precisely where the bug is
    invisible. Only an equality
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

    def test_functions_exec_apply_patch_uses_absolute_target_worktree_for_r19_scoping(self):
        """Literal wrapped patch targets, not hook cwd, scope R19 (#5289)."""
        main_root, _ = self.repository()
        task_root = os.path.join(os.path.dirname(main_root), "task-worktree")
        os.makedirs(task_root)
        task_target = os.path.join(task_root, "scripts", "guard.py")
        main_target = os.path.join(main_root, "scripts", "guard.py")

        def wrapped(*targets: str, quote: str = '"') -> dict:
            patch_text = "".join(f"*** Update File: {target}\n" for target in targets)
            if quote == '"':
                literal = json.dumps(patch_text)
            else:
                escaped = patch_text.replace("\\", "\\\\").replace(quote, "\\" + quote)
                literal = quote + (escaped.replace("\n", "\\n") if quote == "'" else escaped) + quote
            return {
                "cwd": main_root,
                "tool_input": f"await tools.apply_patch({literal});",
            }

        with patch("scripts.agents.guard._current_branch", return_value="main"):
            with patch("scripts.agents.guard._repository_root", return_value=main_root):
                self.assertIsNone(guard.check_r19_fresh_base(wrapped(task_target), "functions.exec"))
                for quote in ("'", "`"):
                    with self.subTest(quote=quote):
                        self.assertIsNone(
                            guard.check_r19_fresh_base(
                                wrapped(task_target, quote=quote), "functions.exec"
                            )
                        )
                self.assertIsNotNone(guard.check_r19_fresh_base(wrapped(main_target), "functions.exec"))
                self.assertIsNotNone(
                    guard.check_r19_fresh_base(wrapped(main_target, task_target), "functions.exec")
                )
                dynamic = {
                    "cwd": main_root,
                    "tool_input": (
                        f"const patch = {json.dumps('*** Update File: ' + task_target + chr(10))}; "
                        "await tools.apply_patch(patch);"
                    ),
                }
                self.assertIsNotNone(guard.check_r19_fresh_base(dynamic, "functions.exec"))
                mixed_dynamic = {
                    "cwd": main_root,
                    "tool_input": (
                        f"await tools.apply_patch({json.dumps('*** Update File: ' + task_target + chr(10))}); "
                        "await tools.apply_patch(patch);"
                    ),
                }
                self.assertIsNotNone(
                    guard.check_r19_fresh_base(mixed_dynamic, "functions.exec")
                )
                mixed_interpolated = {
                    "cwd": main_root,
                    "tool_input": (
                        f"await tools.apply_patch({json.dumps('*** Update File: ' + task_target + chr(10))}); "
                        "await tools.apply_patch(`*** Update File: ${task_target}\\n`);"
                    ),
                }
                self.assertIsNotNone(
                    guard.check_r19_fresh_base(mixed_interpolated, "functions.exec")
                )
                interpolated = {
                    "cwd": main_root,
                    "tool_input": "await tools.apply_patch(`*** Update File: ${task_target}\\n`);",
                }
                self.assertIsNotNone(guard.check_r19_fresh_base(interpolated, "functions.exec"))

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


class CanonicalInvocationContextRegressionTest(unittest.TestCase):
    """#5291/#5292: classify the actual wrapped invocation, not hook prose/cwd."""

    def repository(self, parent: str, name: str, branch: str) -> str:
        root = os.path.join(parent, name)
        os.makedirs(root)
        subprocess.run(
            ["git", "init", "-b", "main"],
            cwd=root,
            capture_output=True,
            text=True,
            check=True,
        )
        subprocess.run(
            [
                "git",
                "-c",
                "user.name=Hook Test",
                "-c",
                "user.email=hook@test.invalid",
                "commit",
                "--allow-empty",
                "-m",
                "base",
            ],
            cwd=root,
            capture_output=True,
            text=True,
            check=True,
        )
        if branch != "main":
            subprocess.run(
                ["git", "switch", "-c", branch],
                cwd=root,
                capture_output=True,
                text=True,
                check=True,
            )
        return os.path.realpath(root)

    def test_wrapped_exec_uses_its_explicit_workdir_repository(self):
        with tempfile.TemporaryDirectory() as temporary:
            desktop = self.repository(temporary, "desktop", "main")
            task = self.repository(temporary, "task", "ChaosEngine/5291")
            source = (
                "await tools.exec_command({"
                f'cmd:"Set-Content note.txt value",workdir:{json.dumps(task)}'
                "});"
            )
            payload = {"cwd": desktop, "tool_input": source}

            self.assertIsNone(guard.check_r19_fresh_base(payload, "functions.exec"))

    def test_patch_target_repository_default_branch_is_refused(self):
        with tempfile.TemporaryDirectory() as temporary:
            desktop = self.repository(temporary, "desktop", "ChaosEngine/source")
            target = self.repository(temporary, "target", "main")
            patch_text = f"*** Update File: {target}/guard.py\n"
            payload = {
                "cwd": desktop,
                "tool_input": f"await tools.apply_patch({json.dumps(patch_text)});",
            }

            reason = guard.check_r19_fresh_base(payload, "functions.exec")

            self.assertIsNotNone(reason)
            self.assertIn("HEAD is main", reason)

    def test_patch_targets_spanning_repositories_fail_closed(self):
        with tempfile.TemporaryDirectory() as temporary:
            desktop = self.repository(temporary, "desktop", "ChaosEngine/source")
            first = self.repository(temporary, "first", "ChaosEngine/first")
            second = self.repository(temporary, "second", "ChaosEngine/second")
            patch_text = (
                f"*** Update File: {first}/one.py\n"
                f"*** Update File: {second}/two.py\n"
            )
            payload = {
                "cwd": desktop,
                "tool_input": f"await tools.apply_patch({json.dumps(patch_text)});",
            }

            reason = guard.check_r19_fresh_base(payload, "functions.exec")

            self.assertIsNotNone(reason)
            self.assertIn("multiple repositories", reason)

    def test_quoted_search_pattern_does_not_create_maven_segments(self):
        command = 'rg -n "literal|mvn -pl shaft-engine test" scripts'

        self.assertEqual([], guard._mvn_segments(command))
        self.assertIsNone(guard.check_r1_maven(command))
        self.assertIsNotNone(
            guard.check_r1_maven("mvn test -DheadlessExecution=true")
        )

    def test_read_only_branch_inspection_is_not_mutation(self):
        self.assertFalse(guard._shell_is_mutation("git branch --show-current"))
        self.assertFalse(guard._shell_is_mutation("git branch --list ChaosEngine/*"))
        self.assertTrue(guard._shell_is_mutation("git branch -D ChaosEngine/old"))

    def test_current_official_hook_documentation_hosts_are_authoritative(self):
        for url in (
            "https://code.visualstudio.com/docs/agents/reference/hooks-reference",
            "https://code.claude.com/docs/en/hooks",
            "https://developers.openai.com/codex/",
        ):
            with self.subTest(url=url):
                self.assertTrue(guard._has_primary_source_url({"url": url}))

    def test_wrapped_web_clients_recognize_the_request_url_only(self):
        for command in (
            "curl.exe -L https://code.visualstudio.com/docs/agents/reference/hooks-reference",
            "(Invoke-WebRequest -Uri https://code.claude.com/docs/en/hooks).StatusCode",
        ):
            with self.subTest(command=command):
                self.assertIn(
                    "authoritative-online-research",
                    guard._research_preflight_events("exec_command", {"cmd": command}),
                )
        self.assertNotIn(
            "authoritative-online-research",
            guard._research_preflight_events(
                "exec_command",
                {"cmd": "curl.exe -o https://code.visualstudio.com/output https://example.com"},
            ),
        )
