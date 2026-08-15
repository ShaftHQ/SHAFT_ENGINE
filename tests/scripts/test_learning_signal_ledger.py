"""Outcome-certified learning signals for the portable agent hook."""

from __future__ import annotations

import tempfile
import unittest
import importlib
import importlib.util
import hashlib
import json
import os
import subprocess  # nosec B404 - tests launch fixed interpreters with controlled fixtures.
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from unittest.mock import patch

from scripts.agents import guard

CONTROLLER = str(Path(guard.__file__).with_name("learning_loop.py"))


class LearningWriteOutcomeTest(unittest.TestCase):
    def test_failed_learning_write_does_not_satisfy_r16(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "failed-memory-write",
                "cwd": directory,
                "tool_name": "mcp__shaft-memory__remember_memory",
                "tool_input": {"title": "not persisted"},
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                self.assertEqual(guard.run_pretooluse(payload), 0)
                events = guard.ledger_events(payload)

        self.assertNotIn("memory-write", events)
        with patch("scripts.agents.guard.ledger_events", return_value=["commit", *events]):
            self.assertIsNotNone(guard.check_r16_learning_loop(payload))

    def test_successful_learning_write_is_certified_only_after_posttooluse(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "successful-memory-write",
                "cwd": directory,
                "tool_name": "mcp__shaft-memory__remember_memory",
                "tool_input": {"title": "persisted"},
                "tool_response": {"status": "ok"},
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                self.assertEqual(guard.run_pretooluse(payload), 0)
                self.assertNotIn("memory-write", guard.ledger_events(payload))
                self.assertEqual(guard.run_posttooluse(payload), 0)
                self.assertIn("memory-write", guard.ledger_events(payload))

    def test_structured_signal_requires_successful_assessment(self):
        learning_loop = importlib.import_module("scripts.agents.learning_loop")
        signal = (
            f'py -3 "{CONTROLLER}" signal '
            "--session-id s --operation-id signal-op-1 --kind user_correction "
            "--incident-id r16 --origin user "
            "--evidence test:red.txt:evidence-hash"
        )
        assessment = (
            f'py -3 "{CONTROLLER}" assess '
            "--session-id s --operation-id assess-op-1 --hypothesis x "
            "--owner scripts/agents/guard.py "
            f"--baseline-ref {'e' * 40} --allowed-path scripts/agents/guard.py "
            "--red-command red --success-predicate fixed --invariant portable "
            "--risk-tier ordinary "
            "--tracking-issue-url https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"
        )
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            artifact = state / "red.txt"
            artifact.write_text("red", encoding="utf-8")
            digest = hashlib.sha256(b"red").hexdigest()
            signal = signal.replace("evidence-hash", digest)
            payload = {
                "session_id": "s",
                "cwd": directory,
                "tool_name": "PowerShell",
                "tool_input": {"command": signal},
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                with patch.object(learning_loop, "default_state_dir", return_value=state):
                    learning_loop.record_signal(
                        state,
                        session_id="s",
                        kind="user_correction",
                        incident_id="r16",
                        origin="user",
                        evidence=[{"kind": "test", "id": "red.txt", "sha256": digest}],
                        evidence_root=state,
                    )
                    learning_loop.record_completion(
                        state,
                        "s",
                        "signal-op-1",
                        "signal",
                        [learning_loop.incident_hash("r16")],
                    )
                    guard.run_posttooluse(payload)
                    events = guard.ledger_events(payload)
                    self.assertTrue(
                        any(event.startswith("learning-signal:") for event in events)
                    )
                    self.assertIsNotNone(guard.check_r16_learning_loop(payload))
                    learning_loop.assess(
                        state,
                        session_id="s",
                        hypothesis="x",
                        owner="scripts/agents/guard.py",
                        baseline_ref="e" * 40,
                        allowed_paths=["scripts/agents/guard.py"],
                        red_command="red",
                        success_predicates=["fixed"],
                        invariants=["portable"],
                        risk_tier="ordinary",
                        tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
                    )
                    learning_loop.record_completion(
                        state,
                        "s",
                        "assess-op-1",
                        "assess",
                        [learning_loop.incident_hash("r16")],
                    )
                    payload["tool_input"] = {"command": assessment}
                    guard.run_posttooluse(payload)
                    self.assertIsNotNone(guard.check_r16_learning_loop(payload))
                    guard.ledger_record(payload, "issue-created:9999")
                    guard.run_posttooluse(payload)
                    self.assertIsNotNone(guard.check_r16_learning_loop(payload))
                    guard.ledger_record(payload, "issue-created:4731")
                    guard.run_posttooluse(payload)
                    self.assertIsNone(guard.check_r16_learning_loop(payload))

    def test_free_form_legacy_no_learning_does_not_unlock_r16(self):
        command = 'py -3 scripts/agents/guard.py --learning-none "No durable learning surfaced"'
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "legacy-none",
                "cwd": directory,
                "tool_name": "Bash",
                "tool_input": {"command": command},
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.run_posttooluse(payload)
                events = guard.ledger_events(payload)
        self.assertFalse(any(event.startswith("learning-none:") for event in events))

    def test_shell_wrapper_cannot_certify_an_inner_memory_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "wrapped-failure",
                "cwd": directory,
                "tool_name": "PowerShell",
                "tool_input": {"command": "memory remember --definitely-invalid; exit 0"},
                "tool_response": "outer command exited 0",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.run_posttooluse(payload)
                self.assertNotIn("memory-write", guard.ledger_events(payload))

    def test_single_successful_memory_cli_command_is_certified(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "exact-memory-cli",
                "cwd": directory,
                "tool_name": "Bash",
                "tool_input": {"command": "memory remember --stdin"},
                "tool_response": "exit 0",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.run_posttooluse(payload)
                self.assertIn("memory-write", guard.ledger_events(payload))

    def test_successful_memory_stdin_pipeline_is_certified(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = {
                "session_id": "memory-pipeline", "cwd": directory, "tool_name": "PowerShell",
                "tool_input": {"command": "'{\"memories\":[]}' | memory remember --stdin"},
                "tool_response": "exit 0",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                guard.run_posttooluse(payload)
                self.assertIn("memory-write", guard.ledger_events(payload))

    def test_help_and_explicit_error_results_never_credit_memory(self):
        cases = (
            ("Bash", "memory remember --help", "usage: memory remember"),
            ("Bash", "memory remember --stdin", {"isError": True}),
            ("mcp__shaft-memory__remember_memory", "", {"isError": True}),
        )
        for index, (tool_name, command, response) in enumerate(cases):
            with self.subTest(tool_name=tool_name, command=command):
                with tempfile.TemporaryDirectory() as directory:
                    payload = {
                        "session_id": f"memory-error-{index}", "cwd": directory,
                        "tool_name": tool_name, "tool_input": {"command": command},
                        "tool_response": response,
                    }
                    with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                        guard.run_posttooluse(payload)
                        self.assertNotIn("memory-write", guard.ledger_events(payload))

    def test_single_quote_backslash_cannot_hide_a_memory_wrapper(self):
        command = "memory remember --stdin 'x\\' ; false; exit 0"
        self.assertFalse(guard._is_learning_write_command(command))

    def test_help_text_cannot_create_or_assess_a_signal(self):
        learning_loop = importlib.import_module("scripts.agents.learning_loop")
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            artifact = state / "red.txt"
            artifact.write_text("red", encoding="utf-8")
            digest = hashlib.sha256(b"red").hexdigest()
            learning_loop.record_signal(
                state,
                session_id="help-bypass",
                kind="user_correction",
                incident_id="fake",
                origin="user",
                evidence=[{"kind": "test", "id": "red.txt", "sha256": digest}],
                evidence_root=state,
            )
            learning_loop.assess(
                state,
                session_id="help-bypass",
                hypothesis="old",
                owner="scripts/agents/guard.py",
                baseline_ref="e" * 40,
                allowed_paths=["scripts/agents/guard.py"],
                red_command="red",
                success_predicates=["fixed"],
                invariants=["safe"],
                risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )
            payload = {
                "session_id": "help-bypass",
                "cwd": directory,
                "tool_name": "Bash",
                "tool_input": {
                    "command": f'py -3 "{CONTROLLER}" assess '
                    "--session-id help-bypass --operation-id stale-help --help"
                },
                "tool_response": "usage: learning_loop.py",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                with patch.object(learning_loop, "default_state_dir", return_value=state):
                    guard.ledger_record(payload, "issue-created:4731")
                    guard.run_posttooluse(payload)
                    self.assertFalse(
                        any(
                            event.startswith(("learning-signal:", "learning-assessed:"))
                            for event in guard.ledger_events(payload)
                        )
                    )

    def test_wrapped_controller_command_cannot_replay_a_completion(self):
        learning_loop = importlib.import_module("scripts.agents.learning_loop")
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            artifact = state / "red.txt"
            artifact.write_text("red", encoding="utf-8")
            digest = hashlib.sha256(b"red").hexdigest()
            receipt = learning_loop.record_signal(
                state,
                session_id="wrapped-controller",
                kind="user_correction",
                incident_id="wrapped",
                origin="user",
                evidence=[{"kind": "test", "id": "red.txt", "sha256": digest}],
                evidence_root=state,
            )
            learning_loop.record_completion(
                state,
                "wrapped-controller",
                "wrapped-op",
                "signal",
                [receipt["incident_hash"]],
            )
            payload = {
                "session_id": "wrapped-controller",
                "cwd": directory,
                "tool_name": "Bash",
                "tool_input": {
                    "command": f'py -3 "{CONTROLLER}" signal '
                    "--session-id wrapped-controller --operation-id wrapped-op --help; exit 0"
                },
                "tool_response": "exit 0",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                with patch.object(learning_loop, "default_state_dir", return_value=state):
                    guard.run_posttooluse(payload)
                    self.assertFalse(
                        any(
                            event.startswith("learning-signal:")
                            for event in guard.ledger_events(payload)
                        )
                    )

    def test_controller_path_as_inert_python_argument_cannot_replay_completion(self):
        payload = {
            "session_id": "inert-controller", "cwd": str(Path(CONTROLLER).parents[2]),
            "tool_name": "PowerShell",
            "tool_input": {"command": f'py -3 -c "pass" "{CONTROLLER}" assess '
                "--session-id inert-controller --operation-id inert-op"},
            "tool_response": "exit 0",
        }
        with patch("scripts.agents.guard._learning_loop.load_completion") as completion:
            completion.return_value = {
                "operation": "assess", "incident_hashes": ["a" * 64], "reason_code": None
            }
            guard.run_posttooluse(payload)
        self.assertFalse(completion.called)

    def test_quoted_separator_inside_argument_keeps_controller_credit(self):
        learning_loop = importlib.import_module("scripts.agents.learning_loop")
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            artifact = state / "red.txt"
            artifact.write_text("red", encoding="utf-8")
            digest = hashlib.sha256(b"red").hexdigest()
            learning_loop.record_signal(
                state, session_id="quoted-separator", kind="user_correction",
                incident_id="quoted", origin="user",
                evidence=[{"kind": "test", "id": "red.txt", "sha256": digest}],
                evidence_root=state,
            )
            candidates = learning_loop.assess(
                state, session_id="quoted-separator", hypothesis="quoted argument",
                owner="scripts/agents/guard.py", baseline_ref="e" * 40,
                allowed_paths=["scripts/agents/guard.py"], red_command="red; then green",
                success_predicates=["fixed"], invariants=["safe"], risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )
            learning_loop.record_completion(
                state, "quoted-separator", "quoted-op", "assess",
                [item["incident_hash"] for item in candidates],
            )
            payload = {
                "session_id": "quoted-separator", "cwd": directory,
                "tool_name": "PowerShell",
                "tool_input": {"command": f'py -3 "{CONTROLLER}" assess '
                    "--session-id quoted-separator --operation-id quoted-op "
                    "--red-command \"red; then green\""},
                "tool_response": "exit 0",
            }
            with patch.dict(guard.os.environ, {"TMPDIR": directory, "TEMP": directory}):
                with patch.object(learning_loop, "default_state_dir", return_value=state):
                    guard.ledger_record(payload, "issue-created:4731")
                    guard.run_posttooluse(payload)
                    observed = guard.ledger_events(payload)
                    self.assertTrue(
                        any(event.startswith("learning-assessed:") for event in observed),
                        observed,
                    )

    def test_each_signal_requires_a_later_incident_bound_assessment(self):
        with patch(
            "scripts.agents.guard.ledger_events",
            return_value=[
                "learning-signal:first",
                "learning-assessed:first",
                "learning-signal:second",
            ],
        ):
            self.assertIsNotNone(guard.check_r16_learning_loop({"session_id": "s"}))


class StructuredLearningReceiptTest(unittest.TestCase):
    def controller(self):
        self.assertIsNotNone(
            importlib.util.find_spec("scripts.agents.learning_loop"),
            "structured learning controller is missing",
        )
        return importlib.import_module("scripts.agents.learning_loop")

    @staticmethod
    def evidence(root: Path, name: str, contents: str, kind: str = "test") -> list[dict]:
        artifact = root / name
        artifact.write_text(contents, encoding="utf-8")
        return [
            {
                "kind": kind,
                "id": name,
                "sha256": hashlib.sha256(contents.encode("utf-8")).hexdigest(),
            }
        ]

    def test_signal_is_redacted_hashed_and_deduplicated_by_incident(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "red.txt", "expected assertion failure")
            receipt = learning_loop.record_signal(
                state,
                session_id="session-secret",
                kind="user_correction",
                incident_id="wrong-r16-credit",
                origin="user",
                evidence=evidence,
                evidence_root=state,
                task_ref="#4721",
            )
            duplicate = learning_loop.record_signal(
                state,
                session_id="session-secret",
                kind="user_correction",
                incident_id="wrong-r16-credit",
                origin="user",
                evidence=evidence,
                evidence_root=state,
                task_ref="#4721",
            )
            stored = learning_loop.load_receipts(state, "session-secret")

        self.assertNotIn("session-secret", str(receipt))
        self.assertNotIn("wrong-r16-credit", str(receipt))
        self.assertNotIn("red.txt", str(receipt))
        self.assertNotIn("#4721", str(receipt))
        self.assertEqual(receipt["session_hash"], duplicate["session_hash"])
        self.assertEqual([receipt], stored)

    def test_assessment_creates_one_quarantined_candidate_per_incident(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "red.txt", "failure")
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="test_failure",
                incident_id="r16-attempt-credit",
                origin="tool",
                evidence=evidence,
                evidence_root=state,
            )
            candidates = learning_loop.assess(
                state,
                session_id="s",
                hypothesis="Successful tool outcomes must be required for learning credit.",
                owner="scripts/agents/guard.py",
                baseline_ref="eb653f01adf0ca78fcb59b2be3e0e93c53274144",
                allowed_paths=["scripts/agents/guard.py", "tests/scripts/test_learning_signal_ledger.py"],
                red_command="py -3 -m unittest tests.scripts.test_learning_signal_ledger -v",
                success_predicates=["failed writes do not satisfy R16"],
                invariants=["successful writes still satisfy R16"],
                risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )

        self.assertEqual(len(candidates), 1)
        self.assertEqual(candidates[0]["status"], "quarantined")
        self.assertNotIn("r16-attempt-credit", str(candidates[0]))
        self.assertNotIn("Successful tool outcomes", str(candidates[0]))

    def test_no_learning_attestation_fails_when_a_signal_exists(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "guard.txt", "R11 refused")
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="guard_block",
                incident_id="correct-refusal",
                origin="tool",
                evidence=evidence,
                evidence_root=state,
            )
            with self.assertRaisesRegex(ValueError, "meaningful signals"):
                learning_loop.attest_no_learning(state, "s", "no_new_evidence")

    def test_degraded_store_is_an_explicit_non_blocking_disposition(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            result = learning_loop.attest_no_learning(state, "s", "store_degraded")
            self.assertEqual("store_degraded", result["reason_code"])

    def test_new_signal_invalidates_a_prior_no_learning_attestation(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.attest_no_learning(state, "s", "no_new_evidence")
            evidence = self.evidence(state, "event.txt", "later signal")
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="user_correction",
                incident_id="later-signal",
                origin="user",
                evidence=evidence,
                evidence_root=state,
            )
            self.assertIsNone(learning_loop.load_attestation(state, "s"))

    def test_receipt_loader_rejects_tampering_and_incomplete_objects(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            session_hash = hashlib.sha256(b"s").hexdigest()
            receipts = state / "receipts"
            receipts.mkdir()
            (receipts / f"{session_hash}.jsonl").write_text(
                json.dumps({"receipt_id": "forged", "incident_hash": "invented"}) + "\n",
                encoding="utf-8",
            )
            self.assertEqual(learning_loop.load_receipts(state, "s"), [])

    def test_receipt_loader_rejects_creator_invalid_kind_and_timestamp(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            receipt = learning_loop.record_signal(
                state, session_id="s", kind="tool_failure",
                incident_id="invalid-persisted-receipt", origin="tool",
                evidence=self.evidence(state, "event.txt", "event"), evidence_root=state,
            )
            receipt["evidence"][0]["kind"] = "RAW_SECRET=hunter2"
            receipt["occurred_at"] = "not-a-time"
            identity = {key: receipt[key] for key in (
                "session_hash", "signal_kind", "incident_hash", "origin", "evidence",
                "task_ref_hash",
            )}
            receipt["receipt_id"] = hashlib.sha256(
                json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
            ).hexdigest()
            next((state / "receipts").glob("*.jsonl")).write_text(
                json.dumps(receipt) + "\n", encoding="utf-8"
            )
            self.assertEqual(learning_loop.load_receipts(state, "s"), [])

    def test_receipt_loader_rejects_non_utc_timestamp(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            receipt = learning_loop.record_signal(
                state, session_id="s", kind="tool_failure", incident_id="non-utc",
                origin="tool", evidence=self.evidence(state, "event.txt", "event"),
                evidence_root=state,
            )
            receipt["occurred_at"] = "2026-08-11T10:00:00+02:00"
            next((state / "receipts").glob("*.jsonl")).write_text(
                json.dumps(receipt) + "\n", encoding="utf-8"
            )
            self.assertEqual(learning_loop.load_receipts(state, "s"), [])

    def test_evidence_digest_is_recomputed_before_a_signal_is_recorded(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "actual.txt", "actual")
            evidence[0]["sha256"] = "f" * 64
            with self.assertRaisesRegex(ValueError, "digest"):
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="tool_failure",
                    incident_id="digest-mismatch",
                    origin="tool",
                    evidence=evidence,
                    evidence_root=state,
                )

    def test_whitespace_and_concurrent_duplicates_create_one_receipt(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "event.txt", "one incident")

            def record(incident: str):
                return learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="review_finding",
                    incident_id=incident,
                    origin="reviewer",
                    evidence=evidence,
                    evidence_root=state,
                )

            with ThreadPoolExecutor(max_workers=4) as executor:
                list(executor.map(record, ["incident", " incident ", "incident", " incident "]))
            stored = learning_loop.load_receipts(state, "s")
            lines = next((state / "receipts").glob("*.jsonl")).read_text(
                encoding="utf-8"
            ).splitlines()
        self.assertEqual(len(stored), 1)
        self.assertEqual(len(lines), 1)

    def test_multiprocess_duplicates_all_succeed_and_create_one_receipt(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory) / "state"
            evidence_root = Path(directory) / "source"
            evidence_root.mkdir()
            evidence = self.evidence(evidence_root, "event.txt", "same incident")
            code = (
                "from pathlib import Path; from scripts.agents.learning_loop import record_signal; "
                "import sys; record_signal(Path(sys.argv[1]), session_id='s', "
                "kind='tool_failure', incident_id='same', origin='tool', "
                "evidence=[{'kind':'test','id':'event.txt','sha256':sys.argv[3]}], "
                "evidence_root=Path(sys.argv[2]))"
            )
            processes = [
                subprocess.Popen(  # nosec B603 - fixed interpreter and controlled fixture arguments.
                    [sys.executable, "-c", code, str(state), str(evidence_root), evidence[0]["sha256"]],
                    cwd=Path(__file__).resolve().parents[2], stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE, text=True,
                )
                for _ in range(6)
            ]
            results = [process.communicate(timeout=10) + (process.returncode,) for process in processes]
            self.assertEqual([result[2] for result in results], [0] * 6, results)
            self.assertEqual(len(learning_loop.load_receipts(state, "s")), 1)

    def test_candidate_identity_covers_the_complete_spec(self):
        learning_loop = self.controller()
        candidates = []
        for risk_tier in ("ordinary", "kernel"):
            with tempfile.TemporaryDirectory() as directory:
                state = Path(directory)
                evidence = self.evidence(state, "red.txt", "failure")
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="test_failure",
                    incident_id="candidate-identity",
                    origin="tool",
                    evidence=evidence,
                    evidence_root=state,
                )
                candidates.append(learning_loop.assess(
                    state, session_id="s", hypothesis="Fix it.",
                    owner="scripts/agents/guard.py", baseline_ref="e" * 40,
                    allowed_paths=["scripts/agents/guard.py"],
                    red_command="RAW_COMMAND_SECRET",
                    success_predicates=["RAW_PREDICATE_SECRET"],
                    invariants=["RAW_INVARIANT_SECRET"], risk_tier=risk_tier,
                    tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
                )[0])
        ordinary, kernel = candidates
        self.assertNotEqual(ordinary["candidate_id"], kernel["candidate_id"])
        self.assertEqual(ordinary["schema_version"], 2)
        self.assertNotIn("RAW_COMMAND_SECRET", str(ordinary))
        self.assertNotIn("RAW_PREDICATE_SECRET", str(ordinary))
        self.assertNotIn("RAW_INVARIANT_SECRET", str(ordinary))

    def test_assessment_requires_one_standalone_tracking_issue_per_incident(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="user_correction",
                incident_id="missing-action-issue",
                origin="user",
                evidence=self.evidence(state, "red.txt", "failure"),
                evidence_root=state,
            )
            with self.assertRaisesRegex(ValueError, "tracking issue"):
                learning_loop.assess(
                    state,
                    session_id="s",
                    hypothesis="Fix it.",
                    owner="scripts/agents/guard.py",
                    baseline_ref="e" * 40,
                    allowed_paths=["scripts/agents/guard.py"],
                    red_command="red",
                    success_predicates=["fixed"],
                    invariants=["safe"],
                    risk_tier="ordinary",
                )

    def test_assessment_binds_distinct_canonical_issues_and_rejects_tampering(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            for incident, filename in (("first-action", "first.txt"), ("second-action", "second.txt")):
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="review_finding",
                    incident_id=incident,
                    origin="reviewer",
                    evidence=self.evidence(state, filename, incident),
                    evidence_root=state,
                )
            common = dict(
                session_id="s",
                hypothesis="Fix each action.",
                owner="scripts/agents/guard.py",
                baseline_ref="e" * 40,
                allowed_paths=["scripts/agents/guard.py"],
                red_command="red",
                success_predicates=["fixed"],
                invariants=["safe"],
                risk_tier="ordinary",
            )
            issue = "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"
            with self.assertRaisesRegex(ValueError, "distinct tracking issue"):
                learning_loop.assess(state, tracking_issue_urls=[issue, issue], **common)
            with self.assertRaisesRegex(ValueError, "tracking issue URLs"):
                learning_loop.assess(
                    state,
                    tracking_issue_urls=[issue, "https://github.com/ShaftHQ/other/issues/9"],
                    **common,
                )
            candidates = learning_loop.assess(
                state,
                tracking_issue_urls=[
                    issue,
                    "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4732",
                ],
                **common,
            )
            self.assertEqual(
                {candidate["tracking_issue_url"] for candidate in candidates},
                {
                    issue,
                    "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4732",
                },
            )
            candidate_path = next((state / "candidates").glob("*.json"))
            tampered = json.loads(candidate_path.read_text(encoding="utf-8"))
            tampered["tracking_issue_url"] = "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/9999"
            candidate_path.write_text(json.dumps(tampered), encoding="utf-8")
            self.assertEqual(len(learning_loop.load_candidates(state)), 1)

    def test_tracking_issue_mapping_is_global_and_cannot_be_rebound(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            common = dict(
                hypothesis="Fix it.", owner="scripts/agents/guard.py",
                baseline_ref="e" * 40, allowed_paths=["scripts/agents/guard.py"],
                red_command="red", success_predicates=["fixed"], invariants=["safe"],
                risk_tier="ordinary",
            )
            for session, incident, filename in (
                ("first-session", "first-action", "first.txt"),
                ("second-session", "second-action", "second.txt"),
            ):
                learning_loop.record_signal(
                    state, session_id=session, kind="review_finding", incident_id=incident,
                    origin="reviewer", evidence=self.evidence(state, filename, incident),
                    evidence_root=state,
                )
            first_issue = "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"
            learning_loop.assess(
                state, session_id="first-session", tracking_issue_urls=[first_issue], **common
            )
            with self.assertRaisesRegex(ValueError, "tracking issue already belongs"):
                learning_loop.assess(
                    state, session_id="second-session", tracking_issue_urls=[first_issue], **common
                )
            with self.assertRaisesRegex(ValueError, "incident already bound"):
                learning_loop.assess(
                    state,
                    session_id="first-session",
                    tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4732"],
                    **common,
                )

    def test_concurrent_sessions_cannot_claim_the_same_tracking_issue(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory) / "state"
            state.mkdir()
            for session, incident, filename in (
                ("first-session", "first-action", "first.txt"),
                ("second-session", "second-action", "second.txt"),
            ):
                learning_loop.record_signal(
                    state, session_id=session, kind="review_finding", incident_id=incident,
                    origin="reviewer", evidence=self.evidence(state, filename, incident),
                    evidence_root=state,
                )
            acquired = Path(directory) / "acquired"
            release = Path(directory) / "release"
            contender_ready = Path(directory) / "contender-ready"
            common_call = (
                "ll.assess(Path(sys.argv[1]), session_id=sys.argv[2], hypothesis='Fix it.', "
                "owner='scripts/agents/guard.py', baseline_ref='e'*40, "
                "allowed_paths=['scripts/agents/guard.py'], red_command='red', "
                "success_predicates=['fixed'], invariants=['safe'], risk_tier='ordinary', "
                "tracking_issue_urls=['https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731'])"
            )
            holder_code = (
                "from pathlib import Path; from contextlib import contextmanager; "
                "import sys,time; import scripts.agents.learning_loop as ll; "
                "real=ll._state_lock; "
                "exec(\"@contextmanager\\ndef held(state,name):\\n with real(state,name):\\n  "
                "Path(sys.argv[3]).write_text('yes')\\n  while not Path(sys.argv[4]).exists(): "
                "time.sleep(.01)\\n  yield\"); ll._state_lock=held; "
                + common_call
            )
            contender_code = (
                "from pathlib import Path; from contextlib import contextmanager; "
                "import sys; import scripts.agents.learning_loop as ll; real=ll._state_lock; "
                "exec(\"@contextmanager\\ndef observed(state,name):\\n "
                "Path(sys.argv[3]).write_text('yes')\\n with real(state,name):\\n  yield\"); "
                "ll._state_lock=observed; "
                + common_call
            )
            holder = subprocess.Popen(  # nosec B603 - fixed interpreter and controlled fixture arguments.
                [sys.executable, "-c", holder_code, str(state), "first-session", str(acquired), str(release)],
                cwd=Path(__file__).resolve().parents[2], stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, text=True,
            )
            deadline = time.monotonic() + 5
            while not acquired.exists() and holder.poll() is None and time.monotonic() < deadline:
                time.sleep(0.01)
            if not acquired.exists():
                release.write_text("abort", encoding="ascii")
                output = holder.communicate(timeout=2)
                self.fail(f"holder never acquired the candidate lock: {output}")
            contender = subprocess.Popen(  # nosec B603 - fixed interpreter and controlled fixture arguments.
                [sys.executable, "-c", contender_code, str(state), "second-session", str(contender_ready)],
                cwd=Path(__file__).resolve().parents[2], stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, text=True,
            )
            deadline = time.monotonic() + 5
            while (
                not contender_ready.exists()
                and contender.poll() is None
                and time.monotonic() < deadline
            ):
                time.sleep(0.01)
            if not contender_ready.exists():
                release.write_text("abort", encoding="ascii")
                holder_output = holder.communicate(timeout=2)
                contender_output = contender.communicate(timeout=2)
                self.fail(
                    "contender never reached the candidate lock: "
                    f"holder={holder_output}, contender={contender_output}"
                )
            with self.assertRaises(subprocess.TimeoutExpired):
                contender.wait(timeout=0.5)
            release.write_text("go", encoding="ascii")
            processes = [holder, contender]
            results = [process.communicate(timeout=10) + (process.returncode,) for process in processes]
            self.assertEqual(sorted(result[2] for result in results), [0, 1], results)
            self.assertTrue(
                any("tracking issue already belongs" in result[1] for result in results),
                results,
            )
            self.assertEqual(len(learning_loop.load_candidates(state)), 1)

    def test_candidate_loader_rejects_schema_v1_without_other_corruption(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.record_signal(
                state, session_id="s", kind="tool_failure", incident_id="old-schema",
                origin="tool", evidence=self.evidence(state, "event.txt", "event"),
                evidence_root=state,
            )
            learning_loop.assess(
                state, session_id="s", hypothesis="valid", owner="scripts/agents/guard.py",
                baseline_ref="e" * 40, allowed_paths=["scripts/agents/guard.py"],
                red_command="red", success_predicates=["fixed"], invariants=["safe"],
                risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )
            candidate_path = next((state / "candidates").glob("*.json"))
            candidate = json.loads(candidate_path.read_text(encoding="utf-8"))
            candidate["schema_version"] = 1
            candidate_path.write_text(json.dumps(candidate), encoding="utf-8")
            self.assertEqual(learning_loop.load_candidates(state), [])

    def test_candidate_loader_rejects_creator_invalid_field_shapes(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.record_signal(
                state, session_id="s", kind="tool_failure", incident_id="invalid-candidate",
                origin="tool", evidence=self.evidence(state, "event.txt", "event"),
                evidence_root=state,
            )
            candidate = learning_loop.assess(
                state, session_id="s", hypothesis="valid first",
                owner="scripts/agents/guard.py", baseline_ref="e" * 40,
                allowed_paths=["scripts/agents/guard.py"], red_command="red",
                success_predicates=["fixed"], invariants=["safe"], risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )[0]
            candidate.update(
                owner="", baseline_ref="not-a-ref", allowed_paths=["../escape"],
                red_command_hash="bad", success_predicate_hashes=[], invariant_hashes=[],
            )
            identity = {key: candidate[key] for key in (
                "receipt_ids", "incident_hash", "hypothesis_hash", "owner", "baseline_ref",
                "allowed_paths", "red_command_hash", "success_predicate_hashes",
                "invariant_hashes", "risk_tier", "tracking_issue_url",
            )}
            candidate["candidate_id"] = hashlib.sha256(
                json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
            ).hexdigest()
            next((state / "candidates").glob("*.json")).write_text(
                json.dumps(candidate), encoding="utf-8"
            )
            self.assertEqual(learning_loop.load_candidates(state), [])

    def test_unbounded_evidence_kind_is_rejected(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(
                state, "event.txt", "event", kind="RAW_TRANSCRIPT_PASSWORD=hunter2"  # nosec B105 - redaction sentinel, not a credential.
            )
            with self.assertRaisesRegex(ValueError, "evidence kind"):
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="tool_failure",
                    incident_id="raw-kind",
                    origin="tool",
                    evidence=evidence,
                    evidence_root=state,
                )

    def test_receipt_is_invalid_when_content_addressed_evidence_disappears(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "event.txt", "event")
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="tool_failure",
                incident_id="missing-evidence",
                origin="tool",
                evidence=evidence,
                evidence_root=state,
            )
            for artifact in (state / "evidence").iterdir():
                artifact.unlink()
            self.assertEqual(learning_loop.load_receipts(state, "s"), [])

    def test_runtime_state_never_persists_raw_evidence_bytes(self):
        learning_loop = self.controller()
        secret = "RAW_TRANSCRIPT_PASSWORD=hunter2"  # nosec B105 - redaction sentinel, not a credential.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state"
            evidence_root = root / "source"
            evidence_root.mkdir()
            learning_loop.record_signal(
                state, session_id="s", kind="tool_failure", incident_id="redacted-bytes",
                origin="tool",
                evidence=self.evidence(evidence_root, "trace.txt", secret, kind="trace"),
                evidence_root=evidence_root,
            )
            persisted = "\n".join(
                path.read_text(encoding="utf-8")
                for path in state.rglob("*")
                if path.is_file()
            )
            self.assertNotIn(secret, persisted)

    def test_receipt_binds_the_redacted_evidence_proof_length(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.record_signal(
                state, session_id="s", kind="tool_failure", incident_id="proof-length",
                origin="tool", evidence=self.evidence(state, "event.txt", "abc"),
                evidence_root=state,
            )
            proof_path = next((state / "evidence").glob("*.proof.json"))
            proof = json.loads(proof_path.read_text(encoding="utf-8"))
            proof["byte_length"] = 999999
            proof_path.write_text(json.dumps(proof), encoding="utf-8")
            self.assertEqual(learning_loop.load_receipts(state, "s"), [])

    def test_incomplete_forged_attestation_is_rejected(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            session_hash = hashlib.sha256(b"s").hexdigest()
            attestations = state / "attestations"
            attestations.mkdir()
            identity = {
                "schema_version": 1,
                "session_hash": session_hash,
                "reason_code": "no_new_evidence",
            }
            attestation_id = hashlib.sha256(
                json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
            ).hexdigest()
            (attestations / f"{session_hash}.json").write_text(
                json.dumps(
                    {"reason_code": "no_new_evidence", "attestation_id": attestation_id}
                ),
                encoding="utf-8",
            )
            self.assertIsNone(learning_loop.load_attestation(state, "s"))

    def test_non_utc_attestation_and_completion_timestamps_are_rejected(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            learning_loop.attest_no_learning(state, "s", "no_new_evidence")
            learning_loop.record_completion(state, "s", "timestamp-op", "attest-none")
            attestation_path = next((state / "attestations").glob("*.json"))
            attestation = json.loads(attestation_path.read_text(encoding="utf-8"))
            attestation["assessed_at"] = "2026-08-11T10:00:00+02:00"
            attestation_path.write_text(json.dumps(attestation), encoding="utf-8")
            completion_path = next((state / "completions").glob("*.json"))
            completion = json.loads(completion_path.read_text(encoding="utf-8"))
            completion["completed_at"] = "forged-but-string"
            completion_path.write_text(json.dumps(completion), encoding="utf-8")
            self.assertIsNone(learning_loop.load_attestation(state, "s"))
            self.assertIsNone(learning_loop.load_completion(state, "s", "timestamp-op"))

    def test_abandoned_lock_is_recovered(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            evidence = self.evidence(state, "event.txt", "event")
            learning_loop.record_signal(
                state,
                session_id="s",
                kind="tool_failure",
                incident_id="stale-lock",
                origin="tool",
                evidence=evidence,
                evidence_root=state,
            )
            session_hash = hashlib.sha256(b"s").hexdigest()
            (state / f".candidate-{session_hash}.lock").write_text(
                "not-a-pid", encoding="ascii"
            )
            candidates = learning_loop.assess(
                state,
                session_id="s",
                hypothesis="recover",
                owner="scripts/agents/guard.py",
                baseline_ref="e" * 40,
                allowed_paths=["scripts/agents/guard.py"],
                red_command="red",
                success_predicates=["fixed"],
                invariants=["safe"],
                risk_tier="ordinary",
                tracking_issue_urls=["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4731"],
            )
        self.assertEqual(len(candidates), 1)

    def test_hard_linked_lock_file_cannot_mutate_outside_state(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state"
            state.mkdir()
            outside = root / "outside.lock"
            outside.write_bytes(b"")
            lock = state / ".demo.lock"
            try:
                os.link(outside, lock)
            except OSError as error:
                self.skipTest(f"hard links are unavailable: {error}")
            with self.assertRaisesRegex(ValueError, "lock.*link|hard link"):
                with learning_loop._state_lock(state, "demo"):
                    pass
            self.assertEqual(outside.read_bytes(), b"")

    def test_symlinked_lock_file_cannot_mutate_outside_state(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state"
            state.mkdir()
            outside = root / "outside.lock"
            outside.write_bytes(b"")
            lock = state / ".demo.lock"
            try:
                os.symlink(outside, lock)
            except OSError as error:
                self.skipTest(f"file links are unavailable: {error}")
            with self.assertRaisesRegex(ValueError, "lock.*link|reparse"):
                with learning_loop._state_lock(state, "demo"):
                    pass
            self.assertEqual(outside.read_bytes(), b"")

    def test_lock_handle_closes_when_identity_stat_fails(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory:
            handle = unittest.mock.MagicMock()
            handle.fileno.return_value = 123
            with patch.object(learning_loop.os, "open", return_value=123):
                with patch.object(learning_loop.os, "fdopen", return_value=handle):
                    with patch.object(learning_loop.os, "fstat", side_effect=OSError("stat failed")):
                        with self.assertRaises(OSError):
                            with learning_loop._state_lock(Path(directory), "demo"):
                                pass
            handle.close.assert_called_once_with()

    def test_state_subdirectory_link_cannot_escape_the_runtime_root(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as outside:
            state = Path(directory)
            link = state / "receipts"
            try:
                os.symlink(outside, link, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links are unavailable: {error}")
            evidence = self.evidence(state, "event.txt", "contained")
            with self.assertRaisesRegex(ValueError, "escapes runtime root|link/reparse"):
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="tool_failure",
                    incident_id="linked-state",
                    origin="tool",
                    evidence=evidence,
                    evidence_root=state,
                )

    def test_state_root_link_cannot_redirect_runtime_writes(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as parent, tempfile.TemporaryDirectory() as outside:
            state = Path(parent) / "state"
            try:
                os.symlink(outside, state, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links are unavailable: {error}")
            evidence_root = Path(parent)
            evidence = self.evidence(evidence_root, "event.txt", "contained")
            with self.assertRaisesRegex(ValueError, "reparse|link"):
                learning_loop.record_signal(
                    state,
                    session_id="s",
                    kind="tool_failure",
                    incident_id="linked-root",
                    origin="tool",
                    evidence=evidence,
                    evidence_root=evidence_root,
                )

    def test_evidence_directory_swap_is_rejected_before_final_write(self):
        learning_loop = self.controller()
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as outside:
            root = Path(directory)
            state = root / "state"
            source = root / "source"
            source.mkdir()
            evidence = self.evidence(source, "event.txt", "contained")
            original = learning_loop._contained_directory
            swapped = False

            def swap_after_validation(state_path, name):
                nonlocal swapped
                result = original(state_path, name)
                if name == "evidence" and not swapped:
                    backup = result.with_name("evidence-original")
                    result.rename(backup)
                    os.symlink(outside, result, target_is_directory=True)
                    swapped = True
                return result

            try:
                with patch.object(learning_loop, "_contained_directory", swap_after_validation):
                    with self.assertRaisesRegex(ValueError, "changed|link|reparse"):
                        learning_loop.record_signal(
                            state, session_id="s", kind="tool_failure",
                            incident_id="directory-swap", origin="tool", evidence=evidence,
                            evidence_root=source,
                        )
                self.assertEqual(list(Path(outside).iterdir()), [])
            except OSError as error:
                self.skipTest(f"directory links are unavailable: {error}")
            finally:
                linked = state / "evidence"
                if linked.is_symlink():
                    linked.unlink()


if __name__ == "__main__":
    unittest.main()
