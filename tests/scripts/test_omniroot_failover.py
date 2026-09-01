import importlib.util
import tempfile
import unittest
import os
from datetime import UTC, datetime, timedelta
from unittest.mock import patch
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroot/scripts/runner.py"
SPEC = importlib.util.spec_from_file_location("omniroot_failover_runner", RUNNER_PATH)
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class OmniRootFailoverTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.state = Path(self.temporary.name) / "state"

    def tearDown(self):
        self.temporary.cleanup()

    def continuity(self):
        resumption = {
            "task": "deliver issue 5489",
            "authority": "owner-approved",
            "checkpoint": "checkpoint-1",
            "completedActions": ["action-1"],
            "trackerUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5489",
            "pullRequestUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/5493",
        }
        return {
            "requiredCapability": "most-intelligent",
            "maxAttempts": 3,
            "retryableExitCodes": [75],
            "backoffSeconds": 0,
            "authoritySha256": RUNNER._sha256(resumption["authority"]),
            "checkpointSha256": RUNNER._sha256(resumption["checkpoint"]),
            "completedActionSha256s": [RUNNER._sha256("action-1")],
            "trackerUrlSha256": RUNNER._sha256(resumption["trackerUrl"]),
            "pullRequestUrlSha256": RUNNER._sha256(resumption["pullRequestUrl"]),
            "alternates": [
                {"identity": "replacement-low", "sessionId": "low", "capability": "default",
                 "target": "low-route", "arguments": ["--candidate", "low"], "resumption": resumption},
                {"identity": "replacement-high", "sessionId": "high", "capability": "most-intelligent",
                 "target": "high-route", "arguments": ["--candidate", "high"], "resumption": resumption},
            ],
        }

    def manifest(self):
        return {
            "schemaVersion": 1,
            "runId": "run",
            "status": "running",
            "delegate": {"identity": "failed", "capability": "high"},
            "continuity": RUNNER._continuity_contract(self.continuity()),
            "timestamps": {},
            "deadline": "2030-01-01T00:00:00+00:00",
        }

    def candidates(self):
        return self.continuity()["alternates"]

    def test_retryable_failure_selects_compatible_replacement_and_preserves_checkpoint(self):
        launched = []
        result = RUNNER._advance_continuity(
            self.manifest(), exit_code=75, group_dead=True,
            candidates=self.candidates(),
            register=lambda session_id: launched.append(("registered", session_id)),
            launch=lambda candidate: launched.append(("launched", candidate["identity"])) or {
                "pid": 9001, "pgid": 9001, "processIdentity": "replacement-process"
            },
        )
        self.assertEqual("running", result["status"])
        self.assertEqual(RUNNER._sha256("replacement-high"), result["delegate"]["identitySha256"])
        self.assertEqual(RUNNER._sha256("checkpoint-1"), result["continuity"]["checkpointSha256"])
        self.assertEqual(RUNNER._sha256("owner-approved"), result["continuity"]["authoritySha256"])
        self.assertEqual([RUNNER._sha256("action-1")], result["continuity"]["completedActionSha256s"])
        self.assertEqual(2, result["continuity"]["attempt"])
        self.assertEqual([("registered", "high"), ("launched", "replacement-high")], launched)

    def test_lower_capability_only_blocks_without_launch(self):
        continuity = self.continuity()
        continuity["alternates"] = [continuity["alternates"][0]]
        manifest = self.manifest()
        manifest["continuity"] = RUNNER._continuity_contract(continuity)
        result = RUNNER._advance_continuity(
            manifest, exit_code=75, group_dead=True,
            candidates=continuity["alternates"],
            register=lambda _session_id: self.fail("must not register"),
            launch=lambda _candidate: self.fail("must not launch"),
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("no compatible continuity alternate", result["continuity"]["reason"])

    def test_unproven_group_death_quarantines(self):
        result = RUNNER._advance_continuity(
            self.manifest(), exit_code=75, group_dead=False,
            candidates=self.candidates(),
            register=lambda _session_id: self.fail("must not register"),
            launch=lambda _candidate: self.fail("must not launch"),
        )
        self.assertEqual("quarantined", result["status"])

    def test_non_retryable_failure_preserves_legacy_blocked_result(self):
        result = RUNNER._advance_continuity(
            self.manifest(), exit_code=2, group_dead=True,
            candidates=self.candidates(),
            register=lambda _session_id: self.fail("must not register"),
            launch=lambda _candidate: self.fail("must not launch"),
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("non-retryable exit", result["continuity"]["reason"])

    def test_runtime_exhaustion_is_terminal_not_a_delegate_continuity_retry(self):
        invalid = self.continuity()
        invalid["retryableExitCodes"] = [78]
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)
        result = RUNNER._advance_continuity(
            self.manifest(), exit_code=78, group_dead=True,
            candidates=self.candidates(),
            register=lambda _session_id: self.fail("must not register"),
            launch=lambda _candidate: self.fail("must not launch"),
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("RUNTIME_EXHAUSTED", result["continuity"]["reason"])

    def test_concurrent_resume_is_idempotent_and_completed_actions_are_hashes_only(self):
        manifest = self.manifest()
        manifest["continuity"]["state"] = "replacement_running"
        launched = []
        result = RUNNER._advance_continuity(
            manifest, exit_code=75, group_dead=True,
            candidates=self.candidates(),
            register=launched.append,
            launch=launched.append,
        )
        self.assertIs(manifest, result)
        self.assertEqual([], launched)
        self.assertNotIn("assignment", str(result["continuity"]).lower())

    def test_registration_failure_is_compensated_and_next_attempt_can_continue(self):
        def fail_registration(_session_id):
            raise RuntimeError("closed")

        result = RUNNER._advance_continuity(
            self.manifest(), exit_code=75, group_dead=True,
            candidates=self.candidates(),
            register=fail_registration,
            launch=lambda _candidate: self.fail("must not launch"),
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("replacement learning registration failed", result["continuity"]["reason"])
        self.assertEqual([], result["continuity"]["participants"])

    def test_contract_rejects_raw_links_secrets_and_duplicate_candidates(self):
        invalid = self.continuity()
        invalid["trackerUrl"] = "https://github.example/secret"
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)
        invalid = self.continuity()
        invalid["alternates"].append(dict(invalid["alternates"][1]))
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)

    def test_contract_rejects_candidate_resumption_not_bound_to_frozen_hashes(self):
        invalid = self.continuity()
        invalid["alternates"][1]["resumption"] = dict(invalid["alternates"][1]["resumption"])
        invalid["alternates"][1]["resumption"]["checkpoint"] = "changed"
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)

    def test_real_signal_status_can_trigger_failover_and_resumption_prevents_replay(self):
        launcher = Path(self.temporary.name) / "signal-launcher.py"
        actions = Path(self.temporary.name) / "actions"
        received = Path(self.temporary.name) / "received"
        launcher.write_text(
            "#!/usr/bin/env python3\nimport json, os, signal\nfrom pathlib import Path\n"
            f"actions=Path({str(actions)!r}); received=Path({str(received)!r})\n"
            "if 'OMNIROOT_RESUMPTION' not in os.environ:\n"
            " actions.write_text('action-1\\n'); os.kill(os.getpid(), signal.SIGTERM)\n"
            "state=json.loads(os.environ['OMNIROOT_RESUMPTION'])\n"
            "print(os.environ['OMNIROOT_RESUMPTION'])\n"
            "assert state['completedActions'] == ['action-1']\n"
            "if 'action-1' not in state['completedActions']:\n actions.write_text(actions.read_text()+'action-1\\n')\n"
            "received.write_text(state['checkpoint'])\n",
            encoding="utf-8",
        )
        launcher.chmod(0o700)
        command, identity = RUNNER._resolved_executable([str(launcher)])
        diagnostic = self.state / "diagnostics/signal.json"
        process = self.state / "processes/signal.json"
        manifest_path = self.state / "runs/signal.json"
        continuity = self.continuity()
        continuity["retryableExitCodes"] = [-15]
        manifest = self.manifest()
        manifest["continuity"] = RUNNER._continuity_contract(continuity)
        RUNNER._write_json(manifest_path, manifest)
        learning = Path(self.temporary.name) / "learning-signal.json"
        from scripts.agents.learning_session import create_runtime
        create_runtime(learning, "root")
        environment = {
            "OMNIROOT_CONTINUITY": __import__("json").dumps(continuity["alternates"]),
            "OMNIROOT_LEARNING_STATE": str(learning), "OMNIROOT_LEARNING_ROOT": "root",
            "OMNIROOT_INVOCATION_MODE": "direct", "OMNIROOT_CREDENTIAL_MODE": "launcher",
            "OMNIROOT_LAUNCHER_ARGC": "1",
        }
        arguments = [str(diagnostic), str(process), str(manifest_path), "10",
                     *(str(value) for value in identity), "--", *command]
        with patch.dict(os.environ, environment, clear=False):
            result = RUNNER._supervise_command(arguments)
        self.assertEqual(0, result)
        self.assertEqual("action-1\n", actions.read_text(encoding="utf-8"))
        self.assertEqual("checkpoint-1", received.read_text(encoding="utf-8"))
        self.assertNotIn("OMNIROOT_RESUMPTION", os.environ)
        persisted = manifest_path.read_text(encoding="utf-8")
        self.assertNotIn("owner-approved", persisted)
        self.assertNotIn("checkpoint-1", persisted)
        self.assertNotIn("action-1", persisted)
        diagnostic_text = diagnostic.read_text(encoding="utf-8")
        self.assertNotIn("owner-approved", diagnostic_text)
        self.assertNotIn("checkpoint-1", diagnostic_text)
        self.assertNotIn("action-1", diagnostic_text)

    def test_contract_caps_total_attempts_and_writers_at_four(self):
        invalid = self.continuity()
        invalid["maxAttempts"] = 5
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)
        invalid = self.continuity()
        invalid["alternates"] *= 2
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)
        invalid = self.continuity()
        invalid["retryableExitCodes"] = [-65]
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._continuity_contract(invalid)

    def test_expired_overall_deadline_blocks_before_registration_or_launch(self):
        manifest = self.manifest()
        manifest["deadline"] = "2000-01-01T00:00:00+00:00"
        result = RUNNER._advance_continuity(
            manifest, exit_code=75, group_dead=True,
            candidates=self.candidates(),
            register=lambda _session_id: self.fail("must not register"),
            launch=lambda _candidate: self.fail("must not launch"),
            now=lambda: datetime(2030, 1, 1, tzinfo=UTC),
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("overall deadline expired", result["continuity"]["reason"])

    def test_expired_supervisor_clears_private_resumption_before_popen(self):
        manifest = self.manifest()
        manifest["deadline"] = "2000-01-01T00:00:00+00:00"
        manifest_path = self.state / "runs/expired.json"
        RUNNER._write_json(manifest_path, manifest)
        with patch.dict(os.environ, {"OMNIROOT_RESUMPTION": "private-envelope"}, clear=False):
            with patch.object(RUNNER.subprocess, "Popen", side_effect=AssertionError("must not launch")):
                diagnostic, result = RUNNER._supervised_diagnostic(
                    ["unused"], diagnostic_path=self.state / "diagnostics/expired.json",
                    process_path=self.state / "processes/expired.json", manifest_path=manifest_path,
                    manifest=manifest, timeout_seconds=10, active_session="failed",
                    learning_state="unused", learning_root="root",
                )
            self.assertIsNone(diagnostic)
            self.assertEqual(1, result)
            self.assertNotIn("OMNIROOT_RESUMPTION", os.environ)

    def test_deadline_expiring_during_backoff_blocks_without_launch(self):
        manifest = self.manifest()
        start = datetime(2029, 12, 31, 23, 59, 59, tzinfo=UTC)
        manifest["deadline"] = (start + timedelta(seconds=1)).isoformat()
        clock = [start]
        compensated = []

        def sleep(_seconds):
            clock[0] += timedelta(seconds=2)

        result = RUNNER._advance_continuity(
            manifest, exit_code=75, group_dead=True,
            candidates=self.candidates(), register=lambda _session_id: None,
            launch=lambda _candidate: self.fail("must not launch"),
            compensate=compensated.append, sleep=sleep, now=lambda: clock[0],
        )
        self.assertEqual("blocked", result["status"])
        self.assertEqual("overall deadline expired", result["continuity"]["reason"])
        self.assertEqual(["high"], compensated)

    def test_legacy_manifest_has_no_continuity_behavior(self):
        manifest = {"status": "running"}
        self.assertIsNone(RUNNER._continuity_contract(None))
        self.assertNotIn("continuity", manifest)

    def test_durable_supervisor_replaces_retryable_process_without_owner_input(self):
        launcher = Path(self.temporary.name) / "launcher.py"
        counter = Path(self.temporary.name) / "counter"
        launcher.write_text(
            "#!/usr/bin/env python3\nfrom pathlib import Path\n"
            f"p=Path({str(counter)!r}); n=int(p.read_text() if p.exists() else '0')+1; p.write_text(str(n)); raise SystemExit(75 if n == 1 else 0)\n",
            encoding="utf-8",
        )
        launcher.chmod(0o700)
        command, identity = RUNNER._resolved_executable([str(launcher)])
        diagnostic = self.state / "diagnostics/run.json"
        process = self.state / "processes/run.json"
        manifest_path = self.state / "runs/run.json"
        manifest = self.manifest()
        manifest["continuity"]["participants"] = [{
            "identitySha256": RUNNER._sha256("failed"),
            "sessionSha256": RUNNER._sha256("failed-session"),
            "capability": "most-intelligent",
        }]
        RUNNER._write_json(manifest_path, manifest)
        learning = Path(self.temporary.name) / "learning.json"
        from scripts.agents.learning_session import create_runtime, register_runtime_participant
        create_runtime(learning, "root")
        register_runtime_participant(learning, "root", "failed-session")
        environment = {
            "OMNIROOT_CONTINUITY": __import__("json").dumps(self.candidates()),
            "OMNIROOT_LEARNING_STATE": str(learning),
            "OMNIROOT_LEARNING_ROOT": "root",
            "OMNIROOT_INVOCATION_MODE": "direct",
            "OMNIROOT_CREDENTIAL_MODE": "launcher",
            "OMNIROOT_LAUNCHER_ARGC": "1",
        }
        arguments = [str(diagnostic), str(process), str(manifest_path), "10",
                     *(str(value) for value in identity), "--", *command]
        with patch.dict(os.environ, environment, clear=False):
            result = RUNNER._supervise_command(arguments)
        final = RUNNER._load_json(manifest_path)
        self.assertEqual(0, result)
        self.assertEqual("review", final["continuity"]["state"])
        self.assertEqual(2, final["continuity"]["attempt"])
        self.assertEqual(2, len(final["continuity"]["participants"]))
        self.assertEqual(0, RUNNER._load_json(diagnostic)["exitCode"])
        self.assertEqual("2", counter.read_text(encoding="utf-8"))

    def test_durable_supervisor_changes_candidate_invocation_without_persisting_it(self):
        launcher = Path(self.temporary.name) / "launcher.py"
        invocations = Path(self.temporary.name) / "invocations"
        launcher.write_text(
            "#!/usr/bin/env python3\nimport sys\nfrom pathlib import Path\n"
            f"p=Path({str(invocations)!r}); old=p.read_text() if p.exists() else ''; p.write_text(old+'|'.join(sys.argv[1:])+'\\n'); raise SystemExit(75 if not old else 0)\n",
            encoding="utf-8",
        )
        launcher.chmod(0o700)
        command, identity = RUNNER._resolved_executable([str(launcher)])
        diagnostic = self.state / "diagnostics/run.json"
        process = self.state / "processes/run.json"
        manifest_path = self.state / "runs/run.json"
        manifest = self.manifest()
        manifest["continuity"]["participants"] = [{
            "identitySha256": RUNNER._sha256("failed"),
            "sessionSha256": RUNNER._sha256("failed-session"),
            "capability": "most-intelligent",
        }]
        RUNNER._write_json(manifest_path, manifest)
        learning = Path(self.temporary.name) / "learning.json"
        from scripts.agents.learning_session import create_runtime, register_runtime_participant
        create_runtime(learning, "root")
        register_runtime_participant(learning, "root", "failed-session")
        candidates = self.candidates()
        environment = {
            "OMNIROOT_CONTINUITY": __import__("json").dumps(candidates),
            "OMNIROOT_LEARNING_STATE": str(learning),
            "OMNIROOT_LEARNING_ROOT": "root",
            "OMNIROOT_INITIAL_SESSION": "failed-session",
            "OMNIROOT_INVOCATION_MODE": "gateway",
            "OMNIROOT_CREDENTIAL_MODE": "launcher",
            "OMNIROOT_LAUNCHER_ARGC": "1",
        }
        initial = [*command, "initial-route", "--port", "20128", "--", "--candidate", "initial"]
        arguments = [str(diagnostic), str(process), str(manifest_path), "10",
                     *(str(value) for value in identity), "--", *initial]
        with patch.dict(os.environ, environment, clear=False):
            result = RUNNER._supervise_command(arguments)
        self.assertEqual(0, result)
        self.assertEqual([
            "initial-route|--port|20128|--|--candidate|initial",
            "high-route|--port|20128|--|--candidate|high",
        ], invocations.read_text(encoding="utf-8").splitlines())
        persisted = manifest_path.read_text(encoding="utf-8")
        self.assertNotIn("high-route", persisted)
        self.assertNotIn("--candidate", persisted)


if __name__ == "__main__":
    unittest.main()
