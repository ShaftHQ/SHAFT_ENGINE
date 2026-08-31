import importlib.util
import tempfile
import unittest
import os
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
        return {
            "requiredCapability": "most-intelligent",
            "maxAttempts": 3,
            "retryableExitCodes": [75],
            "backoffSeconds": 0,
            "authoritySha256": "a" * 64,
            "checkpointSha256": "b" * 64,
            "completedActionSha256s": ["c" * 64],
            "trackerUrlSha256": "d" * 64,
            "pullRequestUrlSha256": "e" * 64,
            "alternates": [
                {"identity": "replacement-low", "sessionId": "low", "capability": "default"},
                {"identity": "replacement-high", "sessionId": "high", "capability": "most-intelligent"},
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
        self.assertEqual("b" * 64, result["continuity"]["checkpointSha256"])
        self.assertEqual(["c" * 64], result["continuity"]["completedActionSha256s"])
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

    def test_concurrent_resume_is_idempotent_and_completed_actions_are_hashes_only(self):
        manifest = self.manifest()
        manifest["continuity"]["state"] = "replacement_running"
        launched = []
        result = RUNNER._advance_continuity(
            manifest, exit_code=75, group_dead=True,
            candidates=self.candidates(),
            register=lambda session_id: launched.append(session_id),
            launch=lambda candidate: launched.append(candidate),
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


if __name__ == "__main__":
    unittest.main()
