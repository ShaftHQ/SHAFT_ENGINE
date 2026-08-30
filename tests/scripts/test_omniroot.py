"""Focused contracts for the optional OmniRoot transport."""

from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import tempfile
import unittest
from datetime import UTC, datetime, timedelta
from pathlib import Path
from urllib.error import HTTPError


ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/omniroot"
RUNNER_PATH = SKILL / "scripts/runner.py"
SPEC = importlib.util.spec_from_file_location("omniroot_runner", RUNNER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("OmniRoot runner could not be loaded")
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class _Response:
    status = 200

    def __init__(self, payload: bytes = b'{"status":"ok","build":"test"}'):
        self.payload = payload

    def read(self, limit: int = -1) -> bytes:
        return self.payload if limit < 0 else self.payload[:limit]

    def __enter__(self):
        return self

    def __exit__(self, *_):
        return False


class _Process:
    pid = 4242

    def poll(self):
        return None


class OmniRootWorkflowTest(unittest.TestCase):
    def test_canonical_workflows_are_exact_and_transport_is_orthogonal(self):
        text = (ROOT / "chaos-engine/references/execution-workflows.md").read_text(encoding="utf-8")
        for workflow in (
            "SOLO",
            "ORCHESTRATOR + SINGLE IMPLEMENTER",
            "ORCHESTRATOR + PARALLEL IMPLEMENTERS",
        ):
            self.assertIn(workflow, text)
        self.assertIn("transport", text.casefold())
        self.assertIn("OmniRoute is absent", text)


class OmniRootProbeTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.config = self.root / "omniroot.json"
        self.launcher = self.root / "launcher"
        self.launcher.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        self.launcher.chmod(0o700)

    def _config(self, *, expired: bool = False) -> None:
        now = datetime.now(UTC)
        expires = now - timedelta(minutes=1) if expired else now + timedelta(minutes=20)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "environment"},
            "attestation": {
                "schemaVersion": 1,
                "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64,
                "serverBuild": "test",
                "verifiedAt": now.isoformat(),
                "expiresAt": expires.isoformat(),
                "noCostConfirmed": True,
                "noPaidFallbackConfirmed": True,
                "privacyConfirmed": True,
                "termsConfirmed": True,
                "deniedProbeTargetSha256": "c" * 64,
                "deniedProbeConfirmed": True,
                "deniedProbeTargetKnownExistingConfirmed": True,
            },
        }), encoding="utf-8")

    def test_absent_gateway_is_normal_not_an_exception(self):
        result = RUNNER.probe(config_path=self.config, opener=lambda *_, **__: (_ for _ in ()).throw(OSError("down")))
        self.assertEqual("ABSENT", result["state"])
        self.assertEqual(RUNNER.DEFAULT_ENDPOINT, result["endpoint"])

    def test_gateway_requires_key_and_current_attestation_before_ready(self):
        self._config()
        health = lambda *_, **__: _Response()
        result = RUNNER.probe(config_path=self.config, opener=health, environ={})
        self.assertEqual("UNAUTHENTICATED", result["state"])
        result = RUNNER.probe(
            config_path=self.config,
            opener=health,
            environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
        )
        self.assertEqual("READY", result["state"])
        self.assertNotIn("present-but-never-recorded", json.dumps(result))

    def test_protected_operator_launcher_needs_no_parent_endpoint_key(self):
        now = datetime.now(UTC)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "launcher"},
            "attestation": {
                "schemaVersion": 1, "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64, "deniedProbeTargetSha256": "c" * 64,
                "serverBuild": "test", "verifiedAt": now.isoformat(),
                "expiresAt": (now + timedelta(minutes=20)).isoformat(),
                "noCostConfirmed": True, "noPaidFallbackConfirmed": True,
                "privacyConfirmed": True, "termsConfirmed": True, "deniedProbeConfirmed": True,
                "deniedProbeTargetKnownExistingConfirmed": True,
            },
        }), encoding="utf-8")
        result = RUNNER.probe(config_path=self.config, opener=lambda *_, **__: _Response(), environ={})
        self.assertEqual("READY", result["state"])
        self.assertNotIn("opaque-profile", json.dumps(result))

    def test_expired_or_oversized_gateway_reply_fails_closed(self):
        self._config(expired=True)
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: _Response(),
            environ={"OMNIROUTE_API_KEY": "present"},
        )
        self.assertEqual("ROUTE_UNQUALIFIED", result["state"])
        self._config()
        huge = b"{" + (b"x" * (RUNNER.MAX_RESPONSE_BYTES + 1)) + b"}"
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: _Response(huge),
            environ={"OMNIROUTE_API_KEY": "present"},
        )
        self.assertEqual("UNHEALTHY", result["state"])

    def test_exhausted_gateway_has_a_distinct_non_ready_state(self):
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: (_ for _ in ()).throw(
                HTTPError(RUNNER.DEFAULT_ENDPOINT, 429, "quota", None, None)
            ),
            environ={"OMNIROUTE_API_KEY": "present"},
        )
        self.assertEqual("RUNTIME_EXHAUSTED", result["state"])


class OmniRootRunnerTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.worktree = self.root / "worktree"
        self.worktree.mkdir()
        subprocess.run(["git", "init", "-q", str(self.worktree)], check=True)
        subprocess.run(["git", "-C", str(self.worktree), "config", "user.email", "test@example.invalid"], check=True)
        subprocess.run(["git", "-C", str(self.worktree), "config", "user.name", "test"], check=True)
        (self.worktree / "README.md").write_text("test\n", encoding="utf-8")
        subprocess.run(["git", "-C", str(self.worktree), "add", "README.md"], check=True)
        subprocess.run(["git", "-C", str(self.worktree), "commit", "-qm", "init"], check=True)
        self.state = self.root / "state"
        self.config = self.root / "omniroot.json"
        self.launcher = self.root / "launcher"
        self.launcher.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        self.launcher.chmod(0o700)
        now = datetime.now(UTC)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "environment"},
            "attestation": {
                "schemaVersion": 1,
                "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64,
                "serverBuild": "test",
                "verifiedAt": now.isoformat(),
                "expiresAt": (now + timedelta(minutes=20)).isoformat(),
                "noCostConfirmed": True,
                "noPaidFallbackConfirmed": True,
                "privacyConfirmed": True,
                "termsConfirmed": True,
                "deniedProbeTargetSha256": "c" * 64,
                "deniedProbeConfirmed": True,
                "deniedProbeTargetKnownExistingConfirmed": True,
            },
        }), encoding="utf-8")

    def test_dispatch_is_fail_closed_and_writes_private_manifest(self):
        launched = []
        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        manifest = RUNNER.dispatch(
            run_id="run-1",
            worktree=self.worktree,
            state_dir=self.state,
            config_path=self.config,
            target="host-cli",
            delegate_args=["--task", "bounded"],
            opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", "OMNIROUTE_API_KEY": "secret"},
            popen=popen,
            process_identity=lambda _: "identity",
        )
        self.assertEqual("running", manifest["status"])
        self.assertEqual([str(self.launcher), "opaque-profile", "host-cli"], launched[0][0][:3])
        self.assertFalse(launched[0][1].get("shell", True))
        self.assertEqual(str(self.worktree), launched[0][1]["cwd"])
        self.assertNotIn("secret", json.dumps(manifest))
        self.assertEqual("ORCHESTRATOR + SINGLE IMPLEMENTER", manifest["workflow"])
        self.assertEqual("task-unspecified", manifest["taskId"])
        self.assertEqual("success", RUNNER.complete(
            run_id="run-2", state_dir=self.state, exit_code=0, changed_paths=[],
            learning_disposition="nothing-durable",
        )["outcome"])
        path = self.state / "runs/run-1.json"
        self.assertEqual(0o600, path.stat().st_mode & 0o777)

    def test_direct_launcher_receives_only_configured_argv_and_delegate_args(self):
        config = json.loads(self.config.read_text(encoding="utf-8"))
        config["launcher"]["invocationMode"] = "direct"
        self.config.write_text(json.dumps(config), encoding="utf-8")
        launched = []
        runtime = {
            "HOME": "/home/agent", "USERPROFILE": "C:/Users/agent",
            "SystemRoot": "C:/Windows", "TEMP": "/tmp", "TMP": "/tmp",
        }

        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        RUNNER.dispatch(
            run_id="direct-1",
            worktree=self.worktree,
            state_dir=self.state,
            config_path=self.config,
            target="host-cli",
            delegate_args=["--task", "bounded"],
            opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", **runtime, "OMNIROUTE_API_KEY": "secret",
                     "AWS_SECRET_ACCESS_KEY": "must-not-leak"},
            popen=popen,
            process_identity=lambda _: "identity",
        )
        self.assertEqual(
            [str(self.launcher), "opaque-profile", "--task", "bounded"],
            launched[0][0],
        )
        expected_runtime = ({"HOME": runtime["HOME"], "TEMP": runtime["TEMP"], "TMP": runtime["TMP"]}
                            if os.name == "posix"
                            else {name: runtime[name] for name in ("USERPROFILE", "SystemRoot", "TEMP", "TMP")})
        self.assertEqual({"PATH", "OMNIROUTE_API_KEY", *expected_runtime}, set(launched[0][1]["env"]))
        self.assertEqual(expected_runtime, {name: launched[0][1]["env"][name] for name in expected_runtime})
        self.assertEqual("secret", launched[0][1]["env"]["OMNIROUTE_API_KEY"])
        self.assertNotIn("AWS_SECRET_ACCESS_KEY", launched[0][1]["env"])

    def test_direct_protected_launcher_gets_runtime_locator_without_credentials(self):
        config = json.loads(self.config.read_text(encoding="utf-8"))
        config["launcher"].update({"invocationMode": "direct", "credentialMode": "launcher"})
        self.config.write_text(json.dumps(config), encoding="utf-8")
        launched = []

        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        RUNNER.dispatch(
            run_id="direct-protected-1", worktree=self.worktree, state_dir=self.state,
            config_path=self.config, target="host-cli", delegate_args=["--task", "bounded"],
            opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", "HOME": "/home/agent", "OMNIROUTE_API_KEY": "secret"},
            popen=popen, process_identity=lambda _: "identity",
        )
        self.assertEqual("/home/agent", launched[0][1]["env"]["HOME"])
        self.assertNotIn("OMNIROUTE_API_KEY", launched[0][1]["env"])

    def test_manifest_freezes_full_delegate_contract_without_private_assignment_data(self):
        manifest = RUNNER.dispatch(
            run_id="full-1", worktree=self.worktree, state_dir=self.state, config_path=self.config,
            target="host-cli", delegate_args=["--task", "bounded"], opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", "OMNIROUTE_API_KEY": "secret"}, popen=lambda *_a, **_k: _Process(),
            process_identity=lambda _: "identity", task_id="task-1", root_session_id="root-1",
            base_commit="a" * 40, integration_branch="ChaosEngine/test",
            integration_worktree=self.worktree, delegate={"identity": "writer-a", "role": "implementer",
                "capability": "default", "assignment": "opaque task", "pathOwnership": ["docs/file.md"]},
            cadence_seconds=600, deadline="2030-01-01T00:00:00+00:00",
        )
        required = {"schemaVersion", "runId", "taskId", "workflow", "rootSessionId", "baseCommit",
                    "integration", "qualification", "delegate", "pid", "processIdentity", "status",
                    "cadenceSeconds", "deadline", "timestamps", "head", "receipt"}
        self.assertTrue(required <= manifest.keys())
        self.assertNotIn("opaque task", json.dumps(manifest))

    def test_dispatch_rejects_dirty_worktree_and_overlapping_or_secret_ownership(self):
        (self.worktree / "README.md").write_text("dirty\n", encoding="utf-8")
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.dispatch(run_id="dirty", worktree=self.worktree, state_dir=self.state, config_path=self.config,
                target="host-cli", delegate_args=[], opener=lambda *_, **__: _Response(),
                environ={"OMNIROUTE_API_KEY": "secret"})
        subprocess.run(["git", "-C", str(self.worktree), "checkout", "--", "README.md"], check=True)
        for run_id, paths in (("secret", [".env"]), ("private", ["private/key.txt"])):
            with self.assertRaises(RUNNER.OmniRootError):
                RUNNER.dispatch(run_id=run_id, worktree=self.worktree, state_dir=self.state, config_path=self.config,
                    target="host-cli", delegate_args=[], opener=lambda *_, **__: _Response(),
                    environ={"OMNIROUTE_API_KEY": "secret"}, delegate={"pathOwnership": paths})

    def test_dispatch_rejects_non_ready_and_stale_cancel_quarantines(self):
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.dispatch(
                run_id="run-1", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: (_ for _ in ()).throw(OSError("down")), environ={},
            )
        RUNNER._write_json(self.state / "runs/run-1.json", {
            "schemaVersion": 1, "runId": "run-1", "status": "running",
            "pid": 4242, "processIdentity": "old", "worktree": str(self.worktree),
        })
        status = RUNNER.cancel("run-1", self.state, process_identity=lambda _: "new")
        self.assertEqual("quarantined", status["status"])

    def test_completion_receipt_is_terminal_and_redacted(self):
        receipt = RUNNER.complete(
            run_id="run-1", state_dir=self.state, exit_code=0,
            changed_paths=["chaos-engine/skills/omniroot/SKILL.md"],
            learning_disposition="nothing-durable", head="a" * 40, clean=True,
            checks=["python3 -m unittest"], blockers=[], adjacent_findings=[],
        )
        self.assertEqual("completed", receipt["status"])
        self.assertEqual("nothing-durable", receipt["learningDisposition"])
        self.assertEqual("success", receipt["outcome"])
        self.assertEqual("a" * 40, receipt["head"])
        self.assertEqual(["python3 -m unittest"], receipt["checks"])
        self.assertEqual(0o600, (self.state / "receipts/run-1.json").stat().st_mode & 0o777)


if __name__ == "__main__":
    unittest.main()
