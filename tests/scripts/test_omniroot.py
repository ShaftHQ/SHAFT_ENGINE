"""Focused contracts for the optional OmniRoot transport."""

from __future__ import annotations

import importlib.util
import io
import inspect
import json
import os
import signal
import subprocess
import sys
import tempfile
import unittest
from unittest import mock
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
        if os.name == "posix":
            self.config.chmod(0o600)

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
        if os.name == "posix":
            self.config.chmod(0o600)
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
    def _dispatch(self, **kwargs):
        contract = {
            "task_id": "task-1", "workflow": "ORCHESTRATOR + SINGLE IMPLEMENTER",
            "root_session_id": "root-1", "base_commit": RUNNER._git(self.worktree, "rev-parse", "HEAD"),
            "integration_branch": "integration", "integration_worktree": self.integration,
            "delegate": {"identity": "writer", "role": "implementer", "capability": "lower",
                         "assignment": "bounded", "pathOwnership": ["docs"]},
            "cadence_seconds": 600, "deadline": "2030-01-01T00:00:00+00:00", "timeout_seconds": 60,
            "learning_state": self.learning_state, "learning_root_session_id": "root-1",
            "delegate_session_id": kwargs.get("run_id", "delegate-1"),
        }
        contract.update(kwargs)
        return RUNNER.dispatch(**contract)

    def test_process_identity_has_no_hard_coded_posix_absolute_path(self):
        self.assertNotIn('"/proc/', inspect.getsource(RUNNER.process_identity))

    def test_process_identity_parses_names_containing_spaces_and_parentheses(self):
        line = "42 (worker name) tricky) S " + " ".join(str(i) for i in range(1, 30))
        self.assertEqual("19", RUNNER._linux_start_time(line))

    def test_executable_identity_detects_replacement_before_launch(self):
        qualified = RUNNER._resolved_executable([str(self.launcher)])
        self.assertIsNotNone(qualified)
        argv, identity = qualified
        replacement = self.root / "replacement"
        replacement.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        replacement.chmod(0o700)
        os.replace(replacement, self.launcher)
        self.assertFalse(RUNNER._same_executable(argv, identity))

    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.repository = self.root / "repository"
        self.worktree = self.root / "worktree"
        self.integration = self.root / "integration"
        self.repository.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repository)], check=True)
        subprocess.run(["git", "-C", str(self.repository), "config", "user.email", "test@example.invalid"], check=True)
        subprocess.run(["git", "-C", str(self.repository), "config", "user.name", "test"], check=True)
        (self.repository / "README.md").write_text("test\n", encoding="utf-8")
        subprocess.run(["git", "-C", str(self.repository), "add", "README.md"], check=True)
        subprocess.run(["git", "-C", str(self.repository), "commit", "-qm", "init"], check=True)
        subprocess.run(["git", "-C", str(self.repository), "worktree", "add", "-q", "-b", "delegate", str(self.worktree)], check=True)
        subprocess.run(["git", "-C", str(self.repository), "worktree", "add", "-q", "-b", "integration", str(self.integration)], check=True)
        self.learning_state = self.root / "learning.json"
        from scripts.agents.learning_session import create_runtime
        create_runtime(self.learning_state, "root-1")
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
        if os.name == "posix":
            self.config.chmod(0o600)

    def test_dispatch_is_fail_closed_and_writes_private_manifest(self):
        launched = []
        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        manifest = self._dispatch(
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
        self.assertEqual("opaque-profile", launched[0][0][1])
        self.assertEqual("host-cli", launched[0][0][2])
        self.assertIn("launchers", Path(launched[0][0][0]).parts)
        self.assertFalse(launched[0][1].get("shell", True))
        self.assertEqual(subprocess.PIPE, launched[0][1]["stdout"])
        self.assertEqual(subprocess.PIPE, launched[0][1]["stderr"])
        self.assertEqual(str(self.worktree), launched[0][1]["cwd"])
        self.assertNotIn("secret", json.dumps(manifest))
        self.assertEqual("ORCHESTRATOR + SINGLE IMPLEMENTER", manifest["workflow"])
        self.assertEqual("task-1", manifest["taskId"])
        path = self.state / "runs/run-1.json"
        self.assertEqual(0o600, path.stat().st_mode & 0o777)

    def test_diagnostics_are_bounded_redacted_and_private(self):
        secret = "route-token-value"
        noisy = (("OMNIROUTE_API_KEY=" + secret + "\n") + ("x" * 70000)).encode()

        class Finished:
            returncode = 7
            stdout = io.BytesIO(noisy)
            stderr = io.BytesIO(b"Authorization: Bearer private-token\nfailed\n")

            def wait(self, timeout=None):
                self.timeout = timeout
                return self.returncode

            def poll(self):
                return self.returncode

        path = self.state / "diagnostics/run-1.json"
        RUNNER._collect_diagnostics(Finished(), path, timeout_seconds=30)
        diagnostic = json.loads(path.read_text(encoding="utf-8"))
        serialized = json.dumps(diagnostic)
        self.assertNotIn(secret, serialized)
        self.assertNotIn("private-token", serialized)
        self.assertLessEqual(len(diagnostic["stdout"].encode()), RUNNER.MAX_DIAGNOSTIC_BYTES)
        self.assertTrue(diagnostic["stdoutTruncated"])
        self.assertEqual(7, diagnostic["exitCode"])
        self.assertEqual(0o600, path.stat().st_mode & 0o777)

    def test_diagnostics_redact_basic_json_and_split_cli_secrets(self):
        raw = (b"Authorization: Basic abc123\n"
               b'{"token":"json-token","password": "json-pass", "api_key":"json-key"}\n'
               b"tool --secret split-secret --api-key=inline-secret\n")
        redacted, _ = RUNNER._redact_diagnostic(raw)
        for secret in ("abc123", "json-token", "json-pass", "json-key", "split-secret", "inline-secret"):
            self.assertNotIn(secret, redacted)
        json_line = redacted.splitlines()[1]
        self.assertEqual("[REDACTED]", json.loads(json_line)["token"])

    def test_missing_config_is_normal_fallback_and_never_launches(self):
        launched = []
        result = RUNNER.probe(
            config_path=self.root / "missing.json", opener=lambda *_, **__: _Response(), environ={}
        )
        self.assertEqual("ROUTE_UNQUALIFIED", result["state"])
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(
                run_id="missing", worktree=self.worktree, state_dir=self.state,
                config_path=self.root / "missing.json", target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={},
                popen=lambda *args, **kwargs: launched.append((args, kwargs)),
            )
        self.assertEqual([], launched)

    def test_direct_launcher_receives_only_configured_argv_and_delegate_args(self):
        config = json.loads(self.config.read_text(encoding="utf-8"))
        config["launcher"]["invocationMode"] = "direct"
        self.config.write_text(json.dumps(config), encoding="utf-8")
        if os.name == "posix":
            self.config.chmod(0o600)
        launched = []
        runtime = {
            "HOME": "/home/agent", "USERPROFILE": "C:/Users/agent",
            "SystemRoot": "C:/Windows", "TEMP": "/tmp", "TMP": "/tmp",
        }

        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        self._dispatch(
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
        self.assertEqual(["opaque-profile", "--task", "bounded"], launched[0][0][1:])
        self.assertIn("launchers", Path(launched[0][0][0]).parts)
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
        if os.name == "posix":
            self.config.chmod(0o600)
        launched = []

        def popen(argv, **kwargs):
            launched.append((argv, kwargs))
            return _Process()

        self._dispatch(
            run_id="direct-protected-1", worktree=self.worktree, state_dir=self.state,
            config_path=self.config, target="host-cli", delegate_args=["--task", "bounded"],
            opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", "HOME": "/home/agent", "OMNIROUTE_API_KEY": "secret"},
            popen=popen, process_identity=lambda _: "identity",
        )
        self.assertEqual("/home/agent", launched[0][1]["env"]["HOME"])
        self.assertNotIn("OMNIROUTE_API_KEY", launched[0][1]["env"])

    def test_manifest_freezes_full_delegate_contract_without_private_assignment_data(self):
        manifest = self._dispatch(
            run_id="full-1", worktree=self.worktree, state_dir=self.state, config_path=self.config,
            target="host-cli", delegate_args=["--task", "bounded"], opener=lambda *_, **__: _Response(),
            environ={"PATH": "/bin", "OMNIROUTE_API_KEY": "secret"}, popen=lambda *_a, **_k: _Process(),
            process_identity=lambda _: "identity", task_id="task-1", root_session_id="root-1",
            base_commit=RUNNER._git(self.worktree, "rev-parse", "HEAD"), integration_branch="integration",
            integration_worktree=self.integration, delegate={"identity": "writer-a", "role": "implementer",
                "capability": "default", "assignment": "opaque task", "pathOwnership": ["docs/file.md"]},
            cadence_seconds=600, deadline="2030-01-01T00:00:00+00:00",
        )
        required = {"schemaVersion", "runId", "taskId", "workflow", "rootSessionId", "baseCommit",
                    "integration", "qualification", "delegate", "monitor", "status",
                    "cadenceSeconds", "deadline", "timestamps", "head", "receipt"}
        self.assertLessEqual(required, manifest.keys())
        self.assertNotIn("opaque task", json.dumps(manifest))

    def test_dispatch_rejects_dirty_worktree_and_overlapping_or_secret_ownership(self):
        (self.worktree / "README.md").write_text("dirty\n", encoding="utf-8")
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(run_id="dirty", worktree=self.worktree, state_dir=self.state, config_path=self.config,
                target="host-cli", delegate_args=[], opener=lambda *_, **__: _Response(),
                environ={"OMNIROUTE_API_KEY": "secret"})
        subprocess.run(["git", "-C", str(self.worktree), "checkout", "--", "README.md"], check=True)
        for run_id, paths in (("secret", [".env"]), ("private", ["private/key.txt"])):
            with self.assertRaises(RUNNER.OmniRootError):
                self._dispatch(run_id=run_id, worktree=self.worktree, state_dir=self.state, config_path=self.config,
                    target="host-cli", delegate_args=[], opener=lambda *_, **__: _Response(),
                    environ={"OMNIROUTE_API_KEY": "secret"}, delegate={"pathOwnership": paths})

    def test_dispatch_rejects_ancestor_descendant_ownership_overlap(self):
        RUNNER._write_json(self.state / "runs/existing.json", {
            "schemaVersion": 1, "runId": "existing", "status": "running",
            "delegate": {"pathOwnership": ["docs"]},
        })
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(
                run_id="nested", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                delegate={"pathOwnership": ["docs/guide.md"]}, popen=lambda *_a, **_k: _Process(),
            )

    def test_dispatch_reservation_is_interprocess_atomic(self):
        runs = RUNNER._private_directory(self.state / "runs")
        lock = runs / ".reservation.lock"
        descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
        os.close(descriptor)
        try:
            with self.assertRaises(RUNNER.OmniRootError):
                with RUNNER._reservation(self.state):
                    self.fail("reservation must not be shared")
        finally:
            lock.unlink()

    def test_state_root_rejects_symlink_component(self):
        real = self.root / "real-state"
        real.mkdir(mode=0o700)
        linked = self.root / "linked-state"
        linked.symlink_to(real, target_is_directory=True)
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._write_json(linked / "runs/run.json", {"ok": True})

    def test_state_root_rejects_nested_symlink_ancestor(self):
        real = self.root / "real"
        (real / "nested").mkdir(parents=True, mode=0o700)
        alias = self.root / "alias"
        alias.symlink_to(real, target_is_directory=True)
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._write_json(alias / "nested/state/runs/run.json", {"ok": True})

    def test_state_reader_rejects_non_private_file(self):
        path = self.state / "runs/run.json"
        RUNNER._write_json(path, {"ok": True})
        if os.name != "posix":
            self.skipTest("POSIX permission contract")
        path.chmod(0o644)
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._load_json(path)

    def test_config_requires_private_owner_mode_on_posix(self):
        if os.name != "posix":
            self.skipTest("POSIX permission contract")
        self.config.chmod(0o644)
        result = RUNNER.probe(
            config_path=self.config, opener=lambda *_, **__: _Response(),
            environ={"OMNIROUTE_API_KEY": "secret"},
        )
        self.assertEqual("ROUTE_UNQUALIFIED", result["state"])

    def test_ready_cache_reprobes_volatile_health(self):
        cache = RUNNER.QualificationCache()
        first = cache.probe(config_path=self.config, opener=lambda *_, **__: _Response(),
                            environ={"OMNIROUTE_API_KEY": "secret"})
        second = cache.probe(config_path=self.config,
                             opener=lambda *_, **__: (_ for _ in ()).throw(OSError("down")),
                             environ={"OMNIROUTE_API_KEY": "secret"})
        self.assertEqual("READY", first["state"])
        self.assertEqual("ABSENT", second["state"])

    def test_dispatch_rejects_non_ready_and_stale_cancel_quarantines(self):
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(
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

    def test_status_moves_finished_process_to_review_using_captured_evidence(self):
        RUNNER._write_json(self.state / "runs/run-4.json", {
            "schemaVersion": 1, "runId": "run-4", "status": "running",
            "pid": 4242, "processIdentity": "old", "timestamps": {},
        })
        diagnostic = {
            "schemaVersion": 1, "exitCode": 0, "timedOut": False,
            "stdout": "done", "stderr": "", "stdoutTruncated": False, "stderrTruncated": False,
        }
        RUNNER._write_json(self.state / "diagnostics/run-4.json", diagnostic)
        RUNNER._write_json(self.state / "processes/run-4.json", {
            "schemaVersion": 1, "pid": 4343, "pgid": 4343, "processIdentity": "delegate",
        })
        with mock.patch.object(RUNNER, "_group_alive", return_value=False):
            result = RUNNER.status("run-4", self.state, process_identity=lambda _: None)
        self.assertEqual("review", result["status"])
        self.assertEqual(0, result["diagnostics"]["exitCode"])
        self.assertEqual(RUNNER._sha256(diagnostic), result["diagnostics"]["sha256"])
        self.assertNotIn("stdout", result["diagnostics"])

    def test_completion_receipt_is_terminal_and_redacted(self):
        head = subprocess.run(["git", "-C", str(self.worktree), "rev-parse", "HEAD"], check=True, capture_output=True, text=True).stdout.strip()
        RUNNER._write_json(self.state / "runs/run-1.json", {
            "schemaVersion": 1, "runId": "run-1", "status": "review", "head": head,
            "delegate": {"pathOwnership": ["chaos-engine/skills/omniroot/SKILL.md"], "worktree": str(self.worktree)},
            "integration": {"branch": "integration", "worktree": str(self.integration)},
            "baseCommit": head,
        })
        diagnostic_path = self.state / "diagnostics/run-1.json"
        RUNNER._write_json(diagnostic_path, {
            "schemaVersion": 1, "exitCode": 0, "timedOut": False,
            "stdout": "ok", "stderr": "", "stdoutTruncated": False, "stderrTruncated": False,
        })
        changed = self.worktree / "chaos-engine/skills/omniroot/SKILL.md"
        changed.parent.mkdir(parents=True)
        changed.write_text("test\n", encoding="utf-8")
        subprocess.run(["git", "-C", str(self.worktree), "add", "."], check=True)
        subprocess.run(["git", "-C", str(self.worktree), "commit", "-qm", "change"], check=True)
        head = RUNNER._git(self.worktree, "rev-parse", "HEAD")
        receipt = RUNNER.complete(
            run_id="run-1", state_dir=self.state, exit_code=0,
            changed_paths=["chaos-engine/skills/omniroot/SKILL.md"],
            learning_disposition="nothing-durable", head=head, clean=True,
            checks=["python3 -m unittest"], blockers=[], adjacent_findings=[],
        )
        self.assertEqual("completed", receipt["status"])
        self.assertEqual("nothing-durable", receipt["learningDisposition"])
        self.assertEqual("success", receipt["outcome"])
        self.assertEqual(head, receipt["head"])
        self.assertEqual(["python3 -m unittest"], receipt["checks"])
        self.assertEqual(RUNNER._sha256(RUNNER._load_json(diagnostic_path)), receipt["diagnostics"]["sha256"])
        self.assertFalse(receipt["diagnostics"]["timedOut"])
        self.assertEqual(0o400, (self.state / "receipts/run-1.json").stat().st_mode & 0o777)

    def test_completion_rejects_exit_code_that_conflicts_with_captured_process(self):
        RUNNER._write_json(self.state / "runs/run-3.json", {
            "schemaVersion": 1, "runId": "run-3", "status": "blocked", "head": "a" * 40,
            "delegate": {"pathOwnership": [], "worktree": str(self.worktree)},
        })
        RUNNER._write_json(self.state / "diagnostics/run-3.json", {
            "schemaVersion": 1, "exitCode": 9, "timedOut": False,
            "stdout": "", "stderr": "failed", "stdoutTruncated": False, "stderrTruncated": False,
        })
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.complete(
                run_id="run-3", state_dir=self.state, exit_code=0, changed_paths=[],
                learning_disposition="nothing-durable",
            )

    def test_probe_and_dispatch_use_one_sealed_config_snapshot(self):
        original = RUNNER._read_config
        calls = []
        def once(path):
            calls.append(path)
            return original(path)
        with mock.patch.object(RUNNER, "_read_config", side_effect=once):
            self._dispatch(run_id="sealed", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                popen=lambda *_a, **_k: _Process(), process_identity=lambda _: "identity")
        self.assertEqual(1, len(calls))

    def test_launcher_identity_binds_content_and_metadata(self):
        qualified = RUNNER._resolved_executable([str(self.launcher)])
        self.assertIsNotNone(qualified)
        argv, identity = qualified
        self.launcher.write_text("#!/bin/sh\nexit 1\n", encoding="utf-8")
        self.launcher.chmod(0o700)
        self.assertFalse(RUNNER._same_executable(argv, identity))

    def test_sealing_rejects_launcher_replaced_after_qualification(self):
        qualified = RUNNER._resolved_executable([str(self.launcher)])
        self.assertIsNotNone(qualified)
        argv, identity = qualified
        replacement = self.root / "new-launcher"
        replacement.write_text("#!/bin/sh\nexit 1\n", encoding="utf-8")
        replacement.chmod(0o700)
        os.replace(replacement, self.launcher)
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._seal_launcher(argv, identity, self.state)

    def test_dispatch_rejects_untracked_and_primary_worktrees(self):
        (self.worktree / "untracked.txt").write_text("x", encoding="utf-8")
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._validate_worktree(self.worktree)
        (self.worktree / "untracked.txt").unlink()
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER._validate_worktree(self.repository)

    def test_corrupt_live_manifest_aborts_reservation(self):
        RUNNER._private_directory(self.state / "runs")
        bad = self.state / "runs/bad.json"
        bad.write_text("not-json", encoding="utf-8")
        bad.chmod(0o600)
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(run_id="new", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                popen=lambda *_a, **_k: _Process(), process_identity=lambda _: "identity")

    def test_dispatch_never_records_running_without_process_identity(self):
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(run_id="unknown", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                popen=lambda *_a, **_k: _Process(), process_identity=lambda _: None)
        self.assertFalse((self.state / "runs/unknown.json").exists())

    def test_learning_registration_failure_prevents_launch_and_cleans_reservation(self):
        launched = []
        with mock.patch("scripts.agents.learning_session.register_runtime_participant",
                        side_effect=RuntimeError("closed")):
            with self.assertRaisesRegex(RUNNER.OmniRootError, "learning registration"):
                self._dispatch(run_id="learning-fail", worktree=self.worktree, state_dir=self.state,
                    config_path=self.config, target="host-cli", delegate_args=[],
                    opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                    popen=lambda *args, **kwargs: launched.append((args, kwargs)))
        self.assertEqual([], launched)
        self.assertFalse((self.state / "runs/learning-fail.json").exists())

    def test_dispatch_rejects_fabricated_default_runtime_contract(self):
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.dispatch(run_id="incomplete", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[])

    def test_cancel_kills_surviving_process_group_after_leader_exits(self):
        RUNNER._write_json(self.state / "runs/cancel-child.json", {
            "schemaVersion": 1, "runId": "cancel-child", "status": "running",
            "monitor": {"pid": 4242, "pgid": 4242, "processIdentity": "monitor"}, "timestamps": {},
        })
        RUNNER._write_json(self.state / "processes/cancel-child.json", {
            "schemaVersion": 1, "pid": 4343, "pgid": 4343, "processIdentity": "delegate",
        })
        signals = []
        with mock.patch.object(RUNNER, "_group_alive", side_effect=([True] * 50) + [False]), \
                mock.patch.object(RUNNER.os, "killpg", side_effect=lambda pid, sig: signals.append((pid, sig))), \
                mock.patch.object(RUNNER.time, "sleep"):
            result = RUNNER.cancel("cancel-child", self.state,
                                   process_identity=lambda pid: "monitor" if pid == 4242 else "delegate")
        self.assertEqual("cancelled", result["status"])
        self.assertEqual([signal.SIGTERM, signal.SIGKILL, signal.SIGTERM], [item[1] for item in signals])

    def test_capture_records_real_delegate_group_with_surviving_child(self):
        if sys.platform != "linux":
            self.skipTest("Linux process topology contract")
        launcher = self.root / "forking-launcher"
        launcher.write_text("#!/usr/bin/env python3\nimport os,time\npid=os.fork()\nif pid==0:\n os.close(1); os.close(2); time.sleep(30)\n", encoding="utf-8")
        launcher.chmod(0o700)
        argv, identity = RUNNER._resolved_executable([str(launcher)])
        diagnostic = self.state / "diagnostics/real.json"
        process_state = self.state / "processes/real.json"
        completed = subprocess.run([sys.executable, str(RUNNER_PATH), "_capture", str(diagnostic),
            str(process_state), "5", *(str(value) for value in identity), "--", *argv],
            check=False, timeout=10)
        self.assertEqual(0, completed.returncode)
        captured = RUNNER._load_json(process_state)
        self.assertTrue(RUNNER._group_alive(captured["pgid"]))
        os.killpg(captured["pgid"], signal.SIGKILL)

    def test_default_state_is_outside_current_worktree(self):
        self.assertNotEqual(Path.cwd(), RUNNER.default_state_path())
        self.assertNotIn(Path.cwd(), RUNNER.default_state_path().parents)

    def test_unsupported_platform_fails_before_state_mutation(self):
        state = self.root / "never-created"
        with mock.patch.object(RUNNER.sys, "platform", "win32"):
            with self.assertRaises(RUNNER.OmniRootError):
                self._dispatch(run_id="unsupported", worktree=self.worktree, state_dir=state,
                    config_path=self.config, target="host-cli", delegate_args=[])
        self.assertFalse(state.exists())

    def test_receipt_publish_failure_leaves_no_partial_target(self):
        target = self.state / "receipts/fail.json"
        with mock.patch.object(RUNNER.os, "link", side_effect=OSError("publish failed")):
            with self.assertRaises(OSError):
                RUNNER._create_immutable_json(target, {"ok": True})
        self.assertFalse(target.exists())

    def test_dispatch_rejects_state_inside_managed_worktree(self):
        with self.assertRaises(RUNNER.OmniRootError):
            self._dispatch(run_id="inside", worktree=self.worktree,
                state_dir=self.worktree / ".state", config_path=self.config,
                target="host-cli", delegate_args=[])

    def test_dispatch_rejects_shared_delegate_and_integration_worktree(self):
        with self.assertRaisesRegex(RUNNER.OmniRootError, "distinct"):
            self._dispatch(run_id="shared", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                integration_worktree=self.worktree, integration_branch="delegate")

    def test_completion_rejects_fabricated_changed_path_claim(self):
        base = RUNNER._git(self.worktree, "rev-parse", "HEAD")
        real = self.worktree / "docs/real.md"
        real.parent.mkdir()
        real.write_text("real\n", encoding="utf-8")
        subprocess.run(["git", "-C", str(self.worktree), "add", "."], check=True)
        subprocess.run(["git", "-C", str(self.worktree), "commit", "-qm", "real"], check=True)
        head = RUNNER._git(self.worktree, "rev-parse", "HEAD")
        RUNNER._write_json(self.state / "runs/fabricated.json", {
            "schemaVersion": 1, "runId": "fabricated", "status": "review", "baseCommit": base,
            "delegate": {"pathOwnership": ["docs"], "worktree": str(self.worktree)},
            "integration": {"branch": "integration", "worktree": str(self.integration)},
        })
        RUNNER._write_json(self.state / "diagnostics/fabricated.json", {
            "schemaVersion": 1, "exitCode": 0, "timedOut": False, "stdout": "", "stderr": "",
            "stdoutTruncated": False, "stderrTruncated": False,
        })
        with self.assertRaisesRegex(RUNNER.OmniRootError, "git diff"):
            RUNNER.complete(run_id="fabricated", state_dir=self.state, exit_code=0,
                changed_paths=["docs/fake.md"], learning_disposition="nothing-durable",
                head=head, clean=True)

    def test_exact_credential_value_is_redacted_even_without_label(self):
        redacted, _ = RUNNER._redact_diagnostic(b"prefix exact-value suffix", secrets=["exact-value"])
        self.assertNotIn("exact-value", redacted)

    def test_completion_requires_manifest_terminal_state_ownership_and_clean_head(self):
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.complete(run_id="unknown", state_dir=self.state, exit_code=0,
                changed_paths=[], learning_disposition="nothing-durable")
        head = RUNNER._git(self.worktree, "rev-parse", "HEAD")
        RUNNER._write_json(self.state / "runs/live.json", {"schemaVersion": 1, "runId": "live",
            "status": "running", "head": head,
            "delegate": {"pathOwnership": ["docs"], "worktree": str(self.worktree)},
            "integration": {"branch": "integration", "worktree": str(self.worktree)}})
        with self.assertRaises(RUNNER.OmniRootError):
            RUNNER.complete(run_id="live", state_dir=self.state, exit_code=0,
                changed_paths=["other/file"], learning_disposition="nothing-durable", head=head)

    def test_cli_accepts_private_dispatch_contract_and_complete_contract(self):
        dispatch_contract = self.root / "dispatch.json"
        dispatch_contract.write_text(json.dumps({"runId": "cli", "worktree": str(self.worktree),
            "target": "host-cli", "delegateArgs": [], "taskId": "task", "workflow": "ORCHESTRATOR + SINGLE IMPLEMENTER",
            "rootSessionId": "root", "baseCommit": RUNNER._git(self.worktree, "rev-parse", "HEAD"),
            "integrationBranch": "integration", "integrationWorktree": str(self.integration),
            "delegate": {"identity": "writer", "role": "implementer", "capability": "lower",
                "assignment": "task", "pathOwnership": ["docs"]}, "cadenceSeconds": 600,
            "deadline": "2030-01-01T00:00:00+00:00", "timeoutSeconds": 60,
            "learningState": str(self.learning_state), "learningRootSessionId": "root-1",
            "delegateSessionId": "cli"}), encoding="utf-8")
        dispatch_contract.chmod(0o600)
        parser_source = inspect.getsource(RUNNER.main)
        self.assertIn("--contract", parser_source)
        self.assertIn('commands.add_parser("complete")', parser_source)


if __name__ == "__main__":
    unittest.main()
