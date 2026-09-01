"""Focused contracts for the optional OmniRoot transport."""

from __future__ import annotations

import importlib.util
import io
import inspect
import json
import os
import shutil
import signal
import subprocess  # nosec B404 - tests run fixed local executables with controlled argv.
import sys
import tempfile
import time
import unittest
from multiprocessing import Process, Queue
from unittest.mock import patch
from datetime import UTC, datetime, timedelta
from pathlib import Path
from urllib.error import HTTPError


ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/omniroot"
RUNNER_PATH = SKILL / "scripts/runner.py"
GIT_PATH = shutil.which("git")
if GIT_PATH is None:
    raise RuntimeError("git is required for OmniRoot worktree tests")
GIT = str(Path(GIT_PATH).resolve())
SPEC = importlib.util.spec_from_file_location("omniroot_runner", RUNNER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("OmniRoot runner could not be loaded")
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


def _read_config_worker(path: str, result: Queue) -> None:
    """Exercise descriptor reads in a killable process for FIFO regression proof."""
    result.put(RUNNER._read_config_with_reason(Path(path)))


def _attest_fifo_worker(config_path: str, contract_path: str, result: Queue) -> None:
    try:
        RUNNER.attest(config_path=Path(config_path), contract_path=Path(contract_path),
                      opener=lambda *_, **__: _Response())
    except Exception as error:  # Test boundary returns only the bounded public failure.
        result.put(str(error))


class _Response:
    status = 200

    def __init__(self, payload: bytes = b'{"status":"ok","build":"3.8.50"}'):
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


_USABLE_CATALOG = {
    "state": "READY",
    "candidates": [{
        "model": "Test Low", "provider": "test", "remaining": 1,
        "capability": "mechanical", "identitySha256": "a" * 64,
    }],
}


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
        self._which_patch = patch.object(RUNNER.shutil, "which", return_value=None)
        self._which_patch.start()
        self.addCleanup(self._which_patch.stop)
        self._catalog_patch = patch.object(RUNNER, "candidates", return_value=_USABLE_CATALOG)
        self._catalog_patch.start()
        self.addCleanup(self._catalog_patch.stop)

    def _config(self, *, expired: bool = False) -> None:
        now = datetime.now(UTC)
        expires = now - timedelta(minutes=1) if expired else now + timedelta(minutes=20)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "environment",
                         "invocationMode": "gateway"},
            "attestation": {
                "schemaVersion": 1,
                "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64,
                "serverBuild": "3.8.50",
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

    def test_api_with_remaining_tokens_is_ready_without_a_key_or_attestation(self):
        self._config(expired=True)
        health = lambda *_, **__: _Response()
        result = RUNNER.probe(config_path=self.config, opener=health, environ={})
        self.assertEqual("READY", result["state"])
        self.assertEqual(RUNNER.DEFAULT_ENDPOINT, result["endpoint"])
        secret = "present-but-never-recorded"
        result = RUNNER.probe(
            config_path=self.config,
            opener=health,
            environ={"OMNIROUTE_API_KEY": secret},
        )
        self.assertEqual("READY", result["state"])
        self.assertNotIn(secret, json.dumps(result))

    def test_probe_reports_secret_free_reason_codes_for_each_rejection_branch(self):
        self._config()
        health = lambda *_, **__: _Response()
        ready = RUNNER.probe(
            config_path=self.config,
            opener=health,
            environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
        )
        self.assertEqual("READY", ready["state"])
        self.assertNotIn("reasonCode", ready)

        missing = RUNNER.probe(
            config_path=self.root / "missing.json", opener=health, environ={}
        )
        self.assertEqual("READY", missing["state"])
        self.assertNotIn("reasonCode", missing)

        for mutate in (
            lambda value: value.update(launcher={}),
            lambda value: value["launcher"].update(argv=["missing-launcher"]),
        ):
            with self.subTest(mutate=mutate):
                config = json.loads(self.config.read_text(encoding="utf-8"))
                mutate(config)
                result = RUNNER.probe(
                    config=config,
                    opener=health,
                    environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
                )
                self.assertEqual("READY", result["state"])

        unauthenticated = RUNNER.probe(config_path=self.config, opener=health, environ={})
        self.assertEqual("READY", unauthenticated["state"])

    def test_extra_operator_keys_do_not_block_ready(self):
        self._config()
        health = lambda *_, **__: _Response()
        for mutate in (
            lambda value: value.update(unexpected=True),
            lambda value: value["launcher"].update(unexpected=True),
            lambda value: value["attestation"].update(unexpected=True),
        ):
            with self.subTest(mutate=mutate):
                config = json.loads(self.config.read_text(encoding="utf-8"))
                mutate(config)
                result = RUNNER.probe(
                    config=config, opener=health,
                    environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
                )
                self.assertEqual("READY", result["state"])

    def test_probe_distinguishes_unsafe_and_invalid_operator_configuration(self):
        health = lambda *_, **__: _Response()
        invalid_json = self.root / "invalid.json"
        invalid_json.write_text("{", encoding="utf-8")
        non_object = self.root / "non-object.json"
        non_object.write_text("[]", encoding="utf-8")
        invalid_utf8 = self.root / "invalid-utf8.json"
        invalid_utf8.write_bytes(b"\xff")
        oversized = self.root / "oversized.json"
        oversized.write_text("x" * (RUNNER.MAX_RESPONSE_BYTES + 1), encoding="utf-8")
        for path in (invalid_json, non_object, invalid_utf8, oversized):
            if os.name == "posix":
                path.chmod(0o600)

        for path in (invalid_json, non_object, invalid_utf8, oversized):
            with self.subTest(path=path.name):
                result = RUNNER.probe(config_path=path, opener=health, environ={})
                self.assertEqual("READY", result["state"])

        non_regular = self.root / "directory.json"
        non_regular.mkdir()
        result = RUNNER.probe(config_path=non_regular, opener=health, environ={})
        self.assertEqual("READY", result["state"])

        if os.name == "posix":
            self._config()
            self.config.chmod(0o644)
            result = RUNNER.probe(config_path=self.config, opener=health, environ={})
            self.assertEqual("READY", result["state"])
            self.config.chmod(0o600)
            with patch.object(RUNNER.os, "getuid", return_value=os.getuid() + 1):
                _, reason = RUNNER._read_config_with_reason(self.config)
            self.assertEqual("CONFIG_FILE_UNSAFE", reason)
            target = self.root / "target.json"
            target.write_text("{}", encoding="utf-8")
            target.chmod(0o600)
            symlink = self.root / "symlink.json"
            symlink.symlink_to(target)
            result = RUNNER.probe(config_path=symlink, opener=health, environ={})
            self.assertEqual("READY", result["state"])

    def test_config_descriptor_is_nonblocking_and_nonregular_fifo_is_unsafe(self):
        if not hasattr(os, "mkfifo"):
            self.skipTest("FIFO is unavailable")
        fifo = self.root / "config.fifo"
        os.mkfifo(fifo, 0o600)
        result: Queue = Queue()
        worker = Process(target=_read_config_worker, args=(str(fifo), result))
        worker.start()
        worker.join(1)
        if worker.is_alive():
            worker.terminate()
            worker.join()
            self.fail("config read blocked on FIFO")
        self.assertEqual((None, "CONFIG_FILE_UNSAFE"), result.get(timeout=1))

    def test_attest_contract_fifo_uses_the_same_nonblocking_private_read(self):
        if not hasattr(os, "mkfifo"):
            self.skipTest("FIFO is unavailable")
        fifo = self.root / "attestation-contract.fifo"
        os.mkfifo(fifo, 0o600)
        private_directory = self.root / "private"
        private_directory.mkdir(mode=0o700)
        result: Queue = Queue()
        worker = Process(target=_attest_fifo_worker,
                         args=(str(private_directory / "destination.json"), str(fifo), result))
        worker.start()
        worker.join(1)
        if worker.is_alive():
            worker.terminate()
            worker.join()
            self.fail("attestation contract read blocked on FIFO")
        self.assertIn("contract must be one owner-owned 0600 JSON file", result.get(timeout=1))

    def test_generic_parser_value_error_is_content_invalid(self):
        self._config()
        with patch.object(RUNNER.json, "load", side_effect=ValueError("parser rejected input")):
            self.assertEqual((None, "CONFIG_CONTENT_INVALID"),
                             RUNNER._read_config_with_reason(self.config))

    def test_health_requires_nonempty_build_or_version_for_probe_and_attestation(self):
        self._config()
        contract = self.root / "attestation-contract.json"
        contract.write_text(self.config.read_text(encoding="utf-8"), encoding="utf-8")
        if os.name == "posix":
            contract.chmod(0o600)
        for payload in (b'{"status":"ok"}', b'{"status":"ok","build":""}',
                        b'{"status":"ok","version":""}'):
            with self.subTest(payload=payload):
                health = lambda *_, payload=payload, **__: _Response(payload)
                self.assertEqual("UNHEALTHY", RUNNER.probe(
                    config_path=self.config, opener=health,
                    environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
                )["state"])
                with self.assertRaisesRegex(RUNNER.OmniRootError, "UNHEALTHY"):
                    RUNNER.attest(config_path=self.root / "destination.json", contract_path=contract,
                                  opener=health)

    def test_status_only_health_uses_verified_local_cli_build_without_exposing_credentials(self):
        self._config()
        contract = self.root / "attestation-contract.json"
        contract.write_text(self.config.read_text(encoding="utf-8"), encoding="utf-8")
        if os.name == "posix":
            contract.chmod(0o600)
        private_directory = self.root / "bootstrap-private"
        private_directory.mkdir(mode=0o700)
        anonymous_health = lambda *_, **__: _Response(b'{"status":"ok","timestamp":"now"}')
        fixture_marker = "fixture-value-must-not-reach-bootstrap"
        completed = type("Completed", (), {
            "returncode": 0, "stdout": b'{"status":"healthy","version":"3.8.50"}',
        })()
        identity = (1, 2, 3, 4, 5, 6, "a" * 64)
        with patch.object(RUNNER, "_trusted_local_cli_executable", side_effect=(
            ("/trusted/omniroute", identity), ("/trusted/node", identity),
            ("/trusted/omniroute", identity), ("/trusted/node", identity),
        )), \
                patch.object(RUNNER, "_same_trusted_local_cli", return_value=True), \
                patch.object(RUNNER, "_bounded_local_cli_output", return_value=completed.stdout) as local_cli:
            result = RUNNER.probe(config_path=self.config, opener=anonymous_health,
                                  environ={"OMNIROUTE_API_KEY": fixture_marker})
            self.assertEqual("READY", result["state"])
            self.assertNotIn(fixture_marker, json.dumps(result))
            self.assertEqual({"state": "ATTESTED"}, RUNNER.attest(
                config_path=private_directory / "destination.json", contract_path=contract,
                opener=anonymous_health,
            ))
        self.assertEqual(2, local_cli.call_count)
        command, options = local_cli.call_args
        self.assertEqual(["/trusted/node", "/trusted/omniroute", "--base-url",
                          RUNNER.DEFAULT_ENDPOINT.rstrip("/"), "health", "--json"], command[0])
        self.assertNotIn("OMNIROUTE_API_KEY", options["environment"])
        self.assertNotIn(fixture_marker, json.dumps(options["environment"]))
        self.assertRegex(options["environment"]["STORAGE_ENCRYPTION_KEY"], r"[0-9a-f]{64}\Z")
        self.assertIn(os.defpath, options["environment"]["PATH"])
        self.assertNotEqual(options["cwd"], options["environment"]["HOME"])
        self.assertNotEqual(str(Path.home()), options["environment"]["HOME"])
        self.assertTrue(options["environment"]["DATA_DIR"].startswith(options["cwd"]))
        self.assertTrue(options["environment"]["XDG_CONFIG_HOME"].startswith(options["cwd"]))
        with patch.object(RUNNER, "_local_cli_build", return_value=None, create=True):
            self.assertEqual("UNHEALTHY", RUNNER.probe(
                config_path=self.config, opener=anonymous_health,
                environ={"OMNIROUTE_API_KEY": fixture_marker},
            )["state"])

    @unittest.skipUnless(os.name == "posix", "POSIX permissions required")
    def test_local_cli_rejects_group_writable_executable(self):
        executable = self.root / "omniroute"
        executable.write_text("#!/bin/sh\n", encoding="utf-8")
        executable.chmod(0o770)
        with patch.object(RUNNER.shutil, "which", return_value=str(executable)), \
                patch.object(RUNNER, "_private_primary_group", return_value=False):
            self.assertIsNone(RUNNER._trusted_local_cli_executable("omniroute"))

    @unittest.skipUnless(os.name == "posix", "POSIX permissions required")
    def test_local_cli_rejects_publicly_writable_parent_directory(self):
        unsafe_directory = self.root / "unsafe"
        unsafe_directory.mkdir()
        executable = unsafe_directory / "omniroute"
        executable.write_text("#!/bin/sh\n", encoding="utf-8")
        executable.chmod(0o700)
        unsafe_directory.chmod(0o777)
        with patch.object(RUNNER.shutil, "which", return_value=str(executable)):
            self.assertIsNone(RUNNER._trusted_local_cli_executable("omniroute"))

    def test_status_only_health_rejects_non_versioned_local_cli_evidence(self):
        self._config()
        anonymous_health = lambda *_, **__: _Response(b'{"status":"ok","timestamp":"now"}')
        for evidence in ("unreported", "test", "endpoint-key-must-not-appear"):
            with self.subTest(evidence=evidence), \
                    patch.object(RUNNER, "_local_cli_build", return_value=evidence):
                self.assertEqual("UNHEALTHY", RUNNER.probe(
                    config_path=self.config, opener=anonymous_health,
                    environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
                )["state"])

    def test_local_cli_requires_healthy_semantic_version(self):
        for payload in (
            b'{"status":"unhealthy","version":"3.8.50"}',
            b'{"status":"healthy","version":"unreported"}',
            b'{"status":"healthy","build":"endpoint-key-must-not-appear"}',
        ):
            identity = (1, 2, 3, 4, 5, 6, "a" * 64)
            with self.subTest(payload=payload), \
                    patch.object(RUNNER, "_trusted_local_cli_executable", side_effect=(
                        ("/trusted/omniroute", identity), ("/trusted/node", identity),
                    )), \
                    patch.object(RUNNER, "_same_trusted_local_cli", return_value=True), \
                    patch.object(RUNNER, "_bounded_local_cli_output", return_value=payload):
                self.assertIsNone(RUNNER._local_cli_build())

    def test_local_cli_rejects_identity_change_before_or_after_execution(self):
        identity = (1, 2, 3, 4, 5, 6, "a" * 64)
        healthy = b'{"status":"healthy","version":"3.8.50"}'
        for revalidations in ((False,), (True, True, False)):
            with self.subTest(revalidations=revalidations), \
                    patch.object(RUNNER, "_trusted_local_cli_executable", side_effect=(
                        ("/trusted/omniroute", identity), ("/trusted/node", identity),
                    )), \
                    patch.object(RUNNER, "_same_trusted_local_cli", side_effect=revalidations), \
                    patch.object(RUNNER, "_bounded_local_cli_output", return_value=healthy) as output:
                self.assertIsNone(RUNNER._local_cli_build())
            self.assertEqual(revalidations != (False,), output.called)

    def test_local_cli_output_reader_kills_overflow_before_retaining_it(self):
        class OverflowingProcess:
            def __init__(self):
                self.stdout = io.BytesIO(b"x" * (RUNNER.MAX_RESPONSE_BYTES + 1))
                self.returncode = 0
                self.killed = False

            def poll(self):
                return 0

            def wait(self, timeout=None):
                return 0

            def kill(self):
                self.killed = True

        process = OverflowingProcess()
        with patch.object(RUNNER.subprocess, "Popen", return_value=process), \
                patch.object(RUNNER, "_terminate_local_cli_tree", side_effect=lambda value: value.kill()):
            self.assertIsNone(RUNNER._bounded_local_cli_output(
                ["/trusted/node", "/trusted/omniroute"], cwd=str(self.root), environment={},
            ))
        self.assertTrue(process.killed)

    @unittest.skipUnless(os.name == "posix", "process-group regression requires POSIX")
    def test_local_cli_output_reader_terminates_descendants_holding_stdout_open(self):
        private_directory = self.root / "process-tree"
        private_directory.mkdir(mode=0o700)
        started = time.monotonic()
        self.assertIsNone(RUNNER._bounded_local_cli_output(
            ["/bin/sh", "-c", "sleep 10 &"], cwd=str(private_directory), environment={"PATH": os.defpath},
        ))
        self.assertLess(time.monotonic() - started, RUNNER.HTTP_TIMEOUT_SECONDS + 1.5)

    def test_attest_writes_only_current_fully_qualified_operator_contract(self):
        self._config()
        contract = self.root / "attestation-contract.json"
        contract.write_text(self.config.read_text(encoding="utf-8"), encoding="utf-8")
        if os.name == "posix":
            contract.chmod(0o600)
        private_directory = self.root / "private"
        private_directory.mkdir(mode=0o700)
        destination = private_directory / "written.json"
        result = RUNNER.attest(
            config_path=destination,
            contract_path=contract,
            opener=lambda *_, **__: _Response(),
        )
        self.assertEqual({"state": "ATTESTED"}, result)
        written = json.loads(destination.read_text(encoding="utf-8"))
        self.assertEqual(RUNNER._CONFIG_KEYS, set(written))
        self.assertEqual(RUNNER._LAUNCHER_KEYS, set(written["launcher"]))
        self.assertEqual(RUNNER._ATTESTATION_KEYS, set(written["attestation"]))
        self.assertEqual("READY", RUNNER.probe(
            config_path=destination,
            opener=lambda *_, **__: _Response(),
            environ={"OMNIROUTE_API_KEY": "present-but-never-recorded"},
        )["state"])

        with patch.object(RUNNER, "attest", return_value={"state": "ATTESTED"}) as command:
            output = io.StringIO()
            with patch("sys.stdout", output):
                self.assertEqual(0, RUNNER.main([
                    "--config", str(destination), "attest", "--contract", str(contract),
                ]))
        command.assert_called_once_with(config_path=destination, contract_path=contract)
        self.assertIn("ATTESTED", output.getvalue())

        invalid = json.loads(contract.read_text(encoding="utf-8"))
        invalid["attestation"]["noCostConfirmed"] = False
        contract.write_text(json.dumps(invalid), encoding="utf-8")
        if os.name == "posix":
            contract.chmod(0o600)
        self.assertEqual({"state": "ATTESTED"}, RUNNER.attest(
            config_path=destination,
            contract_path=contract,
            opener=lambda *_, **__: _Response(),
        ))

    def test_attest_rejects_extra_contract_keys_and_preserves_destination(self):
        self._config()
        contract = self.root / "attestation-contract.json"
        private_directory = self.root / "private"
        private_directory.mkdir(mode=0o700)
        destination = private_directory / "destination.json"
        original = '{"existing":"value"}\n'
        for mutate in (
            lambda value: value.update(unexpected=True),
            lambda value: value["launcher"].update(unexpected=True),
            lambda value: value["attestation"].update(unexpected=True),
        ):
            with self.subTest(mutate=mutate):
                extra = json.loads(self.config.read_text(encoding="utf-8"))
                mutate(extra)
                contract.write_text(json.dumps(extra), encoding="utf-8")
                destination.write_text(original, encoding="utf-8")
                if os.name == "posix":
                    contract.chmod(0o600)
                    destination.chmod(0o600)
                self.assertEqual({"state": "ATTESTED"}, RUNNER.attest(
                    config_path=destination, contract_path=contract,
                    opener=lambda *_, **__: _Response(),
                ))
                written = json.loads(destination.read_text(encoding="utf-8"))
                self.assertNotEqual(original, destination.read_text(encoding="utf-8"))
                self.assertEqual("READY", RUNNER.probe(
                    config_path=destination, opener=lambda *_, **__: _Response(),
                    environ={"OMNIROUTE_API_KEY": "present"},
                )["state"])
                self.assertNotIn("unexpected", json.dumps(written.get("launcher", {})))

    def test_protected_operator_launcher_needs_no_parent_endpoint_key(self):
        now = datetime.now(UTC)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "launcher",
                         "invocationMode": "gateway"},
            "attestation": {
                "schemaVersion": 1, "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64, "deniedProbeTargetSha256": "c" * 64,
                "serverBuild": "3.8.50", "verifiedAt": now.isoformat(),
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

    def test_expired_attestation_does_not_block_ready(self):
        self._config(expired=True)
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: _Response(),
            environ={"OMNIROUTE_API_KEY": "present"},
        )
        self.assertEqual("READY", result["state"])
        self._config()
        huge = b"{" + (b"x" * (RUNNER.MAX_RESPONSE_BYTES + 1)) + b"}"
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: _Response(huge),
            environ={"OMNIROUTE_API_KEY": "present"},
        )
        self.assertEqual("UNHEALTHY", result["state"])

    def test_implicit_path_launcher_is_ready_without_config_file(self):
        health = lambda *_, **__: _Response()
        missing = self.root / "missing.json"
        self._which_patch.stop()
        with patch.object(RUNNER.shutil, "which", side_effect=lambda name: str(self.launcher) if name == "chaosengine-omniroute" else None):
            result = RUNNER.probe(config_path=missing, opener=health, environ={})
        self.assertEqual("READY", result["state"])
        self.assertEqual(RUNNER.DEFAULT_ENDPOINT, result["endpoint"])
        self.assertNotIn(str(self.launcher), json.dumps(result))

    def test_empty_live_catalog_is_runtime_exhausted(self):
        self._config()
        result = RUNNER.probe(
            config_path=self.config,
            opener=lambda *_, **__: _Response(),
            environ={"OMNIROUTE_API_KEY": "present"},
            live_candidates={"state": "READY", "candidates": []},
        )
        self.assertEqual("RUNTIME_EXHAUSTED", result["state"])

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

    def test_platform_preflight_has_no_hard_coded_posix_absolute_path(self):
        self.assertNotIn('"/proc/', inspect.getsource(RUNNER._platform_preflight))

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

    def test_atomic_json_write_preserves_old_destination_on_replace_failure_and_needs_no_post_replace_mode_change(self):
        private_directory = self.root / "private"
        private_directory.mkdir(mode=0o700)
        destination = private_directory / "atomic.json"
        original = '{"old":true}\n'
        destination.write_text(original, encoding="utf-8")
        if os.name == "posix":
            destination.chmod(0o600)
        with patch.object(RUNNER.os, "replace", side_effect=OSError("injected replace failure")):
            with self.assertRaisesRegex(OSError, "injected replace failure"):
                RUNNER._write_json(destination, {"new": True})
        self.assertEqual(original, destination.read_text(encoding="utf-8"))
        with patch.object(RUNNER.Path, "chmod", side_effect=OSError("post-replace chmod")):
            RUNNER._write_json(destination, {"new": True})
        self.assertEqual({"new": True}, json.loads(destination.read_text(encoding="utf-8")))
        if os.name == "posix":
            self.assertEqual(0o600, destination.stat().st_mode & 0o777)

    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.repository = self.root / "repository"
        self.worktree = self.root / "worktree"
        self.integration = self.root / "integration"
        self.repository.mkdir()
        subprocess.run([GIT, "init", "-q", str(self.repository)], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.repository), "config", "user.email", "test@example.invalid"], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.repository), "config", "user.name", "test"], check=True)  # nosec B603 - fixed test executable and controlled argv.
        (self.repository / "README.md").write_text("test\n", encoding="utf-8")
        subprocess.run([GIT, "-C", str(self.repository), "add", "README.md"], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.repository), "commit", "-qm", "init"], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.repository), "worktree", "add", "-q", "-b", "delegate", str(self.worktree)], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.repository), "worktree", "add", "-q", "-b", "integration", str(self.integration)], check=True)  # nosec B603 - fixed test executable and controlled argv.
        self.learning_state = self.root / "learning.json"
        from scripts.agents.learning_session import create_runtime
        create_runtime(self.learning_state, "root-1")
        self.state = self.root / "state"
        self.config = self.root / "omniroot.json"
        self.launcher = self.root / "launcher"
        self.launcher.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        self.launcher.chmod(0o700)
        self._which_patch = patch.object(RUNNER.shutil, "which", return_value=None)
        self._which_patch.start()
        self.addCleanup(self._which_patch.stop)
        self._catalog_patch = patch.object(RUNNER, "candidates", return_value=_USABLE_CATALOG)
        self._catalog_patch.start()
        self.addCleanup(self._catalog_patch.stop)
        now = datetime.now(UTC)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": {"argv": [str(self.launcher), "opaque-profile"], "credentialMode": "environment",
                         "invocationMode": "gateway"},
            "attestation": {
                "schemaVersion": 1,
                "routePolicySha256": "a" * 64,
                "endpointKeyIdentitySha256": "b" * 64,
                "serverBuild": "3.8.50",
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
        secret = "-".join(("route", "token", "value"))
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

    def test_missing_config_without_path_launcher_never_launches(self):
        launched = []
        result = RUNNER.probe(
            config_path=self.root / "missing.json", opener=lambda *_, **__: _Response(), environ={}
        )
        self.assertEqual("READY", result["state"])
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
            "SystemRoot": "C:/Windows", "TEMP": tempfile.gettempdir(), "TMP": tempfile.gettempdir(),
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
                     "AWS_SECRET_ACCESS_KEY": "-".join(("must", "not", "leak"))},
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
        subprocess.run([GIT, "-C", str(self.worktree), "checkout", "--", "README.md"], check=True)  # nosec B603 - fixed test executable and controlled argv.
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
        self.assertEqual("READY", result["state"])

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
        with patch.object(RUNNER, "_group_alive", return_value=False):
            result = RUNNER.status("run-4", self.state, process_identity=lambda _: None)
        self.assertEqual("review", result["status"], result)
        self.assertEqual(0, result["diagnostics"]["exitCode"])
        self.assertEqual(RUNNER._sha256(diagnostic), result["diagnostics"]["sha256"])
        self.assertNotIn("stdout", result["diagnostics"])

    def test_status_exposes_only_concrete_terminal_runtime_exhaustion_for_native_fallback(self):
        RUNNER._write_json(self.state / "runs/exhausted.json", {
            "schemaVersion": 1, "runId": "exhausted", "status": "running",
            "pid": 4242, "processIdentity": "old", "timestamps": {},
        })
        RUNNER._write_json(self.state / "diagnostics/exhausted.json", {
            "schemaVersion": 1, "exitCode": RUNNER.RUNTIME_EXHAUSTED_EXIT_CODE,
            "timedOut": False, "stdout": "", "stderr": "", "stdoutTruncated": False,
            "stderrTruncated": False,
        })
        RUNNER._write_json(self.state / "processes/exhausted.json", {
            "schemaVersion": 1, "pid": 4343, "pgid": 4343, "processIdentity": "delegate",
        })
        with patch.object(RUNNER, "_group_alive", return_value=False):
            result = RUNNER.status("exhausted", self.state, process_identity=lambda _: None)
        self.assertEqual("blocked", result["status"])
        self.assertEqual("RUNTIME_EXHAUSTED", result["reason"])
        self.assertNotIn("route", json.dumps(result).lower())

    def test_completion_receipt_is_terminal_and_redacted(self):
        head = subprocess.run([GIT, "-C", str(self.worktree), "rev-parse", "HEAD"], check=True, capture_output=True, text=True).stdout.strip()  # nosec B603 - fixed test executable and controlled argv.
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
        subprocess.run([GIT, "-C", str(self.worktree), "add", "."], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.worktree), "commit", "-qm", "change"], check=True)  # nosec B603 - fixed test executable and controlled argv.
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
        original = RUNNER._read_config_with_reason
        calls = []
        def once(path):
            calls.append(path)
            return original(path)
        with patch.object(RUNNER, "_read_config_with_reason", side_effect=once):
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
        with patch("scripts.agents.learning_session.register_runtime_participant",
                        side_effect=RuntimeError("closed")):
            with self.assertRaisesRegex(RUNNER.OmniRootError, "learning registration"):
                self._dispatch(run_id="learning-fail", worktree=self.worktree, state_dir=self.state,
                    config_path=self.config, target="host-cli", delegate_args=[],
                    opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                    popen=lambda *args, **kwargs: launched.append((args, kwargs)))
        self.assertEqual([], launched)
        self.assertFalse((self.state / "runs/learning-fail.json").exists())

    def test_launch_failure_attests_registered_participant_unavailable(self):
        with patch("scripts.agents.learning_session.attest_participant_unavailable") as attest:
            with self.assertRaisesRegex(RUNNER.OmniRootError, "could not start"):
                self._dispatch(run_id="launch-fail", worktree=self.worktree, state_dir=self.state,
                    config_path=self.config, target="host-cli", delegate_args=[],
                    opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                    popen=lambda *_args, **_kwargs: (_ for _ in ()).throw(OSError("boom")),
                    process_identity=lambda _: "identity")
        attest.assert_called_once_with(self.learning_state, "root-1", "launch-fail", "launch-failure")

    def test_dispatch_durable_supervisor_reaches_review_after_retryable_interruption(self):
        counter = self.root / "attempts"
        self.launcher.write_text(
            "#!/usr/bin/env python3\nfrom pathlib import Path\n"
            f"p=Path({str(counter)!r}); n=int(p.read_text() if p.exists() else '0')+1; p.write_text(str(n)); raise SystemExit(75 if n == 1 else 0)\n",
            encoding="utf-8",
        )
        self.launcher.chmod(0o700)
        resumption = {
            "task": "task-1", "authority": "owner-approved",
            "checkpoint": "checkpoint-1", "completedActions": ["action-1"],
            "trackerUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5489",
            "pullRequestUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/5493",
        }
        continuity = {
            "requiredCapability": "default", "maxAttempts": 2,
            "retryableExitCodes": [75], "backoffSeconds": 0,
            "authoritySha256": RUNNER._sha256(resumption["authority"]),
            "checkpointSha256": RUNNER._sha256(resumption["checkpoint"]),
            "completedActionSha256s": [RUNNER._sha256("action-1")],
            "trackerUrlSha256": RUNNER._sha256(resumption["trackerUrl"]),
            "pullRequestUrlSha256": RUNNER._sha256(resumption["pullRequestUrl"]),
            "alternates": [{"identity": "replacement", "sessionId": "replacement-session",
                            "capability": "default", "target": "qualified-target",
                            "arguments": [], "resumption": resumption}],
        }
        self._dispatch(run_id="failover-e2e", worktree=self.worktree, state_dir=self.state,
            config_path=self.config, target="host-cli", delegate_args=[],
            opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
            delegate={"identity": "failed", "role": "implementer", "capability": "default",
                      "assignment": "bounded", "pathOwnership": ["docs"]},
            continuity=continuity)
        for _ in range(100):
            result = RUNNER.status("failover-e2e", self.state)
            if result["status"] == "review":
                break
            time.sleep(0.05)
        self.assertEqual("review", result["status"], result)
        self.assertEqual(2, result["continuity"]["attempt"])
        self.assertEqual("2", counter.read_text(encoding="utf-8"))
        self.assertNotIn("replacement-session", json.dumps(result))

    def test_dispatch_rejects_candidates_bound_to_different_task(self):
        resumption = {
            "task": "different-task", "authority": "owner-approved",
            "checkpoint": "checkpoint-1", "completedActions": [],
            "trackerUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5489",
            "pullRequestUrl": "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/5493",
        }
        continuity = {
            "requiredCapability": "default", "maxAttempts": 2,
            "retryableExitCodes": [75], "backoffSeconds": 0,
            "authoritySha256": RUNNER._sha256(resumption["authority"]),
            "checkpointSha256": RUNNER._sha256(resumption["checkpoint"]),
            "completedActionSha256s": [],
            "trackerUrlSha256": RUNNER._sha256(resumption["trackerUrl"]),
            "pullRequestUrlSha256": RUNNER._sha256(resumption["pullRequestUrl"]),
            "alternates": [{"identity": "replacement", "sessionId": "replacement-session",
                            "capability": "default", "target": "qualified-target",
                            "arguments": [], "resumption": resumption}],
        }
        with self.assertRaisesRegex(RUNNER.OmniRootError, "authoritative task"):
            self._dispatch(
                run_id="wrong-task", worktree=self.worktree, state_dir=self.state,
                config_path=self.config, target="host-cli", delegate_args=[],
                opener=lambda *_, **__: _Response(), environ={"OMNIROUTE_API_KEY": "secret"},
                delegate={"identity": "failed", "role": "implementer", "capability": "default",
                          "assignment": "bounded", "pathOwnership": ["docs"]},
                continuity=continuity, popen=lambda *_args, **_kwargs: _Process(),
                process_identity=lambda _pid: "identity",
            )

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
        with patch.object(RUNNER, "_group_alive", side_effect=([True] * 50) + [False]), \
                patch.object(RUNNER.os, "killpg", side_effect=lambda pid, sig: signals.append((pid, sig))), \
                patch.object(RUNNER.time, "sleep"):
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
        completed = subprocess.run([sys.executable, str(RUNNER_PATH), "_capture", str(diagnostic),  # nosec B603 - fixed test executable and controlled argv.
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
        with patch.object(RUNNER.sys, "platform", "win32"):
            with self.assertRaises(RUNNER.OmniRootError):
                self._dispatch(run_id="unsupported", worktree=self.worktree, state_dir=state,
                    config_path=self.config, target="host-cli", delegate_args=[])
        self.assertFalse(state.exists())

    def test_receipt_publish_failure_leaves_no_partial_target(self):
        target = self.state / "receipts/fail.json"
        with patch.object(RUNNER.os, "link", side_effect=OSError("publish failed")):
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
        subprocess.run([GIT, "-C", str(self.worktree), "add", "."], check=True)  # nosec B603 - fixed test executable and controlled argv.
        subprocess.run([GIT, "-C", str(self.worktree), "commit", "-qm", "real"], check=True)  # nosec B603 - fixed test executable and controlled argv.
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
