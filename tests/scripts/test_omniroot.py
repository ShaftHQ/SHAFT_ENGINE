"""Focused contracts for the optional OmniRoot transport."""

from __future__ import annotations

import importlib.util
import json
import os
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

    def _config(self, *, expired: bool = False) -> None:
        now = datetime.now(UTC)
        expires = now - timedelta(minutes=1) if expired else now + timedelta(minutes=20)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": "omniroute",
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
        self.state = self.root / "state"
        self.config = self.root / "omniroot.json"
        now = datetime.now(UTC)
        self.config.write_text(json.dumps({
            "schemaVersion": 1,
            "routeId": "opaque-route",
            "launcher": "omniroute",
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
        self.assertEqual(["omniroute", "run", "host-cli"], launched[0][0][:3])
        self.assertFalse(launched[0][1].get("shell", True))
        self.assertEqual(str(self.worktree), launched[0][1]["cwd"])
        self.assertNotIn("secret", json.dumps(manifest))
        path = self.state / "runs/run-1.json"
        self.assertEqual(0o600, path.stat().st_mode & 0o777)

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
            learning_disposition="nothing-durable",
        )
        self.assertEqual("completed", receipt["status"])
        self.assertEqual("nothing-durable", receipt["learningDisposition"])
        self.assertEqual(0o600, (self.state / "receipts/run-1.json").stat().st_mode & 0o777)


if __name__ == "__main__":
    unittest.main()
