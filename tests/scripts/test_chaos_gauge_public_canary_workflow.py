"""Public excluded-canary bridge contract (#5462)."""

from __future__ import annotations

import unittest
from unittest.mock import patch
import importlib.util
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory

import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "chaos-gauge-public-canary.yml"
BRIDGE_PATH = ROOT / "scripts" / "ci" / "chaos_gauge" / "public_canary_bridge.py"
BRIDGE_SPEC = importlib.util.spec_from_file_location("chaos_gauge_public_canary_bridge", BRIDGE_PATH)
if BRIDGE_SPEC is None or BRIDGE_SPEC.loader is None:
    raise RuntimeError("public canary bridge is unavailable")
BRIDGE = importlib.util.module_from_spec(BRIDGE_SPEC)
BRIDGE_SPEC.loader.exec_module(BRIDGE)


class PublicCanaryWorkflowTest(unittest.TestCase):
    def setUp(self) -> None:
        self.text = WORKFLOW.read_text(encoding="utf-8")
        self.workflow = yaml.safe_load(self.text)
        self.job = self.workflow["jobs"]["excluded-canary"]
        self.steps = self.job["steps"]

    def _step(self, name: str) -> dict[str, object]:
        return next(step for step in self.steps if step.get("name") == name)

    def test_is_manual_excluded_canary_never_a_pilot(self) -> None:
        self.assertIn("workflow_dispatch", self.text)
        self.assertNotIn("full-pilot", self.text)
        self.assertNotIn("--campaign", self.text)
        run = str(self._step("Run excluded two-arm canary")["run"])
        self.assertIn("scripts/ci/chaos_gauge/canary.py", run)
        self.assertIn("--private-read-proven", run)
        self.assertNotIn("campaign.py", run)

    def test_pins_public_source_runtime_and_docker_preflight(self) -> None:
        public = self._step("Checkout merged public source")
        self.assertEqual("ShaftHQ/SHAFT_ENGINE", public["with"]["repository"])
        self.assertEqual("main", public["with"]["ref"])
        self.assertFalse(public["with"]["persist-credentials"])
        capture = str(self._step("Record exact public main revision")["run"])
        self.assertIn("git -C public rev-parse HEAD", capture)
        uv_setup = next(step for step in self.steps if step.get("uses", "").startswith("astral-sh/setup-uv@"))
        self.assertEqual("0.12.7", uv_setup["with"]["version"])
        install = str(self._step("Install pinned native runtime")["run"])
        self.assertNotIn("python3 -m pip install", install)
        self.assertIn("uv pip install --system --require-hashes", install)
        self.assertIn("@openai/codex@0.118.0", install)
        self.assertIn("docker version --format", install)

    def test_teardown_only_runs_after_baseline(self) -> None:
        self.assertEqual("docker-baseline", self._step("Capture Docker baseline")["id"])
        teardown = self._step("Verify Docker teardown")
        self.assertEqual("${{ always() && steps.docker-baseline.outcome == 'success' }}", teardown["if"])
        self.assertIn("comm -13", teardown["run"])

    def test_private_checkout_and_release_capability_precede_provider(self) -> None:
        private = self._step("Checkout pinned private corpus")
        self.assertEqual("ShaftHQ/ChaosGauge-private", private["with"]["repository"])
        self.assertEqual("5c5c00896139c767946747ba38029d88fe750472", private["with"]["ref"])
        self.assertFalse(private["with"]["persist-credentials"])
        capability_index = next(index for index, step in enumerate(self.steps) if step.get("name") == "Prepare private evidence storage")
        provider_index = next(index for index, step in enumerate(self.steps) if step.get("name") == "Run excluded two-arm canary")
        self.assertLess(capability_index, provider_index)
        capability = str(self.steps[capability_index]["run"])
        self.assertIn("public_canary_bridge.py prepare", capability)
        self.assertEqual("${{ secrets.BOT_TOKEN }}", self.steps[capability_index]["env"]["GH_TOKEN"])
        self.assertEqual("read", self.workflow["permissions"]["contents"])
        provider = self.steps[provider_index]["env"]
        self.assertNotIn("BOT_TOKEN", provider)
        self.assertNotIn("GH_TOKEN", provider)

    def test_raw_evidence_cannot_upload_publicly(self) -> None:
        artifact = self._step("Publish sanitized receipt")
        self.assertIn("actions/upload-artifact@", artifact["uses"])
        self.assertEqual("${{ runner.temp }}/chaosgauge-canary-receipt.json", artifact["with"]["path"])
        self.assertNotIn("raw", str(artifact["with"]))
        release = str(self._step("Store evidence in private draft release")["run"])
        self.assertIn("public_canary_bridge.py publish", release)
        bridge = BRIDGE_PATH.read_text(encoding="utf-8")
        self.assertIn("ShaftHQ/ChaosGauge-private", bridge)
        self.assertIn("--draft", bridge)

    def test_paid_raw_result_is_retained_privately_when_receipt_generation_fails(self) -> None:
        failure = self._step("Preserve paid raw result after receipt failure")
        self.assertEqual("${{ always() && steps.private-evidence.outputs.action == 'run' }}", failure["if"])
        self.assertIn("public_canary_bridge.py preserve-failure", str(failure["run"]))
        self.assertIn("test -f \"$CANARY_RAW\"", str(failure["run"]))
        self.assertIn("test ! -f \"$CANARY_RECEIPT\"", str(failure["run"]))
        self.assertLess(
            self.steps.index(failure),
            self.steps.index(self._step("Validate and secret-scan evidence")),
        )

    def test_context_values_never_enter_shell_source(self) -> None:
        for step in self.steps:
            run = step.get("run")
            if isinstance(run, str):
                self.assertNotIn("${{", run)
        self.assertIn("GH_TOKEN: ${{ secrets.BOT_TOKEN }}", self.text)
        self.assertIn("OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}", self.text)
        for step in self.steps:
            if step.get("name") != "Checkout pinned private corpus":
                self.assertNotIn("BOT_TOKEN", step.get("env", {}))

    def test_excluded_canary_uses_local_harbor_without_hub_credential_or_claim(self) -> None:
        """This native-runtime canary must not require or advertise Harbor Hub."""
        provider = self._step("Run excluded two-arm canary")
        self.assertNotIn("HARBOR_API_KEY", provider["env"])
        self.assertNotIn("CHAOSGAUGE_HARBOR_TOKEN", self.text)
        self.assertNotIn("Harbor credential unavailable", str(provider["run"]))
        self.assertNotIn("Harbor Hub", self.text)
        self.assertNotIn("Hub URL", self.text)

    def test_direct_prepare_cli_reaches_credential_gate_from_any_cwd(self) -> None:
        """The documented direct script call must not depend on repository import paths."""
        with TemporaryDirectory() as directory:
            receipt = Path(directory) / "receipt.json"
            command = [
                sys.executable, str(BRIDGE_PATH), "prepare", "--repository", str(ROOT),
                "--run-id", "123", "--receipt-out", str(receipt),
            ]
            environment = {key: value for key, value in os.environ.items() if key not in {"GH_TOKEN", "PYTHONPATH"}}
            for cwd in (ROOT, Path(directory)):
                with self.subTest(cwd=cwd):
                    result = subprocess.run(  # nosec B603 B607 - fixed local regression invocation.
                        command, cwd=cwd, env=environment, capture_output=True, text=True
                    )
                    self.assertNotEqual(0, result.returncode)
                    self.assertIn("GH_TOKEN is unavailable", result.stderr)
                    self.assertNotIn("ModuleNotFoundError", result.stderr)

    def test_owner_exception_has_only_workflow_local_canary_limits(self) -> None:
        self.assertIn("owner-authorized excluded-canary exception", self.text.lower())
        self.assertIn("ShaftHQ/ChaosGauge-private#4", self.text)
        self.assertIn("#5462", self.text)
        self.assertIn("not a provider-side spend cap", self.text)
        self.assertEqual(60, self.job["timeout-minutes"])
        provider = self._step("Run excluded two-arm canary")
        self.assertEqual("120000", provider["env"]["CANARY_MAX_ACCOUNTED_TOKENS"])
        self.assertIn("two-arm accounted-token budget", str(provider["run"]))
        self.assertNotIn("CHAOSGAUGE_OPENAI_API_KEY", self.text)

    def test_metadata_preflight_requires_read_write_and_workflow_scopes_without_writing(self) -> None:
        seen = []

        class Response:
            headers = {"X-OAuth-Scopes": "repo, workflow"}

            def read(self):
                return b'{"private": true, "permissions": {"pull": true, "push": true}}'

            def __enter__(self):
                return self

            def __exit__(self, *_):
                return None

        def opener(request, **_):
            seen.append(request)
            return Response()

        BRIDGE.preflight("token", opener)
        self.assertEqual("GET", seen[0].get_method())
        self.assertEqual("https://api.github.com/repos/ShaftHQ/ChaosGauge-private", seen[0].full_url)

        class ReadOnlyResponse(Response):
            headers = {"X-OAuth-Scopes": "repo"}

        with self.assertRaisesRegex(ValueError, "scopes are not provable"):
            BRIDGE.preflight("token", lambda *_args, **_kwargs: ReadOnlyResponse())

    def test_private_evidence_is_one_deterministic_bundle(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            raw, receipt = root / "raw.json", root / "receipt.json"
            raw.write_text('{"raw":true}\n', encoding="utf-8")
            receipt.write_text('{"receipt":true}\n', encoding="utf-8")
            first = BRIDGE.bundle(raw, receipt, root, "123")
            second = BRIDGE.bundle(raw, receipt, root, "123")
            self.assertEqual(first.read_bytes(), second.read_bytes())
            manifest, entries = BRIDGE.bundle_contents(first)
            self.assertEqual({"raw.json", "receipt.json"}, set(entries))
            self.assertEqual(
                hashlib.sha256(raw.read_bytes()).hexdigest(), manifest["files"]["raw.json"]
            )
            self.assertEqual(
                hashlib.sha256(receipt.read_bytes()).hexdigest(), manifest["files"]["receipt.json"]
            )

    def test_prepare_creates_exact_private_draft_before_provider(self) -> None:
        calls = []
        tag = "chaosgauge-canary-123"
        marker_name = f"{tag}-provider-started.json"
        marker_content = b'{"runId":"123","schemaVersion":1,"state":"provider-started"}'
        release = {
            "tagName": tag, "isDraft": True, "targetCommitish": BRIDGE.PRIVATE_COMMIT,
            "name": "ChaosGauge excluded canary 123", "assets": [],
        }

        def run(arguments, **kwargs):
            calls.append((arguments, kwargs))
            if arguments[2] == "view" and len(calls) == 1:
                raise BRIDGE.subprocess.CalledProcessError(1, arguments)
            if arguments[2] == "view":
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
            if arguments[2] == "upload":
                marker = Path(arguments[4])
                self.assertEqual(marker_name, marker.name)
                self.assertEqual(marker_content, marker.read_bytes())
                release["assets"] = [{
                    "name": marker.name,
                    "digest": f"sha256:{hashlib.sha256(marker_content).hexdigest()}",
                }]
            if arguments[2] == "download":
                Path(arguments[arguments.index("--dir") + 1], marker_name).write_bytes(marker_content)
            return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

        with patch.object(BRIDGE, "preflight"):
            self.assertEqual("run", BRIDGE.prepare(ROOT, "123", "token", run=run))
        self.assertEqual("create", calls[1][0][2])
        self.assertIn("--draft", calls[1][0])
        self.assertTrue(any(arguments[2] == "upload" for arguments, _ in calls))
        self.assertTrue(any(arguments[2] == "download" for arguments, _ in calls))

    def test_prepare_refuses_marker_only_state_after_evidence_upload_failure(self) -> None:
        """A run lease survives post-provider loss, so a rerun never repays provider arms."""
        tag = "chaosgauge-canary-123"
        marker_name = f"{tag}-provider-started.json"
        release = {
            "tagName": tag, "isDraft": True, "targetCommitish": BRIDGE.PRIVATE_COMMIT,
            "name": "ChaosGauge excluded canary 123", "assets": [],
        }

        def run(arguments, **_):
            if arguments[2] == "view":
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
            if arguments[2] == "upload":
                marker = Path(arguments[4])
                release["assets"] = [{
                    "name": marker.name,
                    "digest": f"sha256:{hashlib.sha256(marker.read_bytes()).hexdigest()}",
                }]
            if arguments[2] == "download":
                Path(arguments[arguments.index("--dir") + 1], marker_name).write_text(
                    '{"runId":"123","schemaVersion":1,"state":"provider-started"}', encoding="utf-8"
                )
            return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

        with patch.object(BRIDGE, "preflight"):
            self.assertEqual("run", BRIDGE.prepare(ROOT, "123", "token", run=run))
            with self.assertRaisesRegex(ValueError, "provider start is already recorded"):
                BRIDGE.prepare(ROOT, "123", "token", run=run)

        self.assertEqual([marker_name], [asset["name"] for asset in release["assets"]])

    def test_prepare_recovers_existing_complete_bundle_without_provider(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            raw, receipt, recovered = root / "raw.json", root / "receipt.json", root / "recovered.json"
            raw.write_text('{"raw":true}\n', encoding="utf-8")
            receipt.write_text('{"receipt":true}\n', encoding="utf-8")
            bundle = BRIDGE.bundle(raw, receipt, root, "123")
            marker = root / "chaosgauge-canary-123-provider-started.json"
            marker.write_text('{"runId":"123","schemaVersion":1,"state":"provider-started"}', encoding="utf-8")
            digest = f"sha256:{hashlib.sha256(bundle.read_bytes()).hexdigest()}"
            release = {
                "tagName": "chaosgauge-canary-123", "isDraft": True,
                "targetCommitish": BRIDGE.PRIVATE_COMMIT,
                "name": "ChaosGauge excluded canary 123",
                "assets": [
                    {"name": marker.name, "digest": f"sha256:{hashlib.sha256(marker.read_bytes()).hexdigest()}"},
                    {"name": bundle.name, "digest": digest},
                ],
            }
            calls = []

            def run(arguments, **kwargs):
                calls.append((arguments, kwargs))
                if arguments[2] == "view":
                    return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
                if arguments[2] == "download":
                    destination = Path(arguments[arguments.index("--dir") + 1])
                    name = arguments[arguments.index("--pattern") + 1]
                    destination.joinpath(name).write_bytes({marker.name: marker.read_bytes(), bundle.name: bundle.read_bytes()}[name])
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

            with (
                patch.object(BRIDGE, "preflight"),
                patch.object(BRIDGE, "_validate_contents"),
            ):
                self.assertEqual("recover", BRIDGE.prepare(ROOT, "123", "token", receipt_out=recovered, run=run))
            self.assertEqual(receipt.read_bytes(), recovered.read_bytes())
            self.assertTrue(any(arguments[2] == "download" for arguments, _ in calls))

    def test_prepare_refuses_bundle_without_remote_start_marker(self) -> None:
        release = {
            "tagName": "chaosgauge-canary-123", "isDraft": True,
            "targetCommitish": BRIDGE.PRIVATE_COMMIT,
            "name": "ChaosGauge excluded canary 123",
            "assets": [{
                "name": "chaosgauge-canary-123-evidence.zip",
                "digest": "sha256:" + "0" * 64,
            }],
        }

        def run(arguments, **_):
            return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")

        with patch.object(BRIDGE, "preflight"):
            with self.assertRaisesRegex(ValueError, "release is incomplete"):
                BRIDGE.prepare(ROOT, "123", "token", receipt_out=ROOT / "receipt.json", run=run)

    def test_prepare_refuses_partial_private_evidence_before_provider(self) -> None:
        release = {
            "tagName": "chaosgauge-canary-123", "isDraft": True,
            "targetCommitish": BRIDGE.PRIVATE_COMMIT,
            "name": "ChaosGauge excluded canary 123",
            "assets": [{"name": "raw.json", "digest": "sha256:" + "0" * 64}],
        }

        def run(arguments, **_):
            return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")

        with patch.object(BRIDGE, "preflight"):
            with self.assertRaisesRegex(ValueError, "release is incomplete"):
                BRIDGE.prepare(ROOT, "123", "token", receipt_out=ROOT / "receipt.json", run=run)

    def test_publish_replaces_one_bundle_then_verifies_remote_digest(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            raw, receipt = root / "raw.json", root / "receipt.json"
            raw.write_text('{"raw":true}\n', encoding="utf-8")
            receipt.write_text('{"receipt":true}\n', encoding="utf-8")
            marker = root / "chaosgauge-canary-123-provider-started.json"
            marker.write_text('{"runId":"123","schemaVersion":1,"state":"provider-started"}', encoding="utf-8")
            release = {
                "tagName": "chaosgauge-canary-123", "isDraft": True,
                "targetCommitish": BRIDGE.PRIVATE_COMMIT,
                "name": "ChaosGauge excluded canary 123",
                "assets": [{
                    "name": marker.name,
                    "digest": f"sha256:{hashlib.sha256(marker.read_bytes()).hexdigest()}",
                }],
            }
            calls = []
            uploaded = {marker.name: marker.read_bytes()}

            def run(arguments, **kwargs):
                calls.append((arguments, kwargs))
                if arguments[2] == "view":
                    return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
                if arguments[2] == "upload":
                    uploaded_path = Path(arguments[4])
                    uploaded[uploaded_path.name] = uploaded_path.read_bytes()
                    release["assets"].append({
                        "name": uploaded_path.name,
                        "digest": f"sha256:{hashlib.sha256(uploaded_path.read_bytes()).hexdigest()}",
                    })
                if arguments[2] == "download":
                    name = arguments[arguments.index("--pattern") + 1]
                    Path(arguments[arguments.index("--dir") + 1], name).write_bytes(uploaded[name])
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

            with (
                patch.object(BRIDGE, "preflight"),
                patch.object(BRIDGE, "validate"),
                patch.object(BRIDGE, "_validate_contents"),
            ):
                BRIDGE.publish(raw, receipt, ROOT, "123", "token", run=run)
            upload = next(arguments for arguments, _ in calls if arguments[2] == "upload")
            self.assertEqual(["chaosgauge-canary-123-evidence.zip"], [Path(value).name for value in upload if value.endswith(".zip")])
            self.assertNotIn("--clobber", upload)

    def test_preserve_failure_uploads_secret_scanned_raw_without_an_invalid_receipt(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            raw = root / "raw.json"
            raw.write_text('{"trial_results":[]}\n', encoding="utf-8")
            marker = root / "chaosgauge-canary-123-provider-started.json"
            marker.write_text('{"runId":"123","schemaVersion":1,"state":"provider-started"}', encoding="utf-8")
            release = {
                "tagName": "chaosgauge-canary-123", "isDraft": True,
                "targetCommitish": BRIDGE.PRIVATE_COMMIT,
                "name": "ChaosGauge excluded canary 123",
                "assets": [{
                    "name": marker.name,
                    "digest": f"sha256:{hashlib.sha256(marker.read_bytes()).hexdigest()}",
                }],
            }
            uploaded = {marker.name: marker.read_bytes()}

            def run(arguments, **_):
                if arguments[2] == "view":
                    return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
                if arguments[2] == "upload":
                    uploaded_path = Path(arguments[4])
                    uploaded[uploaded_path.name] = uploaded_path.read_bytes()
                    release["assets"].append({
                        "name": uploaded_path.name,
                        "digest": f"sha256:{hashlib.sha256(uploaded_path.read_bytes()).hexdigest()}",
                    })
                if arguments[2] == "download":
                    name = arguments[arguments.index("--pattern") + 1]
                    Path(arguments[arguments.index("--dir") + 1], name).write_bytes(uploaded[name])
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

            with patch.object(BRIDGE, "preflight"):
                BRIDGE.preserve_failure(raw, ROOT, "123", "token", run=run)

            failure_name = "chaosgauge-canary-123-failure-evidence.zip"
            self.assertEqual({marker.name, failure_name}, {asset["name"] for asset in release["assets"]})
            self.assertEqual("failed", BRIDGE._release_state(release, "123"))
            archive = root / failure_name
            archive.write_bytes(uploaded[failure_name])
            _, content = BRIDGE.failure_bundle_contents(archive)
            self.assertEqual({"raw.json"}, set(content))

            with patch.object(BRIDGE, "preflight"):
                with self.assertRaisesRegex(ValueError, "provider start is already recorded"):
                    BRIDGE.prepare(ROOT, "123", "token", receipt_out=root / "recover.json", run=run)

    def test_preserve_failure_rejects_secret_shaped_raw_evidence_before_release_access(self) -> None:
        with TemporaryDirectory() as directory:
            raw = Path(directory) / "raw.json"
            raw.write_bytes(BRIDGE.SECRET_CANARIES[0])
            with patch.object(BRIDGE, "preflight"):
                with self.assertRaisesRegex(ValueError, "secret-shaped"):
                    BRIDGE.preserve_failure(raw, ROOT, "123", "token")


if __name__ == "__main__":
    unittest.main()
