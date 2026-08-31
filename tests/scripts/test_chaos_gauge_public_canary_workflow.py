"""Public excluded-canary bridge contract (#5462)."""

from __future__ import annotations

import unittest
from unittest.mock import patch
import importlib.util
import hashlib
import json
from pathlib import Path
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

    def test_private_checkout_and_release_capability_precede_provider(self) -> None:
        private = self._step("Checkout pinned private corpus")
        self.assertEqual("ShaftHQ/ChaosGauge-private", private["with"]["repository"])
        self.assertEqual("08551a3db4376438acddd77422554ce710a58624", private["with"]["ref"])
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

    def test_context_values_never_enter_shell_source(self) -> None:
        for step in self.steps:
            run = step.get("run")
            if isinstance(run, str):
                self.assertNotIn("${{", run)
        self.assertIn("GH_TOKEN: ${{ secrets.BOT_TOKEN }}", self.text)
        self.assertIn("OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}", self.text)
        self.assertIn("HARBOR_API_KEY: ${{ secrets.CHAOSGAUGE_HARBOR_TOKEN }}", self.text)
        for step in self.steps:
            if step.get("name") != "Checkout pinned private corpus":
                self.assertNotIn("BOT_TOKEN", step.get("env", {}))

    def test_owner_exception_has_only_workflow_local_canary_limits(self) -> None:
        self.assertIn("owner-authorized excluded-canary exception", self.text.lower())
        self.assertIn("ShaftHQ/ChaosGauge-private#3", self.text)
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
            return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

        with patch.object(BRIDGE, "preflight"):
            self.assertEqual("run", BRIDGE.prepare(ROOT, "123", "token", run=run))
        self.assertEqual("create", calls[1][0][2])
        self.assertIn("--draft", calls[1][0])

    def test_prepare_recovers_existing_complete_bundle_without_provider(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            raw, receipt, recovered = root / "raw.json", root / "receipt.json", root / "recovered.json"
            raw.write_text('{"raw":true}\n', encoding="utf-8")
            receipt.write_text('{"receipt":true}\n', encoding="utf-8")
            bundle = BRIDGE.bundle(raw, receipt, root, "123")
            digest = f"sha256:{hashlib.sha256(bundle.read_bytes()).hexdigest()}"
            release = {
                "tagName": "chaosgauge-canary-123", "isDraft": True,
                "targetCommitish": BRIDGE.PRIVATE_COMMIT,
                "name": "ChaosGauge excluded canary 123",
                "assets": [{"name": bundle.name, "digest": digest}],
            }
            calls = []

            def run(arguments, **kwargs):
                calls.append((arguments, kwargs))
                if arguments[2] == "view":
                    return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
                if arguments[2] == "download":
                    Path(arguments[arguments.index("--dir") + 1], bundle.name).write_bytes(bundle.read_bytes())
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

            with (
                patch.object(BRIDGE, "preflight"),
                patch.object(BRIDGE, "_validate_contents"),
            ):
                self.assertEqual("recover", BRIDGE.prepare(ROOT, "123", "token", receipt_out=recovered, run=run))
            self.assertEqual(receipt.read_bytes(), recovered.read_bytes())
            self.assertTrue(any(arguments[2] == "download" for arguments, _ in calls))

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
            bundle = BRIDGE.bundle(raw, receipt, root, "123")
            digest = f"sha256:{hashlib.sha256(bundle.read_bytes()).hexdigest()}"
            release = {
                "tagName": "chaosgauge-canary-123", "isDraft": True,
                "targetCommitish": BRIDGE.PRIVATE_COMMIT,
                "name": "ChaosGauge excluded canary 123",
                "assets": [{"name": bundle.name, "digest": digest}],
            }
            calls = []

            def run(arguments, **kwargs):
                calls.append((arguments, kwargs))
                if arguments[2] == "view":
                    return BRIDGE.subprocess.CompletedProcess(arguments, 0, json.dumps(release), "")
                if arguments[2] == "download":
                    Path(arguments[arguments.index("--dir") + 1], bundle.name).write_bytes(bundle.read_bytes())
                return BRIDGE.subprocess.CompletedProcess(arguments, 0, "", "")

            with (
                patch.object(BRIDGE, "preflight"),
                patch.object(BRIDGE, "validate"),
                patch.object(BRIDGE, "_validate_contents"),
            ):
                BRIDGE.publish(raw, receipt, ROOT, "123", "token", run=run)
            upload = next(arguments for arguments, _ in calls if arguments[2] == "upload")
            self.assertEqual([bundle.name], [Path(value).name for value in upload if value.endswith(".zip")])
            self.assertIn("--clobber", upload)


if __name__ == "__main__":
    unittest.main()
