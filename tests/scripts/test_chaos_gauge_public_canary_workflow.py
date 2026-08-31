"""Public excluded-canary bridge contract (#5462)."""

from __future__ import annotations

import unittest
from unittest.mock import patch
import importlib.util
from pathlib import Path

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
        install = str(self._step("Install pinned native runtime")["run"])
        self.assertIn("--require-hashes", install)
        self.assertIn("@openai/codex@0.118.0", install)
        self.assertIn("docker version --format", install)

    def test_private_checkout_and_release_capability_precede_provider(self) -> None:
        private = self._step("Checkout pinned private corpus")
        self.assertEqual("ShaftHQ/ChaosGauge-private", private["with"]["repository"])
        self.assertEqual("08551a3db4376438acddd77422554ce710a58624", private["with"]["ref"])
        self.assertFalse(private["with"]["persist-credentials"])
        capability_index = next(index for index, step in enumerate(self.steps) if step.get("name") == "Prove private release capability")
        provider_index = next(index for index, step in enumerate(self.steps) if step.get("name") == "Run excluded two-arm canary")
        self.assertLess(capability_index, provider_index)
        capability = str(self.steps[capability_index]["run"])
        self.assertIn("public_canary_bridge.py preflight", capability)
        self.assertEqual("${{ secrets.BOT_TOKEN }}", self.steps[capability_index]["env"]["BOT_TOKEN"])
        self.assertEqual("read", self.workflow["permissions"]["contents"])

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
        self.assertIn("BOT_TOKEN: ${{ secrets.BOT_TOKEN }}", self.text)
        self.assertIn("OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}", self.text)
        self.assertIn("HARBOR_API_KEY: ${{ secrets.CHAOSGAUGE_HARBOR_TOKEN }}", self.text)

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

    def test_private_draft_release_uses_argv_not_shell_and_only_two_evidence_assets(self) -> None:
        calls = []
        with (
            patch.object(BRIDGE, "preflight"),
            patch.object(BRIDGE, "validate"),
        ):
            BRIDGE.publish(
                Path("raw.json"), Path("receipt.json"), ROOT, "123", "token",
                run=lambda arguments, **kwargs: calls.append((arguments, kwargs)),
            )
        arguments, kwargs = calls[0]
        self.assertEqual(["raw.json", "receipt.json"], [value for value in arguments if value.endswith(".json")])
        self.assertIn("--repo", arguments)
        self.assertEqual("ShaftHQ/ChaosGauge-private", arguments[arguments.index("--repo") + 1])
        self.assertIn("--draft", arguments)
        self.assertTrue(kwargs["check"])


if __name__ == "__main__":
    unittest.main()
