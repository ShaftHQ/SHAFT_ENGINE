"""Native Headroom companion — pin, policy, doctor, SessionStart (#5613)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PIN = ROOT / "chaos-engine/vendor/headroom/PIN.json"
SKILL = ROOT / "chaos-engine/vendor/headroom/skills/headroom/SKILL.md"
POLICY = ROOT / "chaos-engine/headroom_policy.py"
DOC = ROOT / "chaos-engine/references/headroom.md"
NOTICES = ROOT / "chaos-engine/THIRD_PARTY_NOTICES.md"
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class HeadroomCompanionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(POLICY, "ce_headroom_policy")
        cls.lifecycle = load(ROOT / "chaos-engine/hooks/lifecycle.py", "ce_lifecycle_hr")
        cls.install = load(ROOT / "chaos-engine/install.py", "ce_install_hr")

    def test_pin_enforces_agent_90_and_apache(self):
        pin = json.loads(PIN.read_text(encoding="utf-8"))
        self.assertEqual("0.37.0", pin["version"])
        self.assertEqual("headroom-ai", pin["package"])
        self.assertEqual("Apache-2.0", pin["license"])
        self.assertEqual("agent-90", pin["ce_defaults"]["HEADROOM_SAVINGS_PROFILE"])
        self.assertEqual("off", pin["ce_defaults"]["HEADROOM_BEACON"])
        self.assertEqual("disabled", pin["ce_defaults"]["HEADROOM_MEMORY_INJECTION_MODE"])
        self.assertTrue(SKILL.is_file())
        self.assertTrue(DOC.is_file())
        notices = NOTICES.read_text(encoding="utf-8")
        self.assertIn("Headroom", notices)
        self.assertIn("Apache-2.0", notices)

    def test_policy_maps_token_budgets_and_xor_output_shaper(self):
        self.assertEqual(0, self.policy.self_check())
        self.assertEqual("agent-90", self.policy.profile_for_token_budget("ultra" + "-lean"))  # nosec B105
        self.assertEqual("balanced", self.policy.profile_for_token_budget("balanced"))
        self.assertEqual("coding", self.policy.profile_for_token_budget("deep"))
        self.assertEqual(
            "0",
            self.policy.resolve_output_shaper(ponytail_active=True, requested="1"),
        )
        self.assertEqual(
            "1",
            self.policy.resolve_output_shaper(ponytail_active=False, requested="1"),
        )
        env = self.policy.ce_env(token_budget="ultra" + "-lean", ponytail_active=True)  # nosec B105
        self.assertEqual("agent-90", env["HEADROOM_SAVINGS_PROFILE"])
        self.assertEqual("0.10", env["HEADROOM_TARGET_RATIO"])
        self.assertEqual("1", env["HEADROOM_FORCE_KOMPRESS"])
        self.assertEqual("token", env["HEADROOM_MODE"])
        self.assertEqual("off", env["HEADROOM_BEACON"])
        self.assertEqual("disabled", env["HEADROOM_MEMORY_INJECTION_MODE"])
        self.assertEqual("0", env["HEADROOM_OUTPUT_SHAPER"])

    def test_doctor_status_is_optional_with_fix_next(self):
        status = self.policy.doctor_status()
        self.assertEqual("optional", status["taskImpact"])
        self.assertIn(status["status"], {"healthy", "absent", "broken"})
        self.assertIn("headroom-ai==0.37.0", status["pin"])
        self.assertIn("uv tool install", status["detail"])
        # Optional absence stays non-blocking (fix-next None), matching maven-tools-mcp.
        if status["status"] == "absent":
            self.assertIsNone(self.install.component_fix_next("headroom", status))
        broken = {**status, "status": "broken", "detail": status["detail"]}
        fix = self.install.component_fix_next("headroom", broken)
        self.assertIsNotNone(fix)
        self.assertIn("uv tool install", fix)

    def test_session_start_includes_headroom_under_byte_budget(self):
        previous = os.environ.get("CHAOS_ENGINE_TOKEN_BUDGET")
        os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = "ultra" + "-lean"  # nosec B105
        try:
            context = self.lifecycle.session_start_context("t", "activation")
        finally:
            if previous is None:
                os.environ.pop("CHAOS_ENGINE_TOKEN_BUDGET", None)
            else:
                os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = previous
        self.assertIn("agent-90", context)
        self.assertIn("headroom", context.casefold())
        self.assertIn("beacon=off", context.casefold())
        self.assertLessEqual(
            len(context.encode("utf-8")),
            self.lifecycle.SESSION_START_MAX_BYTES,
        )

    def test_host_parity_lists_headroom_row(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertIn("Headroom", text)
        self.assertIn("GAP-HEADROOM-GEMINI", text)

    def test_capability_policy_includes_optional_headroom(self):
        caps, _ = self.install.load_capability_policy(
            ROOT / "chaos-engine", "portable"
        )
        self.assertEqual("optional", caps["headroom"]["taskImpact"])
        self.assertIn("headroom", self.install.CAPABILITY_COMPONENTS)

    def test_export_env_and_install_command(self):
        script = self.policy.export_env_script(token_budget="ultra" + "-lean")  # nosec B105
        self.assertIn("HEADROOM_SAVINGS_PROFILE", script)
        self.assertIn("agent-90", script)
        self.assertIn('headroom-ai==0.37.0', self.policy.install_command())


if __name__ == "__main__":
    unittest.main()
