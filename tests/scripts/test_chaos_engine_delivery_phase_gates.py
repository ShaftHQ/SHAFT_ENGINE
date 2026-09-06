"""Delivery phase gates + opt-in research-before-mutation (#5583)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "chaos-engine/references/delivery-phase-gates.md"


def load_module(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class DeliveryPhaseGatesTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.reflection = load_module("ce_reflection_gates", "chaos-engine/hooks/reflection.py")

    def test_documentation_maps_phases_and_enforcement_env(self):
        text = DOC.read_text(encoding="utf-8")
        self.assertIn("Research receipt", text)
        self.assertIn("CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT", text)
        self.assertIn("research-preflight", text)

    def test_research_preflight_marker_round_trip(self):
        with tempfile.TemporaryDirectory() as temporary:
            with mock.patch.dict(os.environ, {"TMPDIR": temporary, "TEMP": temporary}):
                session = "research-gate-session"
                self.assertFalse(self.reflection.has_research_preflight(session))
                self.assertTrue(self.reflection.record_research_preflight(session))
                self.assertTrue(self.reflection.has_research_preflight(session))

    def test_guard_denies_mutation_until_preflight_when_enforced(self):
        # Load guard after reflection helpers exist
        guard = load_module("ce_guard_gates", "chaos-engine/hooks/guard.py")
        with tempfile.TemporaryDirectory() as temporary:
            env = {
                **os.environ,
                "TMPDIR": temporary,
                "TEMP": temporary,
                "CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT": "1",
                "CHAOS_ENGINE_HOST": "claude",
            }
            event = {
                "hook_event_name": "PreToolUse",
                "session_id": "enforce-research",
                "tool_name": "Bash",
                "tool_input": {"command": "echo changed > out.txt"},
            }
            with mock.patch.dict(os.environ, env, clear=False):
                code = guard._run_event(event, "claude")
                self.assertEqual(2, code)
                self.reflection.record_research_preflight("enforce-research")
                # After preflight, catastrophic/other rules may still apply; echo redirect
                # is a mutation but should pass research gate. Capture stdout.
                import io
                from contextlib import redirect_stdout

                buf = io.StringIO()
                with redirect_stdout(buf):
                    code2 = guard._run_event(event, "claude")
                # May still deny for session/worktree reasons; ensure reason is NOT research
                if code2 == 2:
                    payload = json.loads(buf.getvalue().strip().splitlines()[-1])
                    self.assertNotIn("Research receipt required", payload.get("reason", ""))


if __name__ == "__main__":
    unittest.main()
