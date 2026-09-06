"""SessionStart locator-only / progressive disclosure parity (#5580)."""

from __future__ import annotations

import importlib.util
import io
import json
import sys
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


def load_module(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {relative}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def unwrap_context(payload: dict) -> str:
    if "additionalContext" in payload:
        return str(payload["additionalContext"])
    specific = payload.get("hookSpecificOutput")
    if isinstance(specific, dict) and "additionalContext" in specific:
        return str(specific["additionalContext"])
    raise AssertionError(f"no additionalContext in {payload!r}")


class SessionStartLocatorParityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.kernel = load_module("ce_kernel_ss", "chaos-engine/hooks/kernel.py")
        cls.lifecycle = load_module("ce_lifecycle_ss", "chaos-engine/hooks/lifecycle.py")
        cls.caveman = (
            ROOT / "chaos-engine/vendor/caveman/skills/caveman/SKILL.md"
        ).read_text(encoding="utf-8")
        cls.ponytail = (
            ROOT / "chaos-engine/vendor/ponytail/skills/ponytail/SKILL.md"
        ).read_text(encoding="utf-8")

    def test_builder_stays_under_byte_budget_and_omits_skill_bodies(self):
        context = self.lifecycle.session_start_context("token-fixture", "activation")
        encoded = context.encode("utf-8")
        self.assertLessEqual(len(encoded), self.lifecycle.SESSION_START_MAX_BYTES)
        # Soft token smoke (~4 chars/token): keep well under a 1k-token dump.
        self.assertLessEqual(len(encoded) // 4, 512)
        self.assertNotIn(self.caveman, context)
        self.assertNotIn(self.ponytail, context)
        for name in ("caveman", "ponytail"):
            self.assertIn(
                f"chaos-engine/vendor/{name}/skills/{name}/SKILL.md", context
            )
        self.assertIn("companion intensity", context.casefold())

    def test_every_host_adapts_identical_locator_context(self):
        token = "parity-session-token"
        activation = "ChaosEngine activation"
        base_context = self.lifecycle.session_start_context(token, activation)

        def session_callback(_event, _host):
            print(json.dumps({"additionalContext": base_context}))
            return 0

        contexts: dict[str, str] = {}
        for host in sorted(self.kernel.HOST_CAPABILITIES):
            stdout = io.StringIO()
            stderr = io.StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = self.lifecycle.run_hook_protocol(
                    json.dumps(
                        {
                            "hook_event_name": "SessionStart",
                            "session_id": "ss-parity",
                        }
                    ),
                    {"SessionStart": session_callback},
                    normalize=self.kernel.normalize_hook_input,
                    host_for_input=lambda _raw, selected=host: selected,
                    adapt_output=self.kernel.adapt_hook_output,
                )
            self.assertEqual(0, code)
            rendered = stdout.getvalue().strip()
            self.assertTrue(rendered, msg=f"empty SessionStart payload host={host}")
            payload = json.loads(rendered.splitlines()[-1])
            context = unwrap_context(payload)
            encoded = json.dumps(payload).encode("utf-8")
            self.assertLessEqual(
                len(encoded),
                self.lifecycle.SESSION_START_MAX_BYTES,
                msg=f"host={host} over budget",
            )
            self.assertNotIn(self.caveman, context)
            self.assertNotIn(self.ponytail, context)
            contexts[host] = context

        # Progressive disclosure contract: every host gets the same locators.
        unique = set(contexts.values())
        self.assertEqual(1, len(unique), msg=contexts)

    def test_matrix_sessionstart_row_is_green(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertIn(
            "| SessionStart locator-only / progressive disclosure | A | A | A | A | A |",
            text,
        )
        self.assertIn("test_chaos_engine_sessionstart_locator_parity.py", text)
        self.assertIn("SESSION_START_MAX_BYTES", text)


if __name__ == "__main__":
    unittest.main()
