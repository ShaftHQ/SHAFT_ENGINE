"""Token budget modes ultra-lean | balanced | deep (#5581)."""

from __future__ import annotations

import importlib.util
import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "chaos-engine/references/token-budget-modes.md"


def load_lifecycle():
    path = ROOT / "chaos-engine/hooks/lifecycle.py"
    spec = importlib.util.spec_from_file_location("ce_lifecycle_budget", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TokenBudgetModesTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.lifecycle = load_lifecycle()

    def test_default_is_balanced_and_modes_are_documented(self):
        self.assertEqual("balanced", self.lifecycle.TOKEN_BUDGET_DEFAULT)
        self.assertEqual(
            {"ultra-lean", "balanced", "deep"},
            set(self.lifecycle.TOKEN_BUDGET_MODES),
        )
        text = DOC.read_text(encoding="utf-8")
        for mode in ("ultra-lean", "balanced", "deep"):
            self.assertIn(f"`{mode}`", text)
        self.assertIn("CHAOS_ENGINE_TOKEN_BUDGET", text)

    def test_ultra_lean_guidance_is_demonstrably_smaller(self):
        lean = self.lifecycle.token_budget_guidance("ultra-lean")
        balanced = self.lifecycle.token_budget_guidance("balanced")
        deep = self.lifecycle.token_budget_guidance("deep")
        self.assertLess(len(lean), len(balanced))
        self.assertLess(len(balanced), len(deep))
        self.assertLess(
            self.lifecycle.TOKEN_BUDGET_MODES["ultra-lean"]["read_line_budget"],
            self.lifecycle.TOKEN_BUDGET_MODES["balanced"]["read_line_budget"],
        )
        self.assertLess(
            self.lifecycle.TOKEN_BUDGET_MODES["balanced"]["read_line_budget"],
            self.lifecycle.TOKEN_BUDGET_MODES["deep"]["read_line_budget"],
        )

    def test_session_start_respects_env_mode_under_byte_budget(self):
        for mode in ("ultra-lean", "balanced", "deep"):
            with self.subTest(mode=mode):
                previous = os.environ.get("CHAOS_ENGINE_TOKEN_BUDGET")
                os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = mode
                try:
                    context = self.lifecycle.session_start_context("t", "activation")
                finally:
                    if previous is None:
                        os.environ.pop("CHAOS_ENGINE_TOKEN_BUDGET", None)
                    else:
                        os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = previous
                self.assertIn("token budget", context.casefold())
                self.assertIn(mode, context.casefold())
                self.assertLessEqual(
                    len(context.encode("utf-8")),
                    self.lifecycle.SESSION_START_MAX_BYTES,
                )

    def test_unknown_env_falls_back_to_balanced(self):
        self.assertEqual(
            "balanced",
            self.lifecycle.resolve_token_budget_mode(
                {"CHAOS_ENGINE_TOKEN_BUDGET": "nope"}
            ),
        )


if __name__ == "__main__":
    unittest.main()
