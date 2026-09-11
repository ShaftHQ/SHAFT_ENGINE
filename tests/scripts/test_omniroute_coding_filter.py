"""Unit tests for OmniRoute coding candidate allow/deny ranking helpers."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroute/scripts/runner.py"
SKILL = ROOT / "chaos-engine/skills/omniroute/SKILL.md"
SPEC = importlib.util.spec_from_file_location("omniroute_coding_filter_runner", RUNNER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("OmniRoute runner could not be loaded")
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class OmniRouteCodingFilterTest(unittest.TestCase):
    def test_deny_markers_drop_safety_guard_translate_nano_tiny(self):
        for model in (
            "vendor/safety-guard-1",
            "vendor/content-guard",
            "vendor/translate-en",
            "vendor/flash-nano",
            "vendor/tiny-chat",
        ):
            with self.subTest(model=model):
                self.assertTrue(RUNNER.is_coding_denied(model), model)

    def test_boost_markers_prefer_coding_tagged_ids(self):
        for model in (
            "vendor/qwen3-coder",
            "vendor/devstral-small",
            "vendor/kimi-k2-instruct",
            "vendor/gpt-oss-120b",
            "vendor/claude-sonnet-4",
            "vendor/code-assist",
        ):
            with self.subTest(model=model):
                self.assertTrue(RUNNER.is_coding_boosted(model), model)
                self.assertFalse(RUNNER.is_coding_denied(model), model)

    def test_default_capability_drops_deny_and_ranks_boost_first(self):
        catalog = [
            {"id": "Safety Guard", "provider": "alpha"},
            {"id": "Chat Default", "provider": "alpha"},
            {"id": "Qwen3 Coder", "provider": "beta"},
            {"id": "Flash Nano", "provider": "beta"},
            {"id": "Translate En", "provider": "gamma"},
        ]
        quota = [
            {"provider": "alpha", "remaining": 80, "state": "available"},
            {"provider": "beta", "remaining": 40, "state": "available"},
            {"provider": "gamma", "remaining": 90, "state": "available"},
        ]
        picked = RUNNER.select_live_candidates(
            catalog, quota, required_capability="default",
        )
        models = [item["model"] for item in picked]
        self.assertEqual(["beta/qwen3-coder", "alpha/chat-default"], models)
        self.assertEqual("boost", picked[0]["codingFit"])
        self.assertEqual("neutral", picked[1]["codingFit"])
        for denied in ("safety", "nano", "translate"):
            self.assertTrue(
                all(denied not in model for model in models),
                models,
            )

    def test_explicit_coding_task_filters_even_for_mechanical_capability(self):
        catalog = [
            {"id": "Flash Nano", "provider": "alpha"},
            {"id": "Tool Low", "provider": "alpha"},
            {"id": "Qwen3 Coder", "provider": "alpha"},
        ]
        quota = [{"provider": "alpha", "remaining": 50, "state": "available"}]
        unfiltered = RUNNER.select_live_candidates(
            catalog, quota, required_capability="mechanical",
        )
        self.assertIn("alpha/flash-nano", [item["model"] for item in unfiltered])
        filtered = RUNNER.select_live_candidates(
            catalog, quota, required_capability="mechanical", task="coding",
        )
        models = [item["model"] for item in filtered]
        self.assertNotIn("alpha/flash-nano", models)
        self.assertEqual("alpha/qwen3-coder", models[0])

    def test_invalid_task_raises(self):
        with self.assertRaises(RUNNER.OmniRouteError):
            RUNNER.select_live_candidates(
                [], [], required_capability="default", task="vision",
            )

    def test_skill_documents_coding_filter_and_ready_before_native(self):
        skill = SKILL.read_text(encoding="utf-8")
        collapsed = " ".join(skill.split())
        self.assertIn("Coding candidate filter", skill)
        self.assertIn("safety", skill)
        self.assertIn("gpt-oss", skill)
        self.assertIn("devstral", skill)
        self.assertIn("Dispatch checklist (READY)", skill)
        self.assertIn("before any native host model", collapsed)
        self.assertIn("Catalog ≠ dispatch", skill)
        self.assertIn("auto/coding", skill)
        self.assertIn("--task coding", skill)
        self.assertNotIn("catalog-policy.json", skill)


if __name__ == "__main__":
    unittest.main()
