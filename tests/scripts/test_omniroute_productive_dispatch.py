"""#5764 productive OmniRoute dispatch: matrix, preflight, retry, replay."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroute/scripts/runner.py"
SKILL_PATH = ROOT / "chaos-engine/skills/local-runtimes/references/omniroute.md"


def load_runner():
    spec = importlib.util.spec_from_file_location("omniroute_runner_5764", RUNNER_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class ProductiveDispatchTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.runner = load_runner()

    def test_claude_rejects_kimi_without_discovery_aliases(self):
        r = self.runner
        self.assertFalse(r.run_target_allows_model("claude", "moonshot/kimi-k2.7-code"))
        self.assertFalse(r.run_target_allows_model("claude", "kimi-k2.7-code"))
        self.assertTrue(r.run_target_allows_model("claude", "antigravity/claude-sonnet-4-6"))
        self.assertTrue(r.run_target_allows_model("claude", "cc/claude-sonnet-4-6"))
        self.assertTrue(
            r.run_target_allows_model("claude", "moonshot/kimi-k2.7-code", discovery_aliases=True)
        )

    def test_rank_skips_claude_for_kimi_prefers_opencode(self):
        r = self.runner
        ranked = r.rank_run_targets_for_model(
            "moonshot/kimi-k2.7-code",
            installed=["claude", "opencode", "codex"],
        )
        self.assertEqual(["opencode", "codex"], ranked)
        self.assertEqual(
            "opencode",
            r.select_compatible_run_target(
                "moonshot/kimi-k2.7-code",
                preferred="claude",
                installed=["claude", "opencode", "codex"],
            ),
        )

    def test_opencode_model_arg_prefixes_once(self):
        r = self.runner
        self.assertEqual("omniroute/foo/bar", r.opencode_model_arg("foo/bar"))
        self.assertEqual("omniroute/foo/bar", r.opencode_model_arg("omniroute/foo/bar"))

    def test_retry_budgets_429_catalog_unrecognized_vs_timeout(self):
        r = self.runner
        self.assertEqual(0, r.same_identity_retry_budget("HTTP 429 All antigravity accounts exhausted"))
        self.assertEqual(
            0,
            r.same_identity_retry_budget(
                "Error: Model 'Qwen2.5-72B-Instruct' is not available in the active live catalog"
            ),
        )
        self.assertEqual(0, r.same_identity_retry_budget("[claude-code:unrecognized_model]"))
        self.assertEqual(1, r.same_identity_retry_budget("connection timed out"))
        self.assertTrue(r.diagnostic_stops_omniroute_transport("HTTP 401 unauthorized"))

    def test_replay_2026_09_14_failure_sequence_never_launches(self):
        r = self.runner
        result = r.replay_dispatch_diagnostics(
            [
                {
                    "model": "antigravity/claude-sonnet-4-6",
                    "provider": "antigravity",
                    "diagnostic": "HTTP 429 All antigravity accounts have exhausted their quota",
                },
                {
                    "model": "chutes/Qwen2.5-72B-Instruct",
                    "provider": "chutes",
                    "diagnostic": "Model 'Qwen2.5-72B-Instruct' is not available in the active live catalog",
                },
                {
                    "model": "moonshot/kimi-k2.7-code",
                    "provider": "moonshot",
                    "diagnostic": "[claude-code:unrecognized_model]",
                },
            ],
            preferred_target="claude",
            installed=["claude", "opencode", "codex"],
        )
        self.assertFalse(result["launch_implementer"])
        self.assertGreaterEqual(len(result["skipped"]), 3)

    def test_composed_id_in_provider_live_list(self):
        r = self.runner
        live = [
            {"model": "claude-sonnet-4-6", "provider": "antigravity", "name": "Claude Sonnet"},
            {"model": "kimi-k2.7-code", "provider": "moonshot"},
        ]
        self.assertTrue(
            r.composed_id_in_provider_live_list(
                "antigravity", "claude-sonnet-4-6", live
            )
        )
        self.assertFalse(
            r.composed_id_in_provider_live_list(
                "chutes", "Qwen2.5-72B-Instruct", live
            )
        )

    def test_auto_coding_ids_from_catalog(self):
        r = self.runner
        ids = r.live_auto_coding_ids(
            [
                {"id": "auto/coding"},
                {"model": "auto/coding:fast"},
                {"id": "moonshot/kimi-k2.7-code"},
            ]
        )
        self.assertEqual(["auto/coding", "auto/coding:fast"], ids)

    def test_choose_productive_launch_skips_preflight_failure_and_incompatible(self):
        r = self.runner

        def fail_preflight(model, provider=None, **_kwargs):
            return {"ok": False, "diagnostic": "preflight boom", "launch_implementer": False}

        def ok_preflight(model, provider=None, **_kwargs):
            return {"ok": True, "diagnostic": "", "launch_implementer": True}

        rows = [
            {"model": "moonshot/kimi-k2.7-code", "provider": "moonshot", "identitySha256": "a"},
            {"model": "antigravity/claude-sonnet-4-6", "provider": "antigravity", "identitySha256": "b"},
        ]
        # preferred claude + kimi first: skip incompatible / use opencode after preflight ok
        launch = r.choose_productive_launch(
            rows,
            preferred_target="claude",
            installed=["claude", "opencode"],
            preflight=ok_preflight,
        )
        self.assertEqual("launch", launch["action"])
        self.assertEqual("opencode", launch["target"])
        self.assertEqual("moonshot/kimi-k2.7-code", launch["model"])

        # preflight fail on first compatible → try next
        launch2 = r.choose_productive_launch(
            [
                {"model": "antigravity/claude-sonnet-4-6", "provider": "antigravity"},
                {"model": "groq/openai/gpt-oss-120b", "provider": "groq"},
            ],
            preferred_target="opencode",
            installed=["opencode"],
            preflight=fail_preflight,
        )
        self.assertEqual("runtime_exhausted", launch2["action"])
        self.assertFalse(launch2["launch_implementer"])

    def test_next_target_after_missing_binary(self):
        r = self.runner
        self.assertEqual(
            "opencode",
            r.next_run_target_after_exit(["claude", "opencode", "codex"], "claude", 127),
        )
        self.assertIsNone(
            r.next_run_target_after_exit(["claude", "opencode"], "claude", 1),
        )

    def test_preflight_smoke_stub(self):
        r = self.runner

        def fake_run(command, **_kwargs):
            return SimpleNamespace(returncode=0, stdout="{}", stderr="")

        self.assertTrue(
            r.preflight_smoke("m", "p", run=fake_run)["ok"]
        )

        def fail_run(command, **_kwargs):
            return SimpleNamespace(
                returncode=1,
                stdout="",
                stderr="not available in the active live catalog",
            )

        self.assertFalse(r.preflight_smoke("m", "p", run=fail_run)["ok"])

    def test_skill_documents_matrix_preflight_and_anti_patterns(self):
        text = SKILL_PATH.read_text(encoding="utf-8")
        self.assertIn("CLI target matrix", text)
        self.assertIn("unrecognized_model", text)
        self.assertIn("Preflight before long", text)
        self.assertIn("Operator checklist", text)
        self.assertIn("auto/coding", text)


if __name__ == "__main__":
    unittest.main()
