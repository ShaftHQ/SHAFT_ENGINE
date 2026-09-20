"""Session token usage ledger: local vs cloud, privacy-safe (#5981)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "chaos-engine/session_token_usage.py"


def load():
    spec = importlib.util.spec_from_file_location("session_token_usage", MODULE)
    assert spec and spec.loader
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class SessionTokenUsageTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = load()

    def test_record_and_summarize_local_vs_cloud(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.mod.record(
                "sess-1",
                channel="local",
                prompt_tokens=100,
                completion_tokens=50,
                runtime_class="freetoken",
                project=project,
            )
            self.mod.record(
                "sess-1",
                channel="cloud",
                prompt_tokens=1000,
                completion_tokens=200,
                runtime_class="host-session",
                project=project,
            )
            summary = self.mod.summarize("sess-1", project=project)
            totals = summary["totals"]
            self.assertEqual(totals["localPromptTokens"], 100)
            self.assertEqual(totals["localCompletionTokens"], 50)
            self.assertEqual(totals["cloudPromptTokens"], 1000)
            self.assertEqual(totals["cloudCompletionTokens"], 200)
            cost = summary["cost"]
            self.assertEqual(cost["localTokens"], 150)
            self.assertEqual(cost["cloudTokens"], 1200)
            self.assertGreater(cost["cloudEstimatedUsd"], 0)
            self.assertEqual(cost["localEstimatedUsd"], 0)
            text = self.mod.format_retrospective(summary)
            self.assertIn("local=150", text)
            self.assertIn("cloud=1200", text)
            # privacy: no model/provider strings in ledger file
            ledger_path = self.mod._ledger_path("sess-1", project)
            raw = ledger_path.read_text(encoding="utf-8")
            self.assertNotIn("gpt", raw.lower())
            self.assertNotIn("anthropic", raw.lower())
            self.assertNotIn("openai", raw.lower())

    def test_rejects_model_like_runtime_and_bad_channel(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with self.assertRaises(ValueError):
                self.mod.record(
                    "sess-2",
                    channel="cloud",
                    prompt_tokens=1,
                    completion_tokens=0,
                    runtime_class="gpt-4o",  # type: ignore[arg-type]
                    project=project,
                )
            with self.assertRaises(ValueError):
                self.mod.record(
                    "sess-2",
                    channel="hybrid",  # type: ignore[arg-type]
                    prompt_tokens=1,
                    completion_tokens=0,
                    project=project,
                )

    def test_finalize_attaches_token_usage(self) -> None:
        learn_path = ROOT / "chaos-engine/learning_session.py"
        spec = importlib.util.spec_from_file_location("learning_session", learn_path)
        assert spec and spec.loader
        learn = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(learn)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.mod.record(
                "learn-sess",
                channel="local",
                prompt_tokens=10,
                completion_tokens=5,
                runtime_class="colibri",
                project=project,
            )
            # finalize uses Path.cwd() for state; chdir into project
            import os

            previous = Path.cwd()
            try:
                os.chdir(project)
                # seed ledger under project cwd expectations
                # session_token_usage defaults to Path.cwd()
                receipt = learn.finalize("learn-sess", disposition="no-durable", extract_heuristics=False)
            finally:
                os.chdir(previous)
            self.assertIn("tokenUsage", receipt)
            self.assertEqual(receipt["tokenUsage"]["totals"]["localPromptTokens"], 10)
            self.assertIn("retrospective", receipt["tokenUsage"])


if __name__ == "__main__":
    unittest.main()
