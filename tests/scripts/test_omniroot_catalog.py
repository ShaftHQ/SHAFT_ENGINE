"""Live OmniRoot catalog selection: no cache files, rank from current CLI JSON."""

from __future__ import annotations

import importlib.util
import json
import unittest
import unittest.mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroot/scripts/runner.py"
SKILL = ROOT / "chaos-engine/skills/omniroot/SKILL.md"
WORKFLOWS = ROOT / "chaos-engine/references/execution-workflows.md"
GUIDE = ROOT / "chaos-engine/guides/omniroute.md"
SPEC = importlib.util.spec_from_file_location("omniroot_catalog_runner", RUNNER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("OmniRoot runner could not be loaded")
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class OmniRootCatalogTest(unittest.TestCase):
    def test_decode_strips_ansi_logs_and_trailing_extra_json(self):
        raw = (
            "\x1b[2mLoaded env\x1b[0m\n"
            '[{"id": "Alpha Low", "provider": "one"}]\n'
            '{"ignored": true}\n'
        )
        self.assertEqual(
            [{"id": "Alpha Low", "provider": "one"}],
            RUNNER.decode_cli_json(raw),
        )

    def test_select_drops_exhausted_providers_and_prefers_lower_capability(self):
        catalog = [
            {"id": "Tool Low", "provider": "alpha"},
            {"id": "Tool High", "provider": "alpha"},
            {"id": "Other Default", "provider": "beta"},
            {"id": "Gone Low", "provider": "gamma"},
        ]
        quota = [
            {"provider": "alpha", "remaining": 40, "state": "available"},
            {"provider": "beta", "remaining": 90, "state": "available"},
            {"provider": "gamma", "remaining": 0, "state": "exhausted"},
        ]
        picked = RUNNER.select_live_candidates(catalog, quota, required_capability="default")
        self.assertEqual(
            ["Other Default", "Tool High", "Tool Low"],
            [item["model"] for item in picked],
        )
        self.assertNotIn("Gone Low", [item["model"] for item in picked])

    def test_rate_limit_skips_the_failed_identity_and_picks_a_larger_class(self):
        catalog = [
            {"id": "Tool Low", "provider": "alpha"},
            {"id": "Other Default", "provider": "beta"},
        ]
        quota = [
            {"provider": "alpha", "remaining": 40, "state": "available"},
            {"provider": "beta", "remaining": 90, "state": "available"},
        ]
        first = RUNNER.select_live_candidates(catalog, quota, required_capability="default")
        self.assertEqual("Other Default", first[0]["model"])
        self.assertTrue(RUNNER.diagnostic_is_rate_limited("exceeded retry limit, last status: 429 Too Many Requests"))
        self.assertFalse(RUNNER.diagnostic_is_rate_limited("401 Unauthorized: Invalid API key"))
        skipped = RUNNER.select_live_candidates(
            catalog, quota, required_capability="default",
            skip_identity_sha256s=[first[0]["identitySha256"]],
        )
        self.assertEqual("Tool Low", skipped[0]["model"])
        self.assertNotEqual(first[0]["identitySha256"], skipped[0]["identitySha256"])

    def test_architecture_uses_only_high_class_then_empty_means_native_fallback(self):
        catalog = [
            {"id": "Tool Low", "provider": "alpha"},
            {"id": "Tool High", "provider": "alpha"},
        ]
        quota = [{"provider": "alpha", "remaining": 12, "state": "available"}]
        picked = RUNNER.select_live_candidates(
            catalog, quota, required_capability="most-intelligent",
        )
        self.assertEqual(["Tool High"], [item["model"] for item in picked])
        empty = RUNNER.select_live_candidates(
            [{"id": "Tool Low", "provider": "alpha"}],
            [{"provider": "alpha", "remaining": 12, "state": "available"}],
            required_capability="most-intelligent",
        )
        self.assertEqual([], empty)

    def test_skill_documents_live_query_every_dispatch_without_cache_files(self):
        skill = SKILL.read_text(encoding="utf-8")
        workflows = WORKFLOWS.read_text(encoding="utf-8")
        guide = GUIDE.read_text(encoding="utf-8")
        for text in (skill, workflows):
            self.assertIn("omniroute --output json models", text)
            self.assertIn("omniroute --output json usage quota", text)
            self.assertIn("every dispatch", text)
            self.assertNotIn("catalog-policy.json", text)
            self.assertNotIn("session cache", text.lower())
        self.assertIn("OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open", skill)
        self.assertIn("http://127.0.0.1:20128/api/health", skill)
        self.assertIn("omniroute run --model", skill)
        self.assertIn("JSONDecoder().raw_decode", skill)
        self.assertIn("Use `--output json` before `models`", skill)
        self.assertIn("installed OmniRoute binary", guide)
        self.assertIn("Missing operator config is normal", guide)
        self.assertIn("python3 chaos-engine/skills/omniroot/scripts/runner.py probe", guide)
        self.assertIn("candidates --capability", skill)
        self.assertIn("HTTP 429", skill)
        self.assertIn("Do not pin a Codex profile model", skill)

    def test_candidates_cli_queries_live_commands_and_writes_no_cache(self):
        calls = []

        def fake_run(command, **_kwargs):
            calls.append(command)
            class Result:
                stdout = ""
                returncode = 0
            if command[-1] == "models":
                Result.stdout = json.dumps([
                    {"id": "Live Low", "provider": "alpha"},
                    {"id": "Live High", "provider": "alpha"},
                ])
            else:
                Result.stdout = json.dumps([
                    {"provider": "alpha", "remaining": 7, "state": "available"},
                ])
            return Result()

        with unittest.mock.patch.object(RUNNER.shutil, "which", return_value="/usr/bin/omniroute"):
            with unittest.mock.patch.object(RUNNER.subprocess, "run", side_effect=fake_run):
                result = RUNNER.candidates(required_capability="mechanical")
        self.assertEqual(
            [["omniroute", "--output", "json", "models"],
             ["omniroute", "--output", "json", "usage", "quota"]],
            calls,
        )
        self.assertEqual("READY", result["state"])
        self.assertEqual("Live Low", result["candidates"][0]["model"])
        self.assertFalse(any(Path.cwd().joinpath(name).exists()
                             for name in (".omniroot-catalog.json", "catalog-cache.json")))


if __name__ == "__main__":
    unittest.main()
