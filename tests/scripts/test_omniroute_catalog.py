"""Live OmniRoute catalog selection: no cache files, rank from current CLI JSON."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
import unittest.mock
from datetime import UTC, datetime, timedelta
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroute/scripts/runner.py"
SKILL = ROOT / "chaos-engine/skills/omniroute/SKILL.md"
WORKFLOWS = ROOT / "chaos-engine/references/execution-workflows.md"
GUIDE = ROOT / "chaos-engine/guides/omniroute.md"
SPEC = importlib.util.spec_from_file_location("omniroute_catalog_runner", RUNNER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("OmniRoute runner could not be loaded")
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class OmniRouteCatalogTest(unittest.TestCase):
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
            ["beta/other-default", "alpha/tool-high", "alpha/tool-low"],
            [item["model"] for item in picked],
        )
        self.assertNotIn("Gone Low", [item["model"] for item in picked])
        self.assertNotIn("gone-low", [item["model"] for item in picked])

    def test_select_keeps_supports_vision_models_for_implementation_ranking(self):
        catalog = [
            {"id": "Vision Pro", "provider": "alpha", "supportsVision": True},
            {"id": "Tool Low", "provider": "alpha", "supportsVision": False},
        ]
        quota = [{"provider": "alpha", "remaining": 50, "state": "available"}]
        picked = RUNNER.select_live_candidates(catalog, quota, required_capability="default")
        self.assertEqual(
            ["alpha/vision-pro", "alpha/tool-low"],
            [item["model"] for item in picked],
        )

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
        self.assertEqual("beta/other-default", first[0]["model"])
        self.assertTrue(RUNNER.diagnostic_is_rate_limited("exceeded retry limit, last status: 429 Too Many Requests"))
        self.assertFalse(RUNNER.diagnostic_is_rate_limited("401 Unauthorized: Invalid API key"))
        skipped = RUNNER.select_live_candidates(
            catalog, quota, required_capability="default",
            skip_identity_sha256s=[first[0]["identitySha256"]],
        )
        self.assertEqual("alpha/tool-low", skipped[0]["model"])
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
        self.assertEqual(["alpha/tool-high"], [item["model"] for item in picked])
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
        self.assertIn("models <provider>", skill)
        self.assertIn("Stream closed before `response.completed`", skill)
        self.assertIn("OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open", skill)
        self.assertIn("http://127.0.0.1:20128/api/health", skill)
        self.assertIn("omniroute run --model", skill)
        self.assertIn("JSONDecoder().raw_decode", skill)
        self.assertIn("Use `--output json` before `models`", skill)
        self.assertIn("installed OmniRoute binary", guide)
        self.assertIn("Missing operator config is normal", guide)
        self.assertIn("python3 chaos-engine/skills/omniroute/scripts/runner.py probe", guide)
        self.assertIn("candidates --capability", skill)
        self.assertIn("HTTP 429", skill)
        self.assertIn("not available in the active live catalog", skill)
        self.assertIn("Do not pin a Codex profile model", skill)
        self.assertIn("pass Codex `-c model='<provider>/<id>'`", skill)
        self.assertIn("native id", skill)
        self.assertIn("prefer `model` over `id`/`name`", skill)
        self.assertIn("Do not drop `available: false`", skill)
        self.assertIn("Do not drop `supportsVision: true`", skill)
        self.assertIn("Prefer `omniroute run` over `setup-*`", skill)
        self.assertIn("bin/cli/cli-manifest.mjs", skill)
        self.assertIn("ANTHROPIC_BASE_URL` is the gateway **root", skill)
        self.assertIn("`--model omniroute/<id>`", skill)
        self.assertIn("first installed implementer target: `claude`, then `opencode`, then", skill)
        self.assertIn("OMNIROUTE_ROTATE_ON_400=true", skill)
        self.assertIn("Thinking Budget on the OmniRoute host must be `passthrough`", skill)
        self.assertIn("provider-exhaustion backoff", skill.casefold())
        self.assertIn("exhausted_until", skill)
        self.assertIn("user-local", skill.casefold())
        self.assertIn("not a positive catalog cache", skill.casefold())

    def test_catalog_cli_does_not_forward_ambient_endpoint_key(self):
        seen = []

        def fake_run(command, **kwargs):
            seen.append(kwargs.get("env") or {})
            class Result:
                stdout = json.dumps([{"id": "Live Default", "provider": "alpha"}])
                returncode = 0
            if command[-2:] == ["usage", "quota"]:
                Result.stdout = json.dumps([
                    {"provider": "alpha", "remaining": 7, "state": "available"},
                ])
            return Result()

        with unittest.mock.patch.object(RUNNER.shutil, "which", return_value="/usr/bin/omniroute"):
            with unittest.mock.patch.object(RUNNER.subprocess, "run", side_effect=fake_run):
                with unittest.mock.patch.dict(
                    RUNNER.os.environ,
                    {"OMNIROUTE_API_KEY": "poison-key", "OMNIROUTE_BASE_URL": "http://example.invalid"},
                    clear=False,
                ):
                    result = RUNNER.candidates(required_capability="default")
        self.assertEqual("READY", result["state"])
        self.assertTrue(seen)
        for env in seen:
            self.assertNotIn("OMNIROUTE_API_KEY", env)
            self.assertNotIn("OMNIROUTE_BASE_URL", env)
            self.assertNotIn("poison-key", json.dumps(env))
            self.assertNotIn("example.invalid", json.dumps(env))

    def test_candidates_cli_queries_live_commands_and_writes_no_cache(self):
        calls = []

        def fake_run(command, **_kwargs):
            calls.append(command)
            class Result:
                stdout = ""
                returncode = 0
            if command[-2:] == ["usage", "quota"]:
                Result.stdout = json.dumps([
                    {"provider": "alpha", "remaining": 7, "state": "available"},
                ])
            elif "models" in command:
                Result.stdout = json.dumps([
                    {"id": "Live Low", "provider": "alpha"},
                    {"id": "Live High", "provider": "alpha"},
                ])
            return Result()

        with unittest.mock.patch.object(RUNNER.shutil, "which", return_value="/usr/bin/omniroute"):
            with unittest.mock.patch.object(RUNNER.subprocess, "run", side_effect=fake_run):
                result = RUNNER.candidates(required_capability="mechanical")
        self.assertEqual(
            [["omniroute", "--output", "json", "usage", "quota"],
             ["omniroute", "--output", "json", "models", "alpha"]],
            calls,
        )
        self.assertEqual("READY", result["state"])
        self.assertEqual("alpha/live-low", result["candidates"][0]["model"])
        self.assertFalse(any(Path.cwd().joinpath(name).exists()
                             for name in (".omniroute-catalog.json", "catalog-cache.json")))


    def test_display_names_launch_as_native_ids_and_catalog_miss_skips_identity(self):
        self.assertEqual("glm-4.5", RUNNER.catalog_launch_id("GLM 4.5"))
        self.assertEqual("glm-4.6v", RUNNER.catalog_launch_id("GLM 4.6V (Vision)"))
        self.assertEqual("glm-4.5-air", RUNNER.catalog_launch_id("GLM 4.5 Air"))
        self.assertEqual("gemini-3.1-flash-lite", RUNNER.catalog_launch_id("gemini-3.1-flash-lite"))
        catalog = [
            {"id": "GLM 4.5", "provider": "glm"},
            {"id": "GLM 4.5 Air", "provider": "glm"},
        ]
        quota = [{"provider": "glm", "remaining": 100, "state": "available"}]
        picked = RUNNER.select_live_candidates(catalog, quota, required_capability="default")
        self.assertEqual(["glm/glm-4.5", "glm/glm-4.5-air"], [item["model"] for item in picked])
        self.assertEqual("default", picked[0]["capability"])
        self.assertEqual("mechanical", picked[1]["capability"])
        miss = (
            "ERROR: {\"error\":{\"message\":\"Model 'GLM 4.5' is not available "
            "in the active live catalog for provider 'glm'.\",\"code\":\"bad_request\"}}"
        )
        self.assertTrue(RUNNER.diagnostic_is_catalog_mismatch(miss))
        self.assertFalse(RUNNER.diagnostic_is_catalog_mismatch("401 Unauthorized: Invalid API key"))
        skipped = RUNNER.select_live_candidates(
            catalog, quota, required_capability="default",
            skip_identity_sha256s=[picked[0]["identitySha256"]],
        )
        self.assertEqual("glm/glm-4.5-air", skipped[0]["model"])
        self.assertEqual(
            ["-c", 'model="glm/glm-4.5"'],
            RUNNER.codex_model_overlay("glm", "glm-4.5"),
        )
        self.assertEqual(
            ["-c", 'model="nvidia/z-ai/glm-5.2"'],
            RUNNER.codex_model_overlay("nvidia", "z-ai/glm-5.2"),
        )
        gateway = [
            {"name": "GLM 4.5", "model": "glm-4.5", "provider": "glm", "available": False},
            {"name": "GLM 5.2", "model": "z-ai/glm-5.2", "provider": "nvidia", "available": True},
        ]
        quota = [
            {"provider": "glm", "remaining": 100, "state": "available"},
            {"provider": "nvidia", "remaining": 100, "state": "available"},
        ]
        live = RUNNER.select_live_candidates(gateway, quota, required_capability="default")
        self.assertEqual(["glm/glm-4.5", "nvidia/z-ai/glm-5.2"], [item["model"] for item in live])
        self.assertTrue(RUNNER.diagnostic_is_stream_disconnected(
            "stream disconnected before completion: stream closed before response.completed"
        ))
        self.assertFalse(RUNNER.diagnostic_is_stream_disconnected("401 Unauthorized: Invalid API key"))


class OmniRouteExhaustionBackoffTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.root.chmod(0o700)
        self.state = self.root / "state"
        self.state.mkdir(mode=0o700)

    def test_records_exhaustion_skips_until_retry_and_expires(self):
        cache_path = RUNNER.exhaustion_cache_path(self.state)
        now = datetime(2030, 1, 1, tzinfo=UTC)
        until = now + timedelta(hours=2)
        RUNNER.record_provider_exhaustion(
            "glm",
            exhausted_until=until,
            state_dir=self.state,
            now=now,
            reason="state=exhausted",
        )
        self.assertTrue(cache_path.is_file())
        if cache_path.stat().st_mode & 0o777:
            self.assertEqual(0o600, cache_path.stat().st_mode & 0o777)
        catalog = [
            {"id": "Tool Low", "provider": "glm"},
            {"id": "Other Default", "provider": "beta"},
        ]
        quota = [
            {"provider": "glm", "remaining": 40, "state": "available"},
            {"provider": "beta", "remaining": 90, "state": "available"},
        ]
        skipped = RUNNER.select_live_candidates(
            catalog,
            quota,
            required_capability="default",
            exhaustion_cache=RUNNER.load_exhaustion_cache(self.state, now=now),
        )
        self.assertEqual(["beta/other-default"], [item["model"] for item in skipped])
        expired = RUNNER.select_live_candidates(
            catalog,
            quota,
            required_capability="default",
            exhaustion_cache=RUNNER.load_exhaustion_cache(
                self.state, now=until + timedelta(seconds=1)
            ),
        )
        self.assertEqual(
            ["beta/other-default", "glm/tool-low"],
            [item["model"] for item in expired],
        )

    def test_candidates_path_applies_user_local_exhaustion_overlay(self):
        now = datetime(2030, 1, 1, tzinfo=UTC)
        RUNNER.record_provider_exhaustion(
            "alpha",
            exhausted_until=now + timedelta(minutes=30),
            state_dir=self.state,
            now=now,
            reason="HTTP 429",
        )

        def fake_run(command, **_kwargs):
            class Result:
                stdout = ""
                returncode = 0

            if command[-2:] == ["usage", "quota"]:
                Result.stdout = json.dumps(
                    [
                        {"provider": "alpha", "remaining": 7, "state": "available"},
                        {"provider": "beta", "remaining": 9, "state": "available"},
                    ]
                )
            elif "models" in command:
                provider = command[-1]
                Result.stdout = json.dumps(
                    [{"id": f"{provider.title()} Low", "provider": provider}]
                )
            return Result()

        with unittest.mock.patch.object(RUNNER.shutil, "which", return_value="/usr/bin/omniroute"):
            with unittest.mock.patch.object(RUNNER.subprocess, "run", side_effect=fake_run):
                with unittest.mock.patch.object(
                    RUNNER, "default_state_path", return_value=self.state
                ):
                    with unittest.mock.patch.object(RUNNER, "_utc_now", return_value=now):
                        result = RUNNER.candidates(required_capability="mechanical")
        self.assertEqual("READY", result["state"])
        self.assertEqual(["beta/beta-low"], [item["model"] for item in result["candidates"]])
        self.assertFalse((Path.cwd() / "provider-exhaustion.json").exists())

    def test_quota_exhaustion_signal_updates_cache(self):
        now = datetime(2030, 1, 1, tzinfo=UTC)
        RUNNER.update_exhaustion_cache_from_quota(
            [
                {"provider": "gamma", "remaining": 0, "state": "exhausted"},
                {"provider": "beta", "remaining": 12, "state": "available"},
            ],
            state_dir=self.state,
            now=now,
            retry_after_seconds=3600,
        )
        cache = RUNNER.load_exhaustion_cache(self.state, now=now)
        self.assertIn("gamma", cache.get("providers", {}))
        self.assertNotIn("beta", cache.get("providers", {}))

    def test_production_429_diagnostic_writes_cache_and_candidates_skips(self):
        now = datetime(2030, 1, 1, tzinfo=UTC)
        until = now + timedelta(hours=2)
        diagnostic = "exceeded retry limit, last status: 429 Too Many Requests"

        def fake_run(command, **_kwargs):
            class Result:
                stdout = ""
                returncode = 0

            if command[-2:] == ["usage", "quota"]:
                Result.stdout = json.dumps(
                    [
                        {"provider": "alpha", "remaining": 7, "state": "available"},
                        {"provider": "beta", "remaining": 9, "state": "available"},
                    ]
                )
            elif "models" in command:
                provider = command[-1]
                Result.stdout = json.dumps(
                    [{"id": f"{provider.title()} Low", "provider": provider}]
                )
            return Result()

        with unittest.mock.patch.object(RUNNER.shutil, "which", return_value="/usr/bin/omniroute"):
            with unittest.mock.patch.object(RUNNER.subprocess, "run", side_effect=fake_run):
                with unittest.mock.patch.object(
                    RUNNER, "default_state_path", return_value=self.state
                ):
                    with unittest.mock.patch.object(RUNNER, "_utc_now", return_value=now):
                        first = RUNNER.candidates(required_capability="mechanical")
                        failed = first["candidates"][0]
                        second = RUNNER.candidates(
                            required_capability="mechanical",
                            diagnostic=diagnostic,
                            failed_identity_sha256=failed["identitySha256"],
                            failed_provider=failed["provider"],
                        )
        self.assertEqual("beta/beta-low", failed["model"])
        cache = RUNNER.load_exhaustion_cache(self.state, now=now)
        self.assertIn(failed["identitySha256"], cache.get("identities", {}))
        self.assertEqual(["alpha/alpha-low"], [item["model"] for item in second["candidates"]])
        self.assertNotEqual(failed["identitySha256"], second["candidates"][0]["identitySha256"])
        still_blocked = RUNNER.select_live_candidates(
            [
                {"id": "Alpha Low", "provider": "alpha"},
                {"id": "Beta Low", "provider": "beta"},
            ],
            [
                {"provider": "alpha", "remaining": 7, "state": "available"},
                {"provider": "beta", "remaining": 9, "state": "available"},
            ],
            required_capability="mechanical",
            exhaustion_cache=RUNNER.load_exhaustion_cache(self.state, now=now),
        )
        self.assertEqual(["alpha/alpha-low"], [item["model"] for item in still_blocked])
        expired = RUNNER.select_live_candidates(
            [
                {"id": "Alpha Low", "provider": "alpha"},
                {"id": "Beta Low", "provider": "beta"},
            ],
            [
                {"provider": "alpha", "remaining": 7, "state": "available"},
                {"provider": "beta", "remaining": 9, "state": "available"},
            ],
            required_capability="mechanical",
            exhaustion_cache=RUNNER.load_exhaustion_cache(
                self.state, now=until + timedelta(seconds=1)
            ),
        )
        self.assertEqual(
            ["beta/beta-low", "alpha/alpha-low"],
            [item["model"] for item in expired],
        )

    def test_insufficient_balance_diagnostic_blocks_provider(self):
        now = datetime(2030, 1, 1, tzinfo=UTC)
        recorded = RUNNER.update_exhaustion_cache_from_diagnostic(
            "ERROR: insufficient balance for provider alpha",
            provider="alpha",
            state_dir=self.state,
            now=now,
            retry_after_seconds=1800,
        )
        self.assertTrue(recorded)
        cache = RUNNER.load_exhaustion_cache(self.state, now=now)
        self.assertIn("alpha", cache.get("providers", {}))


if __name__ == "__main__":
    unittest.main()
