"""Local-agency dispatch: READY ranking, ephemeral OpenCode config, no cloud silent fallback."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
DISPATCH = ROOT / "chaos-engine/skills/local-agency/scripts/dispatch.py"

_SPEC = importlib.util.spec_from_file_location("local_agency_dispatch", DISPATCH)
if _SPEC is None or _SPEC.loader is None:
    raise RuntimeError(f"unable to load dispatch module from {DISPATCH}")
dispatch = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(dispatch)


class LocalAgencyDispatchTest(unittest.TestCase):
    def test_source_never_mentions_ft_launch_or_omniroute_exec(self):
        text = DISPATCH.read_text(encoding="utf-8")
        self.assertNotIn("ft launch", text)
        self.assertNotIn("ft serve", text)
        self.assertNotIn("omniroute run", text)
        self.assertNotIn("subprocess", text)
        self.assertIn("may_ft_launch", text)

    def test_loopback_openai_base_rejects_non_loopback(self):
        self.assertTrue(dispatch.loopback_openai_base("http://127.0.0.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("http://10.1.1.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("https://127.0.0.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("http://127.0.0.1:1919/v1/models"))

    def test_resolve_absent_when_all_runtimes_missing(self):
        def absent(runtime: str):
            base = (
                dispatch.FREETOKEN_OPENAI_BASE
                if runtime == "freetoken"
                else dispatch.OPENAI_COMPAT_BASES[runtime]
            )
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": base,
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=absent):
            payload = dispatch.resolve_local()
        self.assertEqual(payload["state"], "ABSENT")
        self.assertIsNone(payload["chosen"])
        self.assertFalse(payload["omniroute_fallback"])
        self.assertFalse(payload["may_ft_launch"])
        self.assertIn("No READY local runtime", payload["advice"])

    def test_resolve_does_not_silent_cloud_fallback_even_with_allow_cloud(self):
        def absent(runtime: str):
            base = (
                dispatch.FREETOKEN_OPENAI_BASE
                if runtime == "freetoken"
                else dispatch.OPENAI_COMPAT_BASES[runtime]
            )
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": base,
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=absent):
            payload = dispatch.resolve_local(allow_cloud=True)
        self.assertEqual(payload["state"], "ABSENT")
        self.assertFalse(payload["omniroute_fallback"])
        self.assertTrue(payload["allow_cloud"])
        self.assertIn("allow-cloud", payload["advice"])

    def test_resolve_prefers_freetoken_when_ready(self):
        def probe(runtime: str):
            if runtime == "freetoken":
                return {
                    "runtime": "freetoken",
                    "state": "READY",
                    "openai_base_url": dispatch.FREETOKEN_OPENAI_BASE,
                    "models": ["gpt-oss-20b"],
                    "provider_id": "freetoken",
                }
            return {
                "runtime": runtime,
                "state": "READY",
                "openai_base_url": dispatch.OPENAI_COMPAT_BASES[runtime],
                "models": ["other"],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            payload = dispatch.resolve_local()
        self.assertEqual(payload["state"], "READY")
        chosen = payload["chosen"]
        assert isinstance(chosen, dict)
        self.assertEqual(chosen["runtime"], "freetoken")
        self.assertEqual(chosen["opencode_model"], "freetoken/gpt-oss-20b")

    def test_opencode_config_is_ephemeral_schema(self):
        chosen = {
            "runtime": "freetoken",
            "provider_id": "freetoken",
            "openai_base_url": "http://127.0.0.1:1919/v1",
            "model": "gpt-oss-20b",
            "opencode_model": "freetoken/gpt-oss-20b",
        }
        cfg = dispatch.opencode_config(chosen)
        self.assertEqual(cfg["model"], "freetoken/gpt-oss-20b")
        provider = cfg["provider"]["freetoken"]
        self.assertEqual(provider["npm"], "@ai-sdk/openai-compatible")
        self.assertEqual(provider["options"]["baseURL"], "http://127.0.0.1:1919/v1")
        with tempfile.TemporaryDirectory() as temporary:  # nosec B108
            path = dispatch.write_ephemeral_config(chosen, Path(temporary))
            self.assertEqual(path.name, "opencode.json")
            self.assertTrue(path.is_file())
            loaded = json.loads(path.read_text(encoding="utf-8"))
            self.assertEqual(loaded["model"], "freetoken/gpt-oss-20b")

    def test_cmd_config_json_contract(self):
        def probe(runtime: str):
            if runtime == "freetoken":
                return {
                    "runtime": "freetoken",
                    "state": "READY",
                    "openai_base_url": "http://127.0.0.1:1919/v1",
                    "models": ["gpt-oss-20b"],
                    "provider_id": "freetoken",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": dispatch.OPENAI_COMPAT_BASES[runtime],
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            with tempfile.TemporaryDirectory() as temporary:  # nosec B108
                args = dispatch.parse_args(["config", "--dir", temporary])
                buf = StringIO()
                with redirect_stdout(buf):
                    code = dispatch.cmd_config(args)
                payload = json.loads(buf.getvalue())
                self.assertEqual(code, 0)
                self.assertEqual(payload["state"], "READY")
                self.assertFalse(payload["durable_config_rewrite"])
                self.assertFalse(payload["may_ft_launch"])
                self.assertFalse(payload["omniroute_fallback"])
                self.assertIn("OPENCODE_CONFIG", payload["env"])
                self.assertTrue(Path(payload["env"]["OPENCODE_CONFIG"]).is_file())

    def test_argv_includes_model_and_prompt(self):
        chosen = {
            "runtime": "freetoken",
            "provider_id": "freetoken",
            "openai_base_url": "http://127.0.0.1:1919/v1",
            "model": "gpt-oss-20b",
            "opencode_model": "freetoken/gpt-oss-20b",
        }
        argv = dispatch.opencode_argv(chosen, prompt="PONG", workdir="/tmp/wt", auto=False, pure=True)
        self.assertEqual(argv[0], "opencode")
        self.assertIn("run", argv)
        self.assertIn("--pure", argv)
        self.assertIn("freetoken/gpt-oss-20b", argv)
        self.assertIn("PONG", argv)


if __name__ == "__main__":
    unittest.main()
