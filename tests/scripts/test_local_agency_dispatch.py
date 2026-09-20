"""Local-agency dispatch: READY ranking, ephemeral OpenCode config, no cloud silent fallback."""

from __future__ import annotations

import importlib.util
import os
import json
import tempfile
import unittest
import unittest.mock as mock
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
DISPATCH = ROOT / "chaos-engine/skills/local-agency/scripts/dispatch.py"

_SPEC = importlib.util.spec_from_file_location("local_agency_dispatch", DISPATCH)
if _SPEC is None or _SPEC.loader is None:
    raise RuntimeError(f"unable to load dispatch module from {DISPATCH}")
dispatch = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(dispatch)


def _base_for(runtime: str) -> str:
    if runtime == "freetoken":
        return dispatch.FREETOKEN_OPENAI_BASE
    if runtime == "colibri":
        return dispatch.COLIBRI_OPENAI_BASE
    return dispatch.OPENAI_COMPAT_BASES[runtime]


class LocalAgencyDispatchTest(unittest.TestCase):
    def test_source_never_mentions_ft_launch_or_omniroute_exec(self):
        text = DISPATCH.read_text(encoding="utf-8")
        self.assertNotIn("ft launch", text)
        self.assertNotIn("ft serve", text)
        self.assertNotIn("omniroute run", text)
        self.assertNotIn("subprocess", text)
        self.assertIn("may_ft_launch", text)

    def test_skill_says_orchestrator_runs_command_when_opencode_mutates_nothing(self):
        skill = (ROOT / "chaos-engine/skills/local-agency/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("no worktree mutation", skill)
        self.assertIn("run the same", skill)
        self.assertIn("command in the worktree", skill)
        self.assertIn("zero tool calls is writer failure", skill)
        self.assertIn("Never use it for independent adversarial review", skill)
        self.assertIn("Do not OpenCode-`Read` a 200-line Java file", skill)
        self.assertIn("`ft launch` / `ft serve`", skill)
        self.assertIn("#6021", skill)
        self.assertIn("CE_ALLOW_BOX_LOCAL_AGENCY", skill)

    def test_delegation_treats_local_coder_as_mechanical_runner(self):
        text = (ROOT / "chaos-engine/references/delegation.md").read_text(encoding="utf-8")
        self.assertIn("mechanical one-command", text)
        self.assertIn("EXIT 0 with zero tool calls is writer failure", text)
        self.assertIn("same host model", text)
        self.assertIn("most-intelligent", text)
        self.assertIn("`ft launch`", text)

    def test_loopback_openai_base_rejects_non_loopback(self):
        self.assertTrue(dispatch.loopback_openai_base("http://127.0.0.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("http://10.1.1.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("https://127.0.0.1:1919/v1"))
        self.assertFalse(dispatch.loopback_openai_base("http://127.0.0.1:1919/v1/models"))

    def test_resolve_absent_when_all_runtimes_missing(self):
        def absent(runtime: str):
            base = _base_for(runtime)
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
            base = _base_for(runtime)
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
                "openai_base_url": _base_for(runtime),
                "models": ["other"],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            payload = dispatch.resolve_local()
        self.assertEqual(payload["state"], "READY")
        chosen = payload["chosen"]
        self.assertIsInstance(chosen, dict)
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
                "openai_base_url": _base_for(runtime),
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
        argv = dispatch.opencode_argv(chosen, prompt="PONG", workdir="worktree-fixture", auto=False, pure=True)
        self.assertEqual(argv[0], "opencode")
        self.assertIn("run", argv)
        self.assertIn("--pure", argv)
        self.assertIn("--variant", argv)
        self.assertEqual(argv[argv.index("--variant") + 1], "medium")
        self.assertIn("freetoken/gpt-oss-20b", argv)
        self.assertIn("PONG", argv)

    def test_argv_cli_emits_pure_and_variant(self):
        def probe(runtime: str):
            if runtime == "freetoken":
                return {
                    "runtime": "freetoken",
                    "state": "READY",
                    "openai_base_url": dispatch.FREETOKEN_OPENAI_BASE,
                    "models": ["coding-moe"],
                    "provider_id": "freetoken",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": _base_for(runtime),
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            with tempfile.TemporaryDirectory() as temporary:  # nosec B108
                args = dispatch.parse_args(
                    ["argv", "--prompt", "one-command", "--workdir", "wt", "--dir", temporary]
                )
                buf = StringIO()
                with redirect_stdout(buf):
                    code = dispatch.cmd_argv(args)
                payload = json.loads(buf.getvalue())
        self.assertEqual(code, 0)
        argv = payload["argv"]
        self.assertIn("--pure", argv)
        self.assertIn("--variant", argv)
        self.assertEqual(argv[argv.index("--variant") + 1], "medium")

    def test_opencode_config_enables_only_local_provider(self):
        chosen = {
            "runtime": "freetoken",
            "provider_id": "freetoken",
            "openai_base_url": "http://127.0.0.1:1919/v1",
            "model": "gpt-oss-20b",
            "opencode_model": "freetoken/gpt-oss-20b",
        }
        cfg = dispatch.opencode_config(chosen)
        self.assertEqual(cfg["enabled_providers"], ["freetoken"])
        self.assertEqual(list(cfg["provider"].keys()), ["freetoken"])
        self.assertNotIn("openai", cfg.get("enabled_providers", []))
        self.assertNotIn("anthropic", cfg.get("enabled_providers", []))

    def test_config_command_env_includes_enabled_providers_allowlist(self):
        """#5882: ephemeral OPENCODE_CONFIG must emit enabled_providers (merge-safe)."""
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
                "state": "ABSENT",
                "openai_base_url": _base_for(runtime),
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.dict(os.environ, {"CE_ALLOW_BOX_LOCAL_AGENCY": "1"}):
          with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            with tempfile.TemporaryDirectory() as temporary:  # nosec B108
                args = dispatch.parse_args(["--prefer", "freetoken", "config", "--dir", temporary])
                buf = StringIO()
                with redirect_stdout(buf):
                    code = dispatch.cmd_config(args)
                payload = json.loads(buf.getvalue())
                self.assertEqual(code, 0)
                cfg_path = Path(payload["env"]["OPENCODE_CONFIG"])
                on_disk = json.loads(cfg_path.read_text(encoding="utf-8"))
                self.assertEqual(on_disk["enabled_providers"], ["freetoken"])
                content = json.loads(payload["env"]["OPENCODE_CONFIG_CONTENT"])
                self.assertEqual(content["enabled_providers"], ["freetoken"])
                self.assertIn("enabled_providers", payload["note"])

    def test_opencode_config_allowlist_matches_each_runtime_provider_id(self):
        for provider_id, base in (
            ("freetoken", "http://127.0.0.1:1919/v1"),
            ("ollama", "http://127.0.0.1:11434/v1"),
            ("lmstudio", "http://127.0.0.1:1234/v1"),
            ("llamacpp", "http://127.0.0.1:8080/v1"),
        ):
            chosen = {
                "runtime": provider_id,
                "provider_id": provider_id,
                "openai_base_url": base,
                "model": "m",
                "opencode_model": f"{provider_id}/m",
            }
            cfg = dispatch.opencode_config(chosen)
            self.assertEqual(cfg["enabled_providers"], [provider_id], provider_id)

    def test_refuses_durable_opencode_dir(self):
        chosen = {
            "runtime": "freetoken",
            "provider_id": "freetoken",
            "openai_base_url": "http://127.0.0.1:1919/v1",
            "model": "gpt-oss-20b",
            "opencode_model": "freetoken/gpt-oss-20b",
        }
        with self.assertRaises(ValueError):
            dispatch.write_ephemeral_config(chosen, Path.home() / ".config" / "opencode")

    def test_fallthrough_when_freetoken_ready_but_empty_models(self):
        def probe(runtime: str):
            if runtime == "freetoken":
                return {
                    "runtime": "freetoken",
                    "state": "READY",
                    "openai_base_url": dispatch.FREETOKEN_OPENAI_BASE,
                    "models": [],
                    "provider_id": "freetoken",
                }
            if runtime == "ollama":
                return {
                    "runtime": "ollama",
                    "state": "READY",
                    "openai_base_url": dispatch.OPENAI_COMPAT_BASES["ollama"],
                    "models": ["coder"],
                    "provider_id": "ollama",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": _base_for(runtime),
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            payload = dispatch.resolve_local()
        self.assertEqual(payload["state"], "READY")
        chosen = payload["chosen"]
        self.assertIsInstance(chosen, dict)
        self.assertEqual(chosen["runtime"], "ollama")
        self.assertEqual(chosen["opencode_model"], "ollama/coder")



    def test_resolve_ranks_colibri_after_openai_compat_peers(self):
        def probe(runtime: str):
            if runtime == "colibri":
                return {
                    "runtime": "colibri",
                    "state": "READY",
                    "openai_base_url": dispatch.COLIBRI_OPENAI_BASE,
                    "models": ["glm-frontier"],
                    "provider_id": "colibri",
                }
            if runtime == "ollama":
                return {
                    "runtime": "ollama",
                    "state": "READY",
                    "openai_base_url": dispatch.OPENAI_COMPAT_BASES["ollama"],
                    "models": ["coder"],
                    "provider_id": "ollama",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": dispatch.FREETOKEN_OPENAI_BASE
                if runtime == "freetoken"
                else dispatch.OPENAI_COMPAT_BASES.get(runtime, dispatch.COLIBRI_OPENAI_BASE),
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            payload = dispatch.resolve_local()
        self.assertEqual(payload["chosen"]["runtime"], "ollama")

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            preferred = dispatch.resolve_local(prefer="colibri")
        self.assertEqual(preferred["chosen"]["runtime"], "colibri")
        self.assertEqual(preferred["chosen"]["opencode_model"], "colibri/glm-frontier")



    def test_brief_subcommand_emits_text_and_receipt_fields(self):
        """#6068: dispatch.py brief [--project PATH] uses ce_brief.build_brief."""
        self.assertTrue(hasattr(dispatch, "cmd_brief"), "cmd_brief missing — implement P1 brief")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            # minimal locators optional; build_brief tolerates missing identity
            buf = StringIO()
            with redirect_stdout(buf):
                args = dispatch.parse_args(["brief", "--project", str(project), "--json"])
                code = dispatch.cmd_brief(args)
            self.assertEqual(code, 0)
            payload = json.loads(buf.getvalue())
            self.assertIn("text", payload)
            self.assertIn("bytes", payload)
            self.assertIn("schemaVersion", payload)
            self.assertIsInstance(payload["text"], str)
            self.assertGreater(payload["bytes"], 0)

    def test_with_ce_brief_flag_on_config_and_argv(self):
        """#6068: --with-ce-brief attaches brief without durable rewrite."""
        self.assertIn("--with-ce-brief", DISPATCH.read_text(encoding="utf-8"))

        def probe(runtime: str):
            if runtime == "freetoken":
                return {
                    "runtime": "freetoken",
                    "state": "READY",
                    "openai_base_url": dispatch.FREETOKEN_OPENAI_BASE,
                    "models": ["m"],
                    "provider_id": "freetoken",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": _base_for(runtime),
                "models": [],
                "provider_id": runtime,
            }

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            with tempfile.TemporaryDirectory() as temporary:
                buf = StringIO()
                with redirect_stdout(buf):
                    args = dispatch.parse_args(["config", "--dir", temporary, "--with-ce-brief"])
                    code = dispatch.cmd_config(args)
                self.assertEqual(code, 0)
                out = json.loads(buf.getvalue())
                self.assertFalse(out.get("durable_config_rewrite"))
                self.assertIn("ce_brief", out)
                self.assertIn("text", out["ce_brief"])

                buf2 = StringIO()
                with redirect_stdout(buf2):
                    args = dispatch.parse_args(
                        ["argv", "--prompt", "PING", "--dir", temporary, "--with-ce-brief"]
                    )
                    code = dispatch.cmd_argv(args)
                self.assertEqual(code, 0)
                out2 = json.loads(buf2.getvalue())
                self.assertIn("ce_brief", out2)
                self.assertIn("text", out2["ce_brief"])

    def test_chat_helper_posts_system_from_ce_brief(self):
        """#6068: optional chat helper POSTs to READY openai-compat with system=brief."""
        self.assertTrue(hasattr(dispatch, "cmd_chat"), "cmd_chat missing — implement P1 chat")
        captured = {}

        def probe(runtime: str):
            if runtime == "llamacpp":
                return {
                    "runtime": "llamacpp",
                    "state": "READY",
                    "openai_base_url": dispatch.OPENAI_COMPAT_BASES["llamacpp"],
                    "models": ["qwen"],
                    "provider_id": "llamacpp",
                }
            return {
                "runtime": runtime,
                "state": "ABSENT",
                "openai_base_url": _base_for(runtime),
                "models": [],
                "provider_id": runtime,
            }

        class _Resp:
            def __enter__(self):
                return self

            def __exit__(self, *args):
                return False

            def read(self):
                return json.dumps(
                    {"choices": [{"message": {"content": "ok"}}]}
                ).encode("utf-8")

        def fake_urlopen(req, timeout=0):
            captured["url"] = req.full_url
            captured["body"] = json.loads(req.data.decode("utf-8"))
            return _Resp()

        with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
            with mock.patch.object(dispatch, "urlopen", side_effect=fake_urlopen):
                buf = StringIO()
                with redirect_stdout(buf):
                    args = dispatch.parse_args(
                        ["--prefer", "llamacpp", "chat", "--prompt", "hi", "--with-ce-brief"]
                    )
                    code = dispatch.cmd_chat(args)
                self.assertEqual(code, 0)
        self.assertIn("/chat/completions", captured["url"])
        self.assertEqual(captured["body"]["messages"][0]["role"], "system")
        self.assertTrue(captured["body"]["messages"][0]["content"])
        self.assertEqual(captured["body"]["messages"][1]["role"], "user")

if __name__ == "__main__":
    unittest.main()
