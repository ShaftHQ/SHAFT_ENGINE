"""ROG FreeToken host gate: fail closed on box; pass when forced (#6021)."""

from __future__ import annotations

import importlib.util
import json
import os
import tempfile
import unittest
import unittest.mock as mock
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "chaos-engine/skills/local-agency/scripts/require_rog_freetoken.py"
DISPATCH = ROOT / "chaos-engine/skills/local-agency/scripts/dispatch.py"

_SPEC = importlib.util.spec_from_file_location("require_rog_freetoken", SCRIPT)
if _SPEC is None or _SPEC.loader is None:
    raise RuntimeError(f"unable to load {SCRIPT}")
gate = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(gate)

_DSPEC = importlib.util.spec_from_file_location("local_agency_dispatch_6021", DISPATCH)
if _DSPEC is None or _DSPEC.loader is None:
    raise RuntimeError(f"unable to load {DISPATCH}")
dispatch = importlib.util.module_from_spec(_DSPEC)
_DSPEC.loader.exec_module(dispatch)


def _read(rel: str) -> str:
    return (ROOT / rel).read_text(encoding="utf-8")


class RequireRogFreetokenTest(unittest.TestCase):
    def test_fails_on_fake_box_hostname(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            payload = gate.require_rog_bound(
                hostname="cursor-box-fake",
                path=Path(tmp),
                environ={},
            )
        self.assertFalse(payload["bound"])
        self.assertEqual(payload["state"], "UNHEALTHY")
        self.assertIn("#6021", str(payload["advice"]))
        self.assertIn("machineId", str(payload["advice"]))

    def test_passes_when_forced_with_env(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            payload = gate.require_rog_bound(
                hostname="cursor-box-fake",
                path=Path(tmp),
                environ={"CE_ALLOW_BOX_LOCAL_AGENCY": "1"},
            )
        self.assertTrue(payload["bound"])
        self.assertEqual(payload["state"], "READY")
        self.assertTrue(payload["allow_box_override"])

    def test_passes_when_hostname_looks_like_rog(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            payload = gate.require_rog_bound(
                hostname="ROG-G14",
                path=Path(tmp),
                environ={},
            )
        self.assertTrue(payload["bound"])
        self.assertEqual(payload["state"], "READY")

    def test_prefer_freetoken_errors_when_not_ready(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            payload = gate.require_prefer_freetoken(
                hostname="cursor-box-fake",
                path=Path(tmp),
                environ={"CE_ALLOW_BOX_LOCAL_AGENCY": "1"},
                freetoken_state="ABSENT",
            )
        self.assertEqual(payload["state"], "UNHEALTHY")
        self.assertIn("FreeToken not READY", str(payload["advice"]))
        self.assertIn("#6021", str(payload["advice"]))

    def test_cli_check_fails_on_box_hostname(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            buf = StringIO()
            with redirect_stdout(buf):
                code = gate.main(
                    ["--hostname", "cursor-box-fake", "--path", tmp, "check"]
                )
        self.assertEqual(code, 1)
        payload = json.loads(buf.getvalue())
        self.assertEqual(payload["state"], "UNHEALTHY")

    def test_cli_passes_when_forced(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            buf = StringIO()
            env = {**os.environ, "CE_ALLOW_BOX_LOCAL_AGENCY": "1"}
            with mock.patch.dict(os.environ, env, clear=True):
                with redirect_stdout(buf):
                    code = gate.main(
                        [
                            "--hostname",
                            "cursor-box-fake",
                            "--path",
                            tmp,
                            "resolve",
                            "--prefer",
                            "freetoken",
                            "--freetoken-state",
                            "READY",
                        ]
                    )
        self.assertEqual(code, 0)
        payload = json.loads(buf.getvalue())
        self.assertEqual(payload["state"], "READY")

    def test_dispatch_prefer_freetoken_respects_gate(self) -> None:
        unhealthy = {
            "state": "UNHEALTHY",
            "bound": False,
            "advice": "ROG FreeToken gate failed (#6021)",
        }
        with mock.patch.object(dispatch, "rog_freetoken_gate") as mocked:
            mocked.return_value.require_rog_bound.return_value = unhealthy
            payload = dispatch.resolve_local(prefer="freetoken")
        self.assertEqual(payload["state"], "UNHEALTHY")
        self.assertIsNone(payload["chosen"])
        self.assertIn("#6021", str(payload["advice"]))
        self.assertFalse(payload["omniroute_fallback"])

    def test_dispatch_prefer_freetoken_errors_when_probe_not_ready(self) -> None:
        bound = {"state": "READY", "bound": True}

        def probe(runtime: str):
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

        with mock.patch.object(dispatch, "rog_freetoken_gate") as mocked:
            mocked.return_value.require_rog_bound.return_value = bound
            with mock.patch.object(dispatch, "probe_runtime", side_effect=probe):
                payload = dispatch.resolve_local(prefer="freetoken")
        self.assertEqual(payload["state"], "UNHEALTHY")
        self.assertIn("FreeToken not READY", str(payload["advice"]))
        self.assertIn("#6021", str(payload["advice"]))

    def test_docs_phrase_lock(self) -> None:
        skill = _read("chaos-engine/skills/local-agency/SKILL.md")
        guide = _read("chaos-engine/guides/local-agency.md")
        owner = _read("chaos-engine/references/process-owner-scrum-master.md")
        lessons = _read("chaos-engine/skills/omniroute/references/living-lessons.md")
        for text in (skill, guide, owner, lessons):
            self.assertIn("#6021", text)
        self.assertIn("CE_ALLOW_BOX_LOCAL_AGENCY", skill)
        self.assertIn("must **not** claim FreeToken", skill)
        self.assertIn("require_rog_freetoken.py", guide)
        self.assertIn("process-owner", owner.lower())
        self.assertIn("machineId", owner)
        self.assertIn("require_rog_freetoken.py", lessons)

    def test_phrase_pack_mentions_6021(self) -> None:
        # Keep the older pack focused; this file owns #6021 phrases.
        self.assertTrue(SCRIPT.is_file())
        self.assertIn("require_rog_freetoken", DISPATCH.read_text(encoding="utf-8"))
        self.assertNotIn("mohab", SCRIPT.read_text(encoding="utf-8").casefold())


if __name__ == "__main__":
    unittest.main()
