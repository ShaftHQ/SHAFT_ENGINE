"""Opt-in per-session tool receipt beside the token ledger."""

from __future__ import annotations

import importlib.util
import inspect
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
USAGE = ROOT / "chaos-engine/session_token_usage.py"
KERNEL = ROOT / "chaos-engine/hooks/kernel.py"
CE_ROOT = ROOT / "chaos-engine"
FORBIDDEN_KEYS = frozenset({"arguments", "result", "error", "path", "model"})


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _keys(value: object) -> set[str]:
    found: set[str] = set()
    if isinstance(value, dict):
        found.update(str(key) for key in value)
        for child in value.values():
            found.update(_keys(child))
    elif isinstance(value, list):
        for child in value:
            found.update(_keys(child))
    return found


class SessionToolReceiptTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load("session_tool_receipt_usage", USAGE)
        self.kernel = _load("session_tool_receipt_kernel", KERNEL)
        if str(CE_ROOT) not in sys.path:
            sys.path.insert(0, str(CE_ROOT))
        import hooks.reflection as reflection

        self.reflection = reflection

    def _receipt_path(self, project: Path, session_id: str) -> Path:
        return (
            project
            / ".chaos-engine-state"
            / "session-token-usage"
            / f"{session_id}.tool-receipt.json"
        )

    def _read(self, project: Path, session_id: str) -> dict:
        return json.loads(self._receipt_path(project, session_id).read_text(encoding="utf-8"))

    def _enable(self):
        return mock.patch.dict(os.environ, {"CHAOS_ENGINE_RUN_RECEIPT": "1"})

    def _disable(self):
        patch = mock.patch.dict(os.environ)
        patch.start()
        os.environ.pop("CHAOS_ENGINE_RUN_RECEIPT", None)
        self.addCleanup(patch.stop)
        return patch

    def test_three_paired_calls_name_slowest_tool(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            session = "sess-receipt"
            path = self._receipt_path(project, session)
            with self._enable():
                self.mod.note_tool_boundary(session, "PreToolUse", "c1", "read", now=100.0, project=project)
                self.mod.note_tool_boundary(session, "PreToolUse", "c2", "grep", now=300.0, project=project)
                self.mod.note_tool_boundary(session, "PreToolUse", "c3", "read", now=200.0, project=project)
                self.assertFalse(path.exists())
                self.mod.note_tool_boundary(session, "PostToolUse", "c1", "read", now=101.0, project=project)
                self.mod.note_tool_boundary(
                    session, "PostToolUseFailure", "c2", "grep", now=302.0, project=project
                )
                self.mod.note_tool_boundary(session, "PostToolUse", "c3", "read", now=203.0, project=project)
            payload = self._read(project, session)
            self.assertEqual(
                {"schemaVersion", "sessionId", "elapsedSeconds", "tokens", "tools", "slowestTool", "cost"},
                set(payload),
            )
            self.assertEqual(1, payload["schemaVersion"])
            self.assertEqual(session, payload["sessionId"])
            tools = {item["name"]: item for item in payload["tools"]}
            self.assertEqual(["grep", "read"], [item["name"] for item in payload["tools"]])
            self.assertEqual({"name", "count", "totalSeconds"}, set(tools["read"]))
            self.assertEqual(2, tools["read"]["count"])
            self.assertEqual(1, tools["grep"]["count"])
            self.assertEqual(4.0, tools["read"]["totalSeconds"])
            self.assertEqual(2.0, tools["grep"]["totalSeconds"])
            self.assertEqual("read", payload["slowestTool"])
            text = self.mod.format_tool_receipt(session, project=project)
            self.assertIn("read count=2 totalSeconds=4.0", text)
            self.assertIn("grep count=1 totalSeconds=2.0", text)
            self.assertIn("slowest read", text)
            table = self.mod.usage_table(path.parent)
            self.assertIn("| Task | Tokens in | Tokens out | Est. cost (USD) |", table)
            self.assertNotIn("| sess-receipt.tool-receipt |", table)
            self.assertIn("slowest read", table)
            self.assertNotIn("super-secret-argument", path.read_text(encoding="utf-8"))

    def test_evaluate_omits_tool_arguments(self) -> None:
        signature = inspect.signature(self.mod.note_tool_boundary)
        self.assertNotIn("tool_input", signature.parameters)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            previous = Path.cwd()
            os.chdir(project)
            try:
                with self._enable():
                    pre = self.kernel.normalize_event(
                        {
                            "hook_event_name": "PreToolUse",
                            "session_id": "sess-boundary",
                            "tool_name": "read",
                            "tool_use_id": "call-secret",
                            "tool_input": {"content": "super-secret-argument"},
                        },
                        "claude",
                    )
                    self.assertIn("super-secret-argument", json.dumps(dict(pre.tool_input)))
                    self.kernel.evaluate(pre)
                    post = self.kernel.normalize_event(
                        {
                            "hook_event_name": "PostToolUse",
                            "session_id": "sess-boundary",
                            "tool_name": "read",
                            "tool_use_id": "call-secret",
                            "tool_input": {"content": "super-secret-argument"},
                        },
                        "claude",
                    )
                    self.kernel.evaluate(post)
            finally:
                os.chdir(previous)
            path = self._receipt_path(project, "sess-boundary")
            text = path.read_text(encoding="utf-8")
            self.assertNotIn("super-secret-argument", text)
            payload = json.loads(text)
            self.assertTrue(FORBIDDEN_KEYS.isdisjoint(_keys(payload)))

    def test_disabled_receipt_writes_no_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self._disable()
            self.mod.note_tool_boundary(
                "sess-off", "PreToolUse", "c1", "read", now=1.0, project=project
            )
            self.mod.note_tool_boundary(
                "sess-off", "PostToolUse", "c1", "read", now=2.0, project=project
            )
            self.assertEqual([], list(project.rglob("*.tool-receipt.json")))
            previous = Path.cwd()
            os.chdir(project)
            try:
                pre = self.kernel.normalize_event(
                    {
                        "hook_event_name": "PreToolUse",
                        "session_id": "sess-off",
                        "tool_name": "read",
                        "tool_use_id": "c1",
                        "tool_input": {},
                    },
                    "claude",
                )
                post = self.kernel.normalize_event(
                    {
                        "hook_event_name": "PostToolUse",
                        "session_id": "sess-off",
                        "tool_name": "read",
                        "tool_use_id": "c1",
                        "tool_input": {},
                    },
                    "claude",
                )
                self.kernel.evaluate(pre)
                self.kernel.evaluate(post)
            finally:
                os.chdir(previous)
            self.assertEqual([], list(project.rglob("*.tool-receipt.json")))

    def test_absent_ledger_tokens_and_cost_are_unknown(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            session = "sess-no-ledger"
            with self._enable():
                self.mod.note_tool_boundary(
                    session, "PostToolUse", "only", "read", now=1.0, project=project
                )
            ledger = project / ".chaos-engine-state" / "session-token-usage" / f"{session}.json"
            self.assertFalse(ledger.exists())
            payload = self._read(project, session)
            self.assertEqual(
                {
                    "localPrompt": "unknown",
                    "localCompletion": "unknown",
                    "cloudPrompt": "unknown",
                    "cloudCompletion": "unknown",
                    "cache": "unknown",
                    "reasoning": "unknown",
                },
                payload["tokens"],
            )
            self.assertEqual("unknown", payload["cost"])
            self.assertNotEqual(0, payload["cost"])
            for value in payload["tokens"].values():
                self.assertNotEqual(0, value)

    def test_elapsed_seconds_follow_reflection(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            session = "sess-elapsed"
            with self._enable():
                with mock.patch.object(self.reflection, "session_elapsed_seconds", return_value=None):
                    self.mod.note_tool_boundary(
                        session, "PostToolUse", "e1", "read", now=1.0, project=project
                    )
                self.assertEqual("unknown", self._read(project, session)["elapsedSeconds"])
                with mock.patch.object(self.reflection, "session_elapsed_seconds", return_value=12.5):
                    self.mod.note_tool_boundary(
                        session, "PostToolUse", "e2", "read", now=2.0, project=project
                    )
            self.assertEqual(12.5, self._read(project, session)["elapsedSeconds"])

    def test_unpaired_post_duration_is_unknown(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            session = "sess-unpaired"
            with self._enable():
                self.mod.note_tool_boundary(
                    session, "PostToolUse", "missing", "read", now=4.0, project=project
                )
                payload = self._read(project, session)
                self.assertEqual("unknown", payload["tools"][0]["totalSeconds"])
                self.assertEqual(1, payload["tools"][0]["count"])
                self.assertEqual("unknown", payload["slowestTool"])
                self.assertNotEqual(0, payload["tools"][0]["totalSeconds"])

    def test_usage_without_receipt_keeps_the_table(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            folder = Path(temporary)
            text = self.mod.usage_table(folder)
            self.assertEqual(
                "| Task | Tokens in | Tokens out | Est. cost (USD) |\n|---|---|---|---|\n",
                text,
            )
            self.assertEqual([], list(folder.glob("*.tool-receipt.json")))


if __name__ == "__main__":
    unittest.main()
