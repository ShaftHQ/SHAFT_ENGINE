"""Contract tests for the Soup Agent Forge row checker (#5125)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
POC_ROOT = ROOT / "tools" / "soup-delegate-poc"
SCRIPT = POC_ROOT / "soup_delegate_poc.py"
SPEC_PATH = POC_ROOT / "mechanical-tools.yaml"
FIXTURE_PATH = POC_ROOT / "fixtures" / "valid.jsonl"

SPEC = importlib.util.spec_from_file_location("soup_delegate_poc", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("Soup delegate PoC module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def valid_row(tool: str = "read_file") -> dict:
    return {
        "messages": [
            {"content": "Please Read one allowlisted file.", "role": "user"},
            {
                "role": "assistant",
                "tool_calls": [
                    {
                        "function": {
                            "arguments": '{"path": "scripts/local-coding-agent/agent.py"}',
                            "name": tool,
                        },
                        "id": "call_0",
                        "type": "function",
                    }
                ],
            },
        ],
        "source_endpoint": f"/{tool}",
        "tool": tool,
    }


class SoupDelegatePocRowTest(unittest.TestCase):
    def test_valid_fixture_rows_pass(self):
        self.assertEqual([], MODULE.validate_spec(SPEC_PATH))
        rows = MODULE.load_jsonl(FIXTURE_PATH)
        self.assertEqual(3, len(rows))
        self.assertEqual(
            {"read_file", "replace_file", "run_focused_test"},
            {row["tool"] for row in rows},
        )
        self.assertEqual([], MODULE.validate_jsonl(FIXTURE_PATH))
        for row in rows:
            self.assertEqual([], MODULE.validate_row(row))

    def test_row_missing_tool_fails(self):
        row = valid_row()
        del row["tool"]
        blockers = MODULE.validate_row(row)
        self.assertTrue(any("tool" in item.lower() for item in blockers))

    def test_row_missing_messages_fails(self):
        row = valid_row()
        del row["messages"]
        blockers = MODULE.validate_row(row)
        self.assertTrue(any("messages" in item.lower() for item in blockers))

    def test_assistant_tool_name_mismatch_fails(self):
        row = valid_row("read_file")
        row["messages"][1]["tool_calls"][0]["function"]["name"] = "replace_file"
        blockers = MODULE.validate_row(row)
        self.assertTrue(blockers)
        self.assertTrue(
            any("tool" in item.lower() or "name" in item.lower() for item in blockers)
        )


class SoupDelegatePocSpecTest(unittest.TestCase):
    def test_spec_missing_run_focused_test_fails(self):
        yaml_text = (
            "openapi: 3.0.3\n"
            "info:\n"
            "  title: incomplete\n"
            "  version: '1.0.0'\n"
            "paths:\n"
            "  /read_file:\n"
            "    post:\n"
            "      operationId: read_file\n"
            "  /replace_file:\n"
            "    post:\n"
            "      operationId: replace_file\n"
        )
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "mechanical-tools.yaml"
            path.write_text(yaml_text, encoding="utf-8")
            blockers = MODULE.validate_spec(path)
        self.assertTrue(any("run_focused_test" in item for item in blockers))


if __name__ == "__main__":
    unittest.main()
