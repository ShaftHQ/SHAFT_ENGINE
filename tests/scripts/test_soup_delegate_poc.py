"""Contract tests for the Soup Agent Forge row checker (#5125)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
POC_ROOT = ROOT / "tools" / "soup-delegate-poc"
SCRIPT = POC_ROOT / "soup_delegate_poc.py"
HARVEST_SCRIPT = POC_ROOT / "harvest.py"
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

    def test_row_missing_source_endpoint_fails(self):
        row = valid_row()
        del row["source_endpoint"]
        blockers = MODULE.validate_row(row)
        self.assertTrue(any("source_endpoint" in item.lower() for item in blockers))

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

    def test_spec_accepts_quoted_and_unquoted_operation_ids(self):
        yaml_text = (
            "openapi: 3.0.3\n"
            "info:\n"
            "  title: quoted\n"
            "  version: '1.0.0'\n"
            "paths:\n"
            "  /read_file:\n"
            "    post:\n"
            "      operationId: \"read_file\"\n"
            "  /replace_file:\n"
            "    post:\n"
            "      operationId: replace_file\n"
            "  /run_focused_test:\n"
            "    post:\n"
            "      operationId: \"run_focused_test\"\n"
        )
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "mechanical-tools.yaml"
            path.write_text(yaml_text, encoding="utf-8")
            self.assertEqual([], MODULE.validate_spec(path))


def _valid_report(**overrides):
    data = {
        "ok": True,
        "model": "mechanical",
        "worktree": "C:/tmp/wt",
        "files_allowed": ["scripts/local-coding-agent/agent.py"],
        "files_changed": ["scripts/local-coding-agent/agent.py"],
        "commit": "abc123",
        "test_command": "py -3 -m unittest tests.scripts.test_local_coding_agent",
        "test_exit": 0,
        "elapsed_ms": 10,
        "loopback": "127.0.0.1:11434",
        "blockers": [],
    }
    data.update(overrides)
    return data


def _write_report_dir(root: Path, report: dict, spec_text: str | None = None) -> Path:
    report_dir = root / "report"
    report_dir.mkdir()
    (report_dir / "report.json").write_text(json.dumps(report), encoding="utf-8")
    if spec_text is not None:
        (report_dir / "spec.md").write_text(spec_text, encoding="utf-8")
    return report_dir


def _load_harvest():
    spec = importlib.util.spec_from_file_location("soup_harvest", HARVEST_SCRIPT)
    if spec is None or spec.loader is None:
        raise RuntimeError("harvest module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SoupDelegatePocHarvestTest(unittest.TestCase):
    def test_valid_report_writes_one_valid_row(self):
        harvest_mod = _load_harvest()
        with tempfile.TemporaryDirectory() as temporary:
            tmp_path = Path(temporary)
            report_dir = _write_report_dir(
                tmp_path,
                _valid_report(),
                spec_text="Please replace one allowlisted file.",
            )
            corpus = tmp_path / "corpus" / "rows.jsonl"
            result = harvest_mod.harvest(str(report_dir), str(corpus))
            self.assertTrue(result["ok"])
            self.assertFalse(result["skipped"])
            self.assertIsInstance(result["row"], dict)
            self.assertTrue(corpus.is_file())
            rows = MODULE.load_jsonl(corpus)
            self.assertEqual(1, len(rows))
            self.assertEqual([], MODULE.validate_row(rows[0]))
            self.assertEqual([], MODULE.validate_row(result["row"]))

    def test_allowlist_violation_skips_without_write(self):
        harvest_mod = _load_harvest()
        with tempfile.TemporaryDirectory() as temporary:
            tmp_path = Path(temporary)
            report_dir = _write_report_dir(
                tmp_path,
                _valid_report(files_changed=["scripts/ci/watch_pr_checks.py"]),
            )
            corpus = tmp_path / "corpus.jsonl"
            result = harvest_mod.harvest(str(report_dir), str(corpus))
            self.assertFalse(result["ok"])
            self.assertTrue(result["skipped"])
            self.assertFalse(corpus.exists())

    def test_missing_report_key_skips_without_write(self):
        harvest_mod = _load_harvest()
        with tempfile.TemporaryDirectory() as temporary:
            tmp_path = Path(temporary)
            report = _valid_report()
            del report["loopback"]
            report_dir = _write_report_dir(tmp_path, report)
            corpus = tmp_path / "corpus.jsonl"
            result = harvest_mod.harvest(str(report_dir), str(corpus))
            self.assertFalse(result["ok"])
            self.assertTrue(result["skipped"])
            self.assertFalse(corpus.exists())

    def test_harvest_source_has_no_soup_train_or_loop_watch(self):
        text = HARVEST_SCRIPT.read_text(encoding="utf-8")
        self.assertNotIn("soup train", text)
        self.assertNotIn("soup loop watch", text)


if __name__ == "__main__":
    unittest.main()
