#!/usr/bin/env python3
"""Turn a local-loop report directory into one Soup-shaped JSONL row (#5127)."""

from __future__ import annotations

import argparse
import importlib.util
import json
import sys
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
AGENT_SCRIPT = ROOT / "scripts" / "local-coding-agent" / "agent.py"
POC_SCRIPT = Path(__file__).resolve().parent / "soup_delegate_poc.py"
SECRET_MARKERS = ("hf_", "sk-", "AKIA", "-----BEGIN", "Bearer ")
DEFAULT_SPEC = "mechanical local-loop harvest"


def _load_module(name: str, path: Path) -> Any:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"{name} could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


AGENT = _load_module("local_coding_agent_harvest", AGENT_SCRIPT)
POC = _load_module("soup_delegate_poc_harvest", POC_SCRIPT)


def _skip(reason: str) -> dict[str, Any]:
    return {"ok": False, "skipped": True, "reason": reason, "row": None}


def _secret_marker(text: str) -> str | None:
    for marker in SECRET_MARKERS:
        if marker in text:
            return marker
    return None


def _read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def harvest(report_dir: str | Path, corpus_path: str | Path) -> dict[str, Any]:
    """Append one Soup row from a local-loop report directory, or skip."""
    report_file = Path(report_dir) / "report.json"
    if not report_file.is_file():
        return _skip("missing report.json")
    try:
        raw_report = _read_text(report_file)
        data = json.loads(raw_report)
    except (OSError, json.JSONDecodeError, UnicodeError) as error:
        return _skip(f"cannot load report.json: {error}")
    if not isinstance(data, dict):
        return _skip("report.json must be an object")

    blockers = AGENT.validate_report(data)
    if blockers:
        return _skip(blockers[0])

    spec_file = Path(report_dir) / "spec.md"
    diff_file = Path(report_dir) / "diff.patch"
    spec_present = spec_file.is_file()
    try:
        spec_text = _read_text(spec_file) if spec_present else ""
        diff_text = _read_text(diff_file) if diff_file.is_file() else ""
    except (OSError, UnicodeError) as error:
        return _skip(f"cannot read report siblings: {error}")

    for label, text in (("report", raw_report), ("spec", spec_text), ("diff", diff_text)):
        if text and _secret_marker(text):
            return _skip(f"secret-shaped text in {label}")

    changed = AGENT.as_path_list(data.get("files_changed"))
    tool = "replace_file" if changed else "read_file"
    user_content = spec_text if spec_present else DEFAULT_SPEC
    row = {
        "source": "local-loop-report",
        "tool": tool,
        "source_endpoint": f"/{tool}",
        "messages": [
            {"role": "user", "content": user_content},
            {
                "role": "assistant",
                "tool_calls": [
                    {
                        "id": "call_0",
                        "type": "function",
                        "function": {
                            "name": tool,
                            "arguments": json.dumps({"paths": changed}),
                        },
                    }
                ],
            },
        ],
    }
    row_blockers = POC.validate_row(row)
    if row_blockers:
        return _skip(row_blockers[0])

    dest = Path(corpus_path)
    dest.parent.mkdir(parents=True, exist_ok=True)
    with dest.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(row, ensure_ascii=False) + "\n")
    return {"ok": True, "skipped": False, "reason": "wrote", "row": row}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--report-dir", required=True)
    parser.add_argument("--corpus", required=True)
    args = parser.parse_args(argv)
    result = harvest(args.report_dir, args.corpus)
    print(result["reason"])
    return 0 if result["ok"] else 2


if __name__ == "__main__":
    sys.exit(main())
