#!/usr/bin/env python3
"""Stdlib checker for Soup Agent Forge row shape (#5125)."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any


REQUIRED_ROW_KEYS = ("messages", "tool", "source_endpoint")
REQUIRED_OPERATION_IDS = ("read_file", "replace_file", "run_focused_test")
OPERATION_ID_PATTERN = re.compile(r"(?m)^\s*operationId:\s*(\S+)\s*$")


def _assistant_tool_name(assistant: dict[str, Any]) -> str | None:
    calls = assistant.get("tool_calls")
    if not isinstance(calls, list) or not calls or not isinstance(calls[0], dict):
        return None
    function = calls[0].get("function")
    if not isinstance(function, dict):
        return None
    name = function.get("name")
    return name if isinstance(name, str) and name else None


def validate_row(obj: Any) -> list[str]:
    """Return blockers for one Agent Forge row. Empty means the row is valid."""
    if not isinstance(obj, dict):
        return ["row must be an object"]
    blockers: list[str] = []
    for key in REQUIRED_ROW_KEYS:
        if key not in obj:
            blockers.append(f"missing {key}")
    tool = obj.get("tool")
    if "tool" in obj and (not isinstance(tool, str) or not tool):
        blockers.append("tool must be a non-empty string")
    messages = obj.get("messages")
    if "messages" in obj:
        if not isinstance(messages, list) or len(messages) < 2:
            blockers.append("messages must contain user then assistant")
        else:
            user, assistant = messages[0], messages[1]
            if not isinstance(user, dict) or user.get("role") != "user":
                blockers.append("messages[0].role must be user")
            if not isinstance(assistant, dict) or assistant.get("role") != "assistant":
                blockers.append("messages[1].role must be assistant")
            else:
                name = _assistant_tool_name(assistant)
                if name is None:
                    blockers.append("assistant tool_calls[0].function.name is missing")
                elif isinstance(tool, str) and name != tool:
                    blockers.append(f"assistant tool name {name} does not match tool {tool}")
    endpoint = obj.get("source_endpoint")
    if "source_endpoint" in obj:
        if not isinstance(endpoint, str) or not endpoint.startswith("/"):
            blockers.append("source_endpoint must be an OpenAPI path")
        elif isinstance(tool, str) and endpoint != f"/{tool}":
            blockers.append(f"source_endpoint {endpoint} does not match tool {tool}")
    return blockers


def load_jsonl(path: str | Path) -> list[dict[str, Any]]:
    """Load JSON objects from a JSONL file. Blank lines are ignored."""
    rows: list[dict[str, Any]] = []
    text = Path(path).read_text(encoding="utf-8")
    for index, raw in enumerate(text.splitlines(), start=1):
        line = raw.strip()
        if not line:
            continue
        value = json.loads(line)
        if not isinstance(value, dict):
            raise ValueError(f"{path}:{index} must be a JSON object")
        rows.append(value)
    return rows


def validate_jsonl(path: str | Path) -> list[str]:
    """Return blockers for every row in a JSONL file."""
    blockers: list[str] = []
    try:
        rows = load_jsonl(path)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        return [f"cannot load jsonl {path}: {error}"]
    for index, row in enumerate(rows, start=1):
        for item in validate_row(row):
            blockers.append(f"row {index}: {item}")
    return blockers


def validate_spec(path: str | Path) -> list[str]:
    """Return blockers when the OpenAPI file lacks the three operationIds."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except OSError as error:
        return [f"cannot read spec {path}: {error}"]
    found = {item.strip("\"'") for item in OPERATION_ID_PATTERN.findall(text)}
    return [
        f"spec missing operationId {name}"
        for name in REQUIRED_OPERATION_IDS
        if name not in found
    ]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command")
    validate = subparsers.add_parser("validate")
    validate.add_argument("--jsonl", required=True)
    validate.add_argument("--spec", required=True)
    args = parser.parse_args(argv)
    if args.command != "validate":
        parser.print_help()
        return 2
    blockers = validate_spec(args.spec) + validate_jsonl(args.jsonl)
    for item in blockers:
        print(item)
    return 1 if blockers else 0


if __name__ == "__main__":
    sys.exit(main())
