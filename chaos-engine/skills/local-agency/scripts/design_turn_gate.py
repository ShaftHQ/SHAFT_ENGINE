#!/usr/bin/env python3
"""Zero-LLM gates for local design/spec turns (CE_BRIEF_LOCATORS)."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

LOCATOR_LINE = re.compile(r"(?m)^CE_BRIEF_LOCATORS:\s*(.+?)\s*$")

SPEC_HEADERS = [
    "Goal",
    "Context",
    "Acceptance",
    "Out of scope",
    "First slice",
    "Validation",
    "Risks",
]


def parse_locator_closing(text: str) -> list[str] | None:
    """Return locator paths from the required closing line, or None if missing."""
    match = LOCATOR_LINE.search(text or "")
    if match is None:
        return None
    raw = match.group(1).strip()
    if raw.lower() == "none":
        return []
    return [part.strip() for part in raw.split("|") if part.strip()]


def brief_locator_paths(brief_text: str) -> list[str]:
    """Locator-only brief lines (non-empty, non-heading)."""
    out: list[str] = []
    for line in (brief_text or "").splitlines():
        item = line.strip()
        if not item or item.startswith("#"):
            continue
        out.append(item)
    return out


def gate_citation(writer_text: str, brief_text: str) -> dict[str, object]:
    """Pass when CE_BRIEF_LOCATORS matches the injected brief paths exactly (order-insensitive)."""
    expected = brief_locator_paths(brief_text)
    got = parse_locator_closing(writer_text)
    if got is None:
        return {
            "ok": False,
            "gate": "brief_citation",
            "advice": "missing CE_BRIEF_LOCATORS closing line",
            "expected": expected,
            "got": None,
        }
    if sorted(got) != sorted(expected):
        return {
            "ok": False,
            "gate": "brief_citation",
            "advice": "CE_BRIEF_LOCATORS must copy brief paths verbatim",
            "expected": expected,
            "got": got,
        }
    return {"ok": True, "gate": "brief_citation", "expected": expected, "got": got}


def gate_schema(writer_text: str, required_headers: list[str]) -> dict[str, object]:
    """Pass when each required markdown ## header is present."""
    missing = [h for h in required_headers if f"## {h}" not in (writer_text or "")]
    if missing:
        return {"ok": False, "gate": "schema", "missing": missing}
    return {"ok": True, "gate": "schema", "missing": []}


def _read_text_file(path_arg: str) -> str:
    """Read a regular file as UTF-8 after resolving the path."""
    path = Path(path_arg).expanduser().resolve()
    if not path.is_file():
        raise FileNotFoundError(f"not a readable file: {path}")
    return path.read_text(encoding="utf-8")


def _emit(result: dict[str, object], as_json: bool) -> int:
    if as_json:
        print(json.dumps(result, sort_keys=True))
    else:
        print("PASS" if result.get("ok") else "FAIL", result)
    return 0 if result.get("ok") else 1


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    cite = sub.add_parser("citation", help="validate CE_BRIEF_LOCATORS against brief text")
    cite.add_argument("--writer", required=True, help="path to writer output")
    cite.add_argument("--brief", required=True, help="path to brief text file")
    cite.add_argument("--json", action="store_true")

    schema = sub.add_parser("schema", help="validate required ## headers for a ticket spec")
    schema.add_argument("--writer", required=True)
    schema.add_argument("--json", action="store_true")

    args = parser.parse_args(argv)
    if args.command == "citation":
        result = gate_citation(_read_text_file(args.writer), _read_text_file(args.brief))
        return _emit(result, args.json)
    result = gate_schema(_read_text_file(args.writer), SPEC_HEADERS)
    return _emit(result, args.json)


if __name__ == "__main__":
    raise SystemExit(main())
