#!/usr/bin/env python3
"""Portable learn-traces collector. Map-reduce stays with isolated host agents."""

from __future__ import annotations

import argparse
import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

SECRET = re.compile(r"(?i)(api[_-]?key|token|password|secret)\s*[:=]\s*\S+")


def redact(text: str) -> str:
    return SECRET.sub(r"\1=<redacted>", text)


def collect(home: Path, out: Path) -> dict[str, object]:
    out.mkdir(parents=True, exist_ok=True)
    sessions: list[dict[str, object]] = []
    dropped = {"missing_roots": 0}
    roots = [
        home / ".grok" / "sessions",
        home / ".claude" / "projects",
        home / ".codex" / "sessions",
    ]
    for root in roots:
        if not root.is_dir():
            dropped["missing_roots"] += 1
    manifest = {
        "schemaVersion": 1,
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "home": str(home),
        "sessions_kept": len(sessions),
        "dropped": dropped,
        "next": "map-reduce-verify via isolated subagents; see references/learn-traces.md",
    }
    (out / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    (out / "sessions").mkdir(exist_ok=True)
    return manifest


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="learn_traces.py")
    sub = parser.add_subparsers(dest="cmd", required=True)
    collect_p = sub.add_parser("collect")
    collect_p.add_argument("--home", type=Path, default=Path.home())
    collect_p.add_argument("--out", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.cmd == "collect":
        manifest = collect(args.home.expanduser(), args.out)
        print(json.dumps({"run_dir": str(args.out), **manifest}))
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
