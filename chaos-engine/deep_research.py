#!/usr/bin/env python3
"""Portable deep-research run scaffold. Hosts fill phases with isolated agents."""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path


def init(query: str, out: Path, breadth: int = 4) -> dict[str, object]:
    if breadth < 2 or breadth > 6:
        breadth = 4
    out.mkdir(parents=True, exist_ok=True)
    run = {
        "schemaVersion": 1,
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "query": query,
        "breadth": breadth,
        "phases": ["plan", "research", "verify", "report"],
        "next": "isolated subagents per references/deep-research.md; do not use a host TUI as owner",
    }
    (out / "run.json").write_text(json.dumps(run, indent=2) + "\n", encoding="utf-8")
    return run


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="deep_research.py")
    sub = parser.add_subparsers(dest="cmd", required=True)
    init_p = sub.add_parser("init")
    init_p.add_argument("--query", required=True)
    init_p.add_argument("--out", type=Path, required=True)
    init_p.add_argument("--breadth", type=int, default=4)
    args = parser.parse_args(argv)
    if args.cmd == "init":
        run = init(args.query, args.out, args.breadth)
        print(json.dumps({"run_dir": str(args.out), **run}))
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
