#!/usr/bin/env python3
"""Lint worker / Task briefs for mandatory ChaosEngine load (Phase 1 memory abstraction).

Every fresh worker brief must:

1. Contain a line ``ce_load: required`` (exact key).
2. Instruct running ``tool.py entry`` (or ``entry --full``).

    python3 chaos-engine/skills/local-agency/scripts/worker_ce_entry.py lint BRIEF.md

Exit 0 pass, 1 fail, 2 usage. Stdlib only.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

CE_LOAD = re.compile(r"(?m)^ce_load:\s*required\s*$")
ENTRY = re.compile(
    r"tool\.py\s+entry(?:\s+--full)?|"
    r"python3\s+\.chaos-engine/tool\.py\s+entry",
    re.I,
)


def lint_brief(text: str) -> list[str]:
    failures: list[str] = []
    if CE_LOAD.search(text) is None:
        failures.append("missing `ce_load: required` line")
    if ENTRY.search(text) is None:
        failures.append("missing ChaosEngine entry instruction (`tool.py entry`)")
    return failures


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    lint = sub.add_parser("lint", help="lint a worker brief file")
    lint.add_argument("brief_file", type=Path)
    args = parser.parse_args(argv)
    if args.command == "lint":
        if not args.brief_file.is_file():
            print(f"worker_ce_entry: not a file: {args.brief_file}", file=sys.stderr)
            return 2
        failures = lint_brief(args.brief_file.read_text(encoding="utf-8"))
        for f in failures:
            print(f"worker_ce_entry: {f}", file=sys.stderr)
        if failures:
            return 1
        print("worker_ce_entry: pass")
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
