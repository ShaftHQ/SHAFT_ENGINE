#!/usr/bin/env python3
"""Error Prone / NullAway per-module ratchet (#6999).

Parses ``[WARNING] <path>.java:[line,col] [Check] message`` lines from a Maven
build run with ``-Perrorprone`` and counts findings per ``module::Check``.
A count above ``.github/errorprone-baseline.json`` fails; only modules named
with ``--modules`` (the PR's changed modules) are judged, so the gate is
diff-scoped. ``--write`` regenerates the baseline from a full-reactor log.
"""
from __future__ import annotations

import argparse
import collections
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / ".github/errorprone-baseline.json"
FINDING = re.compile(r"^\[WARNING\] (?P<path>\S+?\.java):\[\d+,\d+\] \[(?P<check>[A-Za-z0-9]+)\] ")


def module_of(path: str) -> str:
    rel = path.split(str(ROOT) + "/", 1)[-1] if str(ROOT) in path else path
    marker = rel.find("/src/main/")
    return rel[:marker].rsplit("/", 1)[-1] if marker > 0 else "?"


def count(lines) -> collections.Counter:
    seen: set[str] = set()
    counts: collections.Counter = collections.Counter()
    for line in lines:
        match = FINDING.match(line)
        if match and line not in seen:  # Maven can echo a warning twice
            seen.add(line)
            counts[f"{module_of(match['path'])}::{match['check']}"] += 1
    return counts


def regressions(current, baseline: dict[str, int], modules: set[str]) -> list[str]:
    return sorted(
        f"{key}: {n} > baseline {baseline.get(key, 0)}"
        for key, n in current.items()
        if key.split("::", 1)[0] in modules and n > baseline.get(key, 0)
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="+", type=Path)
    parser.add_argument("--modules", default="", help="comma-separated changed modules to judge")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args(argv)
    current: collections.Counter = collections.Counter()
    for log in args.logs:
        current += count(log.read_text(encoding="utf-8", errors="replace").splitlines())
    if args.write:
        BASELINE.write_text(json.dumps(dict(sorted(current.items())), indent=2) + "\n", encoding="utf-8")
        print(f"baseline: {sum(current.values())} findings, {len(current)} module::check keys")
        return 0
    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    modules = {m for m in args.modules.split(",") if m}
    failures = regressions(current, baseline, modules)
    for line in failures:
        print(f"::error::new Error Prone finding {line}")
    print(f"Error Prone ratchet: {len(modules)} module(s) judged, {len(failures)} regression(s)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
