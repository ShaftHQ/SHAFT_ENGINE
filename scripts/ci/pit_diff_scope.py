#!/usr/bin/env python3
"""Diff-scoped mutation testing helper (#7004). Report-only, never blocks.

scope:   changed src/main Java files -> "module<TAB>fqcn,fqcn" lines (capped).
summary: PIT mutations.xml reports -> markdown with killed/survived and score.
"""
from __future__ import annotations

import argparse
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

MAX_MODULES = 2
MAX_CLASSES = 15


def scope(paths: list[str], max_modules: int = MAX_MODULES, max_classes: int = MAX_CLASSES) -> dict[str, list[str]]:
    """Map module -> FQCNs of changed main classes, skipping deleted/non-module files."""
    result: dict[str, list[str]] = {}
    for raw in paths:
        parts = raw.strip().split("/")
        if len(parts) < 5 or parts[1:4] != ["src", "main", "java"] or not parts[-1].endswith(".java"):
            continue
        if parts[-1] in ("package-info.java", "module-info.java"):
            continue
        fqcn = ".".join(parts[4:])[: -len(".java")]
        module = parts[0]
        if module not in result and len(result) >= max_modules:
            continue
        classes = result.setdefault(module, [])
        if len(classes) < max_classes and fqcn not in classes:
            classes.append(fqcn)
    return result


def summarize(reports: list[Path]) -> str:
    counts: dict[str, int] = {}
    survivors: list[str] = []
    for report in reports:
        for mutation in ET.parse(report).getroot().iter("mutation"):
            status = mutation.get("status", "UNKNOWN")
            counts[status] = counts.get(status, 0) + 1
            if status == "SURVIVED" and len(survivors) < 10:
                survivors.append(f"`{mutation.findtext('mutatedClass')}:{mutation.findtext('lineNumber')}` "
                                 f"{mutation.findtext('description')}")
    total = sum(counts.values())
    if total == 0:
        return "### Mutation testing (report-only)\nNo mutations generated for the changed classes.\n"
    detected = counts.get("KILLED", 0) + counts.get("TIMED_OUT", 0) + counts.get("MEMORY_ERROR", 0)
    lines = ["### Mutation testing (report-only)",
             f"Mutation score: **{100 * detected // total}%** ({detected}/{total} detected)",
             "| status | count |", "|---|---|"]
    lines += [f"| {k} | {v} |" for k, v in sorted(counts.items())]
    if survivors:
        lines += ["", "Surviving mutants (first 10):"] + [f"- {s}" for s in survivors]
    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)
    sub.add_parser("scope", help="read changed paths on stdin")
    s = sub.add_parser("summary")
    s.add_argument("root", nargs="?", default=".")
    args = parser.parse_args(argv)
    if args.cmd == "scope":
        for module, classes in scope(sys.stdin.read().splitlines()).items():
            print(f"{module}\t{','.join(classes)}")
    else:
        print(summarize(sorted(Path(args.root).glob("*/target/pit-reports/mutations.xml"))), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
