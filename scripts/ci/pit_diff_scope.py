#!/usr/bin/env python3
"""
Diff-scoped mutation testing helper (#7004); report-only, never blocks.

scope:   changed src/main Java files -> "module<TAB>fqcn,fqcn" lines (capped).
summary: PIT mutations.xml reports -> markdown with killed/survived and score.
"""
from __future__ import annotations

import argparse
import re
import sys
import xml.etree.ElementTree as ET  # nosec B405 - local build output only
from pathlib import Path

MAX_MODULES = 2
MAX_CLASSES = 15


def reactor_modules(pom: Path = Path("pom.xml")) -> set[str] | None:
    """Maven reactor module names from the root pom; None when there is no pom (no filtering)."""
    try:
        text = pom.read_text(encoding="utf-8")
    except OSError:
        return None
    return set(re.findall(r"<module>\s*([^<\s]+)\s*</module>", text))


def scope(paths: list[str], max_modules: int = MAX_MODULES, max_classes: int = MAX_CLASSES,
          reactor: set[str] | None = None) -> dict[str, list[str]]:
    """Map module -> FQCNs of changed main classes, skipping deleted/non-module files.

    Directories outside the Maven reactor (shaft-intellij is Gradle, benchmarks/jmh is standalone) are skipped.
    """
    result: dict[str, list[str]] = {}
    for raw in paths:
        parts = raw.strip().split("/")
        if len(parts) < 5 or parts[1:4] != ["src", "main", "java"] or not parts[-1].endswith(".java"):
            continue
        if parts[-1] in ("package-info.java", "module-info.java"):
            continue
        fqcn = ".".join(parts[4:])[: -len(".java")]
        module = parts[0]
        if reactor is not None and module not in reactor:
            continue
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
        for mutation in ET.parse(report).getroot().iter("mutation"):  # nosec B314 - trusted local build output
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
        for module, classes in scope(sys.stdin.read().splitlines(), reactor=reactor_modules()).items():
            print(f"{module}\t{','.join(classes)}")
    else:
        print(summarize(sorted(Path(args.root).glob("*/target/pit-reports/mutations.xml"))), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
