#!/usr/bin/env python3
"""API/binary compatibility gate (#7002).

Reads each ``<module>/target/japicmp/<name>.xml`` written by
``japicmp:cmp`` against the latest Maven Central release and lists classes
whose ``binaryCompatible`` is false. Breaks block the PR unless it carries the
``breaking-change`` label (plus a release note, enforced by release-note
governance). Prints a markdown summary for the job summary.
"""
from __future__ import annotations

import argparse
import sys
import xml.etree.ElementTree as ET  # nosec B405 - local build output only
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LABEL = "breaking-change"


def breaks(report: Path) -> list[str]:
    tree = ET.parse(report)  # nosec B314 - trusted local build output
    found = []
    for cls in tree.getroot().iter("class"):
        if cls.get("binaryCompatible") == "false":
            changes = sorted({c.get("type") or "?" for c in cls.iter("compatibilityChange") if c.get("binaryCompatible") == "false"})
            found.append(f"{cls.get('fullyQualifiedName')}: {', '.join(changes) or 'incompatible'}")
    return found


def collect(root: Path, modules: list[str]) -> dict[str, list[str]]:
    result: dict[str, list[str]] = {}
    for module in modules:
        for report in sorted((root / module / "target" / "japicmp").glob("*.xml")):
            result.setdefault(module, []).extend(breaks(report))
    return result


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--modules", default="")
    parser.add_argument("--labels", default="", help="comma-separated PR labels")
    parser.add_argument("--root", type=Path, default=ROOT)
    args = parser.parse_args(argv)
    modules = [m for m in args.modules.split(",") if m]
    found = collect(args.root, modules)
    total = sum(len(v) for v in found.values())
    print(f"## API compatibility vs latest release\n\n{total} binary-incompatible class(es) in {len(modules)} module(s).\n")
    for module, items in found.items():
        for item in items:
            print(f"- `{module}` {item}")
    if total and LABEL not in {l.strip() for l in args.labels.split(",")}:
        print(f"\n::error::Binary-incompatible API change. If intended, add the `{LABEL}` label and a release note.")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
