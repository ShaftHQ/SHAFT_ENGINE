#!/usr/bin/env python3
"""SpotBugs + FindSecBugs nightly trend (#6999).

Counts BugInstances per module from every ``<module>/target/spotbugsXml.xml``
and compares them with ``.github/spotbugs-baseline.json``. Exit 2 (regression)
lets the nightly open or update one ``ci-spotbugs`` issue; ``--write`` lowers
the baseline after fixes. Prints a markdown report.
"""
from __future__ import annotations

import argparse
import json
import sys
import xml.etree.ElementTree as ET  # nosec B405 - local build output only
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / ".github/spotbugs-baseline.json"


def module_counts(root: Path) -> dict[str, int]:
    counts: dict[str, int] = {}
    for report in sorted(root.glob("*/target/spotbugsXml.xml")):
        tree = ET.parse(report)  # nosec B314 - trusted local build output
        counts[report.parent.parent.name] = len(tree.getroot().findall("BugInstance"))
    return counts


def regressions(current: dict[str, int], baseline: dict[str, int]) -> list[str]:
    return sorted(f"{m}: {n} > baseline {baseline.get(m, 0)}"
                  for m, n in current.items() if n > baseline.get(m, 0))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args(argv)
    current = module_counts(args.root)
    if args.write:
        BASELINE.write_text(json.dumps(current, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        return 0
    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    print("## SpotBugs + FindSecBugs (nightly)\n\n| Module | Findings | Baseline |\n| --- | --- | --- |")
    for module in sorted(current):
        print(f"| {module} | {current[module]} | {baseline.get(module, 0)} |")
    failures = regressions(current, baseline)
    if failures:
        print("\n**Regressions:**\n" + "\n".join(f"- {line}" for line in failures))
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
