#!/usr/bin/env python3
"""
Fail when the ChaosEngine plugin CHANGELOG/COMPATIBILITY miss the engine version (#6222).

The portable plugin is versioned by the root ``pom.xml`` project version. A
release PR that bumps it must also add ``## <version>`` to the plugin
CHANGELOG and name the version in COMPATIBILITY, otherwise the weekly
``test_assemble_chaos_engine_plugin`` run turns red days later.
"""

from __future__ import annotations

import argparse
import sys
import xml.etree.ElementTree as ET  # nosec B405 - parses the tracked repository pom only.
from pathlib import Path

POM_NAMESPACE = "{http://maven.apache.org/POM/4.0.0}"
CHANGELOG = Path("agent-plugins/chaos-engine/CHANGELOG.md")
COMPATIBILITY = Path("agent-plugins/chaos-engine/COMPATIBILITY.md")


def engine_version(root: Path) -> str:
    """Return the root pom project version, which is the plugin version."""
    version = ET.parse(root / "pom.xml").getroot().findtext(f"{POM_NAMESPACE}version", default="")  # nosec B314
    return version.strip()


def defects(root: Path) -> list[str]:
    """Return human-readable defects; empty when both files cover the version."""
    version = engine_version(root)
    if not version:
        return ["pom.xml has no project version"]
    problems: list[str] = []
    changelog = (root / CHANGELOG).read_text(encoding="utf-8").splitlines()
    heading = f"## {version}"
    if not any(line == heading or line.startswith(heading + " ") for line in changelog):
        problems.append(f"{CHANGELOG} has no '{heading}' entry for the engine version in pom.xml")
    if version not in (root / COMPATIBILITY).read_text(encoding="utf-8"):
        problems.append(f"{COMPATIBILITY} does not name the engine version {version}")
    return problems


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=(__doc__ or "").strip().splitlines()[0])
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    args = parser.parse_args(argv)
    problems = defects(args.root)
    for problem in problems:
        print(f"plugin-changelog: {problem}", file=sys.stderr)
    if not problems:
        print(f"plugin changelog covers chaos-engine {engine_version(args.root)}")
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main())
