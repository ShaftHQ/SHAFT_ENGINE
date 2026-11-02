#!/usr/bin/env python3
"""Shrink-only Javadoc coverage ratchet for the public API of shipped modules (issue #6375)."""

from __future__ import annotations

import argparse
import fnmatch
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
BASELINE_PATH = Path(__file__).with_name("javadoc_coverage_baseline.json")

_PUBLIC_METHOD = re.compile(r"^\s*public\s+(?:[\w<>\[\],.? ]+\s+)?(\w+)\s*\(")
_INTERFACE_METHOD = re.compile(r"^ {4}(?! )(?:default\s+|static\s+)?(?:<[^>]+>\s+)?[\w<>\[\],.? ]+\s+(\w+)\s*\(")
_INTERFACE_DECL = re.compile(r"^public\s+(?:sealed\s+)?@?interface\b", re.MULTILINE)
_TYPE_WORDS = re.compile(r"\b(class|interface|enum|record|new|return|throw|if|for|while|switch|catch|synchronized)\b")


def load_config(path: Path = BASELINE_PATH) -> dict:
    """Read the exclusions and per-file baseline."""
    return json.loads(path.read_text(encoding="utf-8"))


def _has_javadoc(lines: list[str], index: int) -> tuple[bool, bool]:
    """Return (documented, overrides) for the declaration at ``index``."""
    j = index - 1
    while j >= 0:
        text = lines[j].strip()
        if text.startswith("@Override"):
            return True, True
        if text.startswith("@") or not text:
            j -= 1
            continue
        return text.endswith("*/"), False
    return False, False


def undocumented_members(source: str) -> list[int]:
    """Return 1-based line numbers of public members in ``source`` that have no Javadoc."""
    lines = source.split("\n")
    is_interface = bool(_INTERFACE_DECL.search(source))
    missing = []
    in_block_comment = False
    for index, line in enumerate(lines):
        stripped = line.strip()
        if in_block_comment:
            in_block_comment = "*/" not in stripped
            continue
        if stripped.startswith("/*"):
            in_block_comment = "*/" not in stripped
            continue
        if stripped.startswith(("//", "*")):
            continue
        match = _PUBLIC_METHOD.match(line) or (is_interface and _INTERFACE_METHOD.match(line))
        if not match or _TYPE_WORDS.search(line.split("(", 1)[0]):
            continue
        if stripped.startswith(("private", "protected")):
            continue
        documented, _ = _has_javadoc(lines, index)
        if not documented:
            missing.append(index + 1)
    return missing


def _excluded(relative: str, config: dict) -> bool:
    """Return True when ``relative`` matches a documented exclusion."""
    if any(fnmatch.fnmatch(relative, rule["glob"]) for rule in config.get("included_overrides", [])):
        return False
    return any(fnmatch.fnmatch(relative, rule["glob"]) for rule in config["exclusions"])


def scan(root: Path, config: dict) -> dict[str, int]:
    """Count undocumented public members per source file across shipped modules."""
    counts: dict[str, int] = {}
    for source in sorted(root.glob("*/src/main/java/**/*.java")):
        relative = source.relative_to(root).as_posix()
        if _excluded(relative, config):
            continue
        missing = len(undocumented_members(source.read_text(encoding="utf-8", errors="ignore")))
        if missing:
            counts[relative] = missing
    return counts


def compare(counts: dict[str, int], baseline: dict[str, int]) -> tuple[list[str], list[str]]:
    """Return (regressions, stale baseline entries)."""
    regressions = [
        f"{path}: {count} undocumented public members (baseline {baseline.get(path, 0)})"
        for path, count in sorted(counts.items())
        if count > baseline.get(path, 0)
    ]
    stale = [
        f"{path}: baseline {allowed}, now {counts.get(path, 0)}"
        for path, allowed in sorted(baseline.items())
        if counts.get(path, 0) < allowed
    ]
    return regressions, stale


def main(argv: list[str] | None = None) -> int:
    """Run the ratchet; ``--update`` lowers the baseline and never raises it."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true", help="shrink the baseline to the current counts")
    args = parser.parse_args(argv)
    config = load_config()
    counts = scan(REPO_ROOT, config)
    regressions, stale = compare(counts, config["baseline"])
    if regressions:
        print("New undocumented public API (add Javadoc; the baseline only shrinks):")
        print("\n".join(f"  {line}" for line in regressions))
        return 1
    if stale and not args.update:
        print("Javadoc coverage improved; shrink the baseline with:")
        print("  python3 scripts/ci/validate_javadoc_coverage.py --update")
        print("\n".join(f"  {line}" for line in stale))
        return 1
    if args.update:
        config["baseline"] = dict(sorted(counts.items()))
        BASELINE_PATH.write_text(json.dumps(config, indent=2) + "\n", encoding="utf-8")
    print(f"Javadoc coverage OK: {sum(counts.values())} undocumented members left in baseline.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
