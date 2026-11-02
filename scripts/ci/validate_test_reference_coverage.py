#!/usr/bin/env python3
"""Shrink-only ratchet: every public member of shipped modules is called by a test (issue #6375)."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import validate_javadoc_coverage as javadoc  # noqa: E402

REPO_ROOT = javadoc.REPO_ROOT
BASELINE_PATH = Path(__file__).with_name("test_reference_baseline.json")
_CALL = re.compile(r"\b([A-Za-z_]\w*)\s*\(")
_REFERENCE = re.compile(r"::\s*([A-Za-z_]\w*)")


def test_vocabulary(root: Path) -> set[str]:
    """Return every method name called or referenced from a test source."""
    words: set[str] = set()
    for source in list(root.glob("*/src/test/**/*.java")) + list(root.glob("src/test/**/*.java")):
        text = source.read_text(encoding="utf-8", errors="ignore")
        words.update(_CALL.findall(text))
        words.update(_REFERENCE.findall(text))
    return words


def public_members(source: str) -> list[tuple[int, str]]:
    """Return (line, name) for each public member the Javadoc ratchet also counts."""
    lines = source.split("\n")
    is_interface = bool(javadoc._INTERFACE_DECL.search(source))
    members = []
    in_block_comment = False
    for index, line in enumerate(lines):
        stripped = line.strip()
        if in_block_comment:
            in_block_comment = "*/" not in stripped
            continue
        if stripped.startswith("/*"):
            in_block_comment = "*/" not in stripped
            continue
        if stripped.startswith(("//", "*", "private", "protected")):
            continue
        match = javadoc._PUBLIC_METHOD.match(line) or (is_interface and javadoc._INTERFACE_METHOD.match(line))
        if not match or javadoc._TYPE_WORDS.search(line.split("(", 1)[0]):
            continue
        members.append((index + 1, match.group(1)))
    return members


def scan(root: Path, config: dict) -> dict[str, int]:
    """Count public members per file whose name no test calls."""
    words = test_vocabulary(root)
    counts: dict[str, int] = {}
    for source in sorted(root.glob("*/src/main/java/**/*.java")):
        relative = source.relative_to(root).as_posix()
        if javadoc._excluded(relative, config):
            continue
        text = source.read_text(encoding="utf-8", errors="ignore")
        missing = sum(1 for _line, name in public_members(text) if name not in words)
        if missing:
            counts[relative] = missing
    return counts


def compare(counts: dict[str, int], baseline: dict[str, int]) -> tuple[list[str], list[str]]:
    """Return (regressions, stale baseline entries)."""
    regressions = [
        f"{path}: {count} public members no test calls (baseline {baseline.get(path, 0)})"
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
    """Fail on new untested public members or a stale baseline; ``--update`` only shrinks."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=REPO_ROOT)
    parser.add_argument("--update", action="store_true", help="rewrite the baseline when nothing regressed")
    args = parser.parse_args(argv)
    config = javadoc.load_config()
    baseline = json.loads(BASELINE_PATH.read_text(encoding="utf-8"))
    counts = scan(args.root, config)
    regressions, stale = compare(counts, baseline)
    if regressions:
        print("Public members no test calls (add a test):")
        print("\n".join(f"  {line}" for line in regressions))
        return 1
    if args.update:
        BASELINE_PATH.write_text(json.dumps(counts, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        print(f"Baseline updated: {sum(counts.values())} untested public members.")
        return 0
    if stale:
        print("Baseline is stale; run with --update to shrink it:")
        print("\n".join(f"  {line}" for line in stale))
        return 1
    print(f"Test reference coverage OK: {sum(counts.values())} baselined untested public members.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
