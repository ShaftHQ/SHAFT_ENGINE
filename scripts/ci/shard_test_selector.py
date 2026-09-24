#!/usr/bin/env python3
"""
Split one Surefire ``-Dtest`` selector into deterministic shards (#6188).

Shard 1 runs every package glob (``testPackage/unitTests/*``); the named
classes are dealt round-robin across the remaining shards. Exclusions
(``!Class``) are applied to every shard. The union of all shards is exactly
the input selector, so sharding never adds or drops a test class. Measured on
run 36059197717: the package glob took ~260 s and the named classes ~218 s of
the 484 s single-leg test step.
"""

from __future__ import annotations

import argparse
import sys

PREFIX = "-Dtest="


def entries(selector: str) -> list[str]:
    body = selector.strip()
    if body.startswith(PREFIX):
        body = body[len(PREFIX):]
    return [entry.strip() for entry in body.split(",") if entry.strip()]


def is_glob(entry: str) -> bool:
    return "*" in entry or "/" in entry


def shard_entries(selector: str, index: int, total: int) -> list[str]:
    if total < 1 or not 1 <= index <= total:
        raise ValueError(f"shard {index} of {total} is out of range")
    items = entries(selector)
    exclusions = [entry for entry in items if entry.startswith("!")]
    positives = [entry for entry in items if not entry.startswith("!")]
    if total == 1:
        return positives + exclusions
    globs = [entry for entry in positives if is_glob(entry)]
    names = [entry for entry in positives if not is_glob(entry)]
    if index == 1:
        chosen = globs
    else:
        chosen = [name for offset, name in enumerate(names) if offset % (total - 1) == index - 2]
    return chosen + exclusions


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--index", type=int, required=True)
    parser.add_argument("--total", type=int, required=True)
    parser.add_argument("--selector", required=True)
    args = parser.parse_args(argv)
    print(",".join(shard_entries(args.selector, args.index, args.total)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
