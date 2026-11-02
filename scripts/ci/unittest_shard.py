#!/usr/bin/env python3
"""
Run one deterministic shard of a unittest module (#6207).

``python -m scripts.ci.unittest_shard <module> --shard 2/4 -v`` loads every
test of ``<module>``, orders them by test id, and runs those whose position
modulo the shard total equals the shard index minus one. The union of all
shards is exactly the module, and a shard that selects nothing fails so a
shrinking module cannot turn a shard into a silent green no-op.
"""

from __future__ import annotations

import argparse
import sys
import unittest
from typing import Iterator


def iter_tests(suite: unittest.TestSuite) -> Iterator[unittest.TestCase]:
    """Flatten nested suites into individual test cases."""
    for item in suite:
        if isinstance(item, unittest.TestSuite):
            yield from iter_tests(item)
        else:
            yield item


def parse_shard(value: str) -> tuple[int, int]:
    """Parse ``i/n`` with ``1 <= i <= n``."""
    index_text, _, total_text = value.partition("/")
    try:
        index, total = int(index_text), int(total_text)
    except ValueError as error:
        raise argparse.ArgumentTypeError(f"shard must be i/n, got {value!r}") from error
    if total < 1 or not 1 <= index <= total:
        raise argparse.ArgumentTypeError(f"shard must satisfy 1 <= i <= n, got {value!r}")
    return index, total


def select(tests: list[unittest.TestCase], index: int, total: int) -> list[unittest.TestCase]:
    """Round-robin selection over tests ordered by id."""
    ordered = sorted(tests, key=lambda test: test.id())
    return [test for position, test in enumerate(ordered) if position % total == index - 1]


def main(argv: list[str] | None = None) -> int:
    """Run the selected shard and return a process exit code."""
    parser = argparse.ArgumentParser(description="Run one shard of a unittest module")
    parser.add_argument("module")
    parser.add_argument("--shard", required=True, type=parse_shard)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args(argv)
    index, total = args.shard
    loaded = unittest.defaultTestLoader.loadTestsFromName(args.module)
    selected = select(list(iter_tests(loaded)), index, total)
    if not selected:
        print(f"unittest-shard: {args.module} shard {index}/{total} selected no tests", file=sys.stderr)
        return 1
    print(f"unittest-shard: {args.module} shard {index}/{total}: {len(selected)} tests", file=sys.stderr)
    result = unittest.TextTestRunner(verbosity=2 if args.verbose else 1).run(unittest.TestSuite(selected))
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
