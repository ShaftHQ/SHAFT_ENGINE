#!/usr/bin/env python3
"""Fail the build when a JUnit-style test report shows nothing actually ran.

Guards live-test workflows (issue #3932) against a silent false-green: a typo
in the gate system property (e.g. -Dshaft.intellij.liveToolE2E) makes every
test self-skip via Assumptions, and the job still exits 0. Parses the same
JUnit XML schema Maven Surefire and Gradle's Test task both emit
(<testsuite tests="" failures="" errors="" skipped="">), following the
xml.etree.ElementTree precedent at .github/workflows/pr-gate.yml:291-305.
"""

from __future__ import annotations

import argparse
import xml.etree.ElementTree as ET
from pathlib import Path


def summarize(xml_path: Path) -> tuple[int, int, int, int]:
    """Returns (tests, failures, errors, skipped) from one JUnit-style report."""
    root = ET.parse(xml_path).getroot()
    tests = int(root.get("tests", "0"))
    failures = int(root.get("failures", "0"))
    errors = int(root.get("errors", "0"))
    skipped = int(root.get("skipped", "0"))
    return tests, failures, errors, skipped


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reports", nargs="+", type=Path, help="JUnit-style TEST-*.xml report(s)")
    parser.add_argument("--min-executed", type=int, default=1,
                         help="Minimum non-skipped tests required across all reports (default: 1)")
    args = parser.parse_args(argv)

    total_tests = total_failures = total_errors = total_skipped = 0
    for report in args.reports:
        tests, failures, errors, skipped = summarize(report)
        executed = tests - skipped
        print(f"{report}: tests={tests} failures={failures} errors={errors} skipped={skipped} executed={executed}")
        total_tests += tests
        total_failures += failures
        total_errors += errors
        total_skipped += skipped

    total_executed = total_tests - total_skipped
    print(f"TOTAL: tests={total_tests} failures={total_failures} errors={total_errors} "
          f"skipped={total_skipped} executed={total_executed}")

    if total_failures or total_errors:
        print(f"::error::{total_failures} failure(s) / {total_errors} error(s) across the reports")
        return 1
    if total_executed < args.min_executed:
        print(f"::error::only {total_executed} test(s) actually executed (need >= {args.min_executed}); "
              "every test self-skipped -- check the live-gate system property")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
