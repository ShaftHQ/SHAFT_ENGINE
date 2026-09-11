#!/usr/bin/env python3
"""Fail when Surefire/TestNG reports show failures under Maven failure-ignore.

SHAFT's engine Surefire profile may set testFailureIgnore so JaCoCo still
reports. A green Maven exit is then not proof. N-run / wave proof scripts must
parse report XML after each invocation and require zero failed counts
(issue #5739).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


def summarize_surefire_reports(report_root: Path) -> tuple[int, int]:
    """Return (failures, errors) across TEST-*.xml under report_root."""
    failures = 0
    errors = 0
    for path in sorted(report_root.glob("TEST-*.xml")):
        text = path.read_text(encoding="utf-8", errors="replace")
        fm = re.search(r'\bfailures="(\d+)"', text)
        em = re.search(r'\berrors="(\d+)"', text)
        failures += int(fm.group(1)) if fm else 0
        errors += int(em.group(1)) if em else 0
    return failures, errors


def summarize_testng_results(report_root: Path) -> int | None:
    """Return failed count from testng-results.xml, or None when missing."""
    path = report_root / "testng-results.xml"
    if not path.is_file():
        return None
    text = path.read_text(encoding="utf-8", errors="replace")
    match = re.search(r'<testng-results[^>]*\bfailed="(\d+)"', text)
    return int(match.group(1)) if match else 0


def assert_surefire_green(report_root: Path, *, label: str = "run") -> None:
    """Raise SystemExit(1) when reports are missing or show failures/errors."""
    root = Path(report_root)
    surefire = list(root.glob("TEST-*.xml"))
    if surefire:
        failures, errors = summarize_surefire_reports(root)
        if failures or errors:
            print(
                f"FAIL: {label} reported failures={failures} errors={errors}",
                file=sys.stderr,
            )
            raise SystemExit(1)
        print(f"OK: {label} green (failures=0 errors=0)")
        return

    failed = summarize_testng_results(root)
    if failed is None:
        print(
            f"FAIL: {label} produced no Surefire/TestNG report under {root}",
            file=sys.stderr,
        )
        raise SystemExit(1)
    if failed:
        print(f"FAIL: {label} reported failures={failed} errors=0", file=sys.stderr)
        raise SystemExit(1)
    print(f"OK: {label} green (failures=0 errors=0)")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "report_root",
        type=Path,
        help="Directory containing TEST-*.xml and/or testng-results.xml",
    )
    parser.add_argument("--label", default="run", help="Label for OK/FAIL lines")
    args = parser.parse_args(argv)
    try:
        assert_surefire_green(args.report_root, label=args.label)
    except SystemExit as exit_code:
        return int(exit_code.code or 0)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
