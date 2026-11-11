#!/usr/bin/env python3
"""Fail a Codecov upload whose receipt does not name the requested flag.

Run 42912520949 shard-2 queued shaft-engine-shard-2 with HTTP 200 and the PR
comment still dropped that flag. Processing after a queued 200 is upstream.
This guard only rejects a success that cannot show the flag the job requested.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

_FLAG_ARG = re.compile(r"--flag(?:=|\s+)([A-Za-z0-9._-]+)")
_TOKEN_FLAGS = {"-t", "--token"}


def redact_codecov_args(argv: list[str]) -> list[str]:
    """Copy CLI args with upload tokens replaced."""
    redacted: list[str] = []
    skip = False
    for arg in argv:
        if skip:
            redacted.append("<redacted>")
            skip = False
            continue
        if arg in _TOKEN_FLAGS:
            redacted.append(arg)
            skip = True
            continue
        if arg.startswith("--token="):
            redacted.append("--token=<redacted>")
            continue
        redacted.append(arg)
    return redacted


def record_codecov_receipt(argv: list[str], receipt_path: Path) -> None:
    """Write the redacted Codecov CLI argv. This is the upload receipt."""
    receipt_path.parent.mkdir(parents=True, exist_ok=True)
    line = " ".join(redact_codecov_args(argv))
    receipt_path.write_text(line + "\n", encoding="utf-8")


def receipt_errors(receipt: str, requested_flag: str) -> list[str]:
    """Return errors when a requested flag is absent from the upload receipt.

    An empty requested flag is an intentionally unflagged upload. A non-empty
    flag must appear as its own ``--flag`` value. A longer or shorter token
    does not count, and a queued HTTP 200 line does not replace the flag.
    """
    flag = requested_flag.strip()
    if not flag:
        return []
    found = set(_FLAG_ARG.findall(receipt or ""))
    if flag in found:
        return []
    return [f"upload receipt does not name requested Codecov flag {flag}"]


def _load_receipt(path: Path) -> str:
    if not path.is_file():
        return ""
    return path.read_text(encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if args and args[0] == "--record-receipt":
        if len(args) < 3 or args[2] != "--":
            print(
                "usage: upload_jacoco_coverage.py --record-receipt PATH -- ARGS",
                file=sys.stderr,
            )
            return 2
        record_codecov_receipt(args[3:], Path(args[1]))
        return 0

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--flag", default="", help="Codecov flag this upload must name")
    parser.add_argument("--receipt", required=True, type=Path, help="Redacted CLI receipt")
    parsed = parser.parse_args(args)
    errors = receipt_errors(_load_receipt(parsed.receipt), parsed.flag)
    for error in errors:
        print(error, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
