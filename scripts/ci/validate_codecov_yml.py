#!/usr/bin/env python3
"""Validate codecov.yml against Codecov's validator (#6990 lesson).

Codecov silently ignores the WHOLE file when any key is unknown and falls
back to defaults (no carryforward, no targets), so a typo disables every
coverage rule without a visible error. POST the file to the validate
endpoint; anything but "Valid!" fails. ``--soft`` turns network errors into
a warning (pre-push); CI runs it hard.
"""
from __future__ import annotations

import argparse
import sys
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ENDPOINT = "https://codecov.io/validate"


def verdict(response_text: str) -> tuple[bool, str]:
    text = response_text.strip()
    return text.startswith("Valid!"), text.splitlines()[0] if text else "empty response"


def validate(path: Path, *, opener=urllib.request.urlopen) -> str:
    request = urllib.request.Request(ENDPOINT, data=path.read_bytes(), method="POST")
    with opener(request, timeout=20) as response:  # nosec B310 - fixed https endpoint
        return response.read().decode("utf-8", errors="replace")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("path", nargs="?", type=Path, default=ROOT / "codecov.yml")
    parser.add_argument("--soft", action="store_true", help="warn instead of fail when the validator is unreachable")
    args = parser.parse_args(argv)
    try:
        ok, detail = verdict(validate(args.path))
    except (urllib.error.URLError, TimeoutError, OSError) as error:
        print(f"codecov.yml validator unreachable: {error}")
        return 0 if args.soft else 1
    if not ok:
        print(f"codecov.yml rejected by Codecov: {detail}")
        return 1
    print("codecov.yml: Valid!")
    return 0


if __name__ == "__main__":
    sys.exit(main())
