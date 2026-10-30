#!/usr/bin/env python3
"""Read and validate the broadcastable ChaosEngine VERSION.

The releasing lead broadcasts ``CE upgrade <version>`` using the exact
string in ``chaos-engine/VERSION``. Stdlib only.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
VERSION_PATH = ROOT / "chaos-engine" / "VERSION"
VERSION_RE = re.compile(r"^\d{4}\.\d{2}\.\d{2}(\.\d+)?$")


def read_version(path: Path | None = None) -> str:
    """Return the stripped VERSION string; raise ValueError if missing/invalid."""
    target = VERSION_PATH if path is None else Path(path)
    if not target.is_file():
        raise ValueError(f"missing VERSION file: {target}")
    value = target.read_text(encoding="utf-8").strip().splitlines()[0].strip()
    if VERSION_RE.fullmatch(value) is None:
        raise ValueError(f"invalid VERSION {value!r}; expected YYYY.MM.DD or YYYY.MM.DD.N")
    return value


def upgrade_broadcast(version: str | None = None) -> str:
    """Return the canonical broadcast string ``CE upgrade <version>``."""
    value = read_version() if version is None else version.strip()
    if VERSION_RE.fullmatch(value) is None:
        raise ValueError(f"invalid VERSION {value!r}")
    return f"CE upgrade {value}"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("read", help="print chaos-engine/VERSION")
    sub.add_parser("broadcast", help="print CE upgrade <version>")
    check = sub.add_parser("check", help="validate VERSION file; exit 1 on failure")
    check.add_argument("--path", type=Path, default=None)
    args = parser.parse_args(argv)
    try:
        if args.command == "read":
            print(read_version())
            return 0
        if args.command == "broadcast":
            print(upgrade_broadcast())
            return 0
        if args.command == "check":
            value = read_version(args.path)
            print(f"ce_version: pass {value}")
            return 0
    except ValueError as exc:
        print(f"ce_version: {exc}", file=sys.stderr)
        return 1
    return 2


if __name__ == "__main__":
    sys.exit(main())
