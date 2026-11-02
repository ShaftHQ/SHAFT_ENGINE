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
# #7702: current scheme 0.<quarter>.<YYYYMMDD>.<n> (n = merges that Cairo day); legacy YYYY.MM.DD[.N] still parses.
NEW_VERSION_RE = re.compile(r"^(?P<major>\d+)\.(?P<quarter>[1-4])\.(?P<date>\d{8})\.(?P<n>[1-9]\d*)$")
LEGACY_VERSION_RE = re.compile(r"^(?P<y>\d{4})\.(?P<m>\d{2})\.(?P<d>\d{2})(?:\.(?P<ln>\d+))?$")
VERSION_RE = re.compile(r"^(?:\d+\.[1-4]\.\d{8}\.[1-9]\d*|\d{4}\.\d{2}\.\d{2}(?:\.\d+)?)$")


def _quarter_from_yyyymmdd(date_int: int) -> int:
    """Calendar quarter 1-4 derived from a YYYYMMDD stamp (Cairo calendar day)."""
    month = (date_int // 100) % 100
    if not 1 <= month <= 12:
        raise ValueError(f"invalid month in date stamp {date_int}")
    return (month - 1) // 3 + 1


def version_key(value: str) -> tuple[int, int, int]:
    """Comparable key (major, date, n); quarter is derived from the date (#7702).

    Ordering by date (not the quarter field) keeps Q4→Q1 year rollover correct:
    ``0.4.20261231.1`` < ``0.1.20270101.1``.
    """
    text = str(value).strip()
    new = NEW_VERSION_RE.fullmatch(text)
    if new:
        date_int = int(new["date"])
        declared = int(new["quarter"])
        derived = _quarter_from_yyyymmdd(date_int)
        if declared != derived:
            raise ValueError(
                f"CE version {text!r} quarter {declared} does not match date "
                f"(expected {derived})"
            )
        return int(new["major"]), date_int, int(new["n"])
    old = LEGACY_VERSION_RE.fullmatch(text)
    if old:
        date_int = int(old["y"] + old["m"] + old["d"])
        return 0, date_int, int(old["ln"] or 0)
    raise ValueError(f"invalid CE version {text!r}")


def next_version(current: str, merge_day: str) -> str:
    """Next CE version for a merge on ``merge_day`` (YYYY-MM-DD, Cairo); same day bumps n, new day resets to 1."""
    import datetime as dt

    day = dt.date.fromisoformat(merge_day)
    quarter = (day.month - 1) // 3 + 1
    major, cur_date, cur_n = version_key(current)
    stamp = int(f"{day:%Y%m%d}")
    if stamp < cur_date:
        raise ValueError("merge day is before the current CE version date")
    n = cur_n + 1 if stamp == cur_date and NEW_VERSION_RE.fullmatch(current.strip()) else 1
    return f"{major}.{quarter}.{stamp}.{n}"


def read_version(path: Path | None = None) -> str:
    """Return the stripped VERSION string; raise ValueError if missing/invalid."""
    target = VERSION_PATH if path is None else Path(path)
    if not target.is_file():
        raise ValueError(f"missing VERSION file: {target}")
    value = target.read_text(encoding="utf-8").strip().splitlines()[0].strip()
    if VERSION_RE.fullmatch(value) is None:
        raise ValueError(f"invalid VERSION {value!r}; expected 0.<quarter>.<YYYYMMDD>.<n>")
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
