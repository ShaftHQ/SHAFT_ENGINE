#!/usr/bin/env python3
"""L41 spoken-lock title scope gate. Stdlib only; exit 0 pass / 1 fail / 2 usage.

Does not modify craft_qc.py (Coach video sign-off PR owns that surface for L39 video gates).
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

PASS, FAIL, USAGE = 0, 1, 2
TOKEN = re.compile(r"[A-Za-z\u0600-\u06FF]{3,}")
STOP = {
    "the", "and", "for", "with", "from", "that", "this", "you", "are",
    "اللي", "على", "من", "في", "الى", "إلى", "عن", "مع", "مش", "كان",
}


def title_scope_hits(title: str, srt_text: str) -> list[str]:
    tokens = [t for t in TOKEN.findall(title) if t]
    scope = [t for t in tokens if t.lower() not in STOP]
    body_fold = srt_text.casefold()
    hits: list[str] = []
    for tok in scope:
        if tok.casefold() not in body_fold and tok not in srt_text:
            hits.append(f"scope token {tok!r} missing from answer SRT")
    return hits


def report(code: int, **details: object) -> int:
    status = "pass" if code == PASS else "fail" if code == FAIL else "usage"
    print(json.dumps({"check": "titlescope", "status": status, **details}, ensure_ascii=False))
    return code


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(description=__doc__)
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("check")
    c.add_argument("--title", required=True)
    c.add_argument("--srt", required=True, type=Path)
    args = p.parse_args(argv)
    if not args.srt.is_file():
        return report(USAGE, error=f"missing srt: {args.srt}")
    hits = title_scope_hits(args.title, args.srt.read_text(encoding="utf-8"))
    return report(FAIL if hits else PASS, title=args.title, hits=hits)


if __name__ == "__main__":
    sys.exit(main())
