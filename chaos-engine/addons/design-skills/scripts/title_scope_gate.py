#!/usr/bin/env python3
"""L41 spoken-lock title scope gate. Stdlib only; exit 0 pass / 1 fail / 2 usage.

Does not modify craft_qc.py (sibling video sign-off lesson owns that surface for L39 video gates).

Matching is casefold exact, plus light stemming / Arabic definite-article strip /
brand allowlist so titles like "testing basics" hit "tests of the craft" and
brand tokens need not appear in the answer SRT.
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
# Brand / product tokens may be packaging-only and absent from spoken SRT.
# Platform packaging tokens often absent from spoken answer SRT.
# No product/org names — design-skills must stay project-neutral.
BRAND_ALLOW = {
    "youtube", "shorts", "tiktok", "instagram", "linkedin", "podcast",
}
_EN_SUFFIXES = ("ings", "ing", "tion", "tions", "ness", "ment", "ments", "ers", "er", "ied", "ies", "ed", "es", "s")


def _strip_al(token: str) -> str:
    """Strip Arabic definite article الـ / ال when present."""
    if token.startswith("الـ") and len(token) > 4:
        return token[3:]
    if token.startswith("ال") and len(token) > 3:
        return token[2:]
    return token


def _en_stems(token: str) -> set[str]:
    """Cheap English stem variants (no external stemmer)."""
    t = token.casefold()
    out = {t}
    for suf in _EN_SUFFIXES:
        if len(t) > len(suf) + 2 and t.endswith(suf):
            stem = t[: -len(suf)]
            out.add(stem)
            # testing → test (drop trailing t from 'ting' after removing 'ing' already handled)
            if suf == "ing" and stem.endswith("t") and len(stem) > 3:
                out.add(stem)  # testing→test already via ing on "testing" → "test"
            if suf == "ies":
                out.add(stem + "y")
            if suf == "ied":
                out.add(stem + "y")
    return out


def _variants(token: str) -> set[str]:
    raw = _strip_al(token)
    variants = {token.casefold(), raw.casefold()}
    for base in list(variants):
        variants |= _en_stems(base)
        variants.add(_strip_al(base).casefold())
    return {v for v in variants if len(v) >= 2}


def _body_forms(srt_text: str) -> set[str]:
    forms: set[str] = set()
    for tok in TOKEN.findall(srt_text):
        forms |= _variants(tok)
        forms.add(tok.casefold())
    return forms


def title_scope_hits(title: str, srt_text: str) -> list[str]:
    tokens = [t for t in TOKEN.findall(title) if t]
    scope = [t for t in tokens if t.casefold() not in STOP]
    body = _body_forms(srt_text)
    body_fold = srt_text.casefold()
    hits: list[str] = []
    for tok in scope:
        if tok.casefold() in BRAND_ALLOW:
            continue
        variants = _variants(tok)
        if variants & body:
            continue
        # exact substring fallback (unchanged happy path)
        if tok.casefold() in body_fold or tok in srt_text:
            continue
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
