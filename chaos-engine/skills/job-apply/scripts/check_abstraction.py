#!/usr/bin/env python3
"""Fail if the job-apply skill tree contains personal-data patterns.

Owner/brand literals stay out of this portable file (distribution policy).
Those are enforced by tests.scripts.test_job_apply_skill and optional
CE_NEUTRAL_TERMS_FILE / ~/.config/chaos-engine/neutral-terms.txt.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
EMAIL = re.compile(r"(?<![<\w])[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b")
PHONE = re.compile(r"(?:\+|00)?\d[\d().\s-]{7,}\d")
YEARS = re.compile(r"\b(I have|I've)\s+\d+\+?\s+(years?|yrs)\b", re.I)


def _content_files() -> list[Path]:
    skip = {Path(__file__).resolve(), (Path(__file__).parent / "check_abstraction.sh").resolve()}
    files: list[Path] = []
    for path in sorted(ROOT.rglob("*")):
        if not path.is_file() or path.resolve() in skip:
            continue
        if path.suffix.lower() not in {".md", ".py", ".sh"}:
            continue
        files.append(path)
    return files


def _neutral_terms() -> tuple[str, ...]:
    configured = os.environ.get("CE_NEUTRAL_TERMS_FILE", "").strip()
    path = Path(configured) if configured else Path.home() / ".config/chaos-engine/neutral-terms.txt"
    if not path.is_file():
        return ()
    lines = (line.split("#", 1)[0].strip().casefold() for line in path.read_text(encoding="utf-8").splitlines())
    return tuple(line for line in lines if line)


def main() -> int:
    files = _content_files()
    failures: list[str] = []
    for path in files:
        text = path.read_text(encoding="utf-8")
        rel = path.relative_to(ROOT).as_posix()
        for label, pattern in (("email", EMAIL), ("phone", PHONE), ("applicant-stats", YEARS)):
            for match in pattern.finditer(text):
                line = text.count("\n", 0, match.start()) + 1
                failures.append(f"{rel}:{line}: {label}: {match.group(0)!r}")
        lower = text.casefold()
        for term in _neutral_terms():
            if term and term in lower:
                failures.append(f"{rel}: banned-term: {term!r}")
    if failures:
        print("check_abstraction: FAIL", file=sys.stderr)
        for row in failures:
            print(row, file=sys.stderr)
        return 1
    print(f"check_abstraction: ok ({len(files)} files)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
