#!/usr/bin/env python3
"""Content-scan preflight helpers (G-META pattern).

Path/filename denylists alone are not enough. Callers supply tokens and roots;
this module word-boundary scans file contents and exposes a planted --self-test
that must FAIL when a banned token is introduced.

Stdlib only. No project-specific banned stacks live here — pass them in.
"""

from __future__ import annotations

import argparse
import re
import sys
import tempfile
from pathlib import Path

DEFAULT_SUFFIXES = frozenset(
    {
        ".py",
        ".pyi",
        ".md",
        ".txt",
        ".json",
        ".yml",
        ".yaml",
        ".toml",
        ".cfg",
        ".ini",
        ".sh",
        ".js",
        ".ts",
        ".tsx",
        ".jsx",
    }
)


def token_pattern(token: str) -> re.Pattern[str]:
    """Word-boundary regex for one banned token (case-insensitive)."""
    escaped = re.escape(token)
    return re.compile(rf"(?<![A-Za-z0-9_]){escaped}(?![A-Za-z0-9_])", re.IGNORECASE)


def scan_file(path: Path, tokens: list[str]) -> list[str]:
    """Return finding strings for banned tokens found in file content."""
    try:
        text = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return []
    hits: list[str] = []
    for token in tokens:
        if token_pattern(token).search(text):
            hits.append(f"content-scan: banned token {token!r} in {path}")
    return hits


def scan_roots(
    roots: list[Path],
    tokens: list[str],
    *,
    suffixes: frozenset[str] = DEFAULT_SUFFIXES,
) -> list[str]:
    """Content-scan every matching file under roots (not name-only)."""
    findings: list[str] = []
    for root in roots:
        if not root.exists():
            continue
        if root.is_file():
            findings.extend(scan_file(root, tokens))
            continue
        for path in root.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.lower() not in suffixes and path.suffix != "":
                continue
            findings.extend(scan_file(path, tokens))
    return findings


def planted_self_test(token: str = "planted_banned_token_xyz") -> int:
    """Plant a banned token file under a temp tree; expect a finding (exit 2).

    Returns 0 when the scan correctly FAILs (finding present). Returns 2 when
    the scan misses the plant (vacuous / name-only bug).
    """
    with tempfile.TemporaryDirectory(prefix="ce-content-scan-selftest-") as tmp:
        root = Path(tmp)
        planted = root / "src" / "planted_probe.py"
        planted.parent.mkdir(parents=True, exist_ok=True)
        planted.write_text(f"import {token}\n", encoding="utf-8")
        # Name-only would miss if it only checked basename denylist without content.
        findings = scan_roots([root], [token])
        if findings:
            print("content-scan --self-test: PASS (plant detected)")
            for line in findings:
                print(line)
            return 0
        print(
            "content-scan --self-test: FAIL (plant missed — name-only or broken scan)",
            file=sys.stderr,
        )
        return 2


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--self-test",
        action="store_true",
        help="plant a banned token and expect the content scan to detect it",
    )
    parser.add_argument(
        "--token",
        action="append",
        default=[],
        help="banned token to scan for (repeatable; used without --self-test)",
    )
    parser.add_argument(
        "roots",
        nargs="*",
        type=Path,
        help="directories or files to content-scan",
    )
    args = parser.parse_args(argv)
    if args.self_test:
        return planted_self_test()
    if not args.token or not args.roots:
        parser.error("provide --token and roots, or --self-test")
    findings = scan_roots(list(args.roots), list(args.token))
    for line in findings:
        print(line)
    return 2 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
