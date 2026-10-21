#!/usr/bin/env python3
"""MemPalace privacy check (#7242 chunk 6): mined drawers must contain no private data.

Generic patterns (emails, money amounts) plus workspace-private terms from
`.chaos-engine-state/privacy-terms.txt` (one case-insensitive term per line; never
tracked; matched as whole words). Prints counts and drawer ids only, never the matched text. Stdlib only.

    python3 .chaos-engine/mempalace_privacy.py --project .
"""

from __future__ import annotations

import argparse
import importlib.util
import re
import sqlite3
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
TERMS = ".chaos-engine-state/privacy-terms.txt"
ALLOW = ".chaos-engine-state/privacy-allow.txt"
GENERIC = {
    "email": re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"),
    "money": re.compile(
        r"(?:\b(?:EGP|SAR|USD|EUR|AED|LE)\s?\d[\d,.]*|\d[\d,.]*\s?(?:EGP|SAR|USD|EUR|AED|LE|جنيه|ريال)\b|[$€£]\s?\d[\d,.]*)",
        re.IGNORECASE,
    ),
}


def _lines(path: Path) -> list[str]:
    if not path.is_file():
        return []
    return [line.strip() for line in path.read_text(encoding="utf-8").splitlines()
            if line.strip() and not line.lstrip().startswith("#")]


def patterns(project: Path) -> dict[str, re.Pattern[str]]:
    found = dict(GENERIC)
    for number, term in enumerate(_lines(project / TERMS), 1):
        found[f"term-{number}"] = re.compile(rf"(?<!\w){re.escape(term)}(?!\w)", re.IGNORECASE)
    return found


def scan(database: Path, project: Path) -> dict[str, list[str]]:
    """Return {pattern name: [drawer ids]} for every hit not on the allow list."""
    allow = [item.casefold() for item in _lines(project / ALLOW)]
    compiled = patterns(project)
    hits: dict[str, list[str]] = {}
    connection = sqlite3.connect(f"file:{database.as_posix()}?mode=ro", uri=True)
    try:
        for drawer, text in connection.execute("select id, document from documents"):
            text = str(text or "")
            for name, pattern in compiled.items():
                for match in pattern.finditer(text):
                    if match.group(0).casefold() not in allow:
                        hits.setdefault(name, []).append(str(drawer))
                        break
    finally:
        connection.close()
    return hits


def palace_database(project: Path) -> Path:
    spec = importlib.util.spec_from_file_location("_ce_stores_privacy", HERE / "stores.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.resolve_palace(project) / "sqlite_exact.sqlite3"


def status(project: Path) -> tuple[str, str]:
    """(`pass`|`fail`|`none`, detail). `none`: no palace yet (fresh install)."""
    database = palace_database(project)
    if not database.is_file():
        return "none", "no palace"
    try:
        hits = scan(database, project)
    except sqlite3.Error as error:
        return "fail", f"unreadable palace ({type(error).__name__})"
    if not hits:
        return "pass", "0 hits"
    return "fail", ", ".join(f"{name}={len(ids)}" for name, ids in sorted(hits.items()))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--project", default=".")
    args = parser.parse_args(argv)
    verdict, detail = status(Path(args.project).resolve())
    print(f"mempalace privacy: {verdict} ({detail})")
    return 1 if verdict == "fail" else 0


if __name__ == "__main__":
    sys.exit(main())
