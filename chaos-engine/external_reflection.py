#!/usr/bin/env python3
"""Store reflections grounded only in external signals (#6518).

Reflexion (2303.11366) and Huang et al. (2310.01798): a note is kept only when
its ground is tests, CI, or doctor. Self-report and model-only judgment are
refused. This store does not edit shipped skills.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "external-reflection" / "index.json"
SIGNALS = ("tests", "ci", "doctor")
OUTCOMES = ("pass", "fail")
MAX_SUMMARY = 160

PRIVATE = (
    re.compile(
        r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)"
        r"[A-Za-z0-9_:=./+\-]{8,}"
    ),
    re.compile(r"(?i)(?:[A-Z]:\\|/(?:home|users|root|private|opt)/)"),
    re.compile(r"(?i)https?://"),
    re.compile(r"`"),
)


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def index_path(project: Path | None = None) -> Path:
    return project_root(project) / INDEX_RELATIVE


def _empty() -> dict[str, Any]:
    return {"schemaVersion": SCHEMA_VERSION, "updatedAt": 0, "reflections": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("external reflection index schema is unsupported")
    document.setdefault("reflections", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    document["updatedAt"] = int(time.time())
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _check_summary(value: str) -> str:
    text = " ".join(value.split())
    if not text or len(text) > MAX_SUMMARY:
        raise ValueError(f"summary must be 1-{MAX_SUMMARY} characters")
    for pattern in PRIVATE:
        if pattern.search(text):
            raise ValueError("summary contains private text")
    return text


def record_reflection(
    signal: str,
    outcome: str,
    summary: str,
    *,
    project: Path | None = None,
) -> dict[str, Any]:
    """Keep one reflection. signal must be tests, ci, or doctor."""
    if signal not in SIGNALS:
        raise ValueError("reflection signal must be tests, ci, or doctor")
    if outcome not in OUTCOMES:
        raise ValueError("reflection outcome must be pass or fail")
    text = _check_summary(summary)
    item = {
        "id": hashlib.sha256(f"{signal}\n{outcome}\n{text}".encode()).hexdigest()[:16],
        "signal": signal,
        "outcome": outcome,
        "summary": text,
    }
    document = load_index(project)
    rows = document["reflections"]
    if not isinstance(rows, list):
        raise ValueError("external reflection index is invalid")
    kept = [row for row in rows if isinstance(row, dict) and row.get("id") != item["id"]]
    kept.append(item)
    document["reflections"] = kept[-128:]
    _save(document, project)
    return item


def retrieve(query: str, *, top: int = 3, project: Path | None = None) -> list[dict[str, Any]]:
    needle = _check_summary(query).casefold()
    document = load_index(project)
    matched = []
    for row in document["reflections"]:
        if not isinstance(row, dict):
            continue
        haystack = f"{row.get('signal', '')} {row.get('summary', '')}".casefold()
        if needle in haystack:
            matched.append(row)
    matched.sort(key=lambda row: (0 if row.get("outcome") == "fail" else 1, str(row.get("id"))))
    return matched[:top]


def summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    rows = [row for row in document.get("reflections") or [] if isinstance(row, dict)]
    counts = {signal: sum(1 for row in rows if row.get("signal") == signal) for signal in SIGNALS}
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "external-reflection-summary",
        "reflectionCount": len(rows),
        "bySignal": counts,
        "status": "healthy" if rows else "absent",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    record = sub.add_parser("record")
    record.add_argument("--signal", required=True, choices=SIGNALS)
    record.add_argument("--outcome", required=True, choices=OUTCOMES)
    record.add_argument("--summary", required=True)
    record.add_argument("--project", type=Path, default=None)
    show = sub.add_parser("retrieve")
    show.add_argument("--query", required=True)
    show.add_argument("--top", type=int, default=3)
    show.add_argument("--project", type=Path, default=None)
    report = sub.add_parser("summary")
    report.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "record":
        print(
            json.dumps(
                record_reflection(
                    args.signal, args.outcome, args.summary, project=args.project
                ),
                sort_keys=True,
            )
        )
        return 0
    if args.command == "retrieve":
        print(json.dumps(retrieve(args.query, top=args.top, project=args.project), sort_keys=True))
        return 0
    print(json.dumps(summary(args.project), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
