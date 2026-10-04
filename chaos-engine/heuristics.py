#!/usr/bin/env python3
"""ERL-style evolving playbook — retrieve once per task (#5656 / #6532)."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import time
from pathlib import Path
from typing import Any


def _provenance():
    import importlib.util as _ilu

    path = Path(__file__).resolve().with_name("memory_provenance.py")
    spec = _ilu.spec_from_file_location("chaos_engine_memory_provenance", path)
    if spec is None or spec.loader is None:
        raise ImportError("memory_provenance.py missing")
    mod = _ilu.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "heuristics" / "index.json"
MAX_STORE = 32
DEFAULT_TOP = 3
MAX_TEXT = 160
FEEDBACK_OUTCOMES = frozenset({"helpful", "harmful"})


def _net_score(item: dict[str, Any]) -> int:
    return _counter(item, "helpful") - _counter(item, "harmful")


def _counter(item: dict[str, Any], key: str) -> int:
    value = item.get(key, 0)
    if not isinstance(value, int) or isinstance(value, bool) or value < 0:
        return 0
    return value


def _find_item(document: dict[str, Any], item_id: str) -> dict[str, Any]:
    cleaned = str(item_id or "").strip()
    if not cleaned:
        raise ValueError("item id required")
    items = document.get("items") or []
    if not isinstance(items, list):
        raise ValueError("heuristic store corrupted")
    for item in items:
        if isinstance(item, dict) and item.get("id") == cleaned:
            return item
    raise ValueError(f"unknown heuristic id: {cleaned}")

# Privacy: reject secrets, paths, URLs, transcripts (aligned with learning.py intent).
PRIVATE = (
    re.compile(r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)[A-Za-z0-9_:=./+\-]{8,}"),
    re.compile(r"(?i)(?:[A-Z]:\\|/(?:home|users|root|private|opt)/)"),
    re.compile(r"(?i)https?://"),
    re.compile(r"(?i)\b(?:raw\s+)?(?:system\s+)?prompt\b|\btranscript\b|\btraceback\b"),
    re.compile(r"`"),
    re.compile(r"\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b", re.IGNORECASE),
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
    return {"schemaVersion": SCHEMA_VERSION, "updatedAt": 0, "items": []}


def _sanitize_text(value: str) -> str:
    cleaned = re.sub(r"\s+", " ", str(value or "").strip())
    if not cleaned or len(cleaned) > MAX_TEXT:
        raise ValueError("heuristic text missing or oversized")
    if any(pattern.search(cleaned) for pattern in PRIVATE):
        raise ValueError("heuristic privacy gate rejected text")
    return cleaned


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return _empty()
    if not isinstance(document, dict) or not isinstance(document.get("items"), list):
        return _empty()
    document["schemaVersion"] = SCHEMA_VERSION
    return document


def save_index(document: dict[str, Any], project: Path | None = None) -> Path:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = dict(document)
    payload["schemaVersion"] = SCHEMA_VERSION
    payload["updatedAt"] = int(time.time())
    items = payload.get("items")
    if not isinstance(items, list):
        items = []
    payload["items"] = items[-MAX_STORE:]
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def add_heuristic(
    text: str,
    *,
    source: str = "learning-session",
    origin: str | None = None,
    project: Path | None = None,
) -> dict[str, Any]:
    """Append one privacy-safe heuristic; dedupe by content hash."""
    cleaned = _sanitize_text(text)
    source_name = re.sub(r"[^a-z0-9_-]+", "-", str(source).strip().casefold())[:32] or "unknown"
    origin_name = origin if origin is not None else source_name
    item_id = hashlib.sha256(cleaned.encode("utf-8")).hexdigest()[:24]
    document = load_index(project)
    items = document.setdefault("items", [])
    if not isinstance(items, list):
        items = []
        document["items"] = items
    for existing in items:
        if isinstance(existing, dict) and existing.get("id") == item_id:
            return existing
    item = {
        "id": item_id,
        "text": cleaned,
        "source": source_name,
        "at": int(time.time()),
        "helpful": 0,
        "harmful": 0,
        **_provenance().stamp_fields(origin=origin_name),
    }
    items.append(item)
    document["items"] = items[-MAX_STORE:]
    save_index(document, project)
    return item


def extract_from_lessons(
    lessons: list[str],
    *,
    limit: int = DEFAULT_TOP,
    project: Path | None = None,
    source: str = "learning-session",
    origin: str | None = None,
) -> list[dict[str, Any]]:
    """Extract ≤N privacy-safe heuristics from lesson strings (Stop/Learning Session)."""
    if limit < 1:
        raise ValueError("limit must be >= 1")
    added: list[dict[str, Any]] = []
    for lesson in lessons:
        if len(added) >= limit:
            break
        try:
            added.append(
                add_heuristic(lesson, source=source, origin=origin, project=project)
            )
        except ValueError:
            continue
    return added



def record_feedback(
    item_id: str,
    outcome: str,
    *,
    project: Path | None = None,
) -> dict[str, Any]:
    """Increment helpful or harmful on one playbook item (#6532)."""
    name = str(outcome or "").strip().casefold()
    if name not in FEEDBACK_OUTCOMES:
        raise ValueError("outcome must be helpful or harmful")
    document = load_index(project)
    item = _find_item(document, item_id)
    item[name] = _counter(item, name) + 1
    # Normalize the sibling counter so legacy items persist both keys.
    item["helpful"] = _counter(item, "helpful")
    item["harmful"] = _counter(item, "harmful")
    save_index(document, project)
    return item


def apply_delta(
    item_id: str,
    *,
    text: str,
    project: Path | None = None,
) -> dict[str, Any]:
    """Replace playbook item text in place; keep id, counters, and provenance (#6532)."""
    cleaned = _sanitize_text(text)
    document = load_index(project)
    item = _find_item(document, item_id)
    item["text"] = cleaned
    item["at"] = int(time.time())
    item["helpful"] = _counter(item, "helpful")
    item["harmful"] = _counter(item, "harmful")
    save_index(document, project)
    return item


def retrieve_top(top: int = DEFAULT_TOP, project: Path | None = None) -> list[dict[str, Any]]:
    """Return ≤top retrievable playbook items by net score, then recency (#6520 / #6532)."""
    if top < 1:
        raise ValueError("top must be >= 1")
    items = load_index(project).get("items") or []
    if not isinstance(items, list):
        return []
    eligible = [
        item
        for item in items
        if isinstance(item, dict) and isinstance(item.get("text"), str)
    ]
    retrievable = _provenance().filter_retrievable(eligible)
    retrievable.sort(
        key=lambda item: (_net_score(item), int(item.get("at") or 0)),
        reverse=True,
    )
    return retrievable[:top]



def session_start_locator(project: Path | None = None) -> str:
    """Locator only — never inject heuristic prose into SessionStart."""
    items = load_index(project).get("items") or []
    count = len(items) if isinstance(items, list) else 0
    if count <= 0:
        return "Heuristics: `.chaos-engine-state/heuristics/` (empty; retrieve once/task)."
    return (
        f"Heuristics: `.chaos-engine-state/heuristics/` ({count} stored; "
        "retrieve once/task via `retrieve.py heuristics --top 3`; no prose dump)."
    )


def doctor_heuristics_summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    items = document.get("items") or []
    count = len(items) if isinstance(items, list) else 0
    provenance = _provenance().doctor_provenance_summary(project) if count else {
        "schemaVersion": 1,
        "kind": "memory-provenance-summary",
        "total": 0,
        "trusted": 0,
        "verified": 0,
        "quarantined": 0,
        "status": "absent",
        "store": "heuristics",
    }
    item_list = items if isinstance(items, list) else []
    return {
        "schemaVersion": SCHEMA_VERSION,
        "count": count,
        "status": "healthy" if count else "absent",
        "updatedAt": document.get("updatedAt"),
        "provenance": provenance,
        "promotable": sum(
            1 for item in item_list if isinstance(item, dict) and _provenance().is_promotable(item)
        ),
        "helpfulTotal": sum(_counter(item, "helpful") for item in item_list if isinstance(item, dict)),
        "harmfulTotal": sum(_counter(item, "harmful") for item in item_list if isinstance(item, dict)),
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    add = sub.add_parser("add")
    add.add_argument("--text", required=True)
    add.add_argument("--source", default="learning-session")
    add.add_argument("--origin", default=None)
    add.add_argument("--project", type=Path, default=None)
    top = sub.add_parser("retrieve")
    top.add_argument("--top", type=int, default=DEFAULT_TOP)
    top.add_argument("--project", type=Path, default=None)
    loc = sub.add_parser("locator")
    loc.add_argument("--project", type=Path, default=None)
    summary = sub.add_parser("summary")
    summary.add_argument("--project", type=Path, default=None)
    feedback = sub.add_parser("feedback")
    feedback.add_argument("--id", required=True)
    feedback.add_argument("--outcome", required=True, choices=sorted(FEEDBACK_OUTCOMES))
    feedback.add_argument("--project", type=Path, default=None)
    delta = sub.add_parser("delta")
    delta.add_argument("--id", required=True)
    delta.add_argument("--text", required=True)
    delta.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    try:
        if args.command == "add":
            print(json.dumps(
                add_heuristic(
                    args.text,
                    source=args.source,
                    origin=args.origin,
                    project=args.project,
                ),
                sort_keys=True,
            ))
            return 0
        if args.command == "retrieve":
            print(json.dumps({"items": retrieve_top(args.top, args.project)}, sort_keys=True))
            return 0
        if args.command == "locator":
            print(session_start_locator(args.project))
            return 0
        if args.command == "feedback":
            print(json.dumps(
                record_feedback(args.id, args.outcome, project=args.project),
                sort_keys=True,
            ))
            return 0
        if args.command == "delta":
            print(json.dumps(
                apply_delta(args.id, text=args.text, project=args.project),
                sort_keys=True,
            ))
            return 0
        print(json.dumps(doctor_heuristics_summary(args.project), sort_keys=True))
        return 0
    except ValueError as error:
        print(str(error), file=__import__("sys").stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
