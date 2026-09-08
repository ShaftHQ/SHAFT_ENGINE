#!/usr/bin/env python3
"""
Significance-filtered mid-session capture — soft marks, no Task Observer (#5658 / #4).

Writes tiny structured notes under `.chaos-engine-state/significance/` only when
friction is significant (corrections, repeated failures, missing skill coverage,
denials, doctor recovery). Soft PostToolUse path never loads self-improve refs
and never injects mid-turn prose. SessionStart stays locator-only.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
MARKS_RELATIVE = Path(".chaos-engine-state") / "significance" / "marks.json"
MAX_MARKS = 48
MAX_NOTE = 120
KINDS = frozenset(
    {
        "correction",
        "repeated-failure",
        "missing-skill",
        "denial",
        "doctor-recovery",
    }
)
# Privacy: reject secrets, paths, URLs, transcripts (aligned with heuristics/learning).
PRIVATE = (
    re.compile(
        r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)"
        r"[A-Za-z0-9_:=./+\-]{8,}"
    ),
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


def marks_path(project: Path | None = None) -> Path:
    return project_root(project) / MARKS_RELATIVE


def _empty() -> dict[str, Any]:
    return {"schemaVersion": SCHEMA_VERSION, "updatedAt": 0, "items": []}


def _sanitize_note(value: str) -> str:
    cleaned = re.sub(r"\s+", " ", str(value or "").strip())
    if not cleaned or len(cleaned) > MAX_NOTE:
        raise ValueError("significance note missing or oversized")
    if any(pattern.search(cleaned) for pattern in PRIVATE):
        raise ValueError("significance privacy gate rejected note")
    return cleaned


def _sanitize_kind(kind: str) -> str:
    cleaned = re.sub(r"[^a-z0-9-]+", "-", str(kind or "").strip().casefold()).strip("-")
    if cleaned not in KINDS:
        raise ValueError(f"unsupported significance kind: {kind}")
    return cleaned


def load_marks(project: Path | None = None) -> dict[str, Any]:
    path = marks_path(project)
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


def save_marks(document: dict[str, Any], project: Path | None = None) -> Path:
    path = marks_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = dict(document)
    payload["schemaVersion"] = SCHEMA_VERSION
    payload["updatedAt"] = int(time.time())
    items = payload.get("items")
    if not isinstance(items, list):
        items = []
    payload["items"] = items[-MAX_MARKS:]
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def is_significant(
    *,
    kind: str | None = None,
    event_name: str | None = None,
    failed: bool = False,
    denied: bool = False,
    signature: str | None = None,
    prior_signatures: set[str] | None = None,
    explicit: bool = False,
) -> bool:
    """Return True only for significant friction — never always-on observation."""
    if explicit and kind:
        try:
            _sanitize_kind(kind)
            return True
        except ValueError:
            return False
    if denied or (event_name == "PreToolUse" and denied):
        return True
    if event_name == "PostToolUseFailure" or failed:
        if signature and prior_signatures is not None and signature in prior_signatures:
            return True  # repeated-failure
        # First failure still significant (cheap mark); continuous success is not.
        return True
    if kind in KINDS:
        return True
    return False


def _prior_signatures(document: dict[str, Any]) -> set[str]:
    items = document.get("items")
    if not isinstance(items, list):
        return set()
    out: set[str] = set()
    for item in items:
        if isinstance(item, dict) and isinstance(item.get("signature"), str):
            out.add(item["signature"])
    return out


def record_mark(
    kind: str,
    note: str,
    *,
    session_id: str = "",
    signature: str = "",
    project: Path | None = None,
    source: str = "manual",
) -> dict[str, Any] | None:
    """Append one privacy-safe mark when significant; return None if filtered out."""
    try:
        kind_name = _sanitize_kind(kind)
        cleaned = _sanitize_note(note)
    except ValueError:
        return None
    document = load_marks(project)
    sig = re.sub(r"[^a-zA-Z0-9._:-]+", "-", str(signature or cleaned).strip())[:80]
    # Deduplicate identical kind+signature within store.
    items = document.setdefault("items", [])
    if not isinstance(items, list):
        items = []
        document["items"] = items
    item_id = hashlib.sha256(f"{kind_name}:{sig}:{cleaned}".encode("utf-8")).hexdigest()[:24]
    for existing in items:
        if isinstance(existing, dict) and existing.get("id") == item_id:
            return existing
    # Soft path: only persist when filter says significant.
    prior = _prior_signatures(document)
    failed = kind_name in {"repeated-failure", "denial", "doctor-recovery"}
    if not is_significant(
        kind=kind_name,
        failed=failed,
        denied=kind_name == "denial",
        signature=sig,
        prior_signatures=prior,
        explicit=True,
    ):
        return None
    if kind_name == "repeated-failure" and sig and sig not in prior:
        # First sighting of this failure signature stays "repeated-failure" kind
        # only when caller asked; still record as significant friction.
        pass
    item = {
        "id": item_id,
        "kind": kind_name,
        "note": cleaned,
        "signature": sig,
        "sessionId": re.sub(r"[^a-zA-Z0-9._:-]+", "-", str(session_id or ""))[:64],
        "source": re.sub(r"[^a-z0-9_-]+", "-", str(source).strip().casefold())[:32] or "manual",
        "createdAt": int(time.time()),
    }
    items.append(item)
    save_marks(document, project)
    return item


def list_marks(project: Path | None = None) -> list[dict[str, Any]]:
    document = load_marks(project)
    items = document.get("items")
    if not isinstance(items, list):
        return []
    return [item for item in items if isinstance(item, dict)]


def drain_marks(project: Path | None = None) -> list[dict[str, Any]]:
    """Return pending marks and clear the store (Learning Session deferred path)."""
    document = load_marks(project)
    items = list_marks(project)
    document["items"] = []
    save_marks(document, project)
    return items


def soft_post_tool_capture(
    event: dict[str, Any] | None,
    *,
    event_name: str = "",
    failed: bool = False,
    denied: bool = False,
    tool_name: str = "",
    project: Path | None = None,
) -> dict[str, Any] | None:
    """
    Ultra-cheap soft PostToolUse/deny path — state file only, no refs load.

    Default: only significant fail/deny. Opt-in broader capture via
    CHAOS_ENGINE_SIGNIFICANCE_CAPTURE=1 still requires the significance filter
    (never continuous observation of every edit).
    """
    flag = str(os.environ.get("CHAOS_ENGINE_SIGNIFICANCE_CAPTURE") or "").strip().casefold()
    env_broad = flag in {"1", "true", "yes", "on"}
    if not failed and not denied and not env_broad:
        return None
    if not failed and not denied:
        # Broad mode still rejects non-significant happy-path noise.
        return None
    name = event_name or (
        str((event or {}).get("hook_event_name") or (event or {}).get("hookEventName") or "")
    )
    tool = tool_name or str((event or {}).get("tool_name") or (event or {}).get("toolName") or "")
    session_id = str((event or {}).get("session_id") or (event or {}).get("sessionId") or "")
    signature = f"{tool}:{name}"[:80]
    document = load_marks(project)
    prior = _prior_signatures(document)
    kind = "denial" if denied else (
        "repeated-failure" if signature in prior else "repeated-failure"
    )
    # First failure uses repeated-failure kind with a distinct first-fail note;
    # second+ same signature stays repeated-failure (filter still true either way).
    note = (
        f"deny {tool or 'tool'}"[:MAX_NOTE]
        if denied
        else f"fail {tool or 'tool'} ({name or 'PostToolUse'})"[:MAX_NOTE]
    )
    if not is_significant(
        kind=kind,
        event_name=name,
        failed=failed,
        denied=denied,
        signature=signature,
        prior_signatures=prior,
    ):
        return None
    return record_mark(
        kind,
        note,
        session_id=session_id,
        signature=signature,
        project=project,
        source="soft-post-tool",
    )


def session_start_locator(project: Path | None = None) -> str:
    """Locator only — never inject mark prose into SessionStart."""
    count = len(list_marks(project))
    return (
        "Significance: `.chaos-engine-state/significance/` "
        f"(pending={count}; soft fail/deny marks only; no Observer)."
    )


def doctor_significance_summary(project: Path | None = None) -> dict[str, Any]:
    items = list_marks(project)
    by_kind: dict[str, int] = {}
    for item in items:
        kind = str(item.get("kind") or "unknown")
        by_kind[kind] = by_kind.get(kind, 0) + 1
    return {
        "kind": "significance-summary",
        "pending": len(items),
        "byKind": by_kind,
        "policy": "significance-filtered; no Task Observer",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    mark = sub.add_parser("mark", help="explicit significant friction mark")
    mark.add_argument("--kind", required=True, choices=sorted(KINDS))
    mark.add_argument("--note", required=True)
    mark.add_argument("--session-id", default="")
    mark.add_argument("--signature", default="")
    mark.add_argument("--project", type=Path, default=None)
    listed = sub.add_parser("list")
    listed.add_argument("--project", type=Path, default=None)
    drain = sub.add_parser("drain", help="Learning Session deferred drain")
    drain.add_argument("--project", type=Path, default=None)
    loc = sub.add_parser("locator")
    loc.add_argument("--project", type=Path, default=None)
    summary = sub.add_parser("summary")
    summary.add_argument("--project", type=Path, default=None)
    filt = sub.add_parser("filter-check", help="zero-LLM significance predicate")
    filt.add_argument("--kind", default="")
    filt.add_argument("--failed", action="store_true")
    filt.add_argument("--denied", action="store_true")
    filt.add_argument("--event-name", default="")
    filt.add_argument("--explicit", action="store_true")
    args = parser.parse_args(argv)
    if args.command == "mark":
        item = record_mark(
            args.kind,
            args.note,
            session_id=args.session_id,
            signature=args.signature,
            project=args.project,
            source="cli",
        )
        print(json.dumps(item or {"status": "filtered"}, indent=2, sort_keys=True))
        return 0 if item else 1
    if args.command == "list":
        print(json.dumps({"items": list_marks(args.project)}, indent=2, sort_keys=True))
        return 0
    if args.command == "drain":
        print(json.dumps({"drained": drain_marks(args.project)}, indent=2, sort_keys=True))
        return 0
    if args.command == "locator":
        print(session_start_locator(args.project))
        return 0
    if args.command == "summary":
        print(json.dumps(doctor_significance_summary(args.project), indent=2, sort_keys=True))
        return 0
    if args.command == "filter-check":
        ok = is_significant(
            kind=args.kind or None,
            event_name=args.event_name or None,
            failed=args.failed,
            denied=args.denied,
            explicit=args.explicit,
        )
        print(json.dumps({"significant": ok}, indent=2, sort_keys=True))
        return 0 if ok else 1
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
