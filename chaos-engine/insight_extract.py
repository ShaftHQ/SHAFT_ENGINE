#!/usr/bin/env python3
"""Insight bank from success/failure pairs (#6544; ExpeL + ReasoningBank)."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "insights" / "index.json"
MAX_INSIGHTS = 32
MAX_EXPERIENCES = 64
DEFAULT_TOP = 3
MAX_TEXT = 160
MAX_TASK_KEY = 64
INITIAL_IMPORTANCE = 2
OUTCOMES = frozenset({"success", "failure"})
KINDS = frozenset({"pair", "failure-distilled", "success-chunk"})
OPERATORS = frozenset({"ADD", "EDIT", "UPVOTE", "DOWNVOTE"})

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


def _provenance():
    import importlib.util as _ilu

    path = Path(__file__).resolve().with_name("memory_provenance.py")
    spec = _ilu.spec_from_file_location("chaos_engine_memory_provenance", path)
    if spec is None or spec.loader is None:
        raise ImportError("memory_provenance.py missing")
    mod = _ilu.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _heuristics():
    import importlib.util as _ilu

    path = Path(__file__).resolve().with_name("heuristics.py")
    spec = _ilu.spec_from_file_location("chaos_engine_heuristics", path)
    if spec is None or spec.loader is None:
        raise ImportError("heuristics.py missing")
    mod = _ilu.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


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
    return {
        "schemaVersion": SCHEMA_VERSION,
        "updatedAt": 0,
        "experiences": [],
        "insights": [],
    }


def _sanitize_text(value: str) -> str:
    cleaned = re.sub(r"\s+", " ", str(value or "").strip())
    if not cleaned or len(cleaned) > MAX_TEXT:
        raise ValueError("insight text missing or oversized")
    if any(pattern.search(cleaned) for pattern in PRIVATE):
        raise ValueError("insight privacy gate rejected text")
    return cleaned


def _sanitize_task_key(value: str) -> str:
    cleaned = re.sub(r"[^a-z0-9._/-]+", "-", str(value or "").strip().casefold())
    cleaned = cleaned.strip("-./")[:MAX_TASK_KEY]
    if not cleaned:
        raise ValueError("task key required")
    return cleaned


def _importance(item: dict[str, Any]) -> int:
    value = item.get("importance", 0)
    if not isinstance(value, int) or isinstance(value, bool) or value < 0:
        return 0
    return value


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return _empty()
    if not isinstance(document, dict):
        return _empty()
    if not isinstance(document.get("insights"), list):
        document["insights"] = []
    if not isinstance(document.get("experiences"), list):
        document["experiences"] = []
    document["schemaVersion"] = SCHEMA_VERSION
    return document


def save_index(document: dict[str, Any], project: Path | None = None) -> Path:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = dict(document)
    payload["schemaVersion"] = SCHEMA_VERSION
    payload["updatedAt"] = int(time.time())
    insights = payload.get("insights")
    experiences = payload.get("experiences")
    if not isinstance(insights, list):
        insights = []
    if not isinstance(experiences, list):
        experiences = []
    payload["insights"] = insights[-MAX_INSIGHTS:]
    payload["experiences"] = experiences[-MAX_EXPERIENCES:]
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def _find_insight(document: dict[str, Any], insight_id: str) -> dict[str, Any]:
    cleaned = str(insight_id or "").strip()
    if not cleaned:
        raise ValueError("insight id required")
    for item in document.get("insights") or []:
        if isinstance(item, dict) and item.get("id") == cleaned:
            return item
    raise ValueError(f"unknown insight id: {cleaned}")


def _task_outcomes(document: dict[str, Any], task_key: str) -> set[str]:
    found: set[str] = set()
    for item in document.get("experiences") or []:
        if not isinstance(item, dict):
            continue
        if item.get("taskKey") != task_key:
            continue
        outcome = item.get("outcome")
        if outcome in OUTCOMES:
            found.add(str(outcome))
    return found


def record_experience(
    task_key: str,
    outcome: str,
    summary: str,
    *,
    origin: str = "learning-session",
    project: Path | None = None,
) -> dict[str, Any]:
    """Append one success or failure experience for a task key."""
    key = _sanitize_task_key(task_key)
    name = str(outcome or "").strip().casefold()
    if name not in OUTCOMES:
        raise ValueError("outcome must be success or failure")
    cleaned = _sanitize_text(summary)
    experience_id = hashlib.sha256(
        f"{key}|{name}|{cleaned}".encode("utf-8")
    ).hexdigest()[:24]
    document = load_index(project)
    experiences = document.setdefault("experiences", [])
    if not isinstance(experiences, list):
        experiences = []
        document["experiences"] = experiences
    for existing in experiences:
        if isinstance(existing, dict) and existing.get("id") == experience_id:
            return existing
    item = {
        "id": experience_id,
        "taskKey": key,
        "outcome": name,
        "summary": cleaned,
        "at": int(time.time()),
        **_provenance().stamp_fields(origin=origin),
    }
    experiences.append(item)
    document["experiences"] = experiences[-MAX_EXPERIENCES:]
    save_index(document, project)
    return item


def _append_insight(
    document: dict[str, Any],
    *,
    text: str,
    kind: str,
    task_key: str | None,
    origin: str,
    project: Path | None,
) -> dict[str, Any]:
    if kind not in KINDS:
        raise ValueError("invalid insight kind")
    cleaned = _sanitize_text(text)
    insight_id = hashlib.sha256(f"{kind}|{cleaned}".encode("utf-8")).hexdigest()[:24]
    insights = document.setdefault("insights", [])
    if not isinstance(insights, list):
        insights = []
        document["insights"] = insights
    for existing in insights:
        if isinstance(existing, dict) and existing.get("id") == insight_id:
            # Re-ADD of identical text is an UPVOTE (ExpeL robustness).
            existing["importance"] = _importance(existing) + 1
            existing["at"] = int(time.time())
            save_index(document, project)
            return existing
    item = {
        "id": insight_id,
        "text": cleaned,
        "kind": kind,
        "importance": INITIAL_IMPORTANCE,
        "at": int(time.time()),
        **_provenance().stamp_fields(origin=origin),
    }
    if task_key is not None:
        item["taskKey"] = task_key
    insights.append(item)
    document["insights"] = insights[-MAX_INSIGHTS:]
    save_index(document, project)
    return item


def record_pair_insight(
    task_key: str,
    text: str,
    *,
    origin: str = "learning-session",
    project: Path | None = None,
) -> dict[str, Any]:
    """ADD insight from a success/failure pair for the same task key (ExpeL)."""
    key = _sanitize_task_key(task_key)
    document = load_index(project)
    outcomes = _task_outcomes(document, key)
    if "success" not in outcomes or "failure" not in outcomes:
        raise ValueError("pair insight requires success and failure experiences")
    return _append_insight(
        document,
        text=text,
        kind="pair",
        task_key=key,
        origin=origin,
        project=project,
    )


def distill_failure(
    task_key: str,
    text: str,
    *,
    origin: str = "learning-session",
    project: Path | None = None,
) -> dict[str, Any]:
    """ADD a preventative strategy from a failure (ReasoningBank)."""
    key = _sanitize_task_key(task_key)
    document = load_index(project)
    outcomes = _task_outcomes(document, key)
    if "failure" not in outcomes:
        raise ValueError("failure distill requires a failure experience")
    return _append_insight(
        document,
        text=text,
        kind="failure-distilled",
        task_key=key,
        origin=origin,
        project=project,
    )


def record_success_chunk(
    text: str,
    *,
    origin: str = "learning-session",
    project: Path | None = None,
) -> dict[str, Any]:
    """ADD insight distilled from a chunk of successes (ExpeL success critique)."""
    document = load_index(project)
    return _append_insight(
        document,
        text=text,
        kind="success-chunk",
        task_key=None,
        origin=origin,
        project=project,
    )


def apply_operator(
    operator: str,
    *,
    insight_id: str | None = None,
    text: str | None = None,
    project: Path | None = None,
) -> dict[str, Any] | None:
    """Apply ADD/EDIT/UPVOTE/DOWNVOTE. DOWNVOTE at 0 removes the insight."""
    op = str(operator or "").strip().upper()
    if op not in OPERATORS:
        raise ValueError("operator must be ADD, EDIT, UPVOTE, or DOWNVOTE")
    document = load_index(project)
    if op == "ADD":
        if text is None:
            raise ValueError("ADD requires text")
        return _append_insight(
            document,
            text=text,
            kind="success-chunk",
            task_key=None,
            origin="learning-session",
            project=project,
        )
    item = _find_insight(document, str(insight_id or ""))
    if op == "UPVOTE":
        item["importance"] = _importance(item) + 1
        item["at"] = int(time.time())
        save_index(document, project)
        return item
    if op == "EDIT":
        if text is None:
            raise ValueError("EDIT requires text")
        item["text"] = _sanitize_text(text)
        item["importance"] = _importance(item) + 1
        item["at"] = int(time.time())
        save_index(document, project)
        return item
    # DOWNVOTE
    next_score = _importance(item) - 1
    insights = document.get("insights") or []
    if not isinstance(insights, list):
        raise ValueError("insight store corrupted")
    if next_score <= 0:
        document["insights"] = [
            entry
            for entry in insights
            if not (isinstance(entry, dict) and entry.get("id") == item.get("id"))
        ]
        save_index(document, project)
        return None
    item["importance"] = next_score
    item["at"] = int(time.time())
    save_index(document, project)
    return item


def retrieve_top(top: int = DEFAULT_TOP, project: Path | None = None) -> list[dict[str, Any]]:
    """Return ≤top retrievable insights by importance, then recency."""
    if top < 1:
        raise ValueError("top must be >= 1")
    insights = load_index(project).get("insights") or []
    if not isinstance(insights, list):
        return []
    eligible = [
        item
        for item in insights
        if isinstance(item, dict) and isinstance(item.get("text"), str)
    ]
    retrievable = _provenance().filter_retrievable(eligible)
    retrievable.sort(
        key=lambda item: (_importance(item), int(item.get("at") or 0)),
        reverse=True,
    )
    return retrievable[:top]


def promote_to_playbook(
    insight_id: str,
    *,
    project: Path | None = None,
) -> dict[str, Any]:
    """Copy a promotable insight into the heuristics playbook."""
    document = load_index(project)
    item = _find_insight(document, insight_id)
    if not _provenance().is_promotable(item):
        raise ValueError("insight not promotable")
    origin = str(item.get("origin") or "learning-session")
    return _heuristics().add_heuristic(
        str(item["text"]),
        source="insight-extract",
        origin=origin,
        project=project,
    )


def doctor_insights_summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    insights = document.get("insights") or []
    experiences = document.get("experiences") or []
    insight_list = insights if isinstance(insights, list) else []
    experience_list = experiences if isinstance(experiences, list) else []
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "insight-extract-summary",
        "insightCount": len(insight_list),
        "experienceCount": len(experience_list),
        "status": "healthy" if insight_list or experience_list else "absent",
        "updatedAt": document.get("updatedAt"),
        "importanceTotal": sum(
            _importance(item) for item in insight_list if isinstance(item, dict)
        ),
        "promotable": sum(
            1
            for item in insight_list
            if isinstance(item, dict) and _provenance().is_promotable(item)
        ),
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    experience = sub.add_parser("experience")
    experience.add_argument("--task-key", required=True)
    experience.add_argument("--outcome", required=True, choices=sorted(OUTCOMES))
    experience.add_argument("--summary", required=True)
    experience.add_argument("--origin", default="learning-session")
    experience.add_argument("--project", type=Path, default=None)

    pair = sub.add_parser("pair")
    pair.add_argument("--task-key", required=True)
    pair.add_argument("--text", required=True)
    pair.add_argument("--origin", default="learning-session")
    pair.add_argument("--project", type=Path, default=None)

    failure = sub.add_parser("distill-failure")
    failure.add_argument("--task-key", required=True)
    failure.add_argument("--text", required=True)
    failure.add_argument("--origin", default="learning-session")
    failure.add_argument("--project", type=Path, default=None)

    op = sub.add_parser("operator")
    op.add_argument("--op", required=True, choices=sorted(OPERATORS))
    op.add_argument("--id", default=None)
    op.add_argument("--text", default=None)
    op.add_argument("--project", type=Path, default=None)

    top = sub.add_parser("retrieve")
    top.add_argument("--top", type=int, default=DEFAULT_TOP)
    top.add_argument("--project", type=Path, default=None)

    promote = sub.add_parser("promote")
    promote.add_argument("--id", required=True)
    promote.add_argument("--project", type=Path, default=None)

    summary = sub.add_parser("summary")
    summary.add_argument("--project", type=Path, default=None)

    args = parser.parse_args(argv)
    try:
        if args.command == "experience":
            print(
                json.dumps(
                    record_experience(
                        args.task_key,
                        args.outcome,
                        args.summary,
                        origin=args.origin,
                        project=args.project,
                    ),
                    sort_keys=True,
                )
            )
            return 0
        if args.command == "pair":
            print(
                json.dumps(
                    record_pair_insight(
                        args.task_key,
                        args.text,
                        origin=args.origin,
                        project=args.project,
                    ),
                    sort_keys=True,
                )
            )
            return 0
        if args.command == "distill-failure":
            print(
                json.dumps(
                    distill_failure(
                        args.task_key,
                        args.text,
                        origin=args.origin,
                        project=args.project,
                    ),
                    sort_keys=True,
                )
            )
            return 0
        if args.command == "operator":
            result = apply_operator(
                args.op,
                insight_id=args.id,
                text=args.text,
                project=args.project,
            )
            print(json.dumps(result, sort_keys=True))
            return 0
        if args.command == "retrieve":
            print(json.dumps({"items": retrieve_top(args.top, args.project)}, sort_keys=True))
            return 0
        if args.command == "promote":
            print(
                json.dumps(
                    promote_to_playbook(args.id, project=args.project),
                    sort_keys=True,
                )
            )
            return 0
        print(json.dumps(doctor_insights_summary(args.project), sort_keys=True))
        return 0
    except ValueError as error:
        print(str(error), file=__import__("sys").stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
