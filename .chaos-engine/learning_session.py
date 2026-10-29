#!/usr/bin/env python3
"""Portable Learning Session finalizer (#5625); issues-first, no auto draft PRs."""

from __future__ import annotations

import argparse
import contextlib
import hashlib
import importlib.util
import json
import sys
from pathlib import Path


def _load_sibling(name: str):
    path = Path(__file__).resolve().with_name(name)
    spec = importlib.util.spec_from_file_location(f"chaos_engine_{name}", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"{name} missing from installed core")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _load_learning():
    return _load_sibling("learning.py")


def _extract_heuristics(session_id: str, limit: int = 3) -> list[dict]:
    """Pull ≤N privacy-safe heuristics from recent queue lessons (#5656)."""
    try:
        heuristics = _load_sibling("heuristics.py")
        learning = _load_learning()
    except RuntimeError:
        return []
    state = Path.cwd() / ".chaos-engine-state" / "learning"
    lessons: list[str] = []
    try:
        document = learning.queue_document(state)
    except (OSError, ValueError):
        document = {"items": []}
    items = document.get("items") if isinstance(document, dict) else []
    if isinstance(items, list):
        for item in reversed(items):
            if not isinstance(item, dict):
                continue
            lesson = item.get("lesson")
            title = item.get("title")
            if isinstance(lesson, str) and lesson.strip():
                lessons.append(lesson.strip())
            elif isinstance(title, str) and title.strip():
                lessons.append(title.strip())
            if len(lessons) >= limit:
                break
    return heuristics.extract_from_lessons(
        lessons,
        limit=limit,
        project=Path.cwd(),
        source=f"session-{session_id[:24]}",
    )



def _drain_significance(session_id: str) -> list[dict]:
    """Deferred Learning Session path for significance marks (#5658)."""
    try:
        significance = _load_sibling("significance.py")
    except RuntimeError:
        return []
    drained = significance.drain_marks(project=Path.cwd())
    return drained if isinstance(drained, list) else []


def finalize(
    session_id: str,
    *,
    disposition: str = "issues-first",
    extract_heuristics: bool = True,
) -> dict[str, object]:
    """Record a portable completion receipt. Never auto-opens draft PRs."""
    if not isinstance(session_id, str) or not session_id.strip():
        raise ValueError("session id required")
    if disposition not in {"issues-first", "no-durable", "blocked"}:
        raise ValueError("unsupported disposition")
    learning = _load_learning()
    receipt = {
        "schemaVersion": 1,
        "kind": "learning-session-portable-finalize",
        "sessionId": session_id.strip()[:128],
        "disposition": disposition,
        "draftPrs": False,
        "policy": "issues-first",
        "privacyModule": "learning.py",
    }
    if hasattr(learning, "PRIVACY_SCHEMA_VERSION") or hasattr(learning, "queue_learning"):
        receipt["privacyReady"] = True
    heuristics_added: list[dict] = []
    if extract_heuristics and disposition == "issues-first":
        heuristics_added = _extract_heuristics(session_id.strip())
        receipt["heuristicsExtracted"] = len(heuristics_added)
    significance_drained = _drain_significance(session_id.strip())
    receipt["significanceDrained"] = len(significance_drained)
    if significance_drained:
        receipt["significanceKinds"] = sorted(
            {
                str(item.get("kind"))
                for item in significance_drained
                if isinstance(item, dict) and item.get("kind")
            }
        )
    state = Path.cwd() / ".chaos-engine-state" / "learning-session"
    state.mkdir(parents=True, exist_ok=True)
    out = state / f"{session_id.strip()[:64]}.completion.json"
    out.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    receipt["path"] = str(out)
    digest = hashlib.sha256(
        json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()[:16]
    receipt["digest"] = digest
    with contextlib.suppress(Exception):
        counters = _load_sibling("learning_counters.py")
        counters.record_learning_session_digest(digest, project=Path.cwd())
    return receipt


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    final = sub.add_parser("finalize")
    final.add_argument("--session-id", required=True)
    final.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    final.add_argument(
        "--silent",
        action="store_true",
        help="silent-on-success / errors-only (#5654)",
    )
    final.add_argument(
        "--no-heuristics",
        action="store_true",
        help="skip ERL heuristic extract (#5656)",
    )
    runtime = sub.add_parser("finalize-runtime")
    runtime.add_argument("--session-id", required=True)
    runtime.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    runtime.add_argument("--silent", action="store_true")
    runtime.add_argument("--no-heuristics", action="store_true")
    args = parser.parse_args(argv)
    try:
        result = finalize(
            args.session_id,
            disposition=args.disposition,
            extract_heuristics=not args.no_heuristics,
        )
    except (OSError, RuntimeError, ValueError) as error:
        if getattr(args, "silent", False):
            print(str(error), file=sys.stderr)
            return 2
        print(str(error), file=sys.stderr)
        return 1
    if getattr(args, "silent", False):
        return 0
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
