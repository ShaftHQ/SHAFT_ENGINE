"""L86 VERIFY-DROPPED: refuse silent redos without a live-state receipt."""

from __future__ import annotations

import re
from typing import Iterable

REDO = re.compile(
    r"\b(redo|re-do|reopen|re-open|retry|re-try|restart|recreate|re-create)\b",
    re.I,
)
LIVE_MARKERS = re.compile(
    r"(already exists|https?://|gh pr |gh issue |VERIFY-DROPPED|live state|verified live)",
    re.I,
)


def verify_dropped_hits(brief: str) -> list[str]:
    """Return hit codes when a redo brief lacks a live-state receipt."""
    text = brief or ""
    if not REDO.search(text):
        return []
    if LIVE_MARKERS.search(text):
        return []
    return ["missing-live-state-receipt"]


def evaluate(brief: str) -> dict:
    hits = verify_dropped_hits(brief)
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L86 VERIFY-DROPPED failed",
    }


def require_receipt(brief: str, evidence_urls: Iterable[str] | None = None) -> dict:
    urls = [u for u in (evidence_urls or []) if u]
    if urls:
        return {"ok": True, "hits": [], "error": None, "urls": list(urls)}
    return evaluate(brief)
