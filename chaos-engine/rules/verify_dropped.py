"""
L86 VERIFY-DROPPED: refuse silent redos without a live-state receipt.
"""

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
DROPPED = re.compile(r"\bdropped\b|VERIFY-DROPPED", re.I)
BOARD_OWNER = re.compile(
    r"board owner|board_owner|owner of (the )?board|board'?s owner",
    re.I,
)


def verify_dropped_hits(brief: str) -> list[str]:
    """Return hit codes when a redo/dropped brief lacks required receipts."""
    text = brief or ""
    hits: list[str] = []
    if REDO.search(text) and not LIVE_MARKERS.search(text):
        hits.append("missing-live-state-receipt")
    if DROPPED.search(text) and not BOARD_OWNER.search(text):
        hits.append("missing-board-owner-check")
    return hits


def evaluate(brief: str) -> dict:
    hits = verify_dropped_hits(brief)
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L86 VERIFY-DROPPED failed",
    }


def require_receipt(brief: str, evidence_urls: Iterable[str] | None = None) -> dict:
    urls = [u for u in (evidence_urls or []) if u]
    if urls and BOARD_OWNER.search(brief or ""):
        return {"ok": True, "hits": [], "error": None, "urls": list(urls)}
    if urls and not DROPPED.search(brief or ""):
        return {"ok": True, "hits": [], "error": None, "urls": list(urls)}
    return evaluate(brief)
