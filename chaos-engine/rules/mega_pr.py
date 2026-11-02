"""L97 MEGA-PR: group related tickets into the fewest PRs."""

from __future__ import annotations

import re

ONE_PER_TICKET = re.compile(
    r"one\s*pr\s*per\s*(ticket|issue|item|bug)\b",
    re.I,
)
EPIC_OK = re.compile(
    r"(mega\s*pr|fewest\s*prs|one\s*pr\s*per\s*epic|one\s*pr\s*for\s*all|"
    r"coverage\s*epic|intellij\s*revamp|ce\s*mega|batch\s*pr)",
    re.I,
)


def mega_pr_hits(plan: str) -> list[str]:
    """Return hits when a plan insists on one-PR-per-ticket without epic grouping."""
    text = plan or ""
    if not ONE_PER_TICKET.search(text):
        return []
    if EPIC_OK.search(text):
        # explicit mega-PR framing overrides a quoted anti-pattern
        return []
    return ["one-pr-per-ticket"]


def evaluate(plan: str) -> dict:
    hits = mega_pr_hits(plan)
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L97 MEGA-PR failed",
    }
