"""Portable public facts enforced in CE fixtures (no PII).

owner 2026-10-31: trainee count is ``600+ trainees`` / ``أكتر من 600 متدرب``.
Supersedes 484 everywhere in CE facts/fixtures.
"""
from __future__ import annotations

import re
from typing import Any

TRAINEE_COUNT_PUBLIC = "600+ trainees"
TRAINEE_COUNT_AR = "أكتر من 600 متدرب"
SUPERSEDED_TRAINEE_COUNTS = frozenset({"484", "484 trainees", "٤٨٤"})

_LEGACY_484 = re.compile(
    r"(?<!\d)484(\s*\+?\s*trainees?)?(?!\d)",
    re.IGNORECASE,
)


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """FAIL when a fixture still presents 484 as the current public trainee figure."""
    if obs is None or not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"], "error": "facts receipt required"}
    hits: list[str] = []
    text = str(obs.get("text") or obs.get("copy") or obs.get("body") or "")
    figure = str(obs.get("trainee_count") or obs.get("trainees") or "").strip()
    if figure:
        if figure in SUPERSEDED_TRAINEE_COUNTS or figure == "484":
            hits.append("superseded-trainee-count-484")
        elif figure not in {TRAINEE_COUNT_PUBLIC, "600+", TRAINEE_COUNT_AR, "600+ trainees"}:
            # allow explicit 600+ forms only when asserting the public figure
            if "484" in figure:
                hits.append("superseded-trainee-count-484")
    if text and _LEGACY_484.search(text):
        # allow historical notes that mark 484 as superseded
        lowered = text.casefold()
        if "supersed" not in lowered and "historical" not in lowered and "was 484" not in lowered:
            hits.append("copy-claims-484-trainees")
    if obs.get("assert_public_trainee_figure") is True:
        if figure not in {TRAINEE_COUNT_PUBLIC, "600+", TRAINEE_COUNT_AR}:
            hits.append("public-trainee-figure-not-600-plus")
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "public facts gate failed",
        "trainee_count_public": TRAINEE_COUNT_PUBLIC,
        "trainee_count_ar": TRAINEE_COUNT_AR,
    }
