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

# Public founding year (portable). Wrong years in asserted copy FAIL; superseded denylist.
FOUNDED_YEAR_PUBLIC = 2016
SUPERSEDED_FOUNDED_YEARS = frozenset({"2015", "2014", "2017", "2018"})

_LEGACY_FOUNDED = re.compile(
    r"(?<!\d)(2014|2015|2017|2018)(?!\d)",
)


def founded_year_hits(text: str) -> list[str]:
    """Return hits when copy asserts a superseded founding year as current."""
    hits: list[str] = []
    if not text:
        return hits
    lowered = text.casefold()
    for m in _LEGACY_FOUNDED.finditer(text):
        year = m.group(1)
        # allow historical notes
        window = text[max(0, m.start() - 40) : m.end() + 40].casefold()
        if "supersed" in window or "historical" in window or "was " + year in window:
            continue
        if "found" in window or "establish" in window or "since" in window:
            hits.append(f"copy-claims-superseded-founded-year:{year}")
    # explicit wrong constant when asserting founded year in prose as FOUNDED_YEAR_PUBLIC peer
    if str(FOUNDED_YEAR_PUBLIC) not in text and "founded" in lowered and any(
        y in text for y in SUPERSEDED_FOUNDED_YEARS
    ):
        pass  # already covered by loop
    return hits


_LEGACY_484 = re.compile(
    r"(?<!\d)484(\s*\+?\s*trainees?)?(?!\d)",
    re.IGNORECASE,
)


_PUBLIC_TRAINEE_FORMS = frozenset({TRAINEE_COUNT_PUBLIC, "600+", TRAINEE_COUNT_AR, "600+ trainees"})
_ASSERT_TRAINEE_FORMS = frozenset({TRAINEE_COUNT_PUBLIC, "600+", TRAINEE_COUNT_AR})


def _trainee_figure_hits(figure: str, text: str, obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if figure:
        if figure in SUPERSEDED_TRAINEE_COUNTS or figure == "484":
            hits.append("superseded-trainee-count-484")
        elif figure not in _PUBLIC_TRAINEE_FORMS and "484" in figure:
            hits.append("superseded-trainee-count-484")
    if text and _LEGACY_484.search(text):
        lowered = text.casefold()
        if "supersed" not in lowered and "historical" not in lowered and "was 484" not in lowered:
            hits.append("copy-claims-484-trainees")
    if obs.get("assert_public_trainee_figure") is True and figure not in _ASSERT_TRAINEE_FORMS:
        hits.append("public-trainee-figure-not-600-plus")
    return hits


def _founded_year_obs_hits(year: object, text: str, obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if year is not None:
        ys = str(year).strip()
        if ys in SUPERSEDED_FOUNDED_YEARS:
            hits.append("superseded-founded-year")
        elif obs.get("assert_founded_year") is True and ys != str(FOUNDED_YEAR_PUBLIC):
            hits.append("founded-year-mismatch")
    if text:
        hits.extend(founded_year_hits(text))
    return hits


def _identity_pack_hits(figure: str, year: object, obs: dict[str, Any], hits: list[str]) -> list[str]:
    extra: list[str] = []
    if obs.get("assert_identity_pack") is not True:
        return extra
    if obs.get("assert_public_trainee_figure") is not True:
        if figure not in _ASSERT_TRAINEE_FORMS and "public-trainee-figure-not-600-plus" not in hits:
            extra.append("public-trainee-figure-not-600-plus")
    if year is None and obs.get("assert_founded_year") is not False:
        extra.append("identity-pack-missing-founded-year")
    elif year is not None and str(year).strip() != str(FOUNDED_YEAR_PUBLIC):
        if "founded-year-mismatch" not in hits and "superseded-founded-year" not in hits:
            extra.append("founded-year-mismatch")
    return extra


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """FAIL when a fixture still presents 484 as the current public trainee figure."""
    if obs is None or not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"], "error": "facts receipt required"}
    text = str(obs.get("text") or obs.get("copy") or obs.get("body") or "")
    figure = str(obs.get("trainee_count") or obs.get("trainees") or "").strip()
    year = obs.get("founded_year") or obs.get("founding_year")
    hits = _trainee_figure_hits(figure, text, obs)
    hits.extend(_founded_year_obs_hits(year, text, obs))
    hits.extend(_identity_pack_hits(figure, year, obs, hits))
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "public facts gate failed",
        "trainee_count_public": TRAINEE_COUNT_PUBLIC,
        "trainee_count_ar": TRAINEE_COUNT_AR,
        "founded_year_public": FOUNDED_YEAR_PUBLIC,
    }
