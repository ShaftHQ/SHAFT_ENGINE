"""L71: reference-conditioned avatar likeness; glasses + age within refs."""

from __future__ import annotations

from typing import Any

_BANNED = frozenset({
    "insightface", "gfpgan", "codeformer", "latentsync", "syncnet",
})


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    if obs.get("reference_conditioned") is not True:
        hits.append("not-reference-conditioned")
    if obs.get("from_scratch") is True:
        hits.append("from-scratch-generation")
    if obs.get("glasses_present") is not True:
        hits.append("glasses-missing")
    if obs.get("age_within_refs") is not True:
        hits.append("age-outside-refs")
    if obs.get("identity_score_pass") is not True:
        hits.append("identity-score-fail")
    if obs.get("fresh_side_by_side_review") is not True:
        hits.append("missing-fresh-side-by-side")
    if obs.get("doubt") is True:
        hits.append("reviewer-doubt")
    stack = str(obs.get("stack") or obs.get("tools") or "").casefold()
    for banned in _BANNED:
        if banned in stack:
            hits.append(f"banned-stack:{banned}")
    return {"ok": not hits, "hits": hits}
