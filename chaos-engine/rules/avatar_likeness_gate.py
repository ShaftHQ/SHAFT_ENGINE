"""L71: reference-conditioned avatar likeness; SFace thresholds + license."""

from __future__ import annotations

from typing import Any

_BANNED = frozenset({
    "insightface", "gfpgan", "codeformer", "latentsync", "syncnet",
})
_ALLOWED_LICENSES = frozenset({
    "apache", "apache-2.0", "apache2", "apache 2.0",
    "mit", "bsd", "bsd-2-clause", "bsd-3-clause", "bsd-2", "bsd-3",
})
SFACE_MEAN_MIN = 0.768
SFACE_MIN_MIN = 0.70
IDENTITY_MIN = 0.75


def _ref_hits(obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if obs.get("reference_conditioned") is not True:
        hits.append("not-reference-conditioned")
    if obs.get("from_scratch") is True:
        hits.append("from-scratch-generation")
    if obs.get("real_photo_edit") is not True and obs.get("source_is_real_photo") is not True:
        hits.append("missing-real-photo-edit")
    if not str(obs.get("source_ref_photo_path") or obs.get("ref_photo_path") or "").strip():
        hits.append("missing-source-ref-photo-path")
    if not str(obs.get("edit_provenance") or obs.get("edit_receipt") or "").strip():
        hits.append("missing-edit-provenance")
    return hits


def _likeness_hits(obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if obs.get("glasses_present") is not True:
        hits.append("glasses-missing")
    if obs.get("age_within_refs") is not True:
        hits.append("age-outside-refs")
    try:
        mean = float(obs.get("sface_mean"))
        if mean < SFACE_MEAN_MIN:
            hits.append(f"sface-mean-below-threshold:{mean}")
    except (TypeError, ValueError):
        hits.append("missing-sface-mean")
    try:
        smin = float(obs.get("sface_min"))
        if smin < SFACE_MIN_MIN:
            hits.append(f"sface-min-below-threshold:{smin}")
    except (TypeError, ValueError):
        hits.append("missing-sface-min")
    try:
        raw = obs.get("identity_score")
        if raw is None:
            raw = obs.get("identity")
        identity = float(raw)
        if identity < IDENTITY_MIN:
            hits.append(f"identity-below-threshold:{identity}")
    except (TypeError, ValueError):
        if obs.get("identity_score_pass") is not True:
            hits.append("missing-identity-score")
    if obs.get("fresh_side_by_side_review") is not True:
        hits.append("missing-fresh-side-by-side")
    if obs.get("doubt") is True:
        hits.append("reviewer-doubt")
    return hits


def _stack_hits(obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    license_raw = str(obs.get("license") or obs.get("stack_license") or "").strip().casefold()
    if not license_raw:
        hits.append("missing-stack-license")
    elif license_raw not in _ALLOWED_LICENSES:
        hits.append(f"license-not-allowed:{license_raw}")
    stack = str(obs.get("stack") or obs.get("tools") or "").casefold()
    tools_receipt = obs.get("tools_receipt") or obs.get("tool_list")
    if tools_receipt is None and not stack:
        hits.append("missing-tools-receipt")
    hay = stack
    if isinstance(tools_receipt, (list, tuple)):
        hay = hay + " " + " ".join(str(x).casefold() for x in tools_receipt)
    elif tools_receipt is not None:
        hay = hay + " " + str(tools_receipt).casefold()
    for banned in _BANNED:
        if banned in hay:
            hits.append(f"banned-stack:{banned}")
    return hits


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits = _ref_hits(obs) + _likeness_hits(obs) + _stack_hits(obs)
    return {"ok": not hits, "hits": hits}
