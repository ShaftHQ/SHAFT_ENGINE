"""L72: after two full-regen failures, require localized masked edit / cards."""

from __future__ import annotations

from typing import Any


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    try:
        failures = int(obs.get("full_regen_failures") or obs.get("full_regen_fail_count") or 0)
    except (TypeError, ValueError):
        hits.append("bad-full-regen-failures")
        failures = 0
    localized = (
        obs.get("localized_masked_edit") is True
        or obs.get("cards_composite") is True
        or obs.get("masked_edit") is True
    )
    if failures >= 2 and not localized:
        hits.append("need-localized-masked-edit-after-cap-2")
    if failures >= 2:
        if not str(obs.get("prior_candidate_id") or obs.get("last_accepted_id") or "").strip():
            hits.append("missing-prior-candidate-id")
        protected = obs.get("protected_metrics") or obs.get("keep_metrics")
        if not protected:
            hits.append("missing-protected-metrics")
    return {"ok": not hits, "hits": hits}
