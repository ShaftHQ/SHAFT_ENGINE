"""L76: Coach PASS on render at real container width before live paste."""

from __future__ import annotations

from typing import Any


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    if obs.get("coach_pass") is not True and obs.get("coach_accepted") is not True:
        hits.append("missing-coach-pass")
    preview_w = obs.get("preview_width_px") or obs.get("render_width_px")
    real_w = obs.get("real_container_width_px") or obs.get("container_width_px")
    try:
        pw = float(preview_w)
        rw = float(real_w)
    except (TypeError, ValueError):
        hits.append("missing-width-pair")
        return {"ok": False, "hits": hits}
    # Allow 1px rounding.
    if abs(pw - rw) > 1.0:
        hits.append(f"preview-width-mismatch:{pw:g}!={rw:g}")
    if obs.get("live_paste") is True and hits:
        hits.append("live-paste-without-real-width-pass")
    if obs.get("live_broke") is True:
        if obs.get("reverted_same_turn") is not True:
            hits.append("live-break-not-reverted-same-turn")
        if obs.get("reported_same_turn") is not True:
            hits.append("live-break-not-reported-same-turn")
    return {"ok": not hits, "hits": hits}
