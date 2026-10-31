"""L70: no live/external publish without Coach acceptance PASS."""

from __future__ import annotations

from typing import Any

# Explicitly internal surfaces may skip Coach when live_publish is not True.
_INTERNAL = frozenset({
    "internal", "draft", "scratch", "private-note", "private", "local",
})


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    surface = str(
        obs.get("surface") or obs.get("publish_surface") or obs.get("channel") or ""
    ).strip().casefold()
    # Fail-closed default: any named external/unknown surface requires Coach
    # unless explicitly marked internal (live_publish is False + internal surface,
    # or live_publish is False with empty surface).
    explicit_non_live = obs.get("live_publish") is False
    if explicit_non_live and (not surface or surface in _INTERNAL):
        return {"ok": True, "hits": [], "skipped": True}
    if not surface and obs.get("live_publish") is not True:
        # No surface and no live flag → nothing to gate.
        return {"ok": True, "hits": [], "skipped": True}
    # linkedin/blog/x/twitter/facebook/instagram/web/... all require Coach.
    if obs.get("coach_gate_pass") is not True and obs.get("coach_accepted") is not True:
        hits.append("missing-coach-gate-pass")
    if obs.get("identity_surface") is True:
        title = str(obs.get("owner_title") or "")
        if title and title != "Founder and CEO":
            hits.append("owner-title-mismatch")
        if "Co-Founder" in title or title == "Founder":
            hits.append("owner-title-mismatch")
    return {"ok": not hits, "hits": hits}
