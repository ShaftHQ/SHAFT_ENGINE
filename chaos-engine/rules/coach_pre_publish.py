"""L70: no live publish without Coach acceptance PASS."""

from __future__ import annotations

from typing import Any

_LIVE = frozenset({
    "web", "social", "youtube", "yt", "video", "release-notes", "guide",
    "public-issue", "post", "metadata",
})


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    surface = str(obs.get("surface") or obs.get("publish_surface") or "").casefold()
    live = obs.get("live_publish") is True or surface in _LIVE
    if not live:
        return {"ok": True, "hits": [], "skipped": True}
    if obs.get("coach_gate_pass") is not True and obs.get("coach_accepted") is not True:
        hits.append("missing-coach-gate-pass")
    # Owner title fact when identity surfaces apply.
    if obs.get("identity_surface") is True:
        title = str(obs.get("owner_title") or "")
        if title and title != "Founder and CEO":
            hits.append("owner-title-mismatch")
        if "Co-Founder" in title or title == "Founder":
            hits.append("owner-title-mismatch")
    return {"ok": not hits, "hits": hits}
