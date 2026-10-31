"""L66: web pre-publish bidi, WCAG AA, 390+1440, indep visual review."""

from __future__ import annotations

from typing import Any


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    if obs.get("bidi_ok") is not True:
        hits.append("bidi-not-ok")
    if obs.get("no_raw_urls_in_copy") is not True:
        hits.append("raw-urls-in-copy")
    contrast = obs.get("min_contrast_ratio")
    try:
        if float(contrast) < 4.5:
            hits.append("wcag-aa-contrast<4.5")
    except (TypeError, ValueError):
        hits.append("missing-contrast")
    if obs.get("alignment_ok") is not True:
        hits.append("alignment-not-ok")
    widths = obs.get("render_widths_px") or []
    if not isinstance(widths, list) or not ({390, 1440} <= {int(w) for w in widths if str(w).isdigit() or isinstance(w, (int, float))}):
        # also accept ints
        try:
            have = {int(w) for w in widths}
        except (TypeError, ValueError):
            have = set()
        if not ({390, 1440} <= have):
            hits.append("missing-390-or-1440-render")
    if obs.get("independent_visual_review") is not True:
        hits.append("missing-independent-visual-review")
    if obs.get("schema_only_done") is True:
        hits.append("schema-only-not-done")
    return {"ok": not hits, "hits": hits}
