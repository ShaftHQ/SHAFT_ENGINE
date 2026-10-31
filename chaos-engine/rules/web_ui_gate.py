"""L66: web pre-publish — measured overflow/clip + bidi/WCAG/390+1440."""

from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any


def _bbox(obj: Any) -> dict[str, float] | None:
    if not isinstance(obj, dict):
        return None
    try:
        return {
            "x": float(obj.get("x", 0)),
            "y": float(obj.get("y", 0)),
            "w": float(obj.get("w") if obj.get("w") is not None else obj.get("width", 0)),
            "h": float(obj.get("h") if obj.get("h") is not None else obj.get("height", 0)),
        }
    except (TypeError, ValueError):
        return None


def _inside(inner: dict[str, float], outer: dict[str, float], *, tol: float = 1.0) -> bool:
    return (
        inner["x"] >= outer["x"] - tol
        and inner["y"] >= outer["y"] - tol
        and inner["x"] + inner["w"] <= outer["x"] + outer["w"] + tol
        and inner["y"] + inner["h"] <= outer["y"] + outer["h"] + tol
    )


def _element_scroll_hits(index: int, el: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    try:
        sw = el.get("scrollWidth")
        cw = el.get("clientWidth")
        if sw is not None and cw is not None and float(sw) > float(cw) + 1.0:
            hits.append(f"elements[{index}]:overflow-scrollwidth")
    except (TypeError, ValueError):
        hits.append(f"elements[{index}]:bad-scroll-metrics")
    return hits


def _element_bbox_hits(
    index: int,
    el: dict[str, Any],
    *,
    viewport: dict[str, float] | None,
    container: dict[str, float] | None,
) -> list[str]:
    hits: list[str] = []
    box = _bbox(el.get("bbox") or el)
    if box is None:
        return [f"elements[{index}]:missing-bbox"]
    if viewport is not None and not _inside(box, viewport):
        hits.append(f"elements[{index}]:bbox-outside-viewport")
    if container is not None and not _inside(box, container):
        hits.append(f"elements[{index}]:bbox-outside-container")
    ancestors = el.get("clipping_ancestors") or []
    if not isinstance(ancestors, list):
        hits.append(f"elements[{index}]:bad-clipping-ancestors")
        return hits
    for a_i, anc in enumerate(ancestors):
        ab = _bbox(anc.get("bbox") if isinstance(anc, dict) else anc)
        if ab is None:
            hits.append(f"elements[{index}]:clipping-ancestor[{a_i}]-bad-bbox")
            continue
        if not _inside(box, ab):
            hits.append(f"elements[{index}]:clipped-by-ancestor[{a_i}]")
    style = str(el.get("style") or el.get("transform") or "").casefold()
    fe = "calc(50%" in style.replace(" ", "") or "fluid" in str(el.get("engine") or "").casefold()
    if fe and any(
        key in h
        for h in hits
        for key in ("overflow-scrollwidth", "clipped-by-ancestor", "bbox-outside")
    ):
        hits.append(f"elements[{index}]:fluid-engine-off-centre-clip")
    return hits


def layout_overflow_hits(obs: dict[str, Any]) -> list[str]:
    """Measure scrollWidth/bboxes vs viewport, container, clipping ancestors."""
    layout = obs.get("layout") if isinstance(obs.get("layout"), dict) else {}
    elements = layout.get("elements") if layout else obs.get("elements")
    if not isinstance(elements, list) or not elements:
        return ["missing-layout-metrics"]
    viewport = _bbox(layout.get("viewport") or obs.get("viewport"))
    container = _bbox(layout.get("container") or obs.get("container"))
    hits: list[str] = []
    if viewport is None:
        hits.append("missing-viewport-bbox")
    for index, el in enumerate(elements):
        if not isinstance(el, dict):
            hits.append(f"elements[{index}]:bad-row")
            continue
        hits.extend(_element_scroll_hits(index, el))
        hits.extend(
            _element_bbox_hits(index, el, viewport=viewport, container=container)
        )
    return hits


def _receipt_hits(obs: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if obs.get("bidi_ok") is not True:
        hits.append("bidi-not-ok")
    if obs.get("no_raw_urls_in_copy") is not True:
        hits.append("raw-urls-in-copy")
    try:
        if float(obs.get("min_contrast_ratio")) < 4.5:
            hits.append("wcag-aa-contrast<4.5")
    except (TypeError, ValueError):
        hits.append("missing-contrast")
    if obs.get("alignment_ok") is not True:
        hits.append("alignment-not-ok")
    try:
        have = {int(w) for w in (obs.get("render_widths_px") or [])}
    except (TypeError, ValueError):
        have = set()
    if not ({390, 1440} <= have):
        hits.append("missing-390-or-1440-render")
    if obs.get("independent_visual_review") is not True:
        hits.append("missing-independent-visual-review")
    if obs.get("schema_only_done") is True:
        hits.append("schema-only-not-done")
    return hits


def _artifact_width(art: dict[str, Any]) -> int | None:
    raw = art.get("width_px")
    if raw is None:
        raw = art.get("width")
    try:
        return int(raw)
    except (TypeError, ValueError):
        return None


def _one_artifact_hits(index: int, art: Any) -> tuple[list[str], int | None]:
    """Validate one render artifact; return hits + width when known."""
    if not isinstance(art, dict):
        return [f"render_artifacts[{index}]:bad-row"], None
    hits: list[str] = []
    path_value = str(art.get("path") or "").strip()
    path: Path | None = None
    if not path_value:
        hits.append(f"render_artifacts[{index}]:missing-path")
    else:
        path = Path(path_value)
        if not path.is_file():
            hits.append(f"render_artifacts[{index}]:path-missing-file")
    claimed = str(art.get("sha256") or art.get("hash") or "").strip().casefold()
    if not claimed:
        hits.append(f"render_artifacts[{index}]:missing-hash")
    elif path is not None and path.is_file():
        try:
            digest = hashlib.sha256(path.read_bytes()).hexdigest().casefold()
        except OSError:
            hits.append(f"render_artifacts[{index}]:sha256-unreadable")
        else:
            if claimed != digest:
                hits.append(f"render_artifacts[{index}]:sha256-mismatch")
    return hits, _artifact_width(art)


def _artifact_hits(obs: dict[str, Any]) -> list[str]:
    """Require 390 + 1440 render artifacts with path + sha256 verified vs bytes."""
    artifacts = obs.get("render_artifacts")
    if not isinstance(artifacts, list) or not artifacts:
        return ["missing-render-artifacts"]
    hits: list[str] = []
    have_widths: set[int] = set()
    for index, art in enumerate(artifacts):
        row_hits, width = _one_artifact_hits(index, art)
        hits.extend(row_hits)
        if width in (390, 1440):
            have_widths.add(width)  # type: ignore[arg-type]
    if not ({390, 1440} <= have_widths):
        hits.append("missing-390-or-1440-render-artifact")
    return hits


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits = _receipt_hits(obs) + _artifact_hits(obs) + layout_overflow_hits(obs)
    return {"ok": not hits, "hits": hits}
