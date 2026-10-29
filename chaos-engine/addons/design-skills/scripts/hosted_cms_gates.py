"""Stdlib gates for hosted-CMS embed lessons L23–L31 (no browser required)."""

from __future__ import annotations

import math
import re
from typing import Any

OG_TWITTER_META = re.compile(
    r"""<meta\s+[^>]*(?:property|name)\s*=\s*["'](?:og|twitter):""",
    re.IGNORECASE,
)
RELATIVE_ASSET_SRC = re.compile(
    r"""\b(?:src|href)\s*=\s*["'](\.\./[^"']+|assets/[^"']+)["']""",
    re.IGNORECASE,
)
HEADER_HEIGHT_TOKEN = re.compile(r"--header-height", re.IGNORECASE)
SCROLL_MARGIN_TOKEN = re.compile(r"scroll-margin-top", re.IGNORECASE)
TOPBAR_IN_PASTE = re.compile(
    r"""<(?:header|nav)\b[^>]*\b(?:topbar|site-nav|in-page-nav)\b"""
    r"""|<(?:header|nav)\b[^>]*class=["'][^"']*\btopbar\b""",
    re.IGNORECASE,
)


def check_cms_meta(html: str) -> dict[str, Any]:
    """L25: paste must not include og:/twitter: meta."""
    hits = OG_TWITTER_META.findall(html or "")
    # findall on the meta pattern returns the match of first group-less — use finditer
    count = len(list(OG_TWITTER_META.finditer(html or "")))
    if count:
        return {
            "status": "fail",
            "og_twitter_meta": count,
            "note": "CMS SEO panel owns og/twitter; strip paste meta (L25)",
        }
    return {
        "status": "pass",
        "og_twitter_meta": 0,
        "note": "no og:/twitter: meta in paste (L25)",
    }


def check_cms_assets(html: str) -> dict[str, Any]:
    """L24: fail relative local asset paths in paste."""
    found = [m.group(1) for m in RELATIVE_ASSET_SRC.finditer(html or "")]
    if found:
        return {
            "status": "fail",
            "relative_assets": found[:20],
            "count": len(found),
            "note": "rewrite to https or data: after CSP probe (L24)",
        }
    return {
        "status": "pass",
        "relative_assets": [],
        "count": 0,
        "note": "no relative local assets in paste (L24)",
    }


def check_cms_sticky(text: str) -> dict[str, Any]:
    """L23: embed paste/CSS must declare --header-height and scroll-margin-top."""
    has_header = bool(HEADER_HEIGHT_TOKEN.search(text or ""))
    has_margin = bool(SCROLL_MARGIN_TOKEN.search(text or ""))
    missing = []
    if not has_header:
        missing.append("--header-height")
    if not has_margin:
        missing.append("scroll-margin-top")
    if missing:
        return {
            "status": "fail",
            "has_header_height": has_header,
            "has_scroll_margin_top": has_margin,
            "missing": missing,
            "note": "first section must offset host sticky header (L23)",
        }
    return {
        "status": "pass",
        "has_header_height": True,
        "has_scroll_margin_top": True,
        "missing": [],
        "note": "sticky header offset tokens present (L23)",
    }


def check_cms_chrome(html: str) -> dict[str, Any]:
    """L23: paste body must not include in-page topbar/nav chrome."""
    hits = list(TOPBAR_IN_PASTE.finditer(html or ""))
    if hits:
        return {
            "status": "fail",
            "topbar_hits": len(hits),
            "note": "drop in-page topbar/nav; host owns chrome (L23)",
        }
    return {
        "status": "pass",
        "topbar_hits": 0,
        "note": "no in-page topbar/nav in paste (L23)",
    }


def _parse_rgb(css: str) -> tuple[int, int, int] | None:
    m = re.match(r"rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)", str(css or ""), re.I)
    if not m:
        return None
    return int(m.group(1)), int(m.group(2)), int(m.group(3))


def _rel_lum(rgb: tuple[int, int, int]) -> float:
    def f(c: int) -> float:
        x = c / 255.0
        return x / 12.92 if x <= 0.03928 else ((x + 0.055) / 1.055) ** 2.4

    r, g, b = rgb
    return 0.2126 * f(r) + 0.7152 * f(g) + 0.0722 * f(b)


def near_white(css_color: str, *, min_lum: float = 0.85) -> bool:
    rgb = _parse_rgb(css_color)
    if not rgb:
        return False
    if rgb[0] >= 230 and rgb[1] >= 230 and rgb[2] >= 230:
        return True
    return _rel_lum(rgb) >= min_lum


def check_hostile_heading(color: str, *, min_lum: float = 0.85) -> dict[str, Any]:
    """L26: heading on dark must be near-white under hostile CSS."""
    ok = near_white(color, min_lum=min_lum)
    rgb = _parse_rgb(color)
    lum = round(_rel_lum(rgb), 4) if rgb else None
    if not ok:
        return {
            "status": "fail",
            "color": color,
            "luminance": lum,
            "min_lum": min_lum,
            "note": "heading must stay near-white on dark under hostile CSS (L26)",
        }
    return {
        "status": "pass",
        "color": color,
        "luminance": lum,
        "min_lum": min_lum,
        "note": "heading near-white (L26)",
    }


def check_hostile_tile(
    tile_w: float,
    tile_h: float,
    img_x: float,
    img_y: float,
    img_w: float,
    img_h: float,
    tile_x: float = 0.0,
    tile_y: float = 0.0,
    *,
    sq_tol: float = 1.5,
    inset_tol: float = 0.75,
) -> dict[str, Any]:
    """L26: identity tile square; mark fully inside."""
    try:
        tw, th = float(tile_w), float(tile_h)
        ix, iy, iw, ih = float(img_x), float(img_y), float(img_w), float(img_h)
        tx, ty = float(tile_x), float(tile_y)
    except (TypeError, ValueError):
        return {"status": "error", "reasons": ["non-numeric tile/img geometry"]}
    square = abs(tw - th) <= sq_tol
    inside = (
        ix >= tx - inset_tol
        and iy >= ty - inset_tol
        and ix + iw <= tx + tw + inset_tol
        and iy + ih <= ty + th + inset_tol
    )
    not_empty = iw >= 8 and ih >= 8
    base = {
        "tile_w": tw,
        "tile_h": th,
        "square": square,
        "inside": inside,
        "not_empty": not_empty,
        "sq_tol": sq_tol,
        "inset_tol": inset_tol,
    }
    if not (square and inside and not_empty):
        return {
            **base,
            "status": "fail",
            "note": "squircle tile must be square with mark inset (L26)",
        }
    return {**base, "status": "pass", "note": "identity tile geometry ok (L26)"}


def check_cms_width(
    code_w: float,
    column_w: float,
    *,
    min_ratio: float = 0.92,
) -> dict[str, Any]:
    """L27: code container vs content column width ratio."""
    try:
        cw, col = float(code_w), float(column_w)
    except (TypeError, ValueError):
        return {"status": "error", "reasons": ["non-numeric widths"]}
    if not math.isfinite(cw) or not math.isfinite(col) or col <= 0:
        return {"status": "error", "reasons": ["column_w must be > 0 and finite"]}
    ratio = cw / col
    base = {"code_w": cw, "column_w": col, "ratio": round(ratio, 4), "min_ratio": min_ratio}
    if ratio + 1e-9 < min_ratio:
        return {
            **base,
            "status": "fail",
            "note": "code block must span content grid (L27)",
        }
    return {**base, "status": "pass", "note": "full-width ratio ok (L27)"}


def check_cms_duration(
    dur: float,
    *,
    min_s: float = 60.0,
    max_s: float = 120.0,
    peer: float | None = None,
    peer_tol: float = 30.0,
) -> dict[str, Any]:
    """L30: explainer duration band and optional sibling parity."""
    try:
        d = float(dur)
    except (TypeError, ValueError):
        return {"status": "error", "reasons": ["non-numeric dur"]}
    if not math.isfinite(d) or d <= 0:
        return {"status": "error", "reasons": [f"dur NaN or <=0 ({dur!r})"]}
    reasons: list[str] = []
    if d < min_s - 1e-9 or d > max_s + 1e-9:
        reasons.append(f"dur {d} outside [{min_s},{max_s}]")
    peer_delta = None
    if peer is not None:
        try:
            p = float(peer)
        except (TypeError, ValueError):
            return {"status": "error", "reasons": ["non-numeric peer"]}
        peer_delta = abs(d - p)
        if peer_delta > peer_tol + 1e-9:
            reasons.append(f"peer delta {peer_delta} > {peer_tol}")
    base = {
        "dur_s": round(d, 3),
        "min_s": min_s,
        "max_s": max_s,
        "peer_s": None if peer is None else float(peer),
        "peer_delta_s": None if peer_delta is None else round(peer_delta, 3),
        "peer_tol_s": peer_tol,
        "reasons": reasons,
    }
    if reasons:
        return {**base, "status": "fail", "note": "explainer length band/parity (L30)"}
    return {**base, "status": "pass", "note": "duration band ok (L30)"}


REQUIRED_LIVECMS_ROWS = (
    "http_200",
    "no_dup_chrome",
    "assets_loaded",
    "heading_colour",
    "identity_tile",
    "og_title_once",
    "viewport_390",
    "viewport_1440",
)


def check_livecms(report: dict[str, Any]) -> dict[str, Any]:
    """L28/L31: fail-closed live matrix receipt."""
    if not isinstance(report, dict):
        return {"status": "error", "reasons": ["report must be a JSON object"]}
    rows = report.get("rows") if isinstance(report.get("rows"), dict) else report
    missing: list[str] = []
    failed: list[str] = []
    for key in REQUIRED_LIVECMS_ROWS:
        if key not in rows:
            missing.append(key)
            continue
        val = rows[key]
        ok = val is True or val == "pass" or val == "ok"
        if not ok:
            failed.append(key)
    base = {
        "missing": missing,
        "failed": failed,
        "required": list(REQUIRED_LIVECMS_ROWS),
    }
    if missing or failed:
        return {
            **base,
            "status": "fail",
            "note": "livecms matrix incomplete or red (L28/L31)",
        }
    return {**base, "status": "pass", "note": "livecms matrix green (L28)"}
