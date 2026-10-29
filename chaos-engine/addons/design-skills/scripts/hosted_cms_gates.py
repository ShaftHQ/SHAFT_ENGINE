"""Stdlib gates for hosted-CMS embed lessons L23–L31 + L36/L37 paste (no browser)."""

from __future__ import annotations

import math
import re
from typing import Any

OG_TWITTER_META = re.compile(
    r"""<meta\s+[^>]*(?:property|name)\s*=\s*["'](?:og|twitter):""",
    re.IGNORECASE,
)
HTML_LANG = re.compile(
    r'''<\s*html\b[^>]*\blang\s*=''',
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

# L36: Code-block paste must be a fragment — no document shell tags.
DOC_SHELL_TAG = re.compile(
    r"<!doctype\b|<\s*(?:html|head|body)\b",
    re.IGNORECASE,
)
# L36: paste CSS must not target host document roots.
GLOBAL_CSS_ROOT = re.compile(
    r"(?:^|[{};,\s])(?:html|body|:root)(?=[\s.{:#\[,>+~]|$)",
    re.IGNORECASE | re.MULTILINE,
)
# L37: HTML comments in paste leak ops notes on the live page.
HTML_COMMENT_OPEN = re.compile(r"<!--")


def _mask_attr_values(html: str) -> str:
    """Blank quoted attribute values so tag-like text in attrs is not a hit."""

    def repl(match: re.Match[str]) -> str:
        prefix = match.group(1)
        return prefix + (" " * (len(match.group(0)) - len(prefix)))

    return re.sub(r'(=\s*)("[^"]*"|\'[^\']*\')', repl, html or "")


def _style_bodies(html: str) -> list[str]:
    return re.findall(
        r"<style\b[^>]*>(.*?)</style>",
        html or "",
        flags=re.IGNORECASE | re.DOTALL,
    )


def _doc_shell_samples(html: str) -> list[str]:
    masked = _mask_attr_values(html or "")
    return [m.group(0).lower().replace(" ", "") for m in DOC_SHELL_TAG.finditer(masked)]


def _global_css_root_hits(html: str) -> int:
    return sum(len(GLOBAL_CSS_ROOT.findall(body)) for body in _style_bodies(html))


def _html_comment_hits(html: str) -> int:
    # Comments are real markup; attr masking would hide rare data-* FPs only.
    masked = _mask_attr_values(html or "")
    return len(HTML_COMMENT_OPEN.findall(masked))


def check_cms_meta(html: str) -> dict[str, Any]:
    """L25/L36/L37: no og/twitter, document shell, global root CSS, or comments."""
    body = html or ""
    count = len(list(OG_TWITTER_META.finditer(body)))
    lang_hits = len(list(HTML_LANG.finditer(body)))
    shell = _doc_shell_samples(body)
    css_roots = _global_css_root_hits(body)
    comments = _html_comment_hits(body)
    base = {
        "og_twitter_meta": count,
        "html_lang": lang_hits,
        "doc_shell_hits": len(shell),
        "doc_shell_samples": shell[:5],
        "global_css_root_hits": css_roots,
        "html_comment_hits": comments,
    }
    fails: list[str] = []
    if count:
        fails.append("CMS SEO panel owns og/twitter; strip paste meta (L25)")
    if lang_hits:
        fails.append("no <html lang> in paste; set language per block (L25)")
    if shell:
        fails.append(
            "fragment only: strip <!doctype>/<html>/<head>/<body>; "
            "put dir/lang on embed wrapper (L36)"
        )
    if css_roots:
        fails.append(
            "scope CSS to embed wrapper; no html/body/:root selectors (L36)"
        )
    if comments:
        fails.append("no HTML comments in paste; ops notes leak live (L37)")
    if fails:
        return {**base, "status": "fail", "note": "; ".join(fails), "fails": fails}
    return {
        **base,
        "status": "pass",
        "note": "paste fragment clean of meta/shell/comments (L25/L36/L37)",
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


def check_cms_sticky(
    text: str,
    *,
    header_px: float | None = None,
    scroll_margin_px: float | None = None,
) -> dict[str, Any]:
    """Require --header-height and scroll-margin-top; optional px compare."""
    has_header = bool(HEADER_HEIGHT_TOKEN.search(text or ""))
    has_margin = bool(SCROLL_MARGIN_TOKEN.search(text or ""))
    missing = []
    if not has_header:
        missing.append("--header-height")
    if not has_margin:
        missing.append("scroll-margin-top")
    base: dict[str, Any] = {
        "has_header_height": has_header,
        "has_scroll_margin_top": has_margin,
        "missing": missing,
    }
    if missing:
        return {
            **base,
            "status": "fail",
            "note": "first section must offset host sticky header (L23)",
        }
    if header_px is not None or scroll_margin_px is not None:
        if header_px is None or scroll_margin_px is None:
            return {
                "status": "error",
                "reasons": ["pass both --header-px and --scroll-margin-px, or neither"],
            }
        try:
            hp, sm = float(header_px), float(scroll_margin_px)
        except (TypeError, ValueError):
            return {"status": "error", "reasons": ["non-numeric header/scroll margin"]}
        if not math.isfinite(hp) or not math.isfinite(sm):
            return {"status": "error", "reasons": ["non-finite header/scroll margin"]}
        base.update({"header_px": hp, "scroll_margin_px": sm})
        if sm + 1e-9 < hp:
            return {
                **base,
                "status": "fail",
                "note": f"scroll-margin-top {sm}px < sticky header {hp}px (L23)",
            }
    return {
        **base,
        "status": "pass",
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


def _finite_num(name: str, val: float | None) -> float | None:
    if val is None:
        return None
    try:
        out = float(val)
    except (TypeError, ValueError):
        raise ValueError(name) from None
    if not math.isfinite(out):
        raise ValueError(name)
    return out


def _gutter_parity_fails(
    hero_gutter: float | None,
    embed_gutter: float | None,
    gutter_tol: float,
    base: dict[str, Any],
) -> list[str]:
    if (hero_gutter is None) ^ (embed_gutter is None):
        raise ValueError("pass both --hero-gutter and --embed-gutter, or neither")
    if hero_gutter is None or embed_gutter is None:
        return []
    diff = abs(embed_gutter - hero_gutter)
    base.update({
        "hero_gutter": hero_gutter,
        "embed_gutter": embed_gutter,
        "gutter_diff": round(diff, 4),
        "gutter_tol": gutter_tol,
    })
    if diff > gutter_tol + 1e-9:
        return [
            f"gutter mismatch embed={embed_gutter} hero={hero_gutter} "
            f"diff={diff:.1f} (tol ±{gutter_tol})"
        ]
    return []


def _double_pad_fails(
    outer_pad: float | None,
    inner_pad: float | None,
    content_inset: float | None,
    viewport: float | None,
    max_phone_inset: float,
    double_pad_lo: float,
    double_pad_hi: float,
    base: dict[str, Any],
) -> list[str]:
    fails: list[str] = []
    if outer_pad is not None or inner_pad is not None:
        op_v = 0.0 if outer_pad is None else outer_pad
        ip_v = 0.0 if inner_pad is None else inner_pad
        base.update({"outer_pad": op_v, "inner_pad": ip_v})
        if op_v > 0 and ip_v > 0:
            fails.append("no-double-pad: nested padding-inline stack (L27)")
    if content_inset is not None:
        base["content_inset"] = content_inset
        if viewport is not None:
            base["viewport"] = viewport
        phone = viewport is None or viewport <= 430
        if phone and content_inset > max_phone_inset + 1e-9:
            fails.append(
                f"phone over-inset content_inset={content_inset} > "
                f"{max_phone_inset} (double-pad smell)"
            )
        if phone and double_pad_lo - 1e-9 <= content_inset <= double_pad_hi + 1e-9:
            fails.append(
                f"double-pad band content_inset={content_inset} in "
                f"[{double_pad_lo}, {double_pad_hi}] at phone (L27)"
            )
    return fails


def check_cms_width(
    code_w: float,
    column_w: float,
    *,
    min_ratio: float = 0.92,
    hero_gutter: float | None = None,
    embed_gutter: float | None = None,
    gutter_tol: float = 4.0,
    outer_pad: float | None = None,
    inner_pad: float | None = None,
    content_inset: float | None = None,
    viewport: float | None = None,
    max_phone_inset: float = 36.0,
    double_pad_lo: float = 72.0,
    double_pad_hi: float = 80.0,
) -> dict[str, Any]:
    """Full-width ratio plus phone gutter parity / no-double-pad (L27)."""
    try:
        cw, col = float(code_w), float(column_w)
    except (TypeError, ValueError):
        return {"status": "error", "reasons": ["non-numeric widths"]}
    if not math.isfinite(cw) or not math.isfinite(col) or col <= 0:
        return {"status": "error", "reasons": ["column_w must be > 0 and finite"]}
    ratio = cw / col
    base: dict[str, Any] = {
        "code_w": cw,
        "column_w": col,
        "ratio": round(ratio, 4),
        "min_ratio": min_ratio,
    }
    fails: list[str] = []
    if ratio + 1e-9 < min_ratio:
        fails.append("code block must span content grid (L27)")
    try:
        hg = _finite_num("hero_gutter", hero_gutter)
        eg = _finite_num("embed_gutter", embed_gutter)
        op = _finite_num("outer_pad", outer_pad)
        ip = _finite_num("inner_pad", inner_pad)
        ci = _finite_num("content_inset", content_inset)
        vp = _finite_num("viewport", viewport)
        tol = float(gutter_tol)
    except ValueError as exc:
        return {"status": "error", "reasons": [f"non-numeric {exc}"]}
    try:
        fails.extend(_gutter_parity_fails(hg, eg, tol, base))
    except ValueError as exc:
        return {"status": "error", "reasons": [str(exc)]}
    fails.extend(_double_pad_fails(
        op, ip, ci, vp, float(max_phone_inset),
        float(double_pad_lo), float(double_pad_hi), base,
    ))
    if fails:
        return {**base, "status": "fail", "note": "; ".join(fails), "fails": fails}
    return {**base, "status": "pass", "note": "full-width + gutter parity ok (L27)"}



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
    note = "duration band ok (L30)"
    if dur > 100:
        note += f"; advisory: {dur}s above preferred ~60–100s (still ≤{max_s})"
        base["advisory"] = "above_preferred_100s"
    return {**base, "status": "pass", "note": note}


REQUIRED_LIVECMS_ROWS = (
    "http_200",
    "no_dup_chrome",
    "assets_loaded",
    "heading_colour",
    "identity_tile",
    "og_title_once",
    "viewport_390",
    "viewport_1440",
    # L28: media playable + duration ±tol (sentinel "n/a" when no video)
    "media_ok",
    "duration_ok",
    # L31: nested code-container count must be 0
    "nested_containers_0",
    # L36: sticky header logo/nav vs homepage '/' within 1px
    "header_logo_delta_ok",
    "header_nav_delta_ok",
)


def check_livecms(report: dict[str, Any]) -> dict[str, Any]:
    """Fail-closed live matrix receipt for L28/L31/L36 (n/a ok for media rows)."""
    if not isinstance(report, dict):
        return {"status": "error", "reasons": ["report must be a JSON object"]}
    rows = report.get("rows") if isinstance(report.get("rows"), dict) else report
    missing: list[str] = []
    failed: list[str] = []
    na_ok = {"media_ok", "duration_ok"}
    for key in REQUIRED_LIVECMS_ROWS:
        if key not in rows:
            missing.append(key)
            continue
        val = rows[key]
        ok = val is True or val == "pass" or val == "ok"
        if key in na_ok and isinstance(val, str) and val.lower() == "n/a":
            ok = True
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
            "note": "livecms matrix incomplete or red (L28/L31/L36)",
        }
    return {**base, "status": "pass", "note": "livecms matrix green (L28/L31/L36)"}


def check_cms_header_delta(
    home_logo_x: float,
    home_logo_y: float,
    page_logo_x: float,
    page_logo_y: float,
    home_nav_x: float,
    home_nav_y: float,
    page_nav_x: float,
    page_nav_y: float,
    *,
    max_delta_px: float = 1.0,
) -> dict[str, Any]:
    """L36: live header logo/nav position delta vs homepage '/' ≤ max_delta_px."""
    try:
        values = {
            "home_logo_x": float(home_logo_x),
            "home_logo_y": float(home_logo_y),
            "page_logo_x": float(page_logo_x),
            "page_logo_y": float(page_logo_y),
            "home_nav_x": float(home_nav_x),
            "home_nav_y": float(home_nav_y),
            "page_nav_x": float(page_nav_x),
            "page_nav_y": float(page_nav_y),
            "max_delta_px": float(max_delta_px),
        }
    except (TypeError, ValueError):
        return {"status": "error", "reasons": ["non-numeric header positions"]}
    if any(not math.isfinite(v) for v in values.values()):
        return {"status": "error", "reasons": ["non-finite header positions"]}
    logo_dx = abs(values["page_logo_x"] - values["home_logo_x"])
    logo_dy = abs(values["page_logo_y"] - values["home_logo_y"])
    nav_dx = abs(values["page_nav_x"] - values["home_nav_x"])
    nav_dy = abs(values["page_nav_y"] - values["home_nav_y"])
    logo_delta = max(logo_dx, logo_dy)
    nav_delta = max(nav_dx, nav_dy)
    tol = values["max_delta_px"]
    base = {
        **values,
        "logo_delta_px": round(logo_delta, 4),
        "nav_delta_px": round(nav_delta, 4),
        "header_logo_delta_ok": logo_delta <= tol + 1e-9,
        "header_nav_delta_ok": nav_delta <= tol + 1e-9,
    }
    fails: list[str] = []
    if logo_delta > tol + 1e-9:
        fails.append(
            f"header logo delta {logo_delta:.2f}px > {tol}px vs / (L36)"
        )
    if nav_delta > tol + 1e-9:
        fails.append(
            f"header nav delta {nav_delta:.2f}px > {tol}px vs / (L36)"
        )
    if fails:
        return {**base, "status": "fail", "note": "; ".join(fails), "fails": fails}
    return {
        **base,
        "status": "pass",
        "note": "header logo/nav within 1px of homepage / (L36)",
    }
