"""Stdlib gates for hosted-CMS embed lessons L23–L31 + L34/L36/L37 paste (no browser)."""

from __future__ import annotations

import math
import re
from html.parser import HTMLParser
from typing import Any

OG_TWITTER_META = re.compile(
    r"""<meta\s+[^>]*(?:property|name)\s*=\s*["'](?:og|twitter):""",
    re.IGNORECASE,
)
# L34: paste must not carry crawl-block robots (meta or X-Robots-Tag-style).
ROBOTS_META = re.compile(
    r"""<meta\s+(?=[^>]*\b(?:name|http-equiv)\s*=\s*["'](?:robots|googlebot|bingbot|x-robots-tag)["'])"""
    r"""[^>]*\bcontent\s*=\s*["']([^"']*)["']"""
    r"""|<meta\s+(?=[^>]*\bcontent\s*=\s*["']([^"']*)["'])"""
    r"""[^>]*\b(?:name|http-equiv)\s*=\s*["'](?:robots|googlebot|bingbot|x-robots-tag)["']""",
    re.IGNORECASE,
)
XROBOTS_HEADERISH = re.compile(
    r"""(?:^|[\r\n<]|<!--)\s*X-Robots-Tag\s*:\s*([^\r\n<>]*)""",
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
DOC_SHELL_TAGS = frozenset({"html", "head", "body"})
# L36: paste CSS must not target host document roots (selectors, not literals).
# Match only in selector context (tokenizer below): namespace |body / svg|html,
# :is(html), *+html; not at-rule names or url()/declaration args.
_ROOT_IN_SELECTOR = re.compile(
    r"(?<![\w-])(?:(?:[\w-]+|\*)?\|)?(?::root|html|body)(?![\w-])",
    re.IGNORECASE,
)
_CSS_BLOCK_COMMENT = re.compile(r"/\*.*?\*/", re.S)
_CSS_LINE_COMMENT = re.compile(r"(?<!:)//.*?$", re.M)
_CSS_STRING = re.compile(
    r'("(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\')',
    re.S,
)
# HTML5 RCDATA + obsolete rawtext: <!--...--> is character data, not a comment.
# script/style also raw. noscript/iframe/template keep real Comment nodes → L37 fail.
_RAW_OR_RCDATA = frozenset({
    "script",
    "style",
    "textarea",
    "title",
    "xmp",
    "listing",
    "noembed",
    "noframes",
    "plaintext",
})
# Livecms / cmsheader viewports (L28 + L36): phone + desktop matrix.
HEADER_VIEWPORTS = (360, 390, 430, 1440)
HEADER_DELTA_OK_PREFIXES = ("header_logo_delta_ok_", "header_nav_delta_ok_")


def _strip_css_comments_and_strings(src: str) -> str:
    """Drop /* */ and // comments plus quoted strings before selector scans."""
    no_block = _CSS_BLOCK_COMMENT.sub("", src or "")
    no_line = _CSS_LINE_COMMENT.sub("", no_block)
    return _CSS_STRING.sub('""', no_line)


def _count_root_selectors(css: str) -> int:
    """Count html/body/:root type selectors in selector lists only.

    Skips at-rule preludes (@keyframes body, @layer body, @import url(...)) and
    declaration values (url(body.svg), image-set(body)). Still counts selectors
    inside at-rule blocks (@media all{html{}}), functional pseudos (:is(html)),
    and namespace type selectors (|body, svg|html, *|body).
    """
    s = css or ""
    n = len(s)
    hits = 0

    def scan_block(j: int) -> int:
        nonlocal hits
        while j < n:
            while j < n and s[j].isspace():
                j += 1
            if j >= n:
                return j
            if s[j] == "}":
                return j + 1
            if s[j] == "@":
                j = scan_at_rule(j)
                continue
            start = j
            paren = 0
            while j < n:
                ch = s[j]
                if ch == "(":
                    paren += 1
                    j += 1
                elif ch == ")":
                    paren = max(0, paren - 1)
                    j += 1
                elif paren == 0 and ch == "{":
                    hits += len(_ROOT_IN_SELECTOR.findall(s[start:j]))
                    j = scan_block(j + 1)
                    break
                elif paren == 0 and ch == ";":
                    j += 1
                    break
                elif paren == 0 and ch == "}":
                    return j + 1
                else:
                    j += 1
            else:
                return j
        return j

    def scan_at_rule(j: int) -> int:
        # s[j] == '@' — prelude identifiers are names/queries, not selectors.
        j += 1
        while j < n and (s[j].isalnum() or s[j] in "_-"):
            j += 1
        paren = 0
        while j < n:
            ch = s[j]
            if ch == "(":
                paren += 1
                j += 1
            elif ch == ")":
                paren = max(0, paren - 1)
                j += 1
            elif paren == 0 and ch == "{":
                return scan_block(j + 1)
            elif paren == 0 and ch == ";":
                return j + 1
            else:
                j += 1
        return j

    i = 0
    while i < n:
        while i < n and s[i].isspace():
            i += 1
        if i >= n:
            break
        if s[i] == "}":
            i += 1
            continue
        if s[i] == "@":
            i = scan_at_rule(i)
            continue
        start = i
        paren = 0
        while i < n:
            ch = s[i]
            if ch == "(":
                paren += 1
                i += 1
            elif ch == ")":
                paren = max(0, paren - 1)
                i += 1
            elif paren == 0 and ch == "{":
                hits += len(_ROOT_IN_SELECTOR.findall(s[start:i]))
                i = scan_block(i + 1)
                break
            elif paren == 0 and ch == ";":
                i += 1
                break
            elif paren == 0 and ch == "}":
                break
            else:
                i += 1
        else:
            break
    return hits


class _PasteMarkupScanner(HTMLParser):
    """Tokenize paste; judge only real markup. Skip raw/RCDATA text."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.shell: list[str] = []
        self.comments = 0
        self.html_lang = 0
        self.style_bodies: list[str] = []
        self._raw_stack: list[str] = []
        self._style_chunks: list[str] = []

    def _in_raw(self) -> bool:
        return bool(self._raw_stack)

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        name = (tag or "").lower()
        if self._in_raw():
            return
        if name in _RAW_OR_RCDATA:
            self._raw_stack.append(name)
            if name == "style":
                self._style_chunks = []
            return
        if name in DOC_SHELL_TAGS:
            self.shell.append(f"<{name}")
            if name == "html":
                for key, _val in attrs or []:
                    if (key or "").lower() == "lang":
                        self.html_lang += 1
                        break

    def handle_startendtag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        # Void/self-closing — still a shell hit if html/head/body (rare).
        self.handle_starttag(tag, attrs)
        name = (tag or "").lower()
        if name in _RAW_OR_RCDATA and self._raw_stack and self._raw_stack[-1] == name:
            self.handle_endtag(tag)

    def handle_endtag(self, tag: str) -> None:
        name = (tag or "").lower()
        if name in _RAW_OR_RCDATA and self._raw_stack and self._raw_stack[-1] == name:
            if name == "style":
                self.style_bodies.append("".join(self._style_chunks))
                self._style_chunks = []
            self._raw_stack.pop()

    def handle_data(self, data: str) -> None:
        if self._raw_stack and self._raw_stack[-1] == "style":
            self._style_chunks.append(data or "")

    def handle_comment(self, data: str) -> None:
        # Real HTML comments, including conditional <!--[if ...] ... <![endif]-->.
        # Ignore parser false comments inside RCDATA/rawtext (_RAW_OR_RCDATA).
        if self._in_raw():
            return
        self.comments += 1

    def handle_decl(self, decl: str) -> None:
        if self._in_raw():
            return
        if (decl or "").lower().lstrip().startswith("doctype"):
            self.shell.append("<!doctype")


def _scan_paste_markup(html: str) -> _PasteMarkupScanner:
    scanner = _PasteMarkupScanner()
    scanner.feed(html or "")
    scanner.close()
    return scanner


def _doc_shell_samples(html: str) -> list[str]:
    return list(_scan_paste_markup(html).shell)


def _global_css_root_hits(html: str) -> int:
    scanner = _scan_paste_markup(html)
    total = 0
    for body in scanner.style_bodies:
        cleaned = _strip_css_comments_and_strings(body)
        total += _count_root_selectors(cleaned)
    return total


def _html_comment_hits(html: str) -> int:
    return int(_scan_paste_markup(html).comments)



def _robots_directive_hits(html: str) -> list[str]:
    """Return paste snippets that block indexing/following (L34)."""
    body = html or ""
    hits: list[str] = []
    for m in ROBOTS_META.finditer(body):
        content = (m.group(1) or m.group(2) or "").lower()
        if "noindex" in content or "nofollow" in content or content.strip() == "none":
            hits.append(m.group(0)[:120])
    for m in XROBOTS_HEADERISH.finditer(body):
        content = (m.group(1) or "").lower()
        if "noindex" in content or "nofollow" in content or "none" in content.split(","):
            hits.append(m.group(0)[:120])
    return hits


def check_cms_meta(html: str) -> dict[str, Any]:
    """L25/L34/L36/L37: no og/twitter, robots, shell, root CSS, or comments."""
    body = html or ""
    count = len(list(OG_TWITTER_META.finditer(body)))
    scanned = _scan_paste_markup(body)
    lang_hits = int(scanned.html_lang)
    shell = list(scanned.shell)
    css_roots = sum(
        _count_root_selectors(_strip_css_comments_and_strings(b))
        for b in scanned.style_bodies
    )
    comments = int(scanned.comments)
    robots_hits = _robots_directive_hits(body)
    base = {
        "og_twitter_meta": count,
        "html_lang": lang_hits,
        "robots_block_hits": len(robots_hits),
        "robots_samples": robots_hits[:5],
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
    if robots_hits:
        fails.append(
            "no robots noindex/nofollow or X-Robots-Tag in paste; "
            "CMS SEO / live headers own crawl (L34)"
        )
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
        "note": "paste clean of meta/robots/shell/comments (L25/L34/L36/L37)",
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
    # L28 (+ L36 align): phone + desktop matrix
    "viewport_360",
    "viewport_390",
    "viewport_430",
    "viewport_1440",
    # L28: media playable + duration ±tol (sentinel "n/a" when no video)
    "media_ok",
    "duration_ok",
    # L31: nested code-container count must be 0
    "nested_containers_0",
    # L36: sticky header logo/nav vs '/' ≤1px at each viewport
    *(
        key
        for width in HEADER_VIEWPORTS
        for key in (f"header_logo_delta_ok_{width}", f"header_nav_delta_ok_{width}")
    ),
)


def _livecms_rows_without_forged_header(report: dict[str, Any]) -> dict[str, Any]:
    rows_src = report.get("rows") if isinstance(report.get("rows"), dict) else report
    if not isinstance(rows_src, dict):
        return {}
    return {
        key: value
        for key, value in rows_src.items()
        if not (isinstance(key, str) and key.startswith(HEADER_DELTA_OK_PREFIXES))
    }


def _header_measure_payload(report: dict[str, Any], measure: dict[str, Any]) -> dict[str, Any]:
    if "viewports" in measure and isinstance(measure["viewports"], dict):
        return {
            "viewports": measure["viewports"],
            "max_delta_px": measure.get("max_delta_px", report.get("max_delta_px", 1.0)),
        }
    payload: dict[str, Any] = {"viewports": measure}
    if "max_delta_px" in report:
        payload["max_delta_px"] = report["max_delta_px"]
    return payload


def _apply_header_measure(rows: dict[str, Any], report: dict[str, Any]) -> dict[str, Any] | None:
    """Recompute header_* flags into rows. Return an error result dict or None."""
    measure = report.get("header_measure")
    if measure is None:
        measure = report.get("viewports")
    if not isinstance(measure, dict) or not measure:
        return None
    hdr = check_cms_header_delta(_header_measure_payload(report, measure))
    if hdr.get("status") == "error":
        return {
            "status": "fail",
            "missing": [f"header_logo_delta_ok_{w}" for w in HEADER_VIEWPORTS],
            "failed": [],
            "required": list(REQUIRED_LIVECMS_ROWS),
            "note": "header_measure invalid: "
            + "; ".join(hdr.get("reasons") or [hdr.get("note", "")]),
            "header_measure_status": "error",
            "header_measure_trusted": False,
        }
    for width in HEADER_VIEWPORTS:
        rows[f"header_logo_delta_ok_{width}"] = bool(hdr.get(f"header_logo_delta_ok_{width}"))
        rows[f"header_nav_delta_ok_{width}"] = bool(hdr.get(f"header_nav_delta_ok_{width}"))
    rows["__header_measure_trusted__"] = True
    return None


def check_livecms(report: dict[str, Any]) -> dict[str, Any]:
    """Fail-closed live matrix for L28/L31/L36.

    Header delta rows are never trusted from the operator. Any
    ``header_*_delta_ok_*`` keys in the receipt are stripped; flags are
    recomputed from ``header_measure`` / ``viewports`` (raw boxes + content
    widths). A forged receipt that only sets those booleans to true fails.
    """
    if not isinstance(report, dict):
        return {"status": "error", "reasons": ["report must be a JSON object"]}
    rows_src = report.get("rows") if isinstance(report.get("rows"), dict) else report
    if not isinstance(rows_src, dict):
        return {"status": "error", "reasons": ["report must be a JSON object"]}
    rows = _livecms_rows_without_forged_header(report)
    err = _apply_header_measure(rows, report)
    if err is not None:
        return err
    trusted = bool(rows.pop("__header_measure_trusted__", False))
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
        "header_measure_trusted": trusted,
    }
    if missing or failed:
        return {
            **base,
            "status": "fail",
            "note": "livecms matrix incomplete or red (L28/L31/L36)",
        }
    return {**base, "status": "pass", "note": "livecms matrix green (L28/L31/L36)"}


def _xy_pair(node: Any, label: str) -> tuple[float, float] | str:
    if not isinstance(node, dict):
        return f"{label} must be an object with x,y"
    try:
        x, y = float(node["x"]), float(node["y"])
    except (KeyError, TypeError, ValueError):
        return f"{label} needs numeric x,y"
    if not math.isfinite(x) or not math.isfinite(y):
        return f"{label} x,y must be finite"
    return x, y


def _content_width(node: dict[str, Any], key: str, label: str) -> float | str:
    raw = node.get(key)
    if raw is None:
        return f"{label} missing {key} (client/content width for scrollbar normalize)"
    try:
        width = float(raw)
    except (TypeError, ValueError):
        return f"{label} {key} must be numeric"
    if not math.isfinite(width) or width <= 0:
        return f"{label} {key} must be finite > 0"
    return width


def _axis_delta(a: float, b: float, wa: float, wb: float) -> float:
    """Scrollbar-gutter stable axis delta: min(left-anchored, right-anchored)."""
    left = abs(b - a)
    right = abs((wb - b) - (wa - a))
    return min(left, right)


def _viewport_delta(
    home_logo: tuple[float, float],
    page_logo: tuple[float, float],
    home_nav: tuple[float, float],
    page_nav: tuple[float, float],
    *,
    home_width: float,
    page_width: float,
    max_delta_px: float,
) -> tuple[float, float, bool, bool]:
    logo_delta = max(
        _axis_delta(home_logo[0], page_logo[0], home_width, page_width),
        abs(page_logo[1] - home_logo[1]),
    )
    nav_delta = max(
        _axis_delta(home_nav[0], page_nav[0], home_width, page_width),
        abs(page_nav[1] - home_nav[1]),
    )
    tol = max_delta_px
    return (
        logo_delta,
        nav_delta,
        logo_delta <= tol + 1e-9,
        nav_delta <= tol + 1e-9,
    )


def _parse_max_delta(report: dict[str, Any], default: float) -> float | str:
    try:
        tol = float(report.get("max_delta_px", default))
    except (TypeError, ValueError):
        return "non-numeric max_delta_px"
    if not math.isfinite(tol) or tol < 0:
        return "max_delta_px must be finite ≥ 0"
    return tol


def _score_viewport(
    width: int,
    node: dict[str, Any],
    *,
    tol: float,
) -> tuple[dict[str, Any] | None, list[str]]:
    """Return (row, fails) for one viewport width."""
    fails: list[str] = []
    parsed: dict[str, tuple[float, float]] = {}
    for label in ("home_logo", "page_logo", "home_nav", "page_nav"):
        got = _xy_pair(node.get(label), f"{width}.{label}")
        if isinstance(got, str):
            return None, [got]
        parsed[label] = got
    home_w = _content_width(node, "home_content_width", str(width))
    if isinstance(home_w, str):
        return None, [home_w]
    page_w = _content_width(node, "page_content_width", str(width))
    if isinstance(page_w, str):
        return None, [page_w]
    logo_d, nav_d, logo_ok, nav_ok = _viewport_delta(
        parsed["home_logo"],
        parsed["page_logo"],
        parsed["home_nav"],
        parsed["page_nav"],
        home_width=home_w,
        page_width=page_w,
        max_delta_px=tol,
    )
    row = {
        "logo_delta_px": round(logo_d, 4),
        "nav_delta_px": round(nav_d, 4),
        "home_content_width": home_w,
        "page_content_width": page_w,
        f"header_logo_delta_ok_{width}": logo_ok,
        f"header_nav_delta_ok_{width}": nav_ok,
    }
    if not logo_ok:
        fails.append(
            f"viewport {width}: header logo delta {logo_d:.2f}px > {tol}px vs / (L36)"
        )
    if not nav_ok:
        fails.append(
            f"viewport {width}: header nav delta {nav_d:.2f}px > {tol}px vs / (L36)"
        )
    return row, fails


def check_cms_header_delta(
    report: dict[str, Any] | None = None,
    *,
    max_delta_px: float = 1.0,
    **legacy: Any,
) -> dict[str, Any]:
    """L36: per-viewport header logo/nav delta vs '/' ≤ max_delta_px.

    Preferred receipt::
        {"viewports": {"360": {"home_logo": {"x", "y"}, "page_logo": {...},
         "home_nav": {...}, "page_nav": {...},
         "home_content_width": 1425, "page_content_width": 1440}, ...}}

    X deltas use min(left-anchored, right-anchored) so classic scrollbar
    gutter (home clientWidth 1425 vs page 1440) does not false-fail a clean
    site. Legacy scalar kwargs are rejected.
    """
    if legacy and report is None:
        return {
            "status": "error",
            "reasons": [
                "cmsheader requires a viewports receipt "
                f"(widths {list(HEADER_VIEWPORTS)}); scalar kwargs removed"
            ],
        }
    if not isinstance(report, dict):
        return {"status": "error", "reasons": ["report must be a JSON object"]}
    tol = _parse_max_delta(report, max_delta_px)
    if isinstance(tol, str):
        return {"status": "error", "reasons": [tol]}
    raw_vp = report.get("viewports")
    if not isinstance(raw_vp, dict):
        return {"status": "error", "reasons": ["viewports map required"]}
    per: dict[str, Any] = {}
    fails: list[str] = []
    missing_widths: list[int] = []
    for width in HEADER_VIEWPORTS:
        key = str(width)
        node = raw_vp.get(key, raw_vp.get(width))
        if not isinstance(node, dict):
            missing_widths.append(width)
            continue
        row, row_fails = _score_viewport(width, node, tol=tol)
        if row is None:
            fails.extend(row_fails)
            continue
        per[key] = row
        fails.extend(row_fails)
    flat_flags = {
        f"header_logo_delta_ok_{w}": bool(
            (per.get(str(w)) or {}).get(f"header_logo_delta_ok_{w}")
        )
        for w in HEADER_VIEWPORTS
    }
    flat_flags.update({
        f"header_nav_delta_ok_{w}": bool(
            (per.get(str(w)) or {}).get(f"header_nav_delta_ok_{w}")
        )
        for w in HEADER_VIEWPORTS
    })
    base: dict[str, Any] = {
        "viewports": per,
        "required_widths": list(HEADER_VIEWPORTS),
        "missing_widths": missing_widths,
        "max_delta_px": tol,
        **flat_flags,
    }
    if missing_widths:
        fails.insert(
            0,
            "cmsheader missing viewports "
            + ",".join(str(w) for w in missing_widths)
            + f" (need {list(HEADER_VIEWPORTS)})",
        )
    if fails:
        return {**base, "status": "fail", "note": "; ".join(fails), "fails": fails}
    return {
        **base,
        "status": "pass",
        "note": (
            "header logo/nav within 1px of / at "
            + "/".join(str(w) for w in HEADER_VIEWPORTS)
            + " (L36, scrollbar-normalized)"
        ),
    }
