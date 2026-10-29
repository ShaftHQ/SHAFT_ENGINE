"""Cheap public-site SEO + AI-answer-engine gates (curl/HTML; no browser).

Opposite of cmsmeta: check the *published* page/headers, never paste ownership.
"""

from __future__ import annotations

import json
import re
import shutil
import subprocess  # nosec B404 - fixed curl argv list, never a shell.
from html.parser import HTMLParser
from pathlib import Path
from typing import Any
from urllib.parse import urljoin, urlparse

AI_BOTS = (
    "GPTBot",
    "OAI-SearchBot",
    "ChatGPT-User",
    "Google-Extended",
    "PerplexityBot",
    "ClaudeBot",
    "anthropic-ai",
    "Applebot-Extended",
)

REQUIRED_ROWS = (
    "https_final",
    "sitemap_200",
    "robots_200",
    "robots_allows_home",
    "ai_crawlers_allowed",
    "no_noindex",
    "canonical_https",
    "title_ok",
    "meta_desc_ok",
    "h1_one",
    "heading_order",
    "html_lang",
    "og_image_https",
    "llms_txt",
    "links_sample_ok",
    "true_404",
)

AGENT = (
    "Mozilla/5.0 (compatible; ChaosEngine-liveseo/1.0; +https://github.com/ShaftHQ/SHAFT_ENGINE)"
)

META_ROBOTS = re.compile(
    r"""<meta\s+[^>]*name\s*=\s*["']robots["'][^>]*content\s*=\s*["']([^"']+)["']"""
    r"""|<meta\s+[^>]*content\s*=\s*["']([^"']+)["'][^>]*name\s*=\s*["']robots["']""",
    re.IGNORECASE,
)
CANONICAL = re.compile(
    r"""<link\s+[^>]*rel\s*=\s*["']canonical["'][^>]*href\s*=\s*["']([^"']+)["']"""
    r"""|<link\s+[^>]*href\s*=\s*["']([^"']+)["'][^>]*rel\s*=\s*["']canonical["']""",
    re.IGNORECASE,
)
OG_IMAGE = re.compile(
    r"""<meta\s+[^>]*property\s*=\s*["']og:image["'][^>]*content\s*=\s*["']([^"']+)["']"""
    r"""|<meta\s+[^>]*content\s*=\s*["']([^"']+)["'][^>]*property\s*=\s*["']og:image["']""",
    re.IGNORECASE,
)
META_DESC = re.compile(
    r"""<meta\s+[^>]*name\s*=\s*["']description["'][^>]*content\s*=\s*["']([^"']*)["']"""
    r"""|<meta\s+[^>]*content\s*=\s*["']([^"']*)["'][^>]*name\s*=\s*["']description["']""",
    re.IGNORECASE,
)
TITLE = re.compile(r"<title[^>]*>(.*?)</title>", re.IGNORECASE | re.DOTALL)
HTML_LANG = re.compile(r"""<\s*html\b[^>]*\blang\s*=\s*["']([^"']+)["']""", re.IGNORECASE)
SLUG_BAD = re.compile(r"[A-Z_\s]|%20")


class _Outline(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.h1 = 0
        self.levels: list[int] = []
        self.imgs_missing_alt = 0
        self.anchors: list[str] = []
        self._in_title = False
        self.title = ""

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        ad = {k.lower(): (v or "") for k, v in attrs}
        if tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
            level = int(tag[1])
            self.levels.append(level)
            if level == 1:
                self.h1 += 1
        elif tag == "img" and "alt" not in ad:
            self.imgs_missing_alt += 1
        elif tag == "a" and ad.get("href"):
            self.anchors.append(ad["href"])
        elif tag == "title":
            self._in_title = True

    def handle_endtag(self, tag: str) -> None:
        if tag == "title":
            self._in_title = False

    def handle_data(self, data: str) -> None:
        if self._in_title:
            self.title += data


def _ok(val: Any) -> bool:
    return val is True or val in {"pass", "ok", "unfixable", "n/a"}


def parse_robots(text: str) -> dict[str, Any]:
    """Parse robots.txt for home disallow and AI-bot blocks."""
    body = text or ""
    sitemaps = re.findall(r"(?im)^\s*Sitemap:\s*(\S+)", body)
    # Collect rules per user-agent group (last match wins within a group).
    groups: list[tuple[list[str], list[tuple[str, str]]]] = []
    agents: list[str] = []
    rules: list[tuple[str, str]] = []

    def flush() -> None:
        nonlocal agents, rules
        if agents:
            groups.append((agents, rules))
        agents, rules = [], []

    for raw in body.splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        if ":" not in line:
            continue
        key, _, val = line.partition(":")
        key, val = key.strip().lower(), val.strip()
        if key == "user-agent":
            if rules and agents:
                flush()
            agents.append(val)
            rules = []
        elif key in {"disallow", "allow"} and agents:
            rules.append((key, val))
        elif key == "sitemap" and val:
            sitemaps.append(val)
    flush()

    def path_blocked(agent_name: str) -> bool:
        # Find most specific matching group; fall back to *.
        matched: list[tuple[str, str]] | None = None
        star: list[tuple[str, str]] | None = None
        for ags, rls in groups:
            lower = [a.lower() for a in ags]
            if agent_name.lower() in lower:
                matched = rls
            if "*" in lower:
                star = rls
        use = matched if matched is not None else star
        if not use:
            return False
        # Longest matching Disallow/Allow for "/".
        best_dis: str | None = None
        best_all: str | None = None
        for kind, path in use:
            if not path:
                continue
            if path == "/" or path.startswith("/"):
                if kind == "disallow":
                    if best_dis is None or len(path) >= len(best_dis):
                        best_dis = path
                else:
                    if best_all is None or len(path) >= len(best_all):
                        best_all = path
        if best_dis == "/" and (best_all is None or len(best_all) <= 1):
            return True
        return False

    home_blocked = path_blocked("*")
    blocked_bots = [b for b in AI_BOTS if path_blocked(b)]
    return {
        "sitemaps": sitemaps,
        "home_blocked": home_blocked,
        "blocked_ai_bots": blocked_bots,
        "ai_ok": not blocked_bots,
        "allows_home": not home_blocked,
    }


def heading_skips(levels: list[int]) -> list[tuple[int, int]]:
    skips: list[tuple[int, int]] = []
    prev = 0
    for level in levels:
        if prev and level > prev + 1:
            skips.append((prev, level))
        prev = level
    return skips


def check_page_html(html: str) -> dict[str, Any]:
    """On-page SEO rows from published HTML (not paste)."""
    outline = _Outline()
    try:
        outline.feed(html or "")
    except Exception:  # noqa: BLE001 - malformed HTML still yields partial rows
        pass
    title_m = TITLE.search(html or "")
    title = (outline.title or (title_m.group(1) if title_m else "")).strip()
    title = re.sub(r"\s+", " ", title)
    desc_m = META_DESC.search(html or "")
    desc = (desc_m.group(1) or desc_m.group(2) or "").strip() if desc_m else ""
    lang_m = HTML_LANG.search(html or "")
    lang = (lang_m.group(1) or "").strip() if lang_m else ""
    cans = []
    for m in CANONICAL.finditer(html or ""):
        cans.append(m.group(1) or m.group(2) or "")
    ogs = []
    for m in OG_IMAGE.finditer(html or ""):
        ogs.append(m.group(1) or m.group(2) or "")
    robots_vals = []
    for m in META_ROBOTS.finditer(html or ""):
        robots_vals.append((m.group(1) or m.group(2) or "").lower())
    noindex = any("noindex" in v for v in robots_vals)
    skips = heading_skips(outline.levels)
    return {
        "title": title,
        "title_ok": bool(title),
        "title_len": len(title),
        "meta_desc": desc,
        "meta_desc_ok": bool(desc),
        "meta_desc_len": len(desc),
        "html_lang": lang,
        "html_lang_ok": bool(lang),
        "canonicals": cans,
        "canonical_https": len(cans) == 1 and cans[0].lower().startswith("https://"),
        "og_images": ogs,
        "og_image_https": bool(ogs) and all(u.lower().startswith("https://") for u in ogs),
        "no_noindex_meta": not noindex,
        "h1_count": outline.h1,
        "h1_one": outline.h1 == 1,
        "heading_levels": outline.levels,
        "heading_order": not skips,
        "heading_skips": skips,
        "imgs_missing_alt": outline.imgs_missing_alt,
        "anchors": outline.anchors,
    }


def evaluate_bundle(bundle: dict[str, Any]) -> dict[str, Any]:
    """Fail-closed evaluation of a fetched or fixture bundle.

    Expected keys: final_url, page_html, page_headers (dict), robots_text,
    robots_status, sitemap_text, sitemap_status, llms_status, llms_text,
    link_statuses (dict path->int), missing_status (int|None),
    allow_og_unfixable (bool).
    """
    rows: dict[str, Any] = {}
    flags: list[str] = []
    notes: list[str] = []

    final_url = str(bundle.get("final_url") or "")
    parsed = urlparse(final_url)
    rows["https_final"] = parsed.scheme.lower() == "https"

    path = parsed.path or "/"
    if path not in {"", "/"} and SLUG_BAD.search(path):
        flags.append("slug_charset")

    robots_status = int(bundle.get("robots_status") or 0)
    robots_text = str(bundle.get("robots_text") or "")
    rows["robots_200"] = robots_status == 200 and bool(robots_text.strip())
    # Missing robots.txt fails robots_200 only; default crawl allow applies.
    robots = parse_robots(robots_text) if rows["robots_200"] else {
        "sitemaps": [], "home_blocked": False, "blocked_ai_bots": [],
        "ai_ok": True, "allows_home": True,
    }
    rows["robots_allows_home"] = bool(robots.get("allows_home"))
    rows["ai_crawlers_allowed"] = bool(robots.get("ai_ok"))
    if robots.get("blocked_ai_bots"):
        notes.append("blocked_ai:" + ",".join(robots["blocked_ai_bots"]))

    sm_status = int(bundle.get("sitemap_status") or 0)
    sm_text = str(bundle.get("sitemap_text") or "")
    rows["sitemap_200"] = sm_status == 200 and ("<urlset" in sm_text.lower() or "<sitemapindex" in sm_text.lower())

    headers = {str(k).lower(): str(v) for k, v in (bundle.get("page_headers") or {}).items()}
    xrobots = headers.get("x-robots-tag", "").lower()
    page = check_page_html(str(bundle.get("page_html") or ""))
    noindex_hdr = "noindex" in xrobots
    rows["no_noindex"] = bool(page["no_noindex_meta"]) and not noindex_hdr
    rows["canonical_https"] = bool(page["canonical_https"])
    rows["title_ok"] = bool(page["title_ok"])
    rows["meta_desc_ok"] = bool(page["meta_desc_ok"])
    rows["h1_one"] = bool(page["h1_one"])
    rows["heading_order"] = bool(page["heading_order"])
    rows["html_lang"] = bool(page["html_lang_ok"])

    if page["title_ok"] and not (30 <= page["title_len"] <= 60):
        flags.append("title_length")
    if page["meta_desc_ok"] and not (70 <= page["meta_desc_len"] <= 160):
        flags.append("meta_desc_length")
    if page["imgs_missing_alt"]:
        flags.append("img_alt_missing")

    allow_unfix = bool(bundle.get("allow_og_unfixable"))
    if page["og_image_https"]:
        rows["og_image_https"] = True
    elif allow_unfix and page["og_images"]:
        rows["og_image_https"] = "unfixable"
        notes.append("og_image_unfixable")
    elif allow_unfix and not page["og_images"]:
        rows["og_image_https"] = "unfixable"
        notes.append("og_image_absent_unfixable")
    else:
        rows["og_image_https"] = bool(page["og_image_https"])

    llms_status = int(bundle.get("llms_status") or 0)
    llms_text = str(bundle.get("llms_text") or "")
    rows["llms_txt"] = llms_status == 200 and bool(llms_text.strip())

    link_statuses = bundle.get("link_statuses") or {}
    sample_fail = [
        f"{p}:{c}" for p, c in link_statuses.items()
        if int(c) < 200 or int(c) >= 400
    ]
    # Empty sample (no same-origin links found) is ok.
    rows["links_sample_ok"] = not sample_fail

    missing_status = bundle.get("missing_status")
    if missing_status is None:
        rows["true_404"] = "n/a"
    else:
        rows["true_404"] = int(missing_status) == 404

    failed = [k for k in REQUIRED_ROWS if not _ok(rows.get(k))]
    missing = [k for k in REQUIRED_ROWS if k not in rows]
    status = "fail" if failed or missing else ("flag" if flags else "pass")
    return {
        "status": status,
        "rows": rows,
        "failed": failed,
        "missing": missing,
        "flags": flags,
        "notes": notes,
        "page": {
            "title": page["title"],
            "h1_count": page["h1_count"],
            "canonicals": page["canonicals"],
            "html_lang": page["html_lang"],
        },
        "robots": {
            "sitemaps": robots.get("sitemaps"),
            "blocked_ai_bots": robots.get("blocked_ai_bots"),
        },
        "required": list(REQUIRED_ROWS),
        "note": (
            "liveseo red: " + ",".join(failed or missing)
            if failed or missing
            else ("liveseo advisory flags: " + ",".join(flags) if flags else "liveseo green")
        ),
    }


def _curl(url: str, *, head: bool = False, timeout: int = 20) -> tuple[int, str, str, dict[str, str]]:
    """Return status, body, final_url, headers via curl."""
    curl = shutil.which("curl") or "curl"
    hdr_file = None
    try:
        import tempfile
        hdr_file = tempfile.NamedTemporaryFile("w+", delete=False)  # noqa: SIM115
        hdr_path = hdr_file.name
        hdr_file.close()
        cmd = [
            curl, "-sL", "--max-time", str(timeout), "-A", AGENT,
            "-D", hdr_path, "-o", "-", "-w", "%{http_code}\n%{url_effective}",
        ]
        if head:
            cmd.append("-I")
        cmd.append(url)
        result = subprocess.run(  # nosec B603
            cmd, capture_output=True, text=True, check=False,
        )
        out = result.stdout or ""
        # Trailing write-out: last two lines are code and url.
        lines = out.splitlines()
        if len(lines) >= 2:
            final = lines[-1].strip()
            code_s = lines[-2].strip()
            body = "\n".join(lines[:-2])
        else:
            final, code_s, body = url, "000", out
        try:
            code = int(code_s)
        except ValueError:
            code = 0
        headers: dict[str, str] = {}
        raw_h = Path(hdr_path).read_text(encoding="utf-8", errors="replace")
        for block in raw_h.split("\r\n\r\n"):
            for line in block.splitlines():
                if ":" in line and not line.lower().startswith("http/"):
                    k, _, v = line.partition(":")
                    headers[k.strip().lower()] = v.strip()
        return code, body, final, headers
    finally:
        if hdr_file is not None:
            try:
                Path(hdr_file.name).unlink(missing_ok=True)
            except OSError:
                pass


def fetch_bundle(url: str, *, link_sample: int = 5, missing_path: str = "/chaosengine-liveseo-missing-probe-404") -> dict[str, Any]:
    """Fetch a public URL into an evaluate_bundle input."""
    code, html, final, headers = _curl(url)
    parsed = urlparse(final if final.startswith("http") else url)
    origin = f"{parsed.scheme}://{parsed.netloc}"
    r_code, r_text, _, _ = _curl(urljoin(origin + "/", "robots.txt"))
    robots = parse_robots(r_text) if r_code == 200 else {"sitemaps": []}
    sm_url = (robots.get("sitemaps") or [urljoin(origin + "/", "sitemap.xml")])[0]
    s_code, s_text, _, _ = _curl(sm_url)
    l_code, l_text, _, _ = _curl(urljoin(origin + "/", "llms.txt"))
    page = check_page_html(html)
    link_statuses: dict[str, int] = {}
    n = 0
    for href in page["anchors"]:
        if n >= link_sample:
            break
        if href.startswith("#") or href.lower().startswith("mailto:"):
            continue
        abs_u = urljoin(final, href)
        if urlparse(abs_u).netloc != parsed.netloc:
            continue
        c, _, _, _ = _curl(abs_u, head=True)
        link_statuses[urlparse(abs_u).path or "/"] = c
        n += 1
    m_code, _, _, _ = _curl(urljoin(origin + "/", missing_path.lstrip("/")))
    return {
        "final_url": final if code else url,
        "page_status": code,
        "page_html": html if code == 200 else "",
        "page_headers": headers,
        "robots_status": r_code,
        "robots_text": r_text if r_code == 200 else "",
        "sitemap_status": s_code,
        "sitemap_text": s_text if s_code == 200 else "",
        "llms_status": l_code,
        "llms_text": l_text if l_code == 200 else "",
        "link_statuses": link_statuses,
        "missing_status": m_code,
    }


def load_fixture(folder: str | Path) -> dict[str, Any]:
    """Load an offline fixture directory for CI."""
    root = Path(folder)
    meta: dict[str, Any] = {}
    meta_path = root / "meta.json"
    if meta_path.is_file():
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
    def read(name: str) -> str:
        p = root / name
        return p.read_text(encoding="utf-8") if p.is_file() else ""
    robots = read("robots.txt")
    sitemap = read("sitemap.xml")
    llms = read("llms.txt")
    page = read("page.html")
    return {
        "final_url": meta.get("final_url", "https://example.com/"),
        "page_html": page,
        "page_headers": meta.get("headers") or {},
        "robots_status": meta.get("robots_status", 200 if robots else 0),
        "robots_text": robots,
        "sitemap_status": meta.get("sitemap_status", 200 if sitemap else 0),
        "sitemap_text": sitemap,
        "llms_status": meta.get("llms_status", 200 if llms else 0),
        "llms_text": llms,
        "link_statuses": meta.get("link_statuses") or {},
        "missing_status": meta.get("missing_status", 404),
        "allow_og_unfixable": bool(meta.get("allow_og_unfixable")),
    }
