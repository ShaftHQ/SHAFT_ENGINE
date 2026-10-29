"""
Cheap public-site SEO + AI-answer-engine gates (curl/HTML; no browser).

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
    "jsonld_ok",
)

AGENT = (
    "Mozilla/5.0 (compatible; ChaosEngine-liveseo/1.0)"
)
WRITEOUT_MARK = "---CE_LIVESEO_WRITEOUT---"

TITLE = re.compile(r"<title[^>]*>(.*?)</title>", re.IGNORECASE | re.DOTALL)
SLUG_BAD = re.compile(r"[A-Z_\s]|%20")
LD_JSON_SCRIPT = re.compile(
    r"""<script\b[^>]*\btype\s*=\s*["']application/ld\+json["'][^>]*>(.*?)</script>""",
    re.IGNORECASE | re.DOTALL,
)


class _Outline(HTMLParser):
    """Lenient html.parser extract of outline + head SEO attrs (quoted or not)."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.h1 = 0
        self.levels: list[int] = []
        self.imgs_missing_alt = 0
        self.anchors: list[str] = []
        self._in_title = False
        self.title = ""
        self.html_lang = ""
        self.canonicals: list[str] = []
        self.meta_desc = ""
        self.og_images: list[str] = []
        self.robots_vals: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        ad = {k.lower(): (v or "") for k, v in attrs}
        if tag == "html" and ad.get("lang") and not self.html_lang:
            self.html_lang = ad["lang"].strip()
        elif tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
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
        elif tag == "link":
            rels = {p.strip().lower() for p in ad.get("rel", "").split()}
            href = ad.get("href", "").strip()
            if "canonical" in rels and href:
                self.canonicals.append(href)
        elif tag == "meta":
            name = ad.get("name", "").strip().lower()
            prop = ad.get("property", "").strip().lower()
            content = ad.get("content", "").strip()
            if name == "description" and content and not self.meta_desc:
                self.meta_desc = content
            elif name == "robots" and content:
                self.robots_vals.append(content.lower())
            # Exact og:image only — not og:image:width / :height / :secure_url.
            elif prop == "og:image" and content:
                self.og_images.append(content)

    def handle_endtag(self, tag: str) -> None:
        if tag == "title":
            self._in_title = False

    def handle_data(self, data: str) -> None:
        if self._in_title:
            self.title += data


def _ok(val: Any) -> bool:
    return val is True or val in {"pass", "ok", "unfixable", "n/a"}


def _robots_groups(body: str) -> tuple[list[str], list[tuple[list[str], list[tuple[str, str]]]]]:
    """Split robots.txt into sitemap URLs and (agents, rules) groups."""
    sitemaps = re.findall(r"(?im)^\s*Sitemap:\s*(\S+)", body)
    groups: list[tuple[list[str], list[tuple[str, str]]]] = []
    agents: list[str] = []
    rules: list[tuple[str, str]] = []
    for raw in body.splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line or ":" not in line:
            continue
        key, _, val = line.partition(":")
        key, val = key.strip().lower(), val.strip()
        if key == "user-agent":
            if rules and agents:
                groups.append((agents, rules))
                agents, rules = [val], []
            else:
                agents.append(val)
        elif key in {"disallow", "allow"} and agents:
            rules.append((key, val))
        elif key == "sitemap" and val:
            sitemaps.append(val)
    if agents:
        groups.append((agents, rules))
    return sitemaps, groups


def _rules_for_agent(
    groups: list[tuple[list[str], list[tuple[str, str]]]],
    agent_name: str,
) -> list[tuple[str, str]] | None:
    """Pick the agent-specific rule list, else the ``*`` group."""
    matched: list[tuple[str, str]] | None = None
    star: list[tuple[str, str]] | None = None
    needle = agent_name.lower()
    for ags, rls in groups:
        lower = [a.lower() for a in ags]
        if needle in lower:
            matched = rls
        if "*" in lower:
            star = rls
    if matched is not None:
        return matched
    return star


def _home_disallowed(rules: list[tuple[str, str]] | None) -> bool:
    """True when Disallow:/ wins over Allow for the site root."""
    if not rules:
        return False
    best_dis: str | None = None
    best_all: str | None = None
    for kind, path in rules:
        if not path or not (path == "/" or path.startswith("/")):
            continue
        if kind == "disallow":
            if best_dis is None or len(path) >= len(best_dis):
                best_dis = path
        elif best_all is None or len(path) >= len(best_all):
            best_all = path
    return best_dis == "/" and (best_all is None or len(best_all) <= 1)


def parse_robots(text: str) -> dict[str, Any]:
    """
    Parse robots.txt for home disallow and AI-bot blocks.
    """
    sitemaps, groups = _robots_groups(text or "")

    def path_blocked(agent_name: str) -> bool:
        return _home_disallowed(_rules_for_agent(groups, agent_name))

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


def _jsonld_node_has_type(node: Any) -> bool:
    """True when a JSON-LD node (dict/list/@graph) exposes @type."""
    if isinstance(node, list):
        return bool(node) and all(_jsonld_node_has_type(item) for item in node)
    if not isinstance(node, dict):
        return False
    if node.get("@type"):
        return True
    if "@graph" in node:
        return _jsonld_node_has_type(node.get("@graph"))
    return False


def check_jsonld(html: str) -> dict[str, Any]:
    """
    Validate every application/ld+json script: parseable JSON with @type.
    """
    blocks = LD_JSON_SCRIPT.findall(html or "")
    fails: list[str] = []
    for idx, raw in enumerate(blocks):
        try:
            data = json.loads(raw)
        except json.JSONDecodeError as exc:
            fails.append(f"invalid_json:{idx}:{exc.msg}")
            continue
        if not _jsonld_node_has_type(data):
            fails.append(f"missing_type:{idx}")
    ok = not fails
    return {
        "count": len(blocks),
        "fails": fails,
        "jsonld_ok": ok,
        "status": "pass" if ok else "fail",
        "note": (
            "json-ld ok" if ok else "json-ld invalid or missing @type: " + ",".join(fails)
        ),
    }


def check_page_html(html: str) -> dict[str, Any]:
    """
    On-page SEO rows from published HTML (not paste).

    Head attrs (canonical, description, og:image, robots, lang) come from
    html.parser so quoted and unquoted markup both work (Docusaurus).
    """
    outline = _Outline()
    # html.parser is lenient; feed never raises on typical malformed markup.
    outline.feed(html or "")
    title_m = TITLE.search(html or "")
    title = (outline.title or (title_m.group(1) if title_m else "")).strip()
    title = re.sub(r"\s+", " ", title)
    desc = outline.meta_desc
    lang = outline.html_lang
    cans = list(outline.canonicals)
    ogs = list(outline.og_images)
    robots_vals = list(outline.robots_vals)
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
    """
    Fail-closed evaluation of a fetched or fixture bundle.

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

    jsonld = check_jsonld(str(bundle.get("page_html") or ""))
    rows["jsonld_ok"] = bool(jsonld["jsonld_ok"])
    if jsonld.get("fails"):
        notes.append("jsonld:" + ",".join(jsonld["fails"][:5]))

    if page["title_ok"] and not (30 <= page["title_len"] <= 60):
        flags.append("title_length")
    if page["meta_desc_ok"] and not (70 <= page["meta_desc_len"] <= 160):
        flags.append("meta_desc_length")
    if page["imgs_missing_alt"]:
        flags.append("img_alt_missing")

    allow_unfix = bool(bundle.get("allow_og_unfixable"))
    if page["og_image_https"]:
        rows["og_image_https"] = True
    elif page["og_images"]:
        # http (plan-locked) social images: UNFIXABLE / WARN, not hard FAIL (card §14).
        rows["og_image_https"] = "unfixable"
        notes.append("og_image_unfixable")
        flags.append("og_image_http")
    elif allow_unfix:
        rows["og_image_https"] = "unfixable"
        notes.append("og_image_absent_unfixable")
    else:
        rows["og_image_https"] = False

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


def _split_curl_writeout(out: str, fallback_url: str) -> tuple[int, str, str]:
    """
    Split curl stdout into (status, body, final_url) using WRITEOUT_MARK.

    Bodies that omit a trailing newline must not glue into the status code.
    """
    text = out or ""
    idx = text.rfind(WRITEOUT_MARK)
    if idx < 0:
        return 0, text, fallback_url
    body = text[:idx]
    if body.endswith("\n"):
        body = body[:-1]
    lines = text[idx + len(WRITEOUT_MARK) :].splitlines()
    code_s = lines[0].strip() if lines else "000"
    final = lines[1].strip() if len(lines) > 1 else fallback_url
    try:
        code = int(code_s)
    except ValueError:
        code = 0
    return code, body, final or fallback_url


def _curl(url: str, *, head: bool = False, timeout: int = 20) -> tuple[int, str, str, dict[str, str]]:
    """
    Return status, body, final_url, headers via curl.
    """
    curl = shutil.which("curl") or "curl"
    hdr_file = None
    try:
        import tempfile
        hdr_file = tempfile.NamedTemporaryFile("w+", delete=False)  # noqa: SIM115
        hdr_path = hdr_file.name
        hdr_file.close()
        cmd = [
            curl, "-sL", "--max-time", str(timeout), "-A", AGENT,
            "-D", hdr_path, "-o", "-",
            "-w", f"\n{WRITEOUT_MARK}%{{http_code}}\n%{{url_effective}}",
        ]
        if head:
            cmd.append("-I")
        cmd.append(url)
        result = subprocess.run(  # nosec B603
            cmd, capture_output=True, text=True, check=False,
        )
        code, body, final = _split_curl_writeout(result.stdout or "", url)
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
                # Temp header dump is best-effort cleanup after curl.
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
