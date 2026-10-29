"""W16 SEO + AI-answer-engine card and liveseo gate (#liveseo)."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
WEB = DESIGN / "references/web-craft"
QC = DESIGN / "scripts/design_qc.py"
GATES = DESIGN / "scripts/liveseo_gates.py"

CARD = "seo-basics.md"
NEEDLES = (
    "sitemap.xml",
    "robots.txt",
    "noindex",
    "canonical",
    "Exactly one",
    "html lang",
    "llms.txt",
    "GPTBot",
    "OAI-SearchBot",
    "Google-Extended",
    "PerplexityBot",
    "ClaudeBot",
    "without JavaScript",
    "Search Console",
    "liveseo",
    "cmsmeta",
    "UNFIXABLE",
)


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run(  # nosec B603
        [sys.executable, str(QC), *args],
        capture_output=True, text=True, check=False,
    )
    lines = [ln for ln in result.stdout.strip().splitlines() if ln.strip()]
    payload = json.loads(lines[-1]) if lines else {
        "status": "error", "stdout": result.stdout, "stderr": result.stderr,
    }
    return result.returncode, payload


def _write_fixture(folder: Path, *, good: bool) -> Path:
    root = folder / ("good" if good else "bad")
    root.mkdir()
    if good:
        (root / "robots.txt").write_text(
            "User-agent: *\nAllow: /\nSitemap: https://example.com/sitemap.xml\n",
            encoding="utf-8",
        )
        (root / "sitemap.xml").write_text(
            '<?xml version="1.0"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'
            "<url><loc>https://example.com/</loc></url></urlset>",
            encoding="utf-8",
        )
        (root / "llms.txt").write_text(
            "# Example\n> Example is a demo product.\n", encoding="utf-8",
        )
        (root / "page.html").write_text(
            '<!doctype html><html lang="en"><head>'
            "<title>Example product home page title</title>"
            '<meta name="description" content="'
            "Example helps teams ship verified releases with measurable gates "
            'and short quotable answers for search and AI.">'
            '<link rel="canonical" href="https://example.com/">'
            '<meta property="og:image" content="https://example.com/og.png">'
            '<script type="application/ld+json">'
            '{"@context":"https://schema.org","@type":"Organization","name":"Example"}'
            "</script>"
            "</head><body><h1>Example product</h1><h2>Why</h2>"
            '<p>Example helps teams ship verified releases.</p>'
            '<a href="/docs">Docs</a><img src="/a.png" alt="diagram"></body></html>',
            encoding="utf-8",
        )
        (root / "meta.json").write_text(json.dumps({
            "final_url": "https://example.com/",
            "headers": {},
            "link_statuses": {"/docs": 200},
            "missing_status": 404,
        }), encoding="utf-8")
    else:
        # Riskiest assumption: noindex + Disallow:/ must fail closed.
        (root / "robots.txt").write_text(
            "User-agent: *\nDisallow: /\n\nUser-agent: GPTBot\nDisallow: /\n",
            encoding="utf-8",
        )
        (root / "page.html").write_text(
            "<!doctype html><html><head>"
            "<title>x</title>"
            '<meta name="robots" content="noindex,nofollow">'
            "</head><body><h1>One</h1><h3>Skip</h3></body></html>",
            encoding="utf-8",
        )
        (root / "meta.json").write_text(json.dumps({
            "final_url": "http://example.com/Bad_Slug",
            "headers": {"X-Robots-Tag": "noindex"},
            "robots_status": 200,
            "sitemap_status": 0,
            "llms_status": 0,
            "link_statuses": {},
            "missing_status": 200,
        }), encoding="utf-8")
    return root


class W16CardTest(unittest.TestCase):
    def test_card_needles_budget_index(self) -> None:
        raw = (WEB / CARD).read_text(encoding="utf-8")
        self.assertTrue(raw.startswith("---\nname: seo-basics\ndescription: Use when"))
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        self.assertLessEqual(len(raw.splitlines()), 120)
        for section in ("## Use when", "## Crawl", "## On-page", "## Verify", "## Sources"):
            self.assertIn(section, raw)
        text = " ".join(raw.split())
        for needle in NEEDLES:
            self.assertIn(needle, text)
        index = (WEB / "index.md").read_text(encoding="utf-8")
        self.assertIn("(seo-basics.md)", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("16 web-craft cards (W01-W16", inv)
        desc = (DESIGN / "addon.json").read_text(encoding="utf-8")
        self.assertIn("sixteen web-craft cards", desc)
        self.assertIn("SEO and AI-answer-engine basics", desc)
        merge = (DESIGN / "references/site-builders/site-merge-redirects.md").read_text(
            encoding="utf-8",
        )
        self.assertIn("liveseo", merge)
        self.assertIn("seo-basics.md", merge)

    def test_card_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", "carl", *private_terms())
        lowered = (WEB / CARD).read_text(encoding="utf-8").lower()
        for word in banned:
            self.assertNotIn(word, lowered)


class LiveseoGateTest(unittest.TestCase):
    """Riskiest assumption: noindex / Disallow:/ fail closed on published HTML."""

    def test_liveseo_fail_closed_on_noindex_and_robots(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = _write_fixture(Path(folder), good=False)
            code, payload = run_qc("liveseo", "--fixture", str(bad))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            failed = set(payload.get("failed") or [])
            for key in ("no_noindex", "robots_allows_home", "ai_crawlers_allowed", "https_final"):
                self.assertIn(key, failed, payload)

    def test_liveseo_passes_clean_fixture(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            good = _write_fixture(Path(folder), good=True)
            code, payload = run_qc("liveseo", "--fixture", str(good))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            rows = payload.get("rows") or {}
            self.assertTrue(rows.get("llms_txt"))
            self.assertTrue(rows.get("h1_one"))
            self.assertTrue(rows.get("ai_crawlers_allowed"))

    def test_heading_skip_and_module_import(self) -> None:
        self.assertTrue(GATES.is_file())
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        page = mod.check_page_html(
            "<html lang='en'><body><h1>A</h1><h3>Skip</h3></body></html>",
        )
        self.assertFalse(page["heading_order"])
        robots = mod.parse_robots("User-agent: GPTBot\nDisallow: /\n")
        self.assertIn("GPTBot", robots["blocked_ai_bots"])


    def test_jsonld_requires_type_and_rejects_invalid(self) -> None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        ok = mod.check_jsonld(
            '<script type="application/ld+json">'
            '{"@graph":[{"@type":"WebPage","name":"Home"}]}'
            "</script>"
        )
        self.assertTrue(ok["jsonld_ok"], ok)
        bad_json = mod.check_jsonld(
            '<script type="application/ld+json">{not-json</script>'
        )
        self.assertFalse(bad_json["jsonld_ok"], bad_json)
        missing = mod.check_jsonld(
            '<script type="application/ld+json">{"name":"X"}</script>'
        )
        self.assertFalse(missing["jsonld_ok"], missing)
        # No JSON-LD scripts is fine.
        self.assertTrue(mod.check_jsonld("<html></html>")["jsonld_ok"])

    def test_liveseo_fails_on_invalid_jsonld_fixture(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            good = _write_fixture(Path(folder), good=True)
            # Poison the good page with invalid JSON-LD.
            page = (good / "page.html").read_text(encoding="utf-8")
            page = page.replace(
                "</head>",
                '<script type="application/ld+json">{broken</script></head>',
                1,
            )
            (good / "page.html").write_text(page, encoding="utf-8")
            code, payload = run_qc("liveseo", "--fixture", str(good))
            self.assertEqual(1, code, payload)
            self.assertIn("jsonld_ok", set(payload.get("failed") or []), payload)



    def test_split_curl_writeout_without_trailing_newline(self) -> None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        mark = mod.WRITEOUT_MARK
        # Body with no trailing newline before write-out (old parser glued status).
        glued = "</body></html>" + mark + "200\nhttps://shafthq.github.io/"
        code, body, final = mod._split_curl_writeout(glued, "https://x")
        self.assertEqual(200, code)
        self.assertTrue(body.endswith("</html>"), body)
        self.assertIn("shafthq.github.io", final)
        # Normal case with newline before marker (curl -w "\nMARK...").
        normal = "</body></html>\n" + mark + "200\nhttps://shafthq.github.io/"
        code, body, final = mod._split_curl_writeout(normal, "https://x")
        self.assertEqual(200, code)
        self.assertTrue(body.endswith("</html>"), body)

    def test_head_attrs_quoted_unquoted_and_og_image_width_decoy(self) -> None:
        """Docusaurus-like unquoted attrs; ignore og:image:width decoy."""
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        quoted = (
            '<html lang="en"><head>'
            '<link rel="canonical" href="https://example.com/q">'
            '<meta name="description" content="Quoted description for search.">'
            '<meta property="og:image:width" content="1200">'
            '<meta property="og:image" content="https://example.com/og-q.png">'
            '<meta property="og:image:height" content="630">'
            "</head><body><h1>H</h1></body></html>"
        )
        unquoted = (
            "<html lang=en-US dir=ltr><head>"
            "<link data-rh=true rel=canonical href=https://example.com/u />"
            '<meta data-rh=true name=description content="Unquoted desc for AI."/>'
            "<meta property=og:image:width content=1200>"
            "<meta property=og:image "
            "content=https://example.com/og-u.png>"
            "<meta property=og:image:height content=630>"
            "</head><body><h1>H</h1></body></html>"
        )
        for label, html, want_can, want_og, want_desc, want_lang in (
            ("quoted", quoted, "https://example.com/q",
             "https://example.com/og-q.png",
             "Quoted description for search.", "en"),
            ("unquoted", unquoted, "https://example.com/u",
             "https://example.com/og-u.png",
             "Unquoted desc for AI.", "en-US"),
        ):
            page = mod.check_page_html(html)
            self.assertTrue(page["canonical_https"], (label, page))
            self.assertEqual([want_can], page["canonicals"], label)
            self.assertTrue(page["meta_desc_ok"], (label, page))
            self.assertEqual(want_desc, page["meta_desc"], label)
            self.assertTrue(page["og_image_https"], (label, page))
            self.assertEqual([want_og], page["og_images"], label)
            self.assertNotIn("1200", page["og_images"], label)
            self.assertNotIn("630", page["og_images"], label)
            self.assertEqual(want_lang, page["html_lang"], label)

        # robots meta unquoted noindex still detected
        robots = mod.check_page_html(
            "<html lang=en><head>"
            "<meta name=robots content=noindex,nofollow>"
            "</head><body><h1>H</h1></body></html>",
        )
        self.assertFalse(robots["no_noindex_meta"], robots)

    def test_html_lang_accepts_unquoted(self) -> None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        page = mod.check_page_html(
            "<html lang=en-US><head><title>T</title></head>"
            "<body><h1>H</h1></body></html>",
        )
        self.assertTrue(page["html_lang_ok"], page)
        self.assertEqual("en-US", page["html_lang"])

    def test_http_og_image_is_unfixable_not_fail(self) -> None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("liveseo_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        bundle = {
            "final_url": "https://www.example.com/",
            "page_html": (
                "<html lang=en><head>"
                "<title>Example product home page title here</title>"
                '<meta name="description" content="' + ("d" * 80) + '">'
                '<link rel="canonical" href="https://www.example.com/">'
                '<meta property="og:image" '
                'content="http://static1.squarespace.com/og.png">'
                '<script type="application/ld+json">'
                '{"@type":"Organization","name":"Example"}'
                "</script></head><body><h1>Example</h1><h2>Why</h2></body></html>"
            ),
            "page_headers": {},
            "robots_status": 200,
            "robots_text": "User-agent: *\nAllow: /\n",
            "sitemap_status": 200,
            "sitemap_text": "<urlset></urlset>",
            "llms_status": 200,
            "llms_text": "# Example\n",
            "link_statuses": {},
            "missing_status": 404,
        }
        rep = mod.evaluate_bundle(bundle)
        self.assertEqual("unfixable", rep["rows"]["og_image_https"], rep)
        self.assertNotIn("og_image_https", rep.get("failed") or [], rep)
        self.assertIn("og_image_http", rep.get("flags") or [], rep)
        self.assertIn(rep.get("status"), {"pass", "flag"}, rep)


if __name__ == "__main__":
    unittest.main()
