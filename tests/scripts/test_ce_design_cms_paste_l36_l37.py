"""L36 CMS paste fragment-only + L37 no HTML comments gates."""

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
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"

L36 = "cms-paste-fragment-only.md"
L37 = "cms-paste-no-html-comments.md"

HEADER_VIEWPORTS = (360, 390, 430, 1440)

L36_NEEDLES = (
    "Fragment only",
    "<!doctype",
    "<html",
    "<head",
    "<body",
    "dir",
    "lang",
    "wrapper",
    "cmsmeta",
    "cmsheader",
    "1px",
    "documentElement",
    "360",
    "390",
    "430",
    "1440",
)
L37_NEEDLES = (
    "<!--",
    "HTML comments",
    "cmsmeta",
    "ops notes",
    "&lt;!--",
)


def run_qc(*args: str) -> tuple[int, dict]:
    proc = subprocess.run(  # nosec B603 - argv list, no shell.
        [sys.executable, str(QC), *args],
        capture_output=True,
        text=True,
        check=False,
    )
    payload: dict = {}
    if proc.stdout.strip():
        try:
            payload = json.loads(proc.stdout)
        except json.JSONDecodeError:
            payload = {"raw": proc.stdout, "stderr": proc.stderr}
    else:
        payload = {"stderr": proc.stderr}
    return proc.returncode, payload


def _vp_receipt(
    *,
    logo_page_x: float = 12.5,
    bad_width: int | None = None,
    home_content_width: float = 1440.0,
    page_content_width: float = 1440.0,
    nav_page_x: float | None = None,
) -> dict:
    viewports = {}
    for width in HEADER_VIEWPORTS:
        page_x = logo_page_x
        if bad_width is not None and width == bad_width:
            page_x = 20.0  # >1px shift
        nav_x = 200.0 if nav_page_x is None else nav_page_x
        viewports[str(width)] = {
            "home_logo": {"x": 12.0, "y": 8.0},
            "page_logo": {"x": page_x, "y": 8.0},
            "home_nav": {"x": 200.0, "y": 10.0},
            "page_nav": {"x": nav_x, "y": 10.25},
            "home_content_width": home_content_width,
            "page_content_width": page_content_width,
        }
    return {"viewports": viewports, "max_delta_px": 1.0}


def _live_base(**extra) -> dict:
    base = {
        "http_200": True,
        "no_dup_chrome": True,
        "assets_loaded": True,
        "heading_colour": "pass",
        "identity_tile": "pass",
        "og_title_once": True,
        "viewport_360": True,
        "viewport_390": True,
        "viewport_430": True,
        "viewport_1440": True,
        "media_ok": "n/a",
        "duration_ok": "n/a",
        "nested_containers_0": True,
    }
    base.update(extra)
    return base


class LessonCardBudgetTest(unittest.TestCase):
    def test_l36_l37_cards_index_inventory(self) -> None:
        for card, needles in ((L36, L36_NEEDLES), (L37, L37_NEEDLES)):
            raw = (LESSONS / card).read_text(encoding="utf-8")
            self.assertLessEqual(len(raw.encode("utf-8")), 6144, card)
            self.assertIn("## Why", raw)
            self.assertIn("## Verify", raw)
            text = " ".join(raw.split())
            for needle in needles:
                self.assertIn(needle, text, f"{card}:{needle}")
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L36 ", index)
        self.assertIn(f"]({L36})", index)
        self.assertIn("L37 ", index)
        self.assertIn(f"]({L37})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("L01-L37", inv)
        self.assertIn("37 lesson", inv)
        self.assertIn("L35", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("L36", craft)
        embed = (DESIGN / "references/site-builders/hosted-cms-embed.md").read_text(
            encoding="utf-8",
        )
        self.assertIn("L36", embed)
        self.assertIn("L37", embed)
        self.assertIn("cmsheader", embed)
        self.assertIn("360/390/430/1440", embed)
        web = DESIGN / "references/craft/web-design.md"
        self.assertLessEqual(len(web.read_text(encoding="utf-8").splitlines()), 120)

    def test_cards_project_neutral(self) -> None:
        banned = (
            "automatest", "mohab", "hrsd", "elmenus", "stc", "price",
            "carl", "coach", *private_terms(),
        )
        for card in (L36, L37):
            lowered = (LESSONS / card).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{card}:{word}")


class CmsPasteFragmentL36Test(unittest.TestCase):
    """cmsmeta fails full-doc shell paste; passes wrapper dir/lang fragment."""

    def test_cmsmeta_rejects_document_shell_tags(self) -> None:
        cases = {
            "doctype.html": "<!DOCTYPE html><div>x</div>",
            "html_dir.html": '<html dir="rtl"><div>x</div></html>',
            "head.html": "<head><meta charset=utf-8></head><div>x</div>",
            "body.html": "<body><div>x</div></body>",
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertEqual("fail", payload.get("status"), (name, payload))
                self.assertGreaterEqual(payload.get("doc_shell_hits", 0), 1, (name, payload))

    def test_cmsmeta_passes_wrapper_dir_lang_fragment(self) -> None:
        html = (
            '<div class="product-embed" dir="rtl" lang="ar">'
            "<style>.product-embed h1{color:#fff}</style>"
            "<h1>منتج</h1></div>"
        )
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "frag.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertEqual(0, payload.get("doc_shell_hits", -1), payload)
            self.assertEqual(0, payload.get("html_comment_hits", -1), payload)

    def test_cmsmeta_false_positives_escaped_and_attr(self) -> None:
        """Escaped &lt;html and '<html' inside attribute strings must PASS."""
        cases = {
            "escaped.html": (
                "<div><p>Never paste &lt;html dir=rtl&gt; into a Code block.</p></div>"
            ),
            "attr.html": (
                '<div data-tip="example &lt;html&gt; fragment" title=\'see &lt;head&gt;\'>'
                "<h1>Product</h1></div>"
            ),
            "attr_raw.html": (
                '<div title="do not use <html dir=rtl> here"><h1>Product</h1></div>'
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("doc_shell_hits", -1), (name, payload))

    def test_cmsmeta_false_positives_script_style_strings(self) -> None:
        """JSON-LD / JS template / CSS content strings must PASS (not real markup)."""
        cases = {
            "jsonld.html": (
                '<div class="e"><script type="application/ld+json">'
                '{"desc":"never paste <html dir=rtl> or <!-- notes --> into CMS"}'
                "</script><h1>x</h1></div>"
            ),
            "js_template.html": (
                '<div class="e"><script>'
                "const tip = `avoid <html> and <!-- ops --> in paste`;"
                "</script><h1>x</h1></div>"
            ),
            "css_content.html": (
                '<div class="e"><style>'
                '.e::before{content:"<!-- tip --> and <html>"}'
                "</style><h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("doc_shell_hits", -1), (name, payload))
                self.assertEqual(0, payload.get("html_comment_hits", -1), (name, payload))

    def test_cmsmeta_rejects_global_root_css(self) -> None:
        html = (
            '<div class="product-embed">'
            "<style>html{direction:rtl} body{margin:0}</style>"
            "<h1>x</h1></div>"
        )
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "css.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual(1, code, payload)
            self.assertGreaterEqual(payload.get("global_css_root_hits", 0), 1, payload)

    def test_cmsmeta_rejects_is_where_combinator_root_css(self) -> None:
        """FN guard: :is(html) / :where(html,body) / *+html must FAIL."""
        cases = {
            "is.html": "<div><style>:is(html){direction:rtl}</style><h1>x</h1></div>",
            "where.html": "<div><style>:where(html,body){margin:0}</style><h1>x</h1></div>",
            "plus.html": "<div><style>*+html{margin:0}</style><h1>x</h1></div>",
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertGreaterEqual(
                    payload.get("global_css_root_hits", 0), 1, (name, payload),
                )

    def test_cmsmeta_passes_css_comment_root_and_content_comment(self) -> None:
        """FP guard: commented root selectors and content:'<!--' must PASS."""
        cases = {
            "css_comment.html": (
                "<div><style>/* html{} body{} :root{} */.ok{color:red}</style>"
                "<h1>x</h1></div>"
            ),
            "content_sq.html": (
                "<div><style>.x::before{content:'<!--'}</style><h1>x</h1></div>"
            ),
            "content_dq.html": (
                '<div><style>.x::before{content:"<!--"}</style><h1>x</h1></div>'
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("global_css_root_hits", -1), (name, payload))
                self.assertEqual(0, payload.get("html_comment_hits", -1), (name, payload))

    def test_cmsmeta_passes_at_rule_name_root_idents(self) -> None:
        """C1 FP: at-rule names html/body are not type selectors."""
        cases = {
            "keyframes_body.html": (
                "<div><style>@keyframes body{0%{opacity:0}to{opacity:1}}</style>"
                "<h1>x</h1></div>"
            ),
            "keyframes_html.html": (
                "<div><style>@keyframes html{from{left:0}to{left:1px}}</style>"
                "<h1>x</h1></div>"
            ),
            "layer_body.html": (
                "<div><style>@layer body {.x{color:red}}</style><h1>x</h1></div>"
            ),
            "container_body.html": (
                "<div><style>@container body (min-width:1px){.x{color:red}}</style>"
                "<h1>x</h1></div>"
            ),
            "media_body.html": (
                "<div><style>@media body {.x{color:red}}</style><h1>x</h1></div>"
            ),
            "counter_style_body.html": (
                "<div><style>@counter-style body "
                "{system:cyclic;symbols:'*';}</style>"
                "<h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("global_css_root_hits", -1), (name, payload))

    def test_cmsmeta_rejects_root_inside_at_rule_blocks(self) -> None:
        """C1 FN: real type selectors inside at-rule blocks must FAIL."""
        cases = {
            "media_html.html": (
                "<div><style>@media all{html{direction:rtl}}</style><h1>x</h1></div>"
            ),
            "supports_body.html": (
                "<div><style>@supports (display:grid){body{margin:0}}</style>"
                "<h1>x</h1></div>"
            ),
            "layer_base_html.html": (
                "<div><style>@layer base{html{font-size:16px}}</style><h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertGreaterEqual(
                    payload.get("global_css_root_hits", 0), 1, (name, payload),
                )

    def test_cmsmeta_passes_css_function_arg_root_idents(self) -> None:
        """C3 FP: unquoted url()/image-set()/@import args are not selectors."""
        cases = {
            "url_body.html": (
                "<div><style>.x{background:url(body.svg)}</style><h1>x</h1></div>"
            ),
            "image_set_body.html": (
                "<div><style>.x{background:image-set(body 1x)}</style><h1>x</h1></div>"
            ),
            "import_url_body.html": (
                "<div><style>@import url(body.css);.x{color:red}</style>"
                "<h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("global_css_root_hits", -1), (name, payload))

    def test_cmsmeta_rejects_namespace_root_selectors(self) -> None:
        """C4 FN: |body / svg|html / *|body must FAIL."""
        cases = {
            "pipe_body.html": "<div><style>|body{margin:0}</style><h1>x</h1></div>",
            "svg_html.html": "<div><style>svg|html{color:red}</style><h1>x</h1></div>",
            "star_body.html": "<div><style>*|body{margin:0}</style><h1>x</h1></div>",
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertGreaterEqual(
                    payload.get("global_css_root_hits", 0), 1, (name, payload),
                )


    def test_cmsmeta_passes_class_id_attr_pseudo_root_idents(self) -> None:
        """N1 FP: class/id/attr/pseudo-arg html|body idents are not type selectors."""
        cases = {
            "class_body.html": "<div><style>.body{color:red}</style><h1>x</h1></div>",
            "id_body.html": "<div><style>#body{color:red}</style><h1>x</h1></div>",
            "class_html.html": "<div><style>.html{color:red}</style><h1>x</h1></div>",
            "id_html.html": "<div><style>#html{color:red}</style><h1>x</h1></div>",
            "attr_body.html": "<div><style>[body]{color:red}</style><h1>x</h1></div>",
            "attr_name_body.html": (
                "<div><style>[name=body]{color:red}</style><h1>x</h1></div>"
            ),
            "div_body.html": "<div><style>div.body{color:red}</style><h1>x</h1></div>",
            "not_class_body.html": (
                "<div><style>:not(.body){color:red}</style><h1>x</h1></div>"
            ),
            "href_star_body.html": (
                "<div><style>[href*=body]{color:red}</style><h1>x</h1></div>"
            ),
            "lang_body.html": (
                "<div><style>:lang(body){color:red}</style><h1>x</h1></div>"
            ),
            "part_body.html": (
                "<div><style>::part(body){color:red}</style><h1>x</h1></div>"
            ),
            "escape_class_body.html": (
                "<div><style>.\\62 ody{color:red}</style><h1>x</h1></div>"
            ),
            "escape_id_html.html": (
                "<div><style>#\\68 tml{color:red}</style><h1>x</h1></div>"
            ),
            "supports_selector_class_body.html": (
                "<div><style>@supports selector(.body){.x{}}</style>"
                "<h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("global_css_root_hits", -1), (name, payload))

    def test_cmsmeta_rejects_css_escaped_and_supports_selector_roots(self) -> None:
        """N2/N3 FN: CSS-escaped types + @supports selector(body) must FAIL."""
        cases = {
            "escape_body.html": (
                "<div><style>\\62 ody{margin:0}</style><h1>x</h1></div>"
            ),
            "escape_html.html": (
                "<div><style>h\\74 ml{margin:0}</style><h1>x</h1></div>"
            ),
            "body_upper.html": (
                "<div><style>BODY{margin:0}</style><h1>x</h1></div>"
            ),
            "supports_selector_body.html": (
                "<div><style>@supports selector(body){.x{color:red}}</style>"
                "<h1>x</h1></div>"
            ),
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertGreaterEqual(
                    payload.get("global_css_root_hits", 0), 1, (name, payload),
                )


class CmsPasteCommentsL37Test(unittest.TestCase):
    def test_cmsmeta_rejects_html_comments(self) -> None:
        html = "<!-- ops: re-check CTA -->\n<div class=\"product-embed\"><h1>x</h1></div>"
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "cmt.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual(1, code, payload)
            self.assertGreaterEqual(payload.get("html_comment_hits", 0), 1, payload)

    def test_cmsmeta_rejects_conditional_comment(self) -> None:
        """FN guard: IE conditional comments are real markup comments → FAIL."""
        html = "<!--[if IE]><div class=\"x\">legacy</div><![endif]-->\n<div>ok</div>"
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "cond.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertGreaterEqual(payload.get("html_comment_hits", 0), 1, payload)

    def test_cmsmeta_passes_rcdata_rawtext_comment_lookalikes(self) -> None:
        """C2 FP: <!-- --> inside RCDATA/rawtext is character data, not a comment."""
        cases = {
            "textarea.html": "<div><textarea><!-- not a comment --></textarea><h1>x</h1></div>",
            "title.html": "<div><title><!-- not a comment --></title><h1>x</h1></div>",
            "xmp.html": "<div><xmp><!-- x --></xmp><h1>x</h1></div>",
            "listing.html": "<div><listing><!-- x --></listing><h1>x</h1></div>",
            "noembed.html": "<div><noembed><!-- x --></noembed><h1>x</h1></div>",
            "noframes.html": "<div><noframes><!-- x --></noframes><h1>x</h1></div>",
            "plaintext.html": "<div><plaintext><!-- x",
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(
                    (0, "pass"), (code, payload.get("status")), (name, payload),
                )
                self.assertEqual(0, payload.get("html_comment_hits", -1), (name, payload))

    def test_cmsmeta_rejects_real_comments_in_noscript_iframe_template(self) -> None:
        """C2 intentional FAIL: noscript/iframe/template comments (paste hygiene).

        noscript/template: real Comment nodes. iframe: still L37-fail even when
        CPython CDATA_CONTENT_ELEMENTS treats iframe as raw text (≥3.13.17).
        """
        cases = {
            "noscript.html": "<div><noscript><!-- real --></noscript><h1>x</h1></div>",
            "iframe.html": "<div><iframe><!-- real --></iframe><h1>x</h1></div>",
            "template.html": "<div><template><!-- real --></template><h1>x</h1></div>",
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, html in cases.items():
                path = Path(folder) / name
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, (name, payload))
                self.assertEqual("fail", payload.get("status"), (name, payload))
                self.assertGreaterEqual(
                    payload.get("html_comment_hits", 0), 1, (name, payload),
                )

    def test_cmsmeta_rejects_iframe_comment_under_cdata_elements(self) -> None:
        """FN guard: iframe <!-- --> still fails when parser uses CDATA mode."""
        import html.parser as hp

        html = "<div><iframe><!-- real --></iframe><h1>x</h1></div>"
        old = hp.HTMLParser.CDATA_CONTENT_ELEMENTS
        hp.HTMLParser.CDATA_CONTENT_ELEMENTS = (
            "script", "style", "xmp", "iframe", "noembed", "noframes",
        )
        try:
            with tempfile.TemporaryDirectory() as folder:
                path = Path(folder) / "iframe_cdata.html"
                path.write_text(html, encoding="utf-8")
                code, payload = run_qc("cmsmeta", str(path))
                self.assertEqual(1, code, payload)
                self.assertEqual("fail", payload.get("status"), payload)
                self.assertGreaterEqual(
                    payload.get("html_comment_hits", 0), 1, payload,
                )
        finally:
            hp.HTMLParser.CDATA_CONTENT_ELEMENTS = old

    def test_cmsmeta_passes_escaped_comment_text(self) -> None:
        html = "<div><p>Do not leave &lt;!-- notes --&gt; in paste.</p></div>"
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "esc.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertEqual(0, payload.get("html_comment_hits", -1), payload)


class CmsHeaderDeltaL36Test(unittest.TestCase):
    def test_cmsheader_delta_threshold_per_viewport(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            ok_path = Path(folder) / "ok.json"
            bad_path = Path(folder) / "bad.json"
            ok_path.write_text(json.dumps(_vp_receipt()), encoding="utf-8")
            bad_path.write_text(
                json.dumps(_vp_receipt(bad_width=390)), encoding="utf-8",
            )
            code, payload = run_qc("cmsheader", str(ok_path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            for width in HEADER_VIEWPORTS:
                self.assertTrue(
                    payload.get(f"header_logo_delta_ok_{width}"), (width, payload),
                )
                self.assertTrue(
                    payload.get(f"header_nav_delta_ok_{width}"), (width, payload),
                )
            code, payload = run_qc("cmsheader", str(bad_path))
            self.assertEqual(1, code, payload)
            self.assertFalse(payload.get("header_logo_delta_ok_390"), payload)
            self.assertTrue(payload.get("header_nav_delta_ok_390"), payload)
            self.assertTrue(payload.get("header_logo_delta_ok_360"), payload)

    def test_livecms_requires_per_viewport_header_delta_rows(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "live.json"
            path.write_text(json.dumps(_live_base()), encoding="utf-8")
            code, payload = run_qc("livecms", str(path))
            self.assertEqual(1, code, payload)
            missing = set(payload.get("missing") or [])
            for width in HEADER_VIEWPORTS:
                self.assertIn(f"header_logo_delta_ok_{width}", missing)
                self.assertIn(f"header_nav_delta_ok_{width}", missing)
            self.assertFalse(payload.get("header_measure_trusted"), payload)

    def test_livecms_rejects_forged_header_ok_booleans(self) -> None:
        """D1: forged header_*_delta_ok:true without measure must not pass."""
        forged = _live_base()
        for width in HEADER_VIEWPORTS:
            forged[f"header_logo_delta_ok_{width}"] = True
            forged[f"header_nav_delta_ok_{width}"] = True
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "forged.json"
            path.write_text(json.dumps(forged), encoding="utf-8")
            code, payload = run_qc("livecms", str(path))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertFalse(payload.get("header_measure_trusted"), payload)
            missing = set(payload.get("missing") or [])
            self.assertTrue(any(k.startswith("header_") for k in missing), payload)

    def test_livecms_passes_with_header_measure(self) -> None:
        receipt = _live_base(header_measure=_vp_receipt())
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "ok.json"
            path.write_text(json.dumps(receipt), encoding="utf-8")
            code, payload = run_qc("livecms", str(path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertTrue(payload.get("header_measure_trusted"), payload)

    def test_cmsheader_scrollbar_gutter_normalized(self) -> None:
        """D4: 15px nav shift with +15 content width (scrollbar) must PASS."""
        receipt = _vp_receipt(
            logo_page_x=57.59,
            nav_page_x=953.88,
            home_content_width=1425.0,
            page_content_width=1440.0,
        )
        # overwrite home coords to match live automatest.org sample
        for node in receipt["viewports"].values():
            node["home_logo"] = {"x": 57.59, "y": 50.30}
            node["page_logo"] = {"x": 57.59, "y": 50.30}
            node["home_nav"] = {"x": 938.88, "y": 52.16}
            node["page_nav"] = {"x": 953.88, "y": 52.16}
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "gutter.json"
            path.write_text(json.dumps(receipt), encoding="utf-8")
            code, payload = run_qc("cmsheader", str(path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertTrue(payload.get("header_nav_delta_ok_1440"), payload)


if __name__ == "__main__":
    unittest.main()
