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
        self.assertIn("L36-L37", inv)
        self.assertIn("L34/L35 reserved", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("L36", craft)
        embed = (DESIGN / "references/site-builders/hosted-cms-embed.md").read_text(
            encoding="utf-8",
        )
        self.assertIn("L36", embed)
        self.assertIn("L37", embed)
        self.assertIn("cmsheader", embed)
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
    """Riskiest assumption: cmsmeta fails a full-doc <html dir=rtl> paste and
    passes a wrapper fragment with dir/lang on the embed root."""

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
            # Attribute contains raw <html — masked, must PASS.
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


class CmsPasteCommentsL37Test(unittest.TestCase):
    def test_cmsmeta_rejects_html_comments(self) -> None:
        html = "<!-- ops: re-check CTA -->\n<div class=\"product-embed\"><h1>x</h1></div>"
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "cmt.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual(1, code, payload)
            self.assertGreaterEqual(payload.get("html_comment_hits", 0), 1, payload)

    def test_cmsmeta_passes_escaped_comment_text(self) -> None:
        html = "<div><p>Do not leave &lt;!-- notes --&gt; in paste.</p></div>"
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "esc.html"
            path.write_text(html, encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(path))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertEqual(0, payload.get("html_comment_hits", -1), payload)


class CmsHeaderDeltaL36Test(unittest.TestCase):
    def test_cmsheader_delta_threshold(self) -> None:
        # Within 1px → PASS
        code, payload = run_qc(
            "cmsheader",
            "--home-logo-x", "12", "--home-logo-y", "8",
            "--page-logo-x", "12.5", "--page-logo-y", "8",
            "--home-nav-x", "200", "--home-nav-y", "10",
            "--page-nav-x", "200", "--page-nav-y", "10.25",
        )
        self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
        # Logo shifted 2px → FAIL
        code, payload = run_qc(
            "cmsheader",
            "--home-logo-x", "12", "--home-logo-y", "8",
            "--page-logo-x", "14", "--page-logo-y", "8",
            "--home-nav-x", "200", "--home-nav-y", "10",
            "--page-nav-x", "200", "--page-nav-y", "10",
        )
        self.assertEqual(1, code, payload)
        self.assertFalse(payload.get("header_logo_delta_ok"), payload)
        self.assertTrue(payload.get("header_nav_delta_ok"), payload)

    def test_livecms_requires_header_delta_rows(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "live.json"
            path.write_text(json.dumps({
                "http_200": True,
                "no_dup_chrome": True,
                "assets_loaded": True,
                "heading_colour": "pass",
                "identity_tile": "pass",
                "og_title_once": True,
                "viewport_390": True,
                "viewport_1440": True,
                "media_ok": "n/a",
                "duration_ok": "n/a",
                "nested_containers_0": True,
            }), encoding="utf-8")
            code, payload = run_qc("livecms", str(path))
            self.assertEqual(1, code, payload)
            missing = set(payload.get("missing") or [])
            self.assertIn("header_logo_delta_ok", missing)
            self.assertIn("header_nav_delta_ok", missing)


if __name__ == "__main__":
    unittest.main()
