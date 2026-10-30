"""Design-skills batch 3: V07/V08 video craft, W06 measured type, L07/S03 Arabic and eight design_qc checks (#7353)."""

from __future__ import annotations

import json
import shutil
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
import zipfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
REF = DESIGN / "references"
QC = DESIGN / "scripts/design_qc.py"
HAS_FFMPEG = bool(shutil.which("ffmpeg") and shutil.which("ffprobe"))
HYPERFRAMES = "1a238171ec55896a9cb8452c33e4e0fe1063a60c"
PINS = (HYPERFRAMES, "019ab7c199154f4509b55f12d84a4f5b578dc606", "08d3b9d14c2d1719c0b96f703bced8dccfe2076a",
        "fc05be36316e5e5ea2ee0b5ec51519b411d54665", "dbd4588f9e1033efb41dad4bef2f7947c8993d44",
        "aed9cfd3277740755f6bfc1155c7aa645403b760", "c172996495bed0fcd26896a9416b2093fd7073f0",
        "9a6930cf84a822eb458624bd11c61aac5bbdf224", "0e81f040e16d48fcc61d3e1b7ea548542371321f",
        "afa8da942115f2961fdbfa80807ea0b232ff6c00", "0e181866f095b5f06d005133876e926515390d95",
        "8c85352d5d75d4dcbe58480bd138e37b9742bab1", "f83af0bc1d937c8119099a11f8ebbf5e6fb99819",
        "1567581c26ec29f4216c6e6835415bf30343b0e3", "fd1629067909ff55b36b905476de4e5f26062a1f",
        "33d3dea41a341b341d4f5da2e8801ae0c89be2b8", "593377bb048752abb0152deab9fbb1083a6087df",
        "ca8c888abeffa8616b611032510c631d43a263ef", "412f6318084d33b4946f31f06849a34bbb49051a")
NEEDLES = {
    "video-craft/shorts-safe-area-captions.md": (
        "top 168 px", "y 168-1330", "right action rail", "never black, never a logo", "0.1-0.3 s in", "28 px",
        "about 5 English words", "2-3 words high energy", "pause of 150 ms", "fade 0.12 s", "never both",
        "under 60 `screen`", "60-180 `overlay`", "30-40% scrim", "`silencedetect=noise=-35dB:d=0.6`", "20%",
        "by position, not text", "multilingual model", "burned-in subtitles", "30-55% covered", "80% covered",
        "dies after about 8 s", "one card per 6-8 s", "ffprobe media length", "`-g <fps>`", HYPERFRAMES, "Apache-2.0",
        "design_qc.py safezones", "design_qc.py textfloor", "design_qc.py firstframe", "design_qc.py capluma",
        "design_qc.py mediaclamp", "contrast_gate.json", "Frame-measured contrast", "Shorts burned-caption",
        "Hook→caption chrome family", "~0.8 / 1.5 / 1.7 s"),
    "video-craft/film-review-sound.md": (
        "`t=4.2s · where · problem -> exact change · what stays`", "P0/P1/P2", "never a score out of 10",
        "shorter than its read time", "3 s with nothing changing", "2.6%", "empty first frame", "over 15 s with no sound",
        "-0.2 s / +0.2 s", "360 px", "Words-covered test", "one frame after the picture", "3-6 dB", "0.14 gain",
        "150 Hz", "sound measured, not heard", HYPERFRAMES, "design_qc.py deadair"),
    "web-craft/web-typography-measured.md": (
        "display 1.1", "headings 1.2-1.3", "body 1.5-1.6", "at least 1.4", "100-300 only at 28 px", "below 18 px",
        "-0.01em to -0.02em", "+0.05em", "`font-kerning: none`", "`text-wrap: balance`", "past 6 lines",
        "`text-wrap: pretty`", "`font-weight: 650`", "tabular-nums`", "no synthesized bold", "inputs 16 px on iOS",
        "`text-box: trim-both cap alphabetic`", "`from-font`", "code keeps straight quotes", "L02 rule 9 wins",
        "320 px width or 200% zoom", "**Floor**", "MIT", "design_qc.py artrack"),
    "lessons/arabic-rtl-type-mirroring.md": (
        "Letter-spacing stays normal on Arabic", "body 1.5-1.7", "one step", "No italic on Arabic", "الـ, بالـ or للـ",
        "No `direction: ltr` ancestor", "(ص/م)", "no licence: cite only", "design_qc.py arabiccss", "design_qc.py alprefix", "Mirror the visual result, not the code",
        "Directional icons follow reading direction", "Sentinel first", "Cite only", "design_qc.py artrack"),
    "slides/arabic-pptx-native-bidi.md": (
        "No Unicode bidi controls in PPTX text", '`<a:pPr rtl="1">`', '`<a:bodyPr rtlCol="1">`', "`ar-SA`",
        "`<a:cs>`", "One base direction per paragraph", "separate LTR text boxes", "Native bullets only",
        "semantic reading order is Arabic", "not the authority", "PASS or NOT RUN", "never fetched or run",
        "never PPTX", "design_qc.py pptxbidi"),
    "web-craft/ui-spacing-grid-score.md": ("4, 8, 16, 24, 32, 48, 64 px", "not `#000`", "375 / 768 / 1024 / 1440",
                                           "`round(pass / 8 × 10)`", "DESIGN.md", "design_qc.py spacing"),
    "web-craft/color-tokens-oklch.md": ("primitives (`--blue-600`)", "within 15°", "10° hue spread", "fg L under 0.45",
                                        "APCA |Lc| 60", "rendered composite", "`@media (color-gamut: p3)`", "`#121212`",
                                        "did not measure", "design_qc.py tokens", "design_qc.py oklch", "design_qc.py p3fallback"),
    "web-craft/a11y-wcag22-measured.md": ("3:1", "(2.4.11)", "`scroll-margin-top`", "24×24 CSS px", "(2.5.7)", "(2.2.2)",
                                          "`document.title`", "HIGH only", "design_qc.py a11yscan"),
    "web-craft/ui-motion-review.md": ("100+ times", "300 ms or less", "never `ease-in`", "`scale(0)`", "0.9-0.97",
                                      "30-80 ms", "gentler, not zero", "10 px", "live on-screen value", "design_qc.py a11yscan"),
    "web-craft/threejs-post-inspire.md": ("`composer.setSize`", "glow only", "Inspiration only"),
    "web-craft/social-proof-patterns.md": ("decision points", "Credibility ladder", "about 9 quotes", "Authority vs impact",
                                           "3-6 one-line reactions", "challenge → solution → measurable result",
                                           "personal data blurred", "never invent", "cite only"),
    "web-craft/nav-and-pricing-patterns.md": ("mega menu", "announcement bar", "comparison table below", "one highlighted tier",
                                              "never state amounts", "cite only"),
    "web-craft/og-image.md": ("1200×630", "6 words or fewer", "4.5:1", "2.6%", "600 px wide", "design_qc.py ogimage"),
    "video-craft/grade-and-loudness-map.md": ("conform and tag", "luma and contrast before hue", "display-referred",
                                              "`00:12-00:17 what · change · protect`", "20 dB", "-23 LUFS ±1",
                                              "-24 LKFS ±2", "-27 LKFS dialogue-gated", "-16 LUFS ±1", "-14 LUFS",
                                              "--profile r128"),
    "video-craft/sidecar-captions-titles.md": ("Prefer a sidecar file", "42 characters", "20 characters a second", "`WEBVTT`",
                                               "3 a second", "never auto-wrapped", "design_qc.py sidecar"),
    "stills/thumbnails-generated-stills.md": ("at least 5 truthful concepts", "40-60% of the frame", "0-3", "2-4 words",
                                              "never baked", "regenerate", "90%", "away from faces", "Post-type slots",
                                              "design_qc.py thumb"),
    "slides/pptxgenjs-gotchas-notes.md": ("10 × 5.625 in", "never 8-digit", "options object", "`bullet: true`", "`addNotes`",
                                          "delete the whole unused", "60-70%", "150-300 words", "design_qc.py pptxlint"),
    "dataviz/dataviz-integrity-print.md": ("sorted horizontal bar", "Never 3D", "under 6 categories", "area, not radius",
                                           "Type 42", "89 mm", "force SVG mode"),
    "web-craft/diagram-design.md": ("more than 9 nodes",),
    "lessons/mixed-script-captions.md": ("burned captions and HTML only, never PPTX", "arabic-pptx-native-bidi.md"),
}
LRI, PDI = "\u2066", "\u2069"


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run([sys.executable, str(QC), *args], capture_output=True, text=True, check=False)  # nosec B603
    return result.returncode, json.loads(result.stdout.strip().splitlines()[-1])


def ffmpeg(*args: str) -> None:
    subprocess.run(["ffmpeg", "-v", "error", "-y", *args], check=True)  # nosec B603 B607


def pptx(path: Path, paragraphs: list[str]) -> Path:
    body = "".join(paragraphs)
    with zipfile.ZipFile(path, "w") as deck:
        deck.writestr("ppt/slides/slide1.xml", f'<p:sld xmlns:a="a" xmlns:p="p"><a:txBody>{body}</a:txBody></p:sld>')
    return path


class Batch3CardsTest(unittest.TestCase):
    def test_needles_and_budget(self) -> None:
        for name, needles in NEEDLES.items():
            raw = (REF / name).read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                text = " ".join(raw.split())
                for needle in needles:
                    self.assertIn(needle, text)

    def test_reachable_on_demand_only(self) -> None:
        def read(rel: str) -> str:
            return (REF / rel).read_text(encoding="utf-8")
        self.assertIn("../video-craft/index.md", read("craft/index.md"))
        self.assertIn("(shorts-safe-area-captions.md)", read("video-craft/index.md"))
        self.assertIn("(film-review-sound.md)", read("video-craft/index.md"))
        self.assertIn("(web-typography-measured.md)", read("web-craft/index.md"))
        self.assertIn("(arabic-rtl-type-mirroring.md)", read("lessons/index.md"))
        self.assertIn("slides/arabic-pptx-native-bidi.md", read("craft/training-decks.md"))
        self.assertIn("video-craft/shorts-safe-area-captions.md", read("captions-subtitles.md"))
        self.assertIn("../stills/thumbnails-generated-stills.md", read("craft/index.md"))
        self.assertIn("../dataviz/dataviz-integrity-print.md", read("craft/index.md"))
        self.assertIn("slides/pptxgenjs-gotchas-notes.md", read("craft/training-decks.md"))
        for card in ("grade-and-loudness-map", "sidecar-captions-titles"):
            self.assertIn(f"({card}.md)", read("video-craft/index.md"))
        for card in ("ui-spacing-grid-score", "color-tokens-oklch", "a11y-wcag22-measured", "ui-motion-review",
                     "threejs-post-inspire", "social-proof-patterns", "nav-and-pricing-patterns", "og-image",
                     "css-cascade-safety", "seo-basics"):
            self.assertIn(f"({card}.md)", read("web-craft/index.md"))
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        for name in ("video-craft", "stills/", "dataviz/", "web-typography-measured", "og-image", "arabic-pptx-native-bidi"):
            self.assertNotIn(name, router)

    def test_pins_licences_and_skips(self) -> None:
        inventory = (DESIGN / "INVENTORY.md").read_text(encoding="utf-8")
        for sha in PINS:
            self.assertIn(f"`{sha}`", inventory)
        for old in ("b7343e3a9579", "8a1541c4a3ff"):
            self.assertNotIn(old, inventory)
        flat = " ".join(inventory.split())
        for needle in ("`pptx` is proprietary and not used", "cite only (low trust)", "script not fetched or run",
                       "ui-ux-pro-max", "superpowers ai-video-generation", "45 lesson cards (L01-L45",
                       "land-book.com is human browsing only", "not connected (note only)", "16 web-craft cards (W01-W16"):
            self.assertIn(needle, flat)
        desc = (DESIGN / "addon.json").read_text(encoding="utf-8")
        self.assertIn("four video-craft cards", desc)
        self.assertIn("seven lesson cards", desc)

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", *private_terms())
        for name in NEEDLES:
            lowered = (REF / name).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class Batch3StaticChecksTest(unittest.TestCase):
    def pair(self, name: str, bad: str, good: str, suffix: str, *extra: str) -> dict:
        with tempfile.TemporaryDirectory() as folder:
            b, g = Path(folder) / f"bad{suffix}", Path(folder) / f"good{suffix}"
            b.write_text(bad, encoding="utf-8")
            g.write_text(good, encoding="utf-8")
            self.assertEqual(1, run_qc(name, str(b), *extra)[0], name)
            code, payload = run_qc(name, str(g), *extra)
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            return payload

    def test_safezones_px_map(self) -> None:
        def boxes(*items: dict) -> str:
            return json.dumps({"boxes": list(items)})
        good = boxes({"id": "hook", "x": 80, "y": 200, "w": 800, "h": 300},
                     {"id": "texture", "kind": "decoration", "x": 0, "y": 1500, "w": 1080, "h": 300})
        self.pair("safezones", boxes({"id": "status", "x": 80, "y": 100, "w": 800, "h": 120}), good, ".json", "--px-map", "9x16")
        self.pair("safezones", boxes({"id": "cap", "x": 80, "y": 1200, "w": 800, "h": 200}), good, ".json", "--px-map", "9x16")
        self.pair("safezones", boxes({"id": "rail", "x": 600, "y": 900, "w": 450, "h": 100}), good, ".json",
                  "--px-map", "9x16", "--rail", "120")

    def test_textfloor(self) -> None:
        self.pair("textfloor", json.dumps({"frame": "1080x1920", "boxes": [{"id": "small", "size": 26}]}),
                  json.dumps({"frame": "1080x1920", "boxes": [{"id": "ok", "size": 30}, {"id": "dots", "kind": "decoration", "size": 8}]}), ".json")
        self.pair("textfloor", json.dumps({"frame": "1920x1080", "boxes": [{"id": "small", "size": 27}]}),
                  json.dumps({"frame": "1920x1080", "boxes": [{"id": "ok", "size": 29}]}), ".json")

    def test_artrack_flags_tracking_on_arabic_only(self) -> None:
        self.pair("artrack", ':lang(ar) h1 { letter-spacing: 0.05em; }', ':lang(ar) h1 { letter-spacing: normal; }\n.label { letter-spacing: 0.05em; }', ".css")
        self.pair("artrack", '<p style="letter-spacing: 2px">مرحبا بكم</p>', '<p style="letter-spacing: 2px">HELLO</p><p dir="rtl">مرحبا بكم</p>', ".html")
        self.pair("artrack", json.dumps([{"text": "عنوان", "letterSpacing": 4}], ensure_ascii=False), json.dumps([{"text": "عنوان", "letterSpacing": 0}, {"text": "TITLE", "letterSpacing": 4}], ensure_ascii=False), ".json")

    def test_pptxbidi_native_rtl(self) -> None:
        ok = '<a:p><a:pPr rtl="1" algn="r"/><a:r><a:rPr lang="ar-SA"/><a:t>مرحبا بكم</a:t></a:r></a:p>'
        latin = '<a:p><a:r><a:t>Hello</a:t></a:r></a:p>'
        cases = {
            "control": f'<a:p><a:pPr rtl="1"/><a:r><a:rPr lang="ar-SA"/><a:t>اكتب {LRI}npm{PDI} الآن</a:t></a:r></a:p>',
            "no-rtl": '<a:p><a:r><a:rPr lang="ar-SA"/><a:t>مرحبا بكم</a:t></a:r></a:p>',
            "typed-bullet": '<a:p><a:pPr rtl="1"/><a:r><a:rPr lang="ar-SA"/><a:t>• نقطة</a:t></a:r></a:p>',
        }
        with tempfile.TemporaryDirectory() as folder:
            for name, para in cases.items():
                with self.subTest(case=name):
                    self.assertEqual(1, run_qc("pptxbidi", str(pptx(Path(folder) / f"{name}.pptx", [para, latin])))[0])
            code, payload = run_qc("pptxbidi", str(pptx(Path(folder) / "ok.pptx", [ok, latin])))
            self.assertEqual((0, []), (code, payload["warnings"]))
            code, payload = run_qc("pptxbidi", str(pptx(Path(folder) / "nolang.pptx", ['<a:p><a:pPr rtl="1"/><a:r><a:t>مرحبا</a:t></a:r></a:p>'])))
            self.assertEqual(0, code)
            self.assertTrue(payload["warnings"])


class Batch3SurfaceChecksTest(unittest.TestCase):
    def pair(self, name: str, bad: str, good: str, suffix: str, *extra: str) -> dict:
        with tempfile.TemporaryDirectory() as folder:
            b, g = Path(folder) / f"bad{suffix}", Path(folder) / f"good{suffix}"
            b.write_text(bad, encoding="utf-8")
            g.write_text(good, encoding="utf-8")
            code, payload = run_qc(name, str(b), *extra)
            self.assertEqual(1, code, payload)
            code, payload = run_qc(name, str(g), *extra)
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            return payload

    def test_help_renders(self) -> None:
        result = subprocess.run([sys.executable, str(QC), "--help"], capture_output=True, text=True, check=False)  # nosec B603
        self.assertEqual(0, result.returncode, result.stderr[-300:])

    def test_spacing_scale(self) -> None:
        self.pair("spacing", ".card { padding: 20px; gap: 12px; }", ".card { padding: 24px 16px; gap: 8px; margin: 0; }", ".css")

    def test_tokens_tier(self) -> None:
        self.pair("tokens", '.btn { background: var(--blue-600); }\n<div class="bg-blue-600">', ".btn { background: var(--accent); }", ".css")

    def test_a11yscan(self) -> None:
        for bad in ('<meta name="viewport" content="width=device-width, maximum-scale=1">', '<a tabindex="2">x</a>',
                    ".m { transition: all 200ms; }", ".m { transition: opacity 200ms ease-in; }", ".m { transition: transform 450ms ease-out; }"):
            with self.subTest(bad=bad):
                self.pair("a11yscan", bad, '<a tabindex="0">x</a>\n.m { transition: transform 200ms ease-out, opacity 150ms ease-in-out; }', ".html")

    def test_p3fallback(self) -> None:
        self.pair("p3fallback", ".hero { color: color(display-p3 1 0 0); }",
                  ".hero { color: #e00; color: color(display-p3 1 0 0); }\n@media (color-gamut: p3) { .x { color: color(display-p3 0 1 0); } }", ".css")

    def test_oklch_ramp_and_hue_gap(self) -> None:
        good = {"ramps": {"blue": ["oklch(0.97 0.02 250)", "oklch(0.6 0.15 255)", "oklch(0.4 0.12 258)"]},
                "accent": "oklch(0.6 0.15 255)", "status": {"error": "oklch(0.6 0.2 25)"},
                "pairs": [["oklch(0.3 0.02 250)", "oklch(0.97 0.01 250)"]]}
        for key, value in (("ramps", {"blue": ["oklch(0.97 0.05 230)", "oklch(0.5 0.15 260)"]}),
                           ("status", {"info": "oklch(0.6 0.15 245)"}),
                           ("pairs", [["oklch(0.6 0.1 250)", "oklch(0.95 0.01 250)"]])):
            with self.subTest(key=key):
                self.pair("oklch", json.dumps({**good, key: value}), json.dumps(good), ".json")

    def test_oklch_css_input(self) -> None:
        good = ":root { --blue-100: oklch(0.97 0.02 250); --blue-600: oklch(0.6 0.15 255); --accent: oklch(0.6 0.15 255); --error: oklch(0.6 0.2 25); }"
        self.pair("oklch", good.replace("oklch(0.6 0.2 25)", "oklch(0.6 0.15 248)"), good, ".css")
        self.pair("oklch", good.replace("oklch(0.6 0.15 255); --accent", "oklch(0.6 0.15 280); --accent"), good, ".css")

    def test_sidecar(self) -> None:
        good = "WEBVTT\n\n00:00.000 --> 00:02.000\nShort line\n\n00:02.500 --> 00:04.000\nمرحبا بكم\n"
        for bad in ("00:00.000 --> 00:02.000\nno header\n",
                    "WEBVTT\n\n00:02.000 --> 00:01.000\nbackwards\n",
                    "WEBVTT\n\n00:00.000 --> 00:04.000\none\ntwo\nthree\n",
                    "WEBVTT\n\n00:00.000 --> 00:06.000\n" + "x" * 43 + "\n"):
            with self.subTest(bad=bad[:30]):
                payload = self.pair("sidecar", bad, good, ".vtt")
        self.assertEqual([], payload["warnings"])
        with tempfile.TemporaryDirectory() as folder:
            fast = Path(folder) / "fast.srt"
            fast.write_text("1\n00:00:00,000 --> 00:00:01,000\nthirty characters right here!!\n", encoding="utf-8")
            code, payload = run_qc("sidecar", str(fast))
            self.assertEqual(0, code)
            self.assertTrue(payload["warnings"])

    def test_thumb(self) -> None:
        good = {"frame": "1280x720", "headline": "Tests fail silently", "subject": {"x": 600, "y": 80, "w": 640, "h": 640}, "people": 1}
        for key, value in (("headline", "One"), ("headline", "a very long five word"), ("subject", {"x": 0, "y": 0, "w": 300, "h": 300}),
                           ("people", 5), ("headline_baked", True)):
            with self.subTest(key=key):
                self.pair("thumb", json.dumps({**good, key: value}), json.dumps(good), ".json")

    def test_pptxlint_source_and_deck(self) -> None:
        good = 'slide.addText([{ text: "Point", options: { bullet: true, color: "1F2937" } }]);\nslide.addShape("rect", { shadow: { offset: 2 } });'
        for bad in ('slide.addText("x", { color: "#1F2937" });', 'slide.addText("x", { color: "1F2937CC" });',
                    'slide.addText("\u2022 Point");', 'slide.addShape("rect", { shadow: { offset: -2 } });'):
            with self.subTest(bad=bad):
                self.pair("pptxlint", bad, good, ".js")
        with tempfile.TemporaryDirectory() as folder:
            def deck(name: str, off: str) -> str:
                path = Path(folder) / name
                with zipfile.ZipFile(path, "w") as z:
                    z.writestr("ppt/presentation.xml", '<p:presentation><p:sldSz cx="9144000" cy="5143500"/></p:presentation>')
                    z.writestr("ppt/slides/slide1.xml", f'<p:sld><a:off x="{off}" y="0"/><a:ext cx="4000000" cy="1000000"/><a:p><a:r><a:t>Point</a:t></a:r></a:p></p:sld>')
                return str(path)
            self.assertEqual(1, run_qc("pptxlint", deck("off.pptx", "6000000"))[0])
            self.assertEqual(0, run_qc("pptxlint", deck("ok.pptx", "1000000"))[0])

    def test_arabiccss_and_rtl_pass(self) -> None:
        good = ':lang(ar) p { line-height: 1.7; letter-spacing: normal; font-style: normal; }\n<p dir="rtl">مرحبا بكم</p>'
        for bad in (":lang(ar) em { font-style: italic; }", "[dir=rtl] p { line-height: 1.1; }",
                    "[lang=ar] .box { direction: ltr; }", '<p dir="ltr">مرحبا بكم</p>'):
            with self.subTest(bad=bad):
                self.pair("arabiccss", bad, good, ".html")

    def test_alprefix(self) -> None:
        self.pair("alprefix", "تعلم الـPython اليوم\nاكتب بالـ API", "تعلم لغة Python اليوم\nاكتب عبر واجهة API", ".txt")
        caption = f"1\n00:00:00,500 --> 00:00:02,000\nشغّل الـ {LRI}API{PDI} الآن\n"
        self.pair("alprefix", "1\n00:00:00,500 --> 00:00:02,000\nشغّل الـAPI الآن\n", caption, ".srt")

    def test_ogimage(self) -> None:
        def png(path: Path, width: int, height: int) -> str:
            import struct
            import zlib
            def chunk(kind: bytes, data: bytes) -> bytes:
                return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data))
            raw = b"".join(b"\x00" + b"\x00" * width for _ in range(height))
            path.write_bytes(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 0, 0, 0, 0))
                             + chunk(b"IDAT", zlib.compress(raw)) + chunk(b"IEND", b""))
            return str(path)
        with tempfile.TemporaryDirectory() as folder:
            d = Path(folder)
            ok, wrong = png(d / "ok.png", 1200, 630), png(d / "wrong.png", 1200, 600)
            good = {"headline": "Learn test automation", "size": 64, "fg": "#FFFFFF", "bg": "#111827", "brand_mark": True}
            spec = d / "spec.json"
            spec.write_text(json.dumps(good), encoding="utf-8")
            self.assertEqual(0, run_qc("ogimage", ok, "--spec", str(spec))[0])
            self.assertEqual(1, run_qc("ogimage", wrong)[0])
            self.assertEqual(1, run_qc("ogimage", str(d / "missing.png"))[0])
            for key, value in (("headline", "one two three four five six seven"), ("size", 14), ("fg", "#374151"),
                               ("bg", "#E5E7EB"), ("brand_mark", False)):
                with self.subTest(key=key):
                    spec.write_text(json.dumps({**good, key: value}), encoding="utf-8")
                    self.assertEqual(1, run_qc("ogimage", ok, "--spec", str(spec))[0])
            spec.write_text(json.dumps({**good, "headline": "تعلم أتمتة الاختبار"}, ensure_ascii=False), encoding="utf-8")
            self.assertEqual(0, run_qc("ogimage", ok, "--spec", str(spec))[0])


@unittest.skipUnless(HAS_FFMPEG, "ffmpeg not installed")
class Batch3MediaChecksTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.tmp = tempfile.TemporaryDirectory()
        d = Path(cls.tmp.name)
        cls.busy, cls.dead, cls.white, cls.mute = d / "busy.mp4", d / "dead.mp4", d / "white.mp4", d / "mute.mp4"
        ffmpeg("-f", "lavfi", "-i", "testsrc2=s=180x320:r=30:d=4", "-f", "lavfi", "-i", "sine=f=440:d=4", "-shortest", "-pix_fmt", "yuv420p", str(cls.busy))
        ffmpeg("-f", "lavfi", "-i", "color=c=black:s=180x320:r=30:d=5", "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono", "-t", "5", "-pix_fmt", "yuv420p", str(cls.dead))
        ffmpeg("-f", "lavfi", "-i", "color=c=white:s=180x320:r=30:d=3", "-pix_fmt", "yuv420p", str(cls.white))
        ffmpeg("-f", "lavfi", "-i", "testsrc2=s=180x320:r=30:d=16", "-pix_fmt", "yuv420p", str(cls.mute))
        cls.ok_cues, cls.bad_cues = d / "ok.srt", d / "bad.srt"
        cls.ok_cues.write_text("1\n00:00:00,200 --> 00:00:01,500\nأهلا\n\n2\n00:00:02,000 --> 00:00:03,500\nبكم\n", encoding="utf-8")
        cls.bad_cues.write_text("1\n00:00:00,000 --> 00:00:01,500\nأهلا\n\n2\n00:00:02,000 --> 00:00:04,500\nبكم\n", encoding="utf-8")

    @classmethod
    def tearDownClass(cls) -> None:
        cls.tmp.cleanup()

    def test_firstframe(self) -> None:
        self.assertEqual(0, run_qc("firstframe", str(self.busy), "--cues", str(self.ok_cues))[0])
        self.assertEqual(1, run_qc("firstframe", str(self.dead))[0])
        self.assertEqual(1, run_qc("firstframe", str(self.busy), "--cues", str(self.bad_cues))[0])

    def test_deadair(self) -> None:
        self.assertEqual(0, run_qc("deadair", str(self.busy))[0])
        code, payload = run_qc("deadair", str(self.dead))
        self.assertEqual(1, code)
        self.assertIn("dead_stretch", payload["hits"][0])
        code, payload = run_qc("deadair", str(self.mute))
        self.assertTrue(any("film_silent" in hit for hit in payload["hits"]))

    def test_deadair_final_hold_warns(self) -> None:
        held = Path(self.tmp.name) / "held.mp4"
        ffmpeg("-f", "lavfi", "-i", "testsrc2=s=180x320:r=30:d=3", "-f", "lavfi", "-i", "color=c=navy:s=180x320:r=30:d=4",
               "-f", "lavfi", "-i", "sine=f=440:d=3", "-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono:d=4",
               "-filter_complex", "[0:v][2:a][1:v][3:a]concat=n=2:v=1:a=1[v][a]", "-map", "[v]", "-map", "[a]",
               "-pix_fmt", "yuv420p", str(held))
        code, payload = run_qc("deadair", str(held))
        self.assertEqual(0, code, payload)
        self.assertTrue(any("final_hold" in w for w in payload["warnings"]), payload)

    def test_silencegap_mid_answer(self) -> None:
        """U4 G8: RMS mid-answer gap >0.6 s fails; continuous tone passes."""
        d = Path(self.tmp.name)
        gap = d / "gap.wav"
        # 0.5 s tone + 0.9 s silence + 0.5 s tone = mid-answer gap > 0.6 s
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=0.5", "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=0.9",
               "-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-filter_complex", "[0:a][1:a][2:a]concat=n=3:v=0:a=1[a]", "-map", "[a]", str(gap))
        code, payload = run_qc("silencegap", str(gap), "--hook-end", "0", "--max", "0.6")
        self.assertEqual(1, code, payload)
        self.assertTrue(payload.get("fail_gaps"), payload)
        ok = d / "ok_tone.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=2", str(ok))
        code, payload = run_qc("silencegap", str(ok), "--hook-end", "0", "--max", "0.6")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_silencegap_content_end_excludes_end_card_pad(self) -> None:
        """U4 G8 Carl: end-card silence past --content-end must not fail; mid gap still fails."""
        d = Path(self.tmp.name)
        # 1.0 s tone + 0.9 s mid silence + 0.5 s tone + 2.0 s end-card silence
        clip = d / "endpad.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=1.0",
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=0.9",
               "-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=2.0",
               "-filter_complex", "[0:a][1:a][2:a][3:a]concat=n=4:v=0:a=1[a]", "-map", "[a]", str(clip))
        # Without content-end, default duration-0.3 still sees mid gap → fail
        code, payload = run_qc("silencegap", str(clip), "--hook-end", "0", "--max", "0.6")
        self.assertEqual(1, code, payload)
        # With content-end at end of speech (~2.4), mid gap still fails; end pad ignored
        code, payload = run_qc(
            "silencegap", str(clip), "--hook-end", "0", "--content-end", "2.4", "--max", "0.6")
        self.assertEqual(1, code, payload)
        self.assertTrue(any(g["s"] < 2.4 for g in payload.get("fail_gaps", [])), payload)
        # Clean mid-answer + long end pad: content-end excludes pad → pass
        clean = d / "clean_endpad.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=2.0",
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=3.5",
               "-filter_complex", "[0:a][1:a]concat=n=2:v=0:a=1[a]", "-map", "[a]", str(clean))
        code, payload = run_qc(
            "silencegap", str(clean), "--hook-end", "0", "--content-end", "2.0", "--max", "0.6")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        self.assertFalse(payload.get("fail_gaps"), payload)

    def test_silencegap_soft_target_warns(self) -> None:
        """U5 G10: gap between target 0.45 and hard 0.6 warns; hard >0.6 still fails."""
        d = Path(self.tmp.name)
        # 0.5 tone + 0.50 silence + 0.5 tone → mid gap 0.50 (soft warn, hard pass)
        soft = d / "soft.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=0.50",
               "-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-filter_complex", "[0:a][1:a][2:a]concat=n=3:v=0:a=1[a]", "-map", "[a]", str(soft))
        code, payload = run_qc(
            "silencegap", str(soft), "--hook-end", "0", "--content-end", "1.5",
            "--max", "0.6", "--target", "0.45")
        self.assertEqual(0, code, payload)
        self.assertTrue(any("soft_target" in w for w in payload.get("warnings", [])), payload)
        # Comfortable gap ≤0.45: no soft warning
        ok = d / "ok045.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=0.30",
               "-f", "lavfi", "-i", "sine=f=440:d=0.5",
               "-filter_complex", "[0:a][1:a][2:a]concat=n=3:v=0:a=1[a]", "-map", "[a]", str(ok))
        code, payload = run_qc(
            "silencegap", str(ok), "--hook-end", "0", "--content-end", "1.3",
            "--max", "0.6", "--target", "0.45")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        self.assertFalse(payload.get("warnings"), payload)

    def test_mediaclamp(self) -> None:
        self.assertEqual(0, run_qc("mediaclamp", str(self.ok_cues), str(self.busy))[0])
        self.assertEqual(1, run_qc("mediaclamp", str(self.bad_cues), str(self.busy))[0])

    def test_loudness_profile(self) -> None:
        tone = Path(self.tmp.name) / "tone.wav"
        ffmpeg("-f", "lavfi", "-i", "sine=f=1000:d=6", "-af", "volume=-2dB", str(tone))
        code, payload = run_qc("loudness", str(tone), "--profile", "r128")
        self.assertEqual(0, code, payload)
        self.assertEqual(1, run_qc("loudness", str(tone), "--profile", "spotify")[0])

    def test_capluma_blend_and_scrim(self) -> None:
        code, payload = run_qc("capluma", str(self.white), "--cues", str(self.ok_cues), "--box", "20,200,140,40", "--plain")
        self.assertEqual((1, "normal+opaque"), (code, payload["cues"][0]["blend"]))
        code, payload = run_qc("capluma", str(self.dead), "--cues", str(self.ok_cues), "--box", "20,200,140,40", "--plain")
        self.assertEqual((0, "screen"), (code, payload["cues"][0]["blend"]))
        self.assertEqual(0, run_qc("capluma", str(self.white), "--cues", str(self.ok_cues), "--box", "20,200,140,40")[0])


if __name__ == "__main__":
    unittest.main()
