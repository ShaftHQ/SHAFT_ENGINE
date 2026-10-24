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

from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
REF = DESIGN / "references"
QC = DESIGN / "scripts/design_qc.py"
HAS_FFMPEG = bool(shutil.which("ffmpeg") and shutil.which("ffprobe"))
HYPERFRAMES = "1a238171ec55896a9cb8452c33e4e0fe1063a60c"
PINS = (HYPERFRAMES, "019ab7c199154f4509b55f12d84a4f5b578dc606", "08d3b9d14c2d1719c0b96f703bced8dccfe2076a",
        "fc05be36316e5e5ea2ee0b5ec51519b411d54665", "dbd4588f9e1033efb41dad4bef2f7947c8993d44",
        "aed9cfd3277740755f6bfc1155c7aa645403b760")
NEEDLES = {
    "video-craft/shorts-safe-area-captions.md": (
        "top 168 px", "y 168-1330", "right action rail", "never black, never a logo", "0.1-0.3 s in", "28 px",
        "about 5 English words", "2-3 words high energy", "pause of 150 ms", "fade 0.12 s", "never both",
        "under 60 `screen`", "60-180 `overlay`", "30-40% scrim", "`silencedetect=noise=-35dB:d=0.6`", "20%",
        "by position, not text", "multilingual model", "burned-in subtitles", "30-55% covered", "80% covered",
        "dies after about 8 s", "one card per 6-8 s", "ffprobe media length", "`-g <fps>`", HYPERFRAMES, "Apache-2.0",
        "design_qc.py safezones", "design_qc.py textfloor", "design_qc.py firstframe", "design_qc.py capluma",
        "design_qc.py mediaclamp"),
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
        "Letter-spacing stays normal on Arabic", "about 1.7", "one step", "Mirror the visual result, not the code",
        "Directional icons follow reading direction", "Sentinel first", "Cite only", "design_qc.py artrack"),
    "slides/arabic-pptx-native-bidi.md": (
        "No Unicode bidi controls in PPTX text", '`<a:pPr rtl="1">`', '`<a:bodyPr rtlCol="1">`', "`ar-SA`",
        "`<a:cs>`", "One base direction per paragraph", "separate LTR text boxes", "Native bullets only",
        "semantic reading order is Arabic", "not the authority", "PASS or NOT RUN", "never fetched or run",
        "never PPTX", "design_qc.py pptxbidi"),
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
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        for name in ("video-craft", "web-typography-measured", "arabic-rtl-type-mirroring", "arabic-pptx-native-bidi"):
            self.assertNotIn(name, router)

    def test_pins_licences_and_skips(self) -> None:
        inventory = (DESIGN / "INVENTORY.md").read_text(encoding="utf-8")
        for sha in PINS:
            self.assertIn(f"`{sha}`", inventory)
        for old in ("b7343e3a9579", "8a1541c4a3ff"):
            self.assertNotIn(old, inventory)
        flat = " ".join(inventory.split())
        for needle in ("`pptx` is proprietary and not used", "cite only (low trust)", "script not fetched or run",
                       "ui-ux-pro-max", "superpowers ai-video-generation", "7 lesson cards (L01-L07"):
            self.assertIn(needle, flat)
        desc = (DESIGN / "addon.json").read_text(encoding="utf-8")
        self.assertIn("two video-craft cards", desc)
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

    def test_mediaclamp(self) -> None:
        self.assertEqual(0, run_qc("mediaclamp", str(self.ok_cues), str(self.busy))[0])
        self.assertEqual(1, run_qc("mediaclamp", str(self.bad_cues), str(self.busy))[0])

    def test_capluma_blend_and_scrim(self) -> None:
        code, payload = run_qc("capluma", str(self.white), "--cues", str(self.ok_cues), "--box", "20,200,140,40", "--plain")
        self.assertEqual((1, "normal+opaque"), (code, payload["cues"][0]["blend"]))
        code, payload = run_qc("capluma", str(self.dead), "--cues", str(self.ok_cues), "--box", "20,200,140,40", "--plain")
        self.assertEqual((0, "screen"), (code, payload["cues"][0]["blend"]))
        self.assertEqual(0, run_qc("capluma", str(self.white), "--cues", str(self.ok_cues), "--box", "20,200,140,40")[0])


if __name__ == "__main__":
    unittest.main()
