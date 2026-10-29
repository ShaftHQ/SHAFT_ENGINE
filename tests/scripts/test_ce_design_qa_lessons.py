"""Design QA lessons L09-L12: privacy names, endcard live, title punct, qmark baseline."""

from __future__ import annotations

import json
import math
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

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:  # pragma: no cover - skip path when Pillow absent
    Image = ImageDraw = ImageFont = None  # type: ignore[misc, assignment]

CARDS = {
    "privacy-names-gate.md": (
        "Scan caption text and ASR tokens",
        "Vocatives count",
        "Mute notes never PASS alone",
        "design_qc.py privacynames",
        "يا فلان",
        "يا [اسم]",
        "Fail-closed",
        "احسن",
        "يحسنوا",
    ),
    "endcard-band-concat.md": (
        "Burn bands on the content segment only",
        "Gate live end-card frames",
        "design_qc.py endcardlive",
        "endcard.png",
        "ink",
    ),
    "title-punct-by-script.md": (
        "Latin-script string → ASCII",
        "Any Arabic letter → Arabic",
        "design_qc.py titlepunct",
        "Strip URLs",
        "U+003F",
        "U+061F",
    ),
    "qmark-baseline.md": (
        "One typographic baseline",
        "anchor=ls",
        "design_qc.py qmarkbaseline",
        "0.06",
        "mixed-font",
    ),
}


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


def srt(*cues: tuple[float, float, str]) -> str:
    def stamp(s: float) -> str:
        ms = round(s * 1000)
        return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
    return "\n\n".join(
        f"{i}\n{stamp(a)} --> {stamp(b)}\n{text}" for i, (a, b, text) in enumerate(cues, 1)
    ) + "\n"


def _require_pillow(test: unittest.TestCase) -> None:
    if Image is None:
        test.skipTest("Pillow required")


class DesignQaLessonCardsTest(unittest.TestCase):
    def test_cards_needles_budget_index(self) -> None:
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        for name, needles in CARDS.items():
            raw = (LESSONS / name).read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                self.assertIn("## Why", raw)
                self.assertIn("## Verify", raw)
                text = " ".join(raw.split())
                for needle in needles:
                    self.assertIn(needle, text)
                self.assertIn(f"]({name})", index)
        self.assertIn(
            "37 lesson",
            " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split()),
        )
        self.assertIn(
            "privacy names",
            (DESIGN / "references/craft/index.md").read_text(encoding="utf-8"),
        )

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "mohab", "hrsd", "elmenus", "stc", "price", *private_terms())
        for name in CARDS:
            lowered = (LESSONS / name).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class PrivacyNamesGateTest(unittest.TestCase):
    def test_vocative_in_srt_fails_without_fix(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            good = Path(folder) / "good.srt"
            bad.write_text(srt((0, 2, "يا فلان معنديش فكره")), encoding="utf-8")
            good.write_text(srt((0, 2, "يا [اسم] معنديش فكره")), encoding="utf-8")
            self.assertEqual(1, run_qc("privacynames", "--srt", str(bad), "--name", "فلان")[0])
            code, payload = run_qc("privacynames", "--srt", str(good), "--name", "فلان")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_asr_vocative_bigram_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            words = Path(folder) / "w.json"
            words.write_text(json.dumps([
                {"w": "يا", "s": 1.0, "e": 1.2},
                {"w": "فلان", "s": 1.2, "e": 1.5},
            ], ensure_ascii=False), encoding="utf-8")
            code, payload = run_qc("privacynames", "--words", str(words), "--name", "فلان")
            self.assertEqual(1, code, payload)
            self.assertTrue(any("vocative" in h for h in payload["hits"]), payload)

    def test_mute_notes_alone_never_pass(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            clean = Path(folder) / "c.srt"
            clean.write_text(srt((0, 2, "كلام عام من غير اسماء")), encoding="utf-8")
            notes = Path(folder) / "notes.md"
            notes.write_text("muted 12.0-14.0 for privacy\n", encoding="utf-8")
            code, payload = run_qc(
                "privacynames", "--srt", str(clean), "--name", "فلان", "--notes", str(notes),
            )
            self.assertEqual(1, code, payload)
            self.assertIn("notes:mute-without-text-scan", payload["hits"])
            notes.write_text("text-scan SRT+ASR clean; muted 12.0-14.0\n", encoding="utf-8")
            code, payload = run_qc(
                "privacynames", "--srt", str(clean), "--name", "فلان", "--notes", str(notes),
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_lookalike_negatives_do_not_match_stem(self) -> None:
        """احسن / يحسنوا embed حسن but must not trip whole-token denylist."""
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "look.srt"
            cues.write_text(
                srt((0, 2, "احسن حاجة"), (3, 5, "يحسنوا الوضع")),
                encoding="utf-8",
            )
            code, payload = run_qc("privacynames", "--srt", str(cues), "--name", "حسن")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            self.assertEqual([], payload.get("hits", []))

    def test_vocative_only_positive_fails_bare_passes(self) -> None:
        """--vocative-only: يا حسن fails; bare حسن alone does not."""
        with tempfile.TemporaryDirectory() as folder:
            voc = Path(folder) / "voc.srt"
            bare = Path(folder) / "bare.srt"
            voc.write_text(srt((0, 2, "يا حسن تعال")), encoding="utf-8")
            bare.write_text(srt((0, 2, "حسن تعال")), encoding="utf-8")
            code, payload = run_qc(
                "privacynames", "--srt", str(voc), "--name", "حسن", "--vocative-only", "حسن",
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(any("حسن" in h for h in payload["hits"]), payload)
            code, payload = run_qc(
                "privacynames", "--srt", str(bare), "--name", "حسن", "--vocative-only", "حسن",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class TitlePunctGateTest(unittest.TestCase):
    def test_latin_title_rejects_arabic_qmark(self) -> None:
        code, payload = run_qc("titlepunct", "--title", "How to ensure quality by yourself؟")
        self.assertEqual(1, code, payload)
        code, payload = run_qc("titlepunct", "--title", "How to ensure quality by yourself?")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_arabic_title_rejects_ascii_qmark(self) -> None:
        code, payload = run_qc("titlepunct", "--title", "ازاي نضمن الجودة؟")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        code, payload = run_qc("titlepunct", "--title", "ازاي نضمن الجودة?")
        self.assertEqual(1, code, payload)

    def test_url_question_ignored(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            meta = Path(folder) / "METADATA.md"
            meta.write_text(
                "## Title\n\nA plain claim title\n\n"
                "## Description\n\nSee https://example.com/watch?v=abc123 for more.\n",
                encoding="utf-8",
            )
            code, payload = run_qc("titlepunct", "--metadata", str(meta))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_no_inputs_never_pass(self) -> None:
        code, payload = run_qc("titlepunct")
        self.assertNotEqual(0, code, payload)
        self.assertIn(payload.get("status"), ("usage", "fail", "error"))


class EndcardLiveGateTest(unittest.TestCase):
    def _plate(self, path: Path, *, band_text: bool, height: int = 720) -> None:
        _require_pillow(self)
        im = Image.new("RGB", (1280, height), (0, 38, 63))
        dr = ImageDraw.Draw(im)
        dr.rectangle([240, 240, 1040, min(400, height - 1)], fill=(255, 255, 255))
        if band_text and height > 500:
            dr.rectangle([0, 500, 1280, min(675, height - 1)], fill=(0, 38, 63))
            try:
                font = ImageFont.truetype(
                    "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 42
                )
            except OSError:
                font = ImageFont.load_default()
            for y in (520, 560, 600):
                if y < height:
                    dr.text(
                        (40, y), "SYNTHETIC QUESTION BAND TEXT " * 3,
                        font=font, fill=(255, 255, 255),
                    )
        im.save(path)

    def test_banded_live_fails_against_clean(self) -> None:
        _require_pillow(self)
        with tempfile.TemporaryDirectory() as folder:
            clean = Path(folder) / "endcard.png"
            live_bad = Path(folder) / "live-bad.png"
            live_good = Path(folder) / "live-good.png"
            self._plate(clean, band_text=False)
            self._plate(live_bad, band_text=True)
            self._plate(live_good, band_text=False)
            code, payload = run_qc(
                "endcardlive", "--live", str(live_bad), "--clean", str(clean),
                "--band-y", "500", "--band-h", "175",
                "--max-live-ink", "0.005",
            )
            self.assertEqual(1, code, payload)
            code, payload = run_qc(
                "endcardlive", "--live", str(live_good), "--clean", str(clean),
                "--band-y", "500", "--band-h", "175",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_short_clean_oob_band_never_pass(self) -> None:
        """Carl repro: 1280x720 live + 1280x400 clean @band_y=500 must FAIL (was fail-open)."""
        _require_pillow(self)
        with tempfile.TemporaryDirectory() as folder:
            live = Path(folder) / "live.png"
            clean = Path(folder) / "clean.png"
            self._plate(live, band_text=True, height=720)
            self._plate(clean, band_text=False, height=400)
            code, payload = run_qc(
                "endcardlive", "--live", str(live), "--clean", str(clean),
                "--band-y", "500", "--band-h", "175",
            )
            self.assertEqual(1, code, payload)
            self.assertIn(payload.get("status"), ("fail", "usage"))


class QmarkBaselineGateTest(unittest.TestCase):
    def _band(self, path: Path, *, raised: bool) -> None:
        _require_pillow(self)
        im = Image.new("RGBA", (640, 180), (0, 38, 63, 255))
        dr = ImageDraw.Draw(im)
        try:
            font = ImageFont.truetype(
                "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 48
            )
        except OSError:
            font = ImageFont.load_default()
        body = "QUALITY CHECK"
        baseline = 100
        dr.text((40, baseline), body, font=font, fill=(255, 255, 255, 255), anchor="ls")
        bbox = dr.textbbox((40, baseline), body, font=font, anchor="ls")
        qx = bbox[2] + 8
        if raised:
            dr.text((qx, baseline - 28), "?", font=font, fill=(255, 255, 255, 255), anchor="ls")
        else:
            dr.text((qx, baseline), "?", font=font, fill=(255, 255, 255, 255), anchor="ls")
        im.save(path)

    def test_raised_qmark_fails_aligned_passes(self) -> None:
        _require_pillow(self)
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "raised.png"
            good = Path(folder) / "aligned.png"
            self._band(bad, raised=True)
            self._band(good, raised=False)
            code, payload = run_qc("qmarkbaseline", str(bad), "--direction", "ltr")
            self.assertEqual(1, code, payload)
            self.assertGreater(payload.get("bottom_offset_frac", 0), 0.06)
            code, payload = run_qc("qmarkbaseline", str(good), "--direction", "ltr")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_blank_png_never_pass(self) -> None:
        _require_pillow(self)
        with tempfile.TemporaryDirectory() as folder:
            blank = Path(folder) / "blank.png"
            Image.new("RGBA", (640, 180), (0, 38, 63, 255)).save(blank)
            code, payload = run_qc("qmarkbaseline", str(blank), "--direction", "ltr")
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"))
            self.assertEqual("no_text", payload.get("reason"))

    def test_nonfinite_frac_is_fail(self) -> None:
        """Mirrors craft_qc fail rule: not math.isfinite(frac) or frac > thr."""
        self.assertTrue((not math.isfinite(float("nan"))) or (float("nan") > 0.06))
        self.assertTrue((not math.isfinite(float("inf"))) or (float("inf") > 0.06))
        self.assertFalse((not math.isfinite(0.01)) or (0.01 > 0.06))


if __name__ == "__main__":
    unittest.main()
