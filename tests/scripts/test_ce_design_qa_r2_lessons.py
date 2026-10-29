"""Design QA r2 lessons L14-L20: band coverage, mute span, endcard, micro-gap, ASR run_gates, coherence, voice."""

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

try:
    from PIL import Image, ImageDraw
except ImportError:  # pragma: no cover
    Image = ImageDraw = None  # type: ignore[misc, assignment]

CARDS = {
    "band-coverage-gate.md": (
        "Cover every content-segment frame",
        "Gate leaks, not intent",
        "design_qc.py bandcoverage",
        "fails",
    ),
    "mute-span-gate.md": (
        "Per-span cap",
        "2.0 s",
        "10%",
        "design_qc.py mutespan",
        "recut",
    ),
    "endcard-filter-complex.md": (
        "filter_complex",
        "concat demuxer",
        "video-stream duration",
        "yuvj420p",
        "design_qc.py endcardlive",
    ),
    "caption-micro-gap-snap.md": (
        "80 ms",
        "design_qc.py cuegapsnap",
        "Snap gaps",
        "micro-overlaps",
    ),
    "asr-names-run-gates.md": (
        "run_gates",
        "Fail closed",
        "require-words",
        "privacynames",
        "Mute notes never replace",
    ),
    "hard-cut-coherence.md": (
        "one thought",
        "contiguous alternate",
        "word timestamps",
        "ASR across the join",
    ),
    "avatar-voice-first.md": (
        "first dealbreaker",
        "image-to-video",
        "voice-matched",
        "I2V",
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


def _require_pillow(test: unittest.TestCase) -> None:
    if Image is None:
        test.skipTest("Pillow required")


class DesignQaR2LessonCardsTest(unittest.TestCase):
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
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("20 lesson", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("band coverage", craft)
        self.assertIn("avatar voice first", craft)

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "mohab", "hrsd", "elmenus", "stc", *private_terms())
        for name in CARDS:
            lowered = (LESSONS / name).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class BandCoverageGateTest(unittest.TestCase):
    def _frame(self, path: Path, *, leak: bool) -> None:
        _require_pillow(self)
        im = Image.new("RGB", (1280, 720), (0, 38, 63))
        dr = ImageDraw.Draw(im)
        dr.rectangle([0, 500, 1280, 675], fill=(0, 38, 63))
        if leak:
            for y in range(680, 710, 4):
                dr.rectangle([80, y, 1200, y + 2], fill=(255, 255, 255))
        else:
            for y in range(520, 640, 8):
                dr.rectangle([80, y, 1200, y + 3], fill=(255, 255, 255))
        im.save(path)

    def test_leak_below_band_fails_covered_passes(self) -> None:
        _require_pillow(self)
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "leak.png"
            good = Path(folder) / "ok.png"
            self._frame(bad, leak=True)
            self._frame(good, leak=False)
            code, payload = run_qc(
                "bandcoverage", "--frame", str(bad), "--band-y", "500", "--band-h", "175",
            )
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"))
            code, payload = run_qc(
                "bandcoverage", "--frame", str(good), "--band-y", "500", "--band-h", "175",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class MuteSpanGateTest(unittest.TestCase):
    def test_long_span_fails_short_passes(self) -> None:
        code, payload = run_qc("mutespan", "--mute", "17.56,46.40", "--content-dur", "70")
        self.assertEqual(1, code, payload)
        self.assertGreater(payload.get("max_span_s", 0), 2.0)
        code, payload = run_qc("mutespan", "--mute", "1.0,2.5", "--content-dur", "70")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_total_frac_fails(self) -> None:
        args: list[str] = ["mutespan", "--content-dur", "70"]
        for i in range(5):
            a = 10.0 + i * 5
            args.extend(["--mute", f"{a},{a + 2.0}"])
        code, payload = run_qc(*args)
        self.assertEqual(1, code, payload)
        self.assertTrue(any("total_mute_frac" in r for r in payload.get("reasons", [])), payload)


class CueGapSnapGateTest(unittest.TestCase):
    def test_check_fails_on_micro_gap_then_snap_clears(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            src = Path(folder) / "cues.srt"
            src.write_text(
                "1\n00:00:00,000 --> 00:00:01,000\nhello\n\n"
                "2\n00:00:01,050 --> 00:00:02,000\nworld\n",
                encoding="utf-8",
            )
            code, payload = run_qc("cuegapsnap", str(src), "--check", "--max-gap", "0.08")
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("pending_gaps"), payload)
            out = Path(folder) / "fixed.srt"
            code, payload = run_qc(
                "cuegapsnap", str(src), "--out", str(out), "--max-gap", "0.08",
            )
            self.assertEqual(0, code, payload)
            self.assertGreaterEqual(payload.get("snapped", 0), 1)
            code, payload = run_qc("cuegapsnap", str(out), "--check", "--max-gap", "0.08")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_real_pause_untouched(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            src = Path(folder) / "cues.srt"
            src.write_text(
                "1\n00:00:00,000 --> 00:00:01,000\nhello\n\n"
                "2\n00:00:01,200 --> 00:00:02,000\nworld\n",
                encoding="utf-8",
            )
            code, payload = run_qc("cuegapsnap", str(src), "--check", "--max-gap", "0.08")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class AsrNamesRequireWordsTest(unittest.TestCase):
    def test_require_words_fails_when_missing(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            cues.write_text(
                "1\n00:00:00,000 --> 00:00:01,000\nكلام عام\n",
                encoding="utf-8",
            )
            code, payload = run_qc(
                "privacynames", "--srt", str(cues), "--name", "فلان", "--require-words",
            )
            self.assertEqual(1, code, payload)
            self.assertIn("missing words.json", " ".join(payload.get("hits") or []))

    def test_require_words_passes_with_clean_words(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            words = Path(folder) / "w.json"
            cues.write_text(
                "1\n00:00:00,000 --> 00:00:01,000\nكلام عام\n",
                encoding="utf-8",
            )
            words.write_text(
                json.dumps([{"w": "كلام", "s": 0.0, "e": 0.5}], ensure_ascii=False),
                encoding="utf-8",
            )
            code, payload = run_qc(
                "privacynames",
                "--srt", str(cues),
                "--words", str(words),
                "--name", "فلان",
                "--require-words",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


if __name__ == "__main__":
    unittest.main()
