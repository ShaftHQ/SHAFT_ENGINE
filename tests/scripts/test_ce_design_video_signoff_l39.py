"""L39 video sign-off viewer frames: bidisolate, captionlayers, asscapfit, nonviewertext."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
CARD = "video-signoff-viewer-frames.md"

NEEDLES = (
    "bidisolate",
    "captionlayers",
    "asscapfit",
    "nonviewertext",
    "≥1 full-resolution frame per 5 s",
    "Review-brief block",
    "LRI",
    "watch page",
)

LRI, PDI = "\u2066", "\u2069"


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


def write_srt(path: Path, cues: list[tuple[float, float, str]]) -> None:
    blocks = []
    for i, (start, end, text) in enumerate(cues, 1):
        def stamp(t: float) -> str:
            ms = int(round(t * 1000))
            h, rem = divmod(ms, 3600_000)
            m, rem = divmod(rem, 60_000)
            s, milli = divmod(rem, 1000)
            return f"{h:02d}:{m:02d}:{s:02d},{milli:03d}"

        blocks.append(f"{i}\n{stamp(start)} --> {stamp(end)}\n{text}\n")
    path.write_text("\n".join(blocks), encoding="utf-8")


def write_ass(path: Path, fontsize: int, dialogues: list[tuple[str, str, str]]) -> None:
    """dialogues: (start, end, text) with ASS times and optional \\N."""
    header = f"""[Script Info]
ScriptType: v4.00+
PlayResX: 1280
PlayResY: 720

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Karaoke,Cairo,{fontsize},&H00263F&,&H313CE0&,&H00000000&,&H00000000&,1,0,0,0,100,100,0,0,1,0,0,2,60,60,95,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
"""
    lines = [header]
    for start, end, body in dialogues:
        lines.append(f"Dialogue: 0,{start},{end},Karaoke,,0,0,0,,{body}\n")
    path.write_text("".join(lines), encoding="utf-8")




def write_ass_multi(path: Path, styles: list[tuple[str, int]], dialogues: list[tuple[str, str, str, str]]) -> None:
    """styles: (name, fontsize); dialogues: (start, end, style_name, text)."""
    header = """[Script Info]
ScriptType: v4.00+
PlayResX: 1280
PlayResY: 720

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
"""
    lines = [header]
    for name, fontsize in styles:
        lines.append(
            f"Style: {name},Cairo,{fontsize},&H00263F&,&H313CE0&,&H00000000&,&H00000000&,"
            "1,0,0,0,100,100,0,0,1,0,0,2,60,60,95,1\n"
        )
    lines.append(
        "[Events]\n"
        "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n"
    )
    for start, end, style, body in dialogues:
        lines.append(f"Dialogue: 0,{start},{end},{style},,0,0,0,,{body}\n")
    path.write_text("".join(lines), encoding="utf-8")

class LessonCardTest(unittest.TestCase):
    def test_card_needles_and_index(self) -> None:
        body = (LESSONS / CARD).read_text(encoding="utf-8")
        for needle in NEEDLES:
            self.assertIn(needle, body)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn(CARD, index)
        inv = (DESIGN / "INVENTORY.md").read_text(encoding="utf-8")
        self.assertIn("L01-L59", inv)
        self.assertIn("video sign-off viewer frames", inv)


class BidisolateGateTest(unittest.TestCase):
    def test_fails_latin_outside_isolate_passes_with_lri(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            good = Path(folder) / "good.srt"
            write_srt(bad, [(0.0, 2.0, "فيه artificial intelligence لنا")])
            write_srt(good, [(0.0, 2.0, f"فيه {LRI}artificial intelligence{PDI} لنا")])
            code, payload = run_qc("bidisolate", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("latin-outside-isolate" in h for h in payload.get("hits", [])))
            code, payload = run_qc("bidisolate", str(good))
            self.assertEqual(0, code, payload)



    def test_bare_digits_in_rtl_pass_latin_still_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            digits = Path(folder) / "digits.srt"
            latin = Path(folder) / "latin.srt"
            write_srt(digits, [(0.0, 2.0, "فيه 5 خطوات")])
            write_srt(latin, [(0.0, 2.0, "فيه API لنا")])
            code, payload = run_qc("bidisolate", str(digits))
            self.assertEqual(0, code, payload)
            code, payload = run_qc("bidisolate", str(latin))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("API" in h for h in payload.get("hits", [])))


class CaptionlayersGateTest(unittest.TestCase):
    def test_fails_overlapping_ass_dialogue_times(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.ass"
            good = Path(folder) / "good.ass"
            write_ass(
                bad,
                36,
                [
                    ("0:00:00.00", "0:00:05.00", "سطر واحد"),
                    ("0:00:04.00", "0:00:08.00", "تداخل"),
                ],
            )
            write_ass(
                good,
                36,
                [
                    ("0:00:00.00", "0:00:04.00", "سطر واحد"),
                    ("0:00:04.02", "0:00:08.00", "تالي"),
                ],
            )
            code, payload = run_qc("captionlayers", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("overlap" in h for h in payload.get("hits", [])))
            code, payload = run_qc("captionlayers", str(good))
            self.assertEqual(0, code, payload)

    def test_fails_burn_plus_soft_manifest(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "manifest.json"
            path.write_text(
                json.dumps({"burn_captions": True, "sidecar_captions": True}),
                encoding="utf-8",
            )
            code, payload = run_qc("captionlayers", str(path))
            self.assertEqual(1, code, payload)


class AsscapfitGateTest(unittest.TestCase):
    def test_fails_oversized_two_line_style(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.ass"
            good = Path(folder) / "good.ass"
            write_ass(bad, 48, [("0:00:00.00", "0:00:03.00", r"سطر أول\Nسطر ثاني")])
            write_ass(good, 28, [("0:00:00.00", "0:00:03.00", r"سطر أول\Nسطر ثاني")])
            code, payload = run_qc("asscapfit", str(bad), "--band-h", "100")
            self.assertEqual(1, code, payload)
            code, payload = run_qc("asscapfit", str(good), "--band-h", "100")
            self.assertEqual(0, code, payload)



    def test_multi_style_uses_dialogue_style_fontsize(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.ass"
            good = Path(folder) / "good.ass"
            write_ass_multi(
                bad,
                [("Default", 12), ("Karaoke", 48)],
                [("0:00:00.00", "0:00:03.00", "Karaoke", "سطر أول\\Nسطر ثاني")],
            )
            write_ass_multi(
                good,
                [("Default", 12), ("Karaoke", 28)],
                [("0:00:00.00", "0:00:03.00", "Karaoke", "سطر أول\\Nسطر ثاني")],
            )
            code, payload = run_qc("asscapfit", str(bad), "--band-h", "100")
            self.assertEqual(1, code, payload)
            self.assertTrue(any("Karaoke" in h for h in payload.get("hits", [])))
            code, payload = run_qc("asscapfit", str(good), "--band-h", "100")
            self.assertEqual(0, code, payload)


class NonviewertextGateTest(unittest.TestCase):
    def test_fails_heavy_vfx_and_todo(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "chrome.txt"
            good = Path(folder) / "ok.txt"
            bad.write_text("PODCAST • HEAVY VFX • ≥90s CHAPTER\n", encoding="utf-8")
            good.write_text("Automatest Talks\n", encoding="utf-8")
            code, payload = run_qc("nonviewertext", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("heavy-vfx-meta" in h for h in payload.get("hits", [])))
            code, payload = run_qc("nonviewertext", str(good))
            self.assertEqual(0, code, payload)


    def test_empty_or_image_only_dir_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            empty = Path(folder) / "empty"
            empty.mkdir()
            code, payload = run_qc("nonviewertext", str(empty))
            self.assertEqual(1, code, payload)
            self.assertIn("no-text-inputs", payload.get("hits", []))
            frames = Path(folder) / "frames"
            frames.mkdir()
            (frames / "t1s.png").write_bytes(b"\x89PNG\r\n\x1a\n")
            code, payload = run_qc("nonviewertext", str(frames))
            self.assertEqual(1, code, payload)
            self.assertIn("no-text-inputs", payload.get("hits", []))

    def test_direct_png_file_fails_closed(self) -> None:
        """N1: a direct PNG path must not silent-PASS (files=1, hits=[])."""
        with tempfile.TemporaryDirectory() as folder:
            solo = Path(folder) / "solo.png"
            solo.write_bytes(b"\x89PNG\r\n\x1a\n")
            code, payload = run_qc("nonviewertext", str(solo))
            self.assertEqual(1, code, payload)
            self.assertEqual(0, payload.get("files", -1), payload)
            self.assertIn("no-text-inputs", payload.get("hits", []))



class L39LessonNeutralityTest(unittest.TestCase):
    def test_sources_role_neutral(self) -> None:
        raw = (
            Path(__file__).resolve().parents[2]
            / "chaos-engine/addons/design-skills/references/lessons/video-signoff-viewer-frames.md"
        ).read_text(encoding="utf-8")
        self.assertNotRegex(raw.lower(), r"\bcoach\b")
        self.assertNotIn("coach/signoff-", raw)


if __name__ == "__main__":
    unittest.main()
