"""L35 video caption handoff QC: karaoke, script filter, speech gaps, fps, VFX, onset."""

from __future__ import annotations

import json
import math
import shutil
import struct

HAS_FFMPEG = bool(shutil.which("ffmpeg") and shutil.which("ffprobe"))
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
import wave
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
CARD = "video-caption-handoff.md"

NEEDLES = (
    "human-corrected",
    "askaraoke",
    "scriptfilter",
    "speechcuegap",
    "true peak",
    "-1",
    "fpsmatch",
    "vfxspell",
    "onsetclip",
    "3.0",
    "--attestation",
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


def write_attestation(path: Path) -> None:
    path.write_text("human-corrected: ear-checked transcript\n", encoding="utf-8")


def write_tone_wav(path: Path, duration: float, freq: float = 440.0,
                   rate: int = 16000, amp: float = 0.3,
                   silence: list[tuple[float, float]] | None = None) -> None:
    """Mono 16-bit PCM; optional silence windows (start, end) seconds."""
    n = int(duration * rate)
    silent = set()
    for a, b in silence or []:
        for i in range(int(a * rate), min(n, int(b * rate))):
            silent.add(i)
    frames = bytearray()
    for i in range(n):
        if i in silent:
            sample = 0
        else:
            sample = int(amp * 32767 * math.sin(2 * math.pi * freq * (i / rate)))
        frames += struct.pack("<h", sample)
    with wave.open(str(path), "wb") as handle:
        handle.setnchannels(1)
        handle.setsampwidth(2)
        handle.setframerate(rate)
        handle.writeframes(frames)


def write_pulsed_wav(path: Path, duration: float, on: float, off: float,
                     freq: float = 440.0, rate: int = 16000,
                     amp: float = 0.3) -> None:
    """Tone that alternates on/off for the whole duration (bursty speech)."""
    n = int(duration * rate)
    period = on + off
    frames = bytearray()
    for i in range(n):
        t = i / rate
        phase = t % period
        if phase < on:
            sample = int(amp * 32767 * math.sin(2 * math.pi * freq * t))
        else:
            sample = 0
        frames += struct.pack("<h", sample)
    with wave.open(str(path), "wb") as handle:
        handle.setnchannels(1)
        handle.setsampwidth(2)
        handle.setframerate(rate)
        handle.writeframes(frames)


def ffmpeg_mp4(wav: Path, out: Path, fps: str = "30000/1001") -> None:
    """Tiny video from color + wav at a given fps rational."""
    ffmpeg = shutil.which("ffmpeg")
    if not ffmpeg:
        raise unittest.SkipTest("ffmpeg not on PATH")
    subprocess.run(  # nosec B603 - absolute ffmpeg via shutil.which
        [
            ffmpeg, "-v", "error", "-y",
            "-f", "lavfi", "-i", f"color=c=black:s=64x64:r={fps}",
            "-i", str(wav),
            "-shortest", "-c:v", "libx264", "-pix_fmt", "yuv420p",
            "-c:a", "aac", "-b:a", "64k", str(out),
        ],
        check=True, capture_output=True,
    )


class VideoCaptionLessonCardTest(unittest.TestCase):
    def test_card_needles_budget_index(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        self.assertIn("## Why", raw)
        self.assertIn("## Verify", raw)
        text = " ".join(raw.split())
        for needle in NEEDLES:
            self.assertIn(needle, text, needle)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L34 ", index)
        self.assertIn("L35 ", index)
        self.assertIn("](paste-robots-noindex.md)", index)
        self.assertIn(f"]({CARD})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("38 lesson", inv)
        self.assertIn("L01-L38", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("video caption handoff", craft)
        self.assertIn("paste robots noindex", craft)
        ve = DESIGN / "references/craft/video-editing.md"
        self.assertLessEqual(len(ve.read_text(encoding="utf-8").splitlines()), 120)
        web = DESIGN / "references/craft/web-design.md"
        self.assertLessEqual(len(web.read_text(encoding="utf-8").splitlines()), 120)

    def test_card_is_project_neutral(self) -> None:
        banned = (
            "automatest", "mohab", "hrsd", "elmenus", "stc", "price",
            "carl", "coach", "podcast", *private_terms(),
        )
        lowered = (LESSONS / CARD).read_text(encoding="utf-8").lower()
        for word in banned:
            self.assertNotIn(word, lowered, word)


class AsrKaraokeGateTest(unittest.TestCase):
    def test_askaraoke_red_on_word_level_green_on_attested_phrases(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            bad = root / "bad.srt"
            good = root / "good.srt"
            attest = root / "attest.txt"
            write_attestation(attest)
            # 10 one-word micro cues → karaoke
            write_srt(bad, [(i * 0.3, i * 0.3 + 0.25, f"w{i}") for i in range(10)])
            write_srt(
                good,
                [
                    (0.0, 1.2, "This is a full readable phrase"),
                    (1.3, 2.5, "Another corrected caption line here"),
                    (2.6, 3.8, "Human ear-checked wording stays"),
                ],
            )
            code, payload = run_qc(
                "askaraoke", str(bad), "--attestation", str(attest),
            )
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(
                any("karaoke_ratio" in h for h in payload.get("hits", [])), payload,
            )

            code, payload = run_qc(
                "askaraoke", str(good), "--attestation", str(attest),
            )
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_askaraoke_fn_seven_karaoke_cues_fail(self) -> None:
        """FN1: 7 word-level cues must fail (old min_cues=8 floor was a false negative)."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            srt = root / "seven.srt"
            attest = root / "attest.txt"
            write_attestation(attest)
            write_srt(srt, [(i * 0.3, i * 0.3 + 0.25, f"w{i}") for i in range(7)])
            code, payload = run_qc(
                "askaraoke", str(srt), "--attestation", str(attest),
            )
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(
                any("karaoke_ratio" in h for h in payload.get("hits", [])), payload,
            )

    def test_askaraoke_fn_phrases_without_attestation_fail(self) -> None:
        """Phrase-level raw ASR without human-corrected attestation must fail."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            srt = root / "raw.srt"
            write_srt(
                srt,
                [
                    (0.0, 1.2, "This is a full readable phrase"),
                    (1.3, 2.5, "Another corrected caption line here"),
                    (2.6, 3.8, "Human ear-checked wording stays"),
                ],
            )
            code, payload = run_qc("askaraoke", str(srt))
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("attestation" in h for h in payload.get("hits", [])), payload,
            )


class ScriptFilterGateTest(unittest.TestCase):
    def test_scriptfilter_fails_cjk_and_filler_passes_clean(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad_cjk = Path(folder) / "cjk.srt"
            bad_fill = Path(folder) / "fill.srt"
            good = Path(folder) / "good.srt"
            write_srt(bad_cjk, [(0.0, 1.0, "كم من ال客ستمر تي")])
            write_srt(bad_fill, [(0.0, 2.0, "حلو حلو حلو حلو حلو")])
            write_srt(good, [(0.0, 1.5, "هذا سطر عربي clean English mix")])
            code, payload = run_qc("scriptfilter", str(bad_cjk))
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("unexpected script" in h for h in payload.get("hits", [])),
                payload,
            )

            code, payload = run_qc("scriptfilter", str(bad_fill))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("filler" in h for h in payload.get("hits", [])), payload)

            code, payload = run_qc("scriptfilter", str(good))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_scriptfilter_fn_emoji_and_triple_filler_fail(self) -> None:
        """FN2: emoji-only cues and 3× filler must fail."""
        with tempfile.TemporaryDirectory() as folder:
            emoji = Path(folder) / "emoji.srt"
            fill3 = Path(folder) / "fill3.srt"
            write_srt(emoji, [(0.0, 1.0, "🔥🔥🔥🔥🔥")])
            write_srt(fill3, [(0.0, 1.5, "حلو حلو حلو")])
            code, payload = run_qc("scriptfilter", str(emoji))
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("unexpected script" in h and "symbol" in h
                    for h in payload.get("hits", [])),
                payload,
            )

            code, payload = run_qc("scriptfilter", str(fill3))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("filler" in h for h in payload.get("hits", [])), payload)

    def test_scriptfilter_fn_cyrillic_thai_deva_fail(self) -> None:
        """Non-allowed scripts (Cyrillic/Thai/Devanagari) must fail under ar,latn."""
        with tempfile.TemporaryDirectory() as folder:
            for name, sample in (
                ("cyrl", "Привет мир"),
                ("thai", "สวัสดี"),
                ("deva", "नमस्ते"),
            ):
                srt = Path(folder) / f"{name}.srt"
                write_srt(srt, [(0.0, 1.0, sample)])
                code, payload = run_qc("scriptfilter", str(srt))
                self.assertEqual(1, code, payload)
                self.assertTrue(
                    any("unexpected script" in h for h in payload.get("hits", [])),
                    payload,
                )


class SpeechCueGapGateTest(unittest.TestCase):
    """Riskiest assumption: >3s hole over bursty speech fails; silence hole passes."""

    def test_speechcuegap_red_over_speech_green_over_silence(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            speech = root / "speech.wav"
            write_tone_wav(speech, 8.0)
            hole = root / "hole.srt"
            write_srt(hole, [(0.0, 2.0, "start phrase here"), (6.0, 8.0, "end phrase here")])
            code, payload = run_qc(
                "speechcuegap", str(speech), "--cues", str(hole), "--max", "3.0",
            )
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(payload.get("hits"), payload)

            quiet = root / "quiet.wav"
            write_tone_wav(quiet, 8.0, silence=[(2.0, 6.0)])
            code, payload = run_qc(
                "speechcuegap", str(quiet), "--cues", str(hole), "--max", "3.0",
            )
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_speechcuegap_fails_bursty_speech_in_hole(self) -> None:
        """FN3 (riskiest): 4s hole over 200ms-on/50ms-off speech must FAIL."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            pulsed = root / "bursty.wav"
            write_pulsed_wav(pulsed, 8.0, on=0.2, off=0.05)
            hole = root / "hole.srt"
            write_srt(hole, [(0.0, 2.0, "start phrase here"), (6.0, 8.0, "end phrase here")])
            code, payload = run_qc(
                "speechcuegap", str(pulsed), "--cues", str(hole), "--max", "3.0",
            )
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(payload.get("hits"), payload)
            overlap = payload["hits"][0].get("speech_overlap", 0)
            self.assertGreater(overlap, 0.0, payload)


@unittest.skipUnless(HAS_FFMPEG, "ffmpeg not installed")
class FpsMatchGateTest(unittest.TestCase):
    def test_fpsmatch_red_on_drift_green_on_match(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            wav = root / "a.wav"
            write_tone_wav(wav, 0.5)
            src = root / "src.mp4"
            out_bad = root / "out30.mp4"
            out_ok = root / "out5994.mp4"
            ffmpeg_mp4(wav, src, fps="30000/1001")
            ffmpeg_mp4(wav, out_ok, fps="30000/1001")
            ffmpeg_mp4(wav, out_bad, fps="30")
            code, payload = run_qc("fpsmatch", str(out_bad), "--source", str(src))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)

            code, payload = run_qc("fpsmatch", str(out_ok), "--source", str(src))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)


class VfxSpellGateTest(unittest.TestCase):
    def test_vfxspell_fails_typo_passes_clean(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            lex = root / "lex.txt"
            lex.write_text("cloud\ncomputing\nsupervised\nlearning\n", encoding="utf-8")
            bad = root / "bad.txt"
            good = root / "good.txt"
            bad.write_text("CLOUB COMPUTING\n", encoding="utf-8")
            good.write_text("CLOUD COMPUTING\n", encoding="utf-8")
            code, payload = run_qc("vfxspell", str(bad), "--lexicon", str(lex))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("CLOUB" in h or "Cloub" in h or "cloub" in h.lower()
                                for h in payload.get("hits", [])), payload)

            code, payload = run_qc("vfxspell", str(good), "--lexicon", str(lex))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_vfxspell_fn_two_edit_typo_fails(self) -> None:
        """FN4: 2-edit CLUODD must fail (old unknown=ignore + 1-edit-only was FN)."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            lex = root / "lex.txt"
            lex.write_text("cloud\n", encoding="utf-8")
            labels = root / "labels.txt"
            labels.write_text("CLUODD\n", encoding="utf-8")
            code, payload = run_qc("vfxspell", str(labels), "--lexicon", str(lex))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)
            hit = " ".join(payload.get("hits", [])).lower()
            self.assertIn("cluodd", hit, payload)

    def test_vfxspell_fn_arabic_unknown_fails(self) -> None:
        """Arabic/unknown VFX tokens must fail under default unknown=fail."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            lex = root / "lex.txt"
            lex.write_text("cloud\n", encoding="utf-8")
            labels = root / "labels.txt"
            labels.write_text("سحابة\n", encoding="utf-8")
            code, payload = run_qc("vfxspell", str(labels), "--lexicon", str(lex))
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("not in lexicon" in h for h in payload.get("hits", [])), payload,
            )


class OnsetClipGateTest(unittest.TestCase):
    def test_onsetclip_fails_clipped_fragment_passes_padded_phrase(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            clipped = root / "clip.wav"
            write_tone_wav(clipped, 2.0)
            bad_srt = root / "bad.srt"
            write_srt(bad_srt, [(0.0, 0.30, "ويط"), (0.74, 2.0, "كل رول كامل هنا")])
            code, payload = run_qc("onsetclip", str(clipped), "--cues", str(bad_srt))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)

            padded = root / "pad.wav"
            write_tone_wav(padded, 2.5, silence=[(0.0, 0.25)])
            good_srt = root / "good.srt"
            write_srt(
                good_srt,
                [(0.30, 1.5, "كل رول كامل هنا مع جملة"), (1.6, 2.4, "سطر تاني واضح")],
            )
            code, payload = run_qc("onsetclip", str(padded), "--cues", str(good_srt))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_onsetclip_fn_onset_just_outside_old_window_fails(self) -> None:
        """FN5: clipped open with onset/cue at 0.06s must fail (old early_onset=0.05)."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            # Silence until 0.06s, then speech; short one-token cue from 0.06.
            wav = root / "late.wav"
            write_tone_wav(wav, 2.0, silence=[(0.0, 0.06)])
            srt = root / "late.srt"
            write_srt(srt, [(0.06, 0.35, "ويط"), (0.80, 2.0, "كل رول كامل هنا")])
            code, payload = run_qc("onsetclip", str(wav), "--cues", str(srt))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(payload.get("hits"), payload)

    def test_onsetclip_fn_multi_token_clip_fails(self) -> None:
        """Multi-token clipped open at t=0 must fail."""
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            wav = root / "multi.wav"
            write_tone_wav(wav, 2.0)
            srt = root / "multi.srt"
            write_srt(
                srt,
                [(0.0, 0.35, "ويط كل ده"), (0.80, 2.0, "كل رول كامل هنا")],
            )
            code, payload = run_qc("onsetclip", str(wav), "--cues", str(srt))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)


@unittest.skipUnless(HAS_FFMPEG, "ffmpeg not installed")
class LoudnessPeakDocTest(unittest.TestCase):
    """Lesson reuses design_qc loudness; assert measured TP gate works on a hot wav."""

    def test_loudness_fails_hot_true_peak(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            hot = Path(folder) / "hot.wav"
            write_tone_wav(hot, 0.4, amp=0.99)
            code, payload = run_qc(
                "loudness", str(hot), "--max-peak", "-1", "--target", "-14",
                "--tolerance", "20",
            )
            self.assertEqual(1, code, payload)
            problems = payload.get("problems") or []
            self.assertTrue(any("true peak" in p for p in problems), payload)


if __name__ == "__main__":
    unittest.main()
