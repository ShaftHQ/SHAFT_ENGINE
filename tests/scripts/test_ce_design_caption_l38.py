"""L38 caption phonetic + mute gate: termlexicon allow, listen-verify, mute silent, mutedburn."""

from __future__ import annotations

import json
import math
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
import wave
from array import array
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
CARD = "caption-phonetic-mute-gate.md"

CARD_NEEDLES = (
    "L38",
    "termlexicon",
    "listenverify",
    "mutesilent",
    "mutedburn",
    "--allow",
    "سيكتت",
    "كوبيلوت",
    "[muted]",
    "Egyptian",
    "Cue-text",
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


def srt(*cues: tuple[float, float, str]) -> str:
    def stamp(s: float) -> str:
        ms = round(s * 1000)
        return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
    return "\n\n".join(
        f"{i}\n{stamp(a)} --> {stamp(b)}\n{text}" for i, (a, b, text) in enumerate(cues, 1)
    ) + "\n"


def write_tone_wav(path: Path, duration: float, freq: float = 440.0,
                   silence: list[tuple[float, float]] | None = None) -> None:
    rate = 16000
    n = int(round(duration * rate))
    silent = set()
    for a, b in silence or []:
        for i in range(int(a * rate), min(n, int(b * rate))):
            silent.add(i)
    samples = array("h")
    for i in range(n):
        if i in silent:
            samples.append(0)
        else:
            t = i / rate
            samples.append(int(0.3 * 32767 * math.sin(2 * math.pi * freq * t)))
    with wave.open(str(path), "wb") as handle:
        handle.setnchannels(1)
        handle.setsampwidth(2)
        handle.setframerate(rate)
        handle.writeframes(samples.tobytes())


class LessonCardBudgetTest(unittest.TestCase):
    def test_l38_card_index_inventory(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144, CARD)
        self.assertIn("## Why", raw)
        self.assertIn("## Verify", raw)
        text = " ".join(raw.split())
        for needle in CARD_NEEDLES:
            self.assertIn(needle, text, needle)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L38 ", index)
        self.assertIn(f"]({CARD})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("L01-L47", inv)
        self.assertIn("47 lesson", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("caption phonetic + mute gate", craft)

    def test_card_project_neutral(self) -> None:
        banned = (
            "automatest", "mohab", "hrsd", "elmenus", "stc", "price",
            "carl", "coach", *private_terms(),
        )
        lowered = (LESSONS / CARD).read_text(encoding="utf-8").lower()
        for word in banned:
            self.assertNotIn(word, lowered, word)


class TermLexiconAllowL38Test(unittest.TestCase):
    def test_tech_phonetic_still_fails(self) -> None:
        """FN: leftover Arabic-script tech transliteration fails."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "افتح والداتايز الآن")), encoding="utf-8")
            code, payload = run_qc("termlexicon", str(bad))
            self.assertEqual(1, code, payload)

    def test_egyptian_loanword_allow_passes(self) -> None:
        """FP guard: allowlisted Egyptian loanword stays Arabic."""
        with tempfile.TemporaryDirectory() as folder:
            # موبايل is default Egyptian allow — must not fail even if --term maps it
            good = Path(folder) / "good.srt"
            good.write_text(srt((0, 2, "افتح الموبايل الآن")), encoding="utf-8")
            code, payload = run_qc(
                "termlexicon", str(good), "--term", "موبايل=mobile", "--allow", "موبايل",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_default_egyptian_allow_without_term(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            good = Path(folder) / "good.srt"
            good.write_text(srt((0, 2, "معايا فلوس في المحفظة")), encoding="utf-8")
            code, payload = run_qc("termlexicon", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class ListenVerifyL38Test(unittest.TestCase):
    def test_fn_secret_ice_bank_phonetic(self) -> None:
        """FN: سيكتت ايس بانك must fail listen-verify."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "طب سيكتت ايس بانك خلاص")), encoding="utf-8")
            code, payload = run_qc("listenverify", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)

    def test_fn_copilot_loan_shape(self) -> None:
        """B1 FN: كوبيلوت (unseeded) fails via loan-shape heuristic."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "افتح كوبيلوت الآن")), encoding="utf-8")
            code, payload = run_qc("listenverify", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("كوبيلوت" in h for h in payload.get("hits", [])), payload)

    def test_fn_codebase_loan_shape_phrase(self) -> None:
        """B1 FN: الكوبيبايس (article + loan shape) fails."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "ودي الكوبيبايس بتاع المشروع")), encoding="utf-8")
            code, payload = run_qc("listenverify", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)

    def test_fn_bankana_seed_substring_despite_ba_prefix(self) -> None:
        """B2 FN: بانكانا keeps seed substring بانك despite leading ب."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "بانكانا غلط")), encoding="utf-8")
            code, payload = run_qc("listenverify", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("بانكانا" in h for h in payload.get("hits", [])), payload)

    def test_fp_clean_arabic_passes(self) -> None:
        """FP: ordinary Egyptian Arabic speech must pass."""
        with tempfile.TemporaryDirectory() as folder:
            good = Path(folder) / "good.srt"
            good.write_text(
                srt((0, 2, "إحنا محتاجين نبني جودة من الأول")),
                encoding="utf-8",
            )
            code, payload = run_qc("listenverify", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_fp_real_albank_not_seed(self) -> None:
        """FP: البنك (real Arabic) ≠ seed بانك after ال- strip."""
        with tempfile.TemporaryDirectory() as folder:
            good = Path(folder) / "good.srt"
            good.write_text(srt((0, 2, "رحت البنك الصبح")), encoding="utf-8")
            code, payload = run_qc("listenverify", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_allow_clears_seed(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "c.srt"
            path.write_text(srt((0, 2, "سيكتت ايس بانك")), encoding="utf-8")
            code, payload = run_qc(
                "listenverify", str(path),
                "--allow", "سيكتت", "--allow", "ايس", "--allow", "بانك",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class MuteSilentL38Test(unittest.TestCase):
    def test_audible_mute_range_fails(self) -> None:
        """FN: mute window that still has speech fails."""
        with tempfile.TemporaryDirectory() as folder:
            wav = Path(folder) / "mix.wav"
            write_tone_wav(wav, 3.0)  # continuous tone
            code, payload = run_qc("mutesilent", str(wav), "--mute", "1.0,2.0")
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)

    def test_silent_mute_range_passes(self) -> None:
        """FP: mute window that is actually silent passes."""
        with tempfile.TemporaryDirectory() as folder:
            wav = Path(folder) / "mix.wav"
            write_tone_wav(wav, 3.0, silence=[(1.0, 2.0)])
            code, payload = run_qc("mutesilent", str(wav), "--mute", "1.0,2.0")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class MutedBurnL38Test(unittest.TestCase):
    def test_burned_muted_placeholder_fails(self) -> None:
        """FN: burned [muted] caption fails."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "السعر كان [muted] جنيه")), encoding="utf-8")
            code, payload = run_qc("mutedburn", str(bad))
            self.assertEqual(1, code, payload)

    def test_muted_case_and_whitespace_variants_fail(self) -> None:
        """N1 FN: [MuTeD] and [ muted ] in cue text fail (SRT-only scope)."""
        with tempfile.TemporaryDirectory() as folder:
            for label, cue in (
                ("case", "السعر كان [MuTeD] جنيه"),
                ("ws", "السعر كان [ muted ] جنيه"),
            ):
                bad = Path(folder) / f"{label}.srt"
                bad.write_text(srt((0, 2, cue)), encoding="utf-8")
                code, payload = run_qc("mutedburn", str(bad))
                self.assertEqual(1, code, (label, payload))

    def test_redacted_amount_without_placeholder_passes(self) -> None:
        """FP: redacted amount text without [muted] passes."""
        with tempfile.TemporaryDirectory() as folder:
            good = Path(folder) / "good.srt"
            good.write_text(srt((0, 2, "السعر كان [مبلغ] جنيه")), encoding="utf-8")
            code, payload = run_qc("mutedburn", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


if __name__ == "__main__":
    unittest.main()
