"""Caption and video lessons batch b: L02 bidi neutrals, L06 caption/video QC and their design_qc checks (#7351)."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
LRI, PDI = "\u2066", "\u2069"
NEEDLES = {
    "caption-video-qc.md": (
        "sample cue midpoints", "at least 0.25 s", "every cue midpoint plus the hook", "no caption gap",
        "Cutaway-in-crop", "frame-exact boundaries", "`select=between(n,a,b)`", "150–250 ms",
        "ear-checked word timestamps", "Pad end-card silence",
        "ASR hallucinates on pauses", "2 or more models", "Hook = spoken words", "cross-source agreement",
        "shot cut and a clean sentence onset", "re-runs the caption checks", "one at a time, detached",
        "Silence-gap on the **final** render", "design_qc.py silencegap",
        "exact-raw", "design_qc.py txdiff",
        "design_qc.py cuemids", "design_qc.py cuegaps", "design_qc.py asrhall", "design_qc.py boundprefix",
        "design_qc.py fadetail", "design_qc.py bidi"),
    "mixed-script-captions.md": (
        "Neutral characters break isolates", "ASCII ' inside a word", "LRI U+2066 ... PDI U+2069",
        "hook and label text", "normal RTL order: warn", "design_qc.py bidi",
        "unicode-bidi: plaintext", "unicode-bidi: isolate", "direction: rtl", "leading RLM",
        "author hard newlines", "cue midpoints", "Article **right** of Latin", "SRT must carry it",
        "Tech-term lexicon", "design_qc.py termlexicon", "--term AR=LATIN",
        "Trailing Latin sentence punct", "trailing-latin-punct-outside-isolate", ".AI"),
}


def srt(*cues: tuple[float, float, str]) -> str:
    def stamp(s: float) -> str:
        ms = round(s * 1000)
        return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
    return "\n\n".join(f"{i}\n{stamp(a)} --> {stamp(b)}\n{text}" for i, (a, b, text) in enumerate(cues, 1)) + "\n"


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run([sys.executable, str(QC), *args], capture_output=True, text=True, check=False)  # nosec B603
    return result.returncode, json.loads(result.stdout.strip().splitlines()[-1])


class CaptionLessonCardsTest(unittest.TestCase):
    def test_needles_budget_and_reachability(self) -> None:
        for name, needles in NEEDLES.items():
            raw = (LESSONS / name).read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                text = " ".join(raw.split())
                for needle in needles:
                    self.assertIn(needle, text)
        self.assertIn("(caption-video-qc.md)", (LESSONS / "index.md").read_text(encoding="utf-8"))
        self.assertIn("lessons/caption-video-qc.md", (DESIGN / "references/captions-subtitles.md").read_text(encoding="utf-8"))
        self.assertNotIn("caption-video-qc", (DESIGN / "SKILL.md").read_text(encoding="utf-8"))
        self.assertIn("7 lesson cards (L01-L07", " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split()))

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", "india", *private_terms())
        for name in NEEDLES:
            lowered = (LESSONS / name).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class CaptionChecksTest(unittest.TestCase):
    def pair(self, name: str, bad: str, good: str, suffix: str = ".srt", *extra: str) -> dict:
        with tempfile.TemporaryDirectory() as folder:
            b, g = Path(folder) / f"bad{suffix}", Path(folder) / f"good{suffix}"
            b.write_text(bad, encoding="utf-8")
            g.write_text(good, encoding="utf-8")
            self.assertEqual(1, run_qc(name, str(b), *extra)[0], name)
            code, payload = run_qc(name, str(g), *extra)
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            return payload

    def test_bidi_neutrals_in_captions(self) -> None:
        self.pair("bidi", srt((0, 2, "هذا It\u2019s important جدا")), srt((0, 2, "هذا It's important جدا")))
        self.pair("bidi", srt((0, 2, "اكتب command --help الآن")), srt((0, 2, f"اكتب {LRI}command --help{PDI} الآن")))

    def test_bidi_covers_hook_and_label_text(self) -> None:
        self.pair("bidi", json.dumps({"hook": "تعلم Note: الآن"}), json.dumps({"hook": f"تعلم {LRI}Note:{PDI} الآن"}), ".json")
        self.pair("bidi", '<p dir="rtl">قل "Hello" الآن</p>', '<p dir="rtl">قل <bdi>"Hello"</bdi> الآن</p>', ".html")

    def test_percent_in_rtl_is_a_warning_not_a_failure(self) -> None:
        payload = self.pair("bidi", srt((0, 2, "نسبة 70% من Note: هنا")), srt((0, 2, "نسبة 70% من الناس")))
        self.assertTrue(any("percent-in-rtl" in w for w in payload["warnings"]))

    def test_cuegaps_at_splice_join(self) -> None:
        self.pair("cuegaps", srt((0, 1.8, "أ"), (2, 4, "ب")), srt((0, 2, "أ"), (2, 4, "ب")), ".srt", "--join", "2.0")

    def test_asrhall_flags_pause_outros(self) -> None:
        self.pair("asrhall", "كلام مفيد\nشكرا للمشاهدة\n", "كلام مفيد\n", ".txt")
        self.pair("asrhall", json.dumps([{"word": "Bye", "start": 9, "end": 9.4}]), json.dumps([{"word": "maybe", "start": 9, "end": 9.4}]), ".json")

    def test_asrhall_joins_consecutive_words(self) -> None:
        def words(*tokens: str) -> str:
            return json.dumps([{"word": w, "start": i, "end": i + 0.4} for i, w in enumerate(tokens)])
        self.pair("asrhall", words("كلام", "اشتركوا", "في", "القناة"), words("اشتركوا", "معنا", "في", "القناة"), ".json")
        self.pair("asrhall", json.dumps({"segments": [{"words": [{"w": "شكرا", "s": 1, "e": 1.3}, {"w": "للمشاهدة", "s": 1.3, "e": 2}]}]}),
                  json.dumps({"segments": [{"words": [{"w": "شكرا", "s": 1, "e": 1.3}, {"w": "لكم", "s": 1.3, "e": 2}]}]}), ".json")

    def test_short_word_keys(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            words = Path(folder) / "w.json"
            words.write_text(json.dumps([{"w": "a", "s": 0, "e": 2.0}]), encoding="utf-8")
            code, payload = run_qc("fadetail", "--words", str(words), "--end", "2.3", "--fade", "0.12")
            self.assertEqual((0, 2.0), (code, payload["last_word"]))
            self.assertEqual(1, run_qc("fadetail", "--words", str(words), "--end", "2.1", "--fade", "0.3")[0])
        self.pair("asrhall", json.dumps({"words": [{"w": "Thanks", "s": 1, "e": 1.2}, {"w": "for", "s": 1.2, "e": 1.3}, {"w": "watching", "s": 1.3, "e": 2}]}),
                  json.dumps({"words": [{"w": "Thanks", "s": 1, "e": 1.2}, {"w": "for", "s": 1.2, "e": 1.3}, {"w": "coming", "s": 1.3, "e": 2}]}), ".json")

    def test_bidi_reports_real_srt_line_and_cue(self) -> None:
        cues = [(i * 2, i * 2 + 1.5, "مرحبا بكم") for i in range(25)] + [(50, 52, "اكتب command --help الآن")]
        with tempfile.TemporaryDirectory() as folder:
            file = Path(folder) / "c.srt"
            file.write_text(srt(*cues), encoding="utf-8")
            line = next(n for n, text in enumerate(file.read_text(encoding="utf-8").splitlines(), 1) if "--help" in text)
            code, payload = run_qc("bidi", str(file))
        self.assertEqual((1, 103), (code, line))
        self.assertEqual([f"{file}:103 (cue 26) dash-at-latin-edge"], payload["hits"])

    def test_boundprefix_after_cue_edit(self) -> None:
        self.pair("boundprefix", srt((0, 2, "اذهب إلى البيت و\nثم ارجع")), srt((0, 2, "اذهب إلى البيت\nوثم ارجع")))

    def test_fadetail_clears_last_word(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            words = Path(folder) / "w.json"
            words.write_text(json.dumps([{"word": "a", "start": 0, "end": 2.0}]), encoding="utf-8")
            self.assertEqual(1, run_qc("fadetail", "--words", str(words), "--end", "2.1", "--fade", "0.3")[0])
            self.assertEqual(0, run_qc("fadetail", "--words", str(words), "--end", "2.3", "--fade", "0.12")[0])

    def test_cuemids_snaps_probes_inside_cues(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            cues.write_text(srt((0, 1.8, "أ"), (2, 4, "ب")), encoding="utf-8")
            code, payload = run_qc("cuemids", str(cues), "--probe", "1.9", "--probe", "0.1")
            self.assertEqual((0, [0.9, 3.0], [1.55, 0.25]), (code, payload["midpoints"], payload["probes"]))


    def test_termlexicon_flags_arabic_script_tech_terms(self) -> None:
        """U2 G7: leftover Arabic-script tech transliterations fail until restored to Latin."""
        self.pair(
            "termlexicon",
            srt((0, 2, "افتح والداتايز الآن")),
            srt((0, 2, "افتح والـ database الآن")),
        )
        # --term extends the placeholder lexicon (project terms stay out of the default map)
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            bad.write_text(srt((0, 2, "شغل الفوبار الآن")), encoding="utf-8")
            self.assertEqual(1, run_qc("termlexicon", str(bad), "--term", "فوبار=foobar")[0])
            good = Path(folder) / "good.srt"
            good.write_text(srt((0, 2, "شغل الـ foobar الآن")), encoding="utf-8")
            code, payload = run_qc("termlexicon", str(good), "--term", "فوبار=foobar")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


    def test_bidi_trailing_latin_punct_inside_isolate(self) -> None:
        """U4 G8: trailing Latin .!? outside LRI…PDI fails under RTL."""
        self.pair(
            "bidi",
            srt((0, 2, f"تعلم {LRI}AI{PDI}. الآن")),
            srt((0, 2, f"تعلم {LRI}AI.{PDI} الآن")),
        )

    def test_txdiff_gates_on_exact_raw_not_fuzzy(self) -> None:
        """U4 G9: exact-raw miss fails even when fuzzy@0.72 would match."""
        with tempfile.TemporaryDirectory() as folder:
            srt_path = Path(folder) / "cues.srt"
            words_path = Path(folder) / "words.json"
            # إحنا vs احنا: SequenceMatcher ratio ~0.75 >= 0.72 but not exact-raw equal
            srt_path.write_text(srt((0, 2, "إحنا نستخدم")), encoding="utf-8")
            words_path.write_text(json.dumps([
                {"w": "احنا", "s": 0.0, "e": 0.8},
                {"w": "نستخدم", "s": 0.9, "e": 1.8},
            ], ensure_ascii=False), encoding="utf-8")
            code, payload = run_qc("txdiff", str(srt_path), str(words_path), "--max", "0.0")
            self.assertEqual(1, code, payload)
            self.assertGreater(payload["diff_exact_raw"], 0)
            self.assertEqual(payload["diff_fuzzy"], 0.0)
            # exact match passes
            words_path.write_text(json.dumps([
                {"w": "إحنا", "s": 0.0, "e": 0.8},
                {"w": "نستخدم", "s": 0.9, "e": 1.8},
            ], ensure_ascii=False), encoding="utf-8")
            code, payload = run_qc("txdiff", str(srt_path), str(words_path), "--max", "0.0")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            self.assertEqual(payload["diff_exact_raw"], 0.0)



if __name__ == "__main__":
    unittest.main()
