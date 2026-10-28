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
        "Silence-gap on the **final** render", "design_qc.py silencegap", "--content-end",
        "≤0.45 s", "--target 0.45", "Antecedent check", "design_qc.py antecedent",
        "exact-raw", "design_qc.py txdiff",
        "design_qc.py cuemids", "design_qc.py cuegaps", "design_qc.py asrhall", "design_qc.py boundprefix",
        "design_qc.py fadetail", "design_qc.py bidi"),
    "mixed-script-captions.md": (
        "Neutral characters break isolates", "ASCII ' inside a word", "LRI U+2066 ... PDI U+2069",
        "hook and label text", "normal RTL order: warn", "design_qc.py bidi",
        "unicode-bidi: plaintext", "unicode-bidi: isolate", "direction: rtl", "leading RLM",
        "author hard newlines", "cue midpoints", "Article **right** of Latin", "SRT must carry it",
        "Tech/loan lexicon", "design_qc.py termlexicon", "--term AR=LATIN", "فري",
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



    def test_hookmodality_question_vs_claim(self) -> None:
        """U6 G11: spoken question with claim hook fails; matching ؟ passes."""
        with tempfile.TemporaryDirectory() as folder:
            hook_bad = Path(folder) / "hook_bad.txt"
            hook_good = Path(folder) / "hook_good.txt"
            spoken = Path(folder) / "spoken.txt"
            spoken.write_text("وهل هو هيبدل الـ testers ولا لا؟", encoding="utf-8")
            hook_bad.write_text("هيبدل الـ testers", encoding="utf-8")
            hook_good.write_text("وهل هو هيبدل الـ testers؟", encoding="utf-8")
            self.assertEqual(1, run_qc("hookmodality", "--hook", str(hook_bad), "--spoken", str(spoken))[0])
            code, payload = run_qc("hookmodality", "--hook", str(hook_good), "--spoken", str(spoken))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


    def test_qaanswer_drops_dodge_only_tail(self) -> None:
        """U9 G14: dodge/pivot-only post-question transcript fails; real answer passes."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.txt"
            good = Path(folder) / "good.txt"
            span_bad = Path(folder) / "span_bad.json"
            span_good = Path(folder) / "span_good.json"
            bad.write_text("مش عارف. طب خلينا نتكلم عن حاجة تانية في الحكومة.", encoding="utf-8")
            good.write_text(
                "الحل إنك تبني جودة من الأول: tests مع الـ codegen قبل ما تطلق.",
                encoding="utf-8",
            )
            span_bad.write_text("{}", encoding="utf-8")
            span_good.write_text('{"answer_beat": 12.4}', encoding="utf-8")
            self.assertEqual(1, run_qc("qaanswer", "--after", str(bad))[0])
            code, payload = run_qc("qaanswer", "--after", str(good), "--span", str(span_good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            self.assertEqual(1, run_qc("qaanswer", "--span", str(span_bad))[0])


    def test_qaanswer_fails_real_u9_row9_dodge_pivot(self) -> None:
        """U9 G14 Carl: real OcgL ~2078.758–2108 post-Q (unit-9 DROPPED) must FAIL.

        Transcript excerpt is the public talk span cited in unit-9 DROPPED.md
        (dodge «مش عارف» → gov/OpenAI pivot → «السؤال اللي بعديه»), not a private path.
        U8 qa answer beat must still PASS.
        """
        # Post-question only (after «هتعمل ايه»); abs ~2084.4–2108.76
        row9_after = (
            "هل انت خلاص فهتحتاجراب\n"
            "مش عارف بس بالنسبه\n"
            "لما الشركات تبقى مش قادره تجيب من\n"
            "الحكومه الامريكيه انها تكيب اللي هم فيها\n"
            "دي دلوقتي\n"
            "دي اوبن اي تمام\n"
            "واسالوا يعني تعرفوا اوبن بتعمل ايه طيب\n"
            "السؤال اللي بعديه\n"
        )
        # unit-8 work/7.srt answer beat (after the automation/AI question)
        u8_after = (
            "لا، حضرتك انت محتاج الاتنين\n"
            "لما الناس كانوا بيخشوا خناقة مثلًا، ما\n"
            "كانوا بيخشوا خناقة كده إيه؟ بالإيد، إيد في\n"
            "إيد، بعدين اكتشفوا السيف\n"
            "بعدين اكتشفوا الدرع. فحضرتك لا\n"
            "اتعلمت تستخدم السيف ولا اتعلمت تستخدم\n"
            "الدرع\n"
        )
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "row9_after.txt"
            good = Path(folder) / "u8_after.txt"
            bad.write_text(row9_after, encoding="utf-8")
            good.write_text(u8_after, encoding="utf-8")
            code_bad, payload_bad = run_qc("qaanswer", "--after", str(bad))
            self.assertEqual(1, code_bad, payload_bad)
            self.assertTrue(payload_bad.get("problems"), payload_bad)
            code_good, payload_good = run_qc("qaanswer", "--after", str(good))
            self.assertEqual((0, "pass"), (code_good, payload_good["status"]), payload_good)

    def test_hookmodality_type_qa_requires_answer_beat(self) -> None:
        """U9 G14: hookmodality --type qa fails without answer beat in --after."""
        with tempfile.TemporaryDirectory() as folder:
            hook = Path(folder) / "hook.txt"
            spoken = Path(folder) / "spoken.txt"
            after_bad = Path(folder) / "after_bad.txt"
            after_good = Path(folder) / "after_good.txt"
            hook.write_text("مفيش ChatGPT free tier — هتعمل إيه؟", encoding="utf-8")
            spoken.write_text("مفيش ChatGPT free tier — هتعمل إيه؟", encoding="utf-8")
            after_bad.write_text("مش عارف.", encoding="utf-8")
            after_good.write_text(
                "هبني جودة من الأول وأغطي الـ flows بـ tests قبل الإطلاق.",
                encoding="utf-8",
            )
            self.assertEqual(
                1,
                run_qc(
                    "hookmodality", "--hook", str(hook), "--spoken", str(spoken),
                    "--type", "qa", "--after", str(after_bad),
                )[0],
            )
            code, payload = run_qc(
                "hookmodality", "--hook", str(hook), "--spoken", str(spoken),
                "--type", "qa", "--after", str(after_good),
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_termlexicon_flags_common_loanwords(self) -> None:
        """U9 G15: default lexicon covers فري→free and سولو→solo (no brands)."""
        self.pair(
            "termlexicon",
            srt((0, 2, "مفيش حاجه اسمها فري للسولو")),
            srt((0, 2, "مفيش حاجه اسمها free للـ solo")),
        )

    def test_opendoubt_flags_self_flagged_doubt(self) -> None:
        """U9 G15: open self-flagged doubt fails; cleared review passes."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.md"
            good = Path(folder) / "good.md"
            bad.write_text(
                "# SELF-REVIEW\n\nDoubt: not sure whether فري should stay Arabic.\n"
                "Ask Coach to decide script form.\n",
                encoding="utf-8",
            )
            good.write_text(
                "# SELF-REVIEW\n\nOpen doubts: none\n\nG7 Latin free on card+SRT.\n",
                encoding="utf-8",
            )
            self.assertEqual(1, run_qc("opendoubt", str(bad))[0])
            code, payload = run_qc("opendoubt", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_hookbleed_straddle_fails(self) -> None:
        """U6 G11: cue straddling hook_end with under-hook speech fails."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.srt"
            good = Path(folder) / "good.srt"
            bad.write_text(srt((0.0, 5.0, "under hook and past")), encoding="utf-8")
            good.write_text(
                srt((0.0, 3.2, "under hook only"), (3.5, 5.0, "post hook only")),
                encoding="utf-8",
            )
            self.assertEqual(1, run_qc("hookbleed", str(bad), "--hook-end", "3.5")[0])
            code, payload = run_qc("hookbleed", str(good), "--hook-end", "3.5")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_softjump_requires_soft_flag_on_big_cut(self) -> None:
        """U6 G12: large removed bridge without soft/punch-in fails."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.json"
            good = Path(folder) / "good.json"
            bad.write_text(json.dumps({"cuts": [{"removed_s": 25.8, "soft": False}]}), encoding="utf-8")
            good.write_text(json.dumps({"cuts": [{"removed_s": 25.8, "punch_in": True}]}), encoding="utf-8")
            self.assertEqual(1, run_qc("softjump", str(bad))[0])
            code, payload = run_qc("softjump", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_antecedent_requires_referent_noun(self) -> None:
        """U7 G13: epithet without antecedent noun fails; noun present passes."""
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.txt"
            good = Path(folder) / "good.txt"
            bad.write_text("وبعدين المغرور وقع", encoding="utf-8")
            good.write_text("الأرنب المغرور وقع", encoding="utf-8")
            self.assertEqual(
                1, run_qc("antecedent", str(bad), "--epithet", "المغرور", "--noun", "الأرنب")[0])
            code, payload = run_qc(
                "antecedent", str(good), "--epithet", "المغرور", "--noun", "الأرنب")
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)



if __name__ == "__main__":
    unittest.main()
