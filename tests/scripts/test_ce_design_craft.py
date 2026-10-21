"""Craft checks from the design level-up (#7104, #7105 motionmaxxing, #7106 research)."""

from __future__ import annotations

import json
import re
import shutil
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
QC = DESIGN / "scripts/design_qc.py"
HAS_FFMPEG = bool(shutil.which("ffmpeg") and shutil.which("ffprobe"))


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run(  # nosec B603 - current interpreter, fixed script.
        [sys.executable, str(QC), *args], capture_output=True, text=True, check=False
    )
    return result.returncode, json.loads(result.stdout.strip().splitlines()[-1])


def ffmpeg(*args: str) -> None:
    subprocess.run([shutil.which("ffmpeg"), "-hide_banner", "-loglevel", "error", "-y", *args],  # nosec B603
                   check=True)


class CraftLintTests(unittest.TestCase):
    def check(self, name: str, bad: str, good: str, suffix: str = ".html", *extra: str) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad_file, good_file = Path(folder) / f"bad{suffix}", Path(folder) / f"good{suffix}"
            bad_file.write_text(bad, encoding="utf-8")
            good_file.write_text(good, encoding="utf-8")
            self.assertEqual(1, run_qc(name, str(bad_file), *extra)[0], name)
            code, payload = run_qc(name, str(good_file), *extra)
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_chrome_flags_hud_counters_timecodes_and_tracked_caps(self):  # #7105 G5
        self.check("chrome", '<div class="corner">02 / SPEAK</div><i>00:00:12</i>\n'
                   '<b style="letter-spacing: 0.3em; text-transform: uppercase">Scene</b>',
                   '<h1 style="letter-spacing: -0.01em">Tests that explain themselves</h1>')

    def test_copyslop_flags_slogan_copy(self):  # #7105 never-ship 6
        self.check("copyslop", "Meet Shaftly. Unlock the future of testing. Fast. Simple. Reliable.",
                   "Write the test once. The report shows every step it took.", ".txt")

    def test_fadeonly_flags_opacity_only_entries(self):  # #7105 R1.6
        self.check("fadeonly", "@keyframes in { from { opacity: 0 } to { opacity: 1 } }",
                   "@keyframes in { from { opacity: .2; transform: translateY(24px) } to { opacity: 1; transform: none } }",
                   ".css")

    def test_springs_allows_one_visible_spring(self):  # #7105 R1.10-R1.11
        self.check("springs", "gsap.to(a,{ease:'back.out(2)'});gsap.to(b,{ease:'elastic.out'});",
                   "gsap.to(a,{ease:'back.out(1.4)'});gsap.to(b,{ease:'power3.out'});", ".js")

    def test_motiondur_flags_slow_defaults(self):  # #7105 R1.1
        self.check("motiondur", ".a{transition: transform 0.9s}\n.b{animation: rise 1200ms}\n.c{transition: opacity 1s}",
                   ".a{transition: transform 0.4s}\n.b{animation: rise 330ms}\n.c{transition: opacity 0.2s}", ".css")

    def test_measure_flags_lines_outside_45_to_75_characters(self):  # #7106 web
        self.check("measure", "p { max-width: 110ch }", "p { max-width: 66ch }", ".css")

    def test_typescale_needs_clear_steps_and_few_sizes(self):  # #7106 graphic
        self.check("typescale", "h1{font-size:30px}h2{font-size:29px}p{font-size:16px}",
                   "h1{font-size:61px}h2{font-size:39px}h3{font-size:25px}p{font-size:16px}", ".css")

    def test_safezones_keep_text_out_of_platform_ui(self):  # #7106 social
        with tempfile.TemporaryDirectory() as folder:
            bad, good = Path(folder) / "bad.json", Path(folder) / "good.json"
            bad.write_text(json.dumps({"boxes": [{"id": "cta", "x": 0.1, "y": 0.8, "w": 0.8, "h": 0.1}]}), encoding="utf-8")
            good.write_text(json.dumps({"boxes": [{"id": "cta", "x": 0.1, "y": 0.5, "w": 0.8, "h": 0.1}]}), encoding="utf-8")
            self.assertEqual(1, run_qc("safezones", str(bad))[0])
            self.assertEqual(0, run_qc("safezones", str(good))[0])

    def test_story_safezones_keep_sticker_and_profile_bands_clear(self):  # story 1080x1920
        with tempfile.TemporaryDirectory() as folder:
            bad, good = Path(folder) / "bad.json", Path(folder) / "good.json"
            sticker = {"id": "cta", "x": 0.1, "y": 0.75, "w": 0.8, "h": 0.1}  # bottom edge 0.85 > 0.82
            bad.write_text(json.dumps({"boxes": [sticker]}), encoding="utf-8")
            good.write_text(json.dumps({"boxes": [{**sticker, "y": 0.70}]}), encoding="utf-8")
            self.assertEqual(1, run_qc("safezones", str(bad), "--format", "story")[0])
            code, payload = run_qc("safezones", str(good), "--format", "story")
            self.assertEqual((0, "story"), (code, payload["format"]))


@unittest.skipUnless(HAS_FFMPEG, "ffmpeg not installed")
class CraftMediaTests(unittest.TestCase):
    def test_endhold_flags_a_long_frozen_ending(self):  # #7105 G3
        with tempfile.TemporaryDirectory() as folder:
            moving, frozen = Path(folder) / "m.mp4", Path(folder) / "f.mp4"
            ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=4", "-pix_fmt", "yuv420p", str(moving))
            ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=2", "-vf", "tpad=stop_mode=clone:stop_duration=3",
                   "-pix_fmt", "yuv420p", str(frozen))
            self.assertEqual(1, run_qc("endhold", str(frozen))[0])
            self.assertEqual(0, run_qc("endhold", str(moving), "--min", "0")[0])

    def test_motionband_flags_a_film_too_still_to_exist(self):  # #7105 law 8
        with tempfile.TemporaryDirectory() as folder:
            still, busy = Path(folder) / "s.mp4", Path(folder) / "b.mp4"
            ffmpeg("-f", "lavfi", "-i", "color=c=gray:s=320x180:r=30:d=3", "-pix_fmt", "yuv420p", str(still))
            ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=3", "-pix_fmt", "yuv420p", str(busy))
            self.assertEqual(1, run_qc("motionband", str(still))[0])
            code, payload = run_qc("motionband", str(busy), "--low", "1", "--high", "100")
            self.assertEqual(0, code, payload)

    def test_hook_needs_a_poster_frame_and_early_change(self):  # #7106 hooks
        with tempfile.TemporaryDirectory() as folder:
            black, good = Path(folder) / "k.mp4", Path(folder) / "g.mp4"
            ffmpeg("-f", "lavfi", "-i", "color=c=black:s=320x180:r=30:d=3", "-pix_fmt", "yuv420p", str(black))
            ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=3", "-pix_fmt", "yuv420p", str(good))
            self.assertEqual(1, run_qc("hook", str(black))[0])
            self.assertEqual(0, run_qc("hook", str(good))[0])

    def test_cutrhythm_flags_overlong_shots(self):  # #7106 editing
        with tempfile.TemporaryDirectory() as folder:
            long_shot, cut = Path(folder) / "l.mp4", Path(folder) / "c.mp4"
            ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=8", "-pix_fmt", "yuv420p", str(long_shot))
            ffmpeg("-f", "lavfi", "-i", "color=c=red:s=320x180:r=30:d=1.5", "-f", "lavfi", "-i",
                   "color=c=blue:s=320x180:r=30:d=2.5", "-f", "lavfi", "-i", "color=c=white:s=320x180:r=30:d=1",
                   "-filter_complex", "[0][1][2]concat=n=3:v=1", "-pix_fmt", "yuv420p", str(cut))
            self.assertEqual(1, run_qc("cutrhythm", str(long_shot), "--max-shot", "4")[0])
            code, payload = run_qc("cutrhythm", str(cut), "--max-shot", "4")
            self.assertEqual(0, code, payload)


class CraftCardTests(unittest.TestCase):
    CARDS = ("web-design", "graphic-design", "logo-identity", "video-editing", "color-grading-craft", "voice-over-craft",
             "motion-graphics-craft", "graphical-overlays", "transitions", "hooks", "social-video")
    CAPPED = CARDS + ("training-decks",)  # C12 has no research file yet

    def test_twelve_craft_cards_have_targets_and_an_anchored_rubric(self):
        for name in self.CAPPED:
            text = (DESIGN / "references/craft" / f"{name}.md").read_text(encoding="utf-8")
            for heading in ("## Numeric targets", "## Rubric", "## Verify", "## References"):
                self.assertIn(heading, text, name)
            self.assertLessEqual(len(text.splitlines()), 120, name)

    def test_research_has_220_linked_references(self):
        links = 0
        for name in self.CARDS:
            text = (DESIGN / "references/research" / f"{name}.md").read_text(encoding="utf-8")
            count = text.count("](https://")
            self.assertGreaterEqual(count, 20, name)
            links += count
        self.assertGreaterEqual(links, 220)

    def test_motionmaxxing_map_attribution_and_exploration_stage(self):
        table = (DESIGN / "references/research/motionmaxxing-map.md").read_text(encoding="utf-8")
        for word in ("adopt", "adapt", "already have", "reject", "Apache-2.0", "8c8ec0f"):
            self.assertIn(word, table)
        self.assertIn("motionmaxxing", (DESIGN / "INVENTORY.md").read_text(encoding="utf-8"))
        runbook = (DESIGN / "references/pipelines/runbook.md").read_text(encoding="utf-8")
        self.assertIn("three directions", runbook)
        bank = (DESIGN / "references/craft/innovation-bank.md").read_text(encoding="utf-8")
        self.assertGreaterEqual(sum(1 for line in bank.splitlines() if re.match(r"\d+\. ", line)), 20)


if __name__ == "__main__":
    unittest.main()
