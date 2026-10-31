"""L46 render freshness + L47 frozen-tail + snapper-words share-file gate."""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
INV = DESIGN / "INVENTORY.md"


def run_qc(*args: str, env: dict | None = None) -> tuple[int, dict]:
    merged = os.environ.copy()
    if env:
        merged.update(env)
    proc = subprocess.run(  # nosec B603
        [sys.executable, str(QC), *args],
        capture_output=True,
        text=True,
        check=False,
        env=merged,
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
    for index, (start, end, text) in enumerate(cues, 1):
        def stamp(s: float) -> str:
            ms = round(s * 1000)
            return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
        blocks.append(f"{index}\n{stamp(start)} --> {stamp(end)}\n{text}\n")
    path.write_text("\n".join(blocks), encoding="utf-8")


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    digest.update(path.read_bytes())
    return digest.hexdigest()


def make_color_mp4(path: Path, *, color: str, seconds: float = 1.0) -> None:
    ffmpeg = shutil.which("ffmpeg")
    if not ffmpeg:
        raise unittest.SkipTest("ffmpeg not installed")
    subprocess.run(  # nosec B603
        [
            ffmpeg, "-y", "-f", "lavfi", "-i",
            f"color=c={color}:s=64x64:d={seconds}",
            "-f", "lavfi", "-i", f"sine=frequency=440:duration={seconds}",
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac",
            "-shortest", str(path),
        ],
        capture_output=True,
        check=True,
    )


class L46LessonCardTest(unittest.TestCase):
    def test_card_and_index(self) -> None:
        card = "render-freshness-sha256.md"
        raw = (LESSONS / card).read_text(encoding="utf-8")
        self.assertTrue(raw.startswith("---\nname:"))
        for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
            self.assertIn(section, raw)
        for needle in ("L46", "renderfresh", "REUSE_MP4", "sha256"):
            self.assertIn(needle, raw)
        self.assertNotRegex(raw.lower(), r"\bcoach\b|\bcarl\b")
        ext = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        self.assertIn(card, ext)
        inv = " ".join(INV.read_text(encoding="utf-8").split())
        self.assertIn("L01-L64", inv)
        self.assertIn("63 lesson", inv)


class L47LessonCardTest(unittest.TestCase):
    def test_card_and_index(self) -> None:
        card = "avatar-frozen-tail.md"
        raw = (LESSONS / card).read_text(encoding="utf-8")
        self.assertTrue(raw.startswith("---\nname:"))
        for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
            self.assertIn(section, raw)
        for needle in ("L47", "frozentail", "frame-diff", "speech"):
            self.assertIn(needle, raw)
        self.assertNotRegex(raw.lower(), r"\bcoach\b|\bcarl\b")
        ext = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        self.assertIn(card, ext)


class RenderFreshTest(unittest.TestCase):
    def test_same_sha_fails_and_reuse_env_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            prior = root / "prior.mp4"
            current = root / "current.mp4"
            make_color_mp4(prior, color="red")
            current.write_bytes(prior.read_bytes())
            code, payload = run_qc(
                "renderfresh", str(current), "--prior-sha256", sha256_file(prior)
            )
            self.assertEqual(1, code, payload)
            self.assertIn("mp4-sha256-unchanged", payload.get("hits", []))

            other = root / "other.mp4"
            make_color_mp4(other, color="blue")
            code, payload = run_qc(
                "renderfresh", str(other), "--prior", str(prior),
                env={"REUSE_MP4": "1"},
            )
            self.assertEqual(1, code, payload)
            self.assertIn("REUSE_MP4", payload.get("hits", []))

            code, payload = run_qc("renderfresh", str(other), "--prior", str(prior))
            self.assertEqual(0, code, payload)


class FrozenTailTest(unittest.TestCase):
    def test_static_tail_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            clip = Path(folder) / "still.mp4"
            # still color for 3s → last 2s are static
            make_color_mp4(clip, color="green", seconds=3.0)
            code, payload = run_qc("frozentail", str(clip), "--tail", "2", "--max", "0.2")
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertTrue(hits, payload)


class CapSyncSnapperShareTest(unittest.TestCase):
    def test_shared_snapper_words_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            write_srt(cues, [(0.0, 1.0, "Hello"), (1.5, 3.0, "world")])
            words = Path(folder) / "words.json"
            words.write_text(
                json.dumps({"words": [
                    {"word": "Hello", "start": 0.05},
                    {"word": "world", "start": 1.55},
                ]}),
                encoding="utf-8",
            )
            code, payload = run_qc(
                "capsync", str(cues), "--onsets", str(words),
                "--snapper-words", str(words),
            )
            self.assertEqual(1, code, payload)
            self.assertIn("onsets-share-snapper-words", payload.get("hits", []))

    def test_hardlinked_snapper_words_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            write_srt(cues, [(0.0, 1.0, "Hello"), (1.5, 3.0, "world")])
            onsets = Path(folder) / "onsets.json"
            onsets.write_text(
                json.dumps({"words": [
                    {"word": "Hello", "start": 0.05},
                    {"word": "world", "start": 1.55},
                ]}),
                encoding="utf-8",
            )
            hard = Path(folder) / "hard.json"
            os.link(onsets, hard)
            self.assertNotEqual(onsets.resolve(), hard.resolve())
            self.assertTrue(os.path.samefile(onsets, hard))
            code, payload = run_qc(
                "capsync", str(cues), "--onsets", str(onsets),
                "--snapper-words", str(hard),
            )
            self.assertEqual(1, code, payload)
            self.assertIn("onsets-share-snapper-words", payload.get("hits", []))


if __name__ == "__main__":
    unittest.main()
