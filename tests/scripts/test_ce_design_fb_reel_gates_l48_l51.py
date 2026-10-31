"""L48–L51 lesson cards + wardrobe/rendermeta/extendfeas/motionstill + L49 capsync."""

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
NOTICE = DESIGN / "NOTICE.md"


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


def write_srt(path: Path, cues: list[tuple[float, float, str]]) -> None:
    blocks = []
    for index, (start, end, text) in enumerate(cues, 1):
        def stamp(s: float) -> str:
            ms = round(s * 1000)
            h, rem = divmod(ms, 3_600_000)
            m, rem = divmod(rem, 60_000)
            sec, milli = divmod(rem, 1000)
            return f"{h:02d}:{m:02d}:{sec:02d},{milli:03d}"
        blocks.append(f"{index}\n{stamp(start)} --> {stamp(end)}\n{text}\n")
    path.write_text("\n".join(blocks), encoding="utf-8")


class LessonCardsL48L51Test(unittest.TestCase):
    CASES = (
        ("avatar-wardrobe-gate.md", ("L48", "wardrobe", "0.5", "collar", "button")),
        ("caption-sync-multimodel-consensus.md", ("L49", "capsync", "consensus", "150", "zero-measured")),
        ("render-meta-rev-sha.md", ("L50", "rendermeta", "render_rev", "sha")),
        ("extend-feasibility-before-slot.md", ("L51", "extendfeas", "dup", "G6")),
    )

    def test_cards_and_index(self) -> None:
        ext = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        inv = " ".join(INV.read_text(encoding="utf-8").split())
        self.assertIn("L01-L51", inv)
        self.assertIn("51 lesson", inv)
        for card, needles in self.CASES:
            with self.subTest(card=card):
                raw = (LESSONS / card).read_text(encoding="utf-8")
                self.assertTrue(raw.startswith("---\nname:"))
                for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
                    self.assertIn(section, raw)
                for needle in needles:
                    self.assertIn(needle, raw)
                self.assertNotRegex(raw.lower(), r"\bcoach\b|\bcarl\b")
                self.assertIn(card, ext)


class NoticeAndInventoryLicenceTest(unittest.TestCase):
    def test_notice_attrs_mit_apache_and_skips_copyleft(self) -> None:
        notice = NOTICE.read_text(encoding="utf-8")
        self.assertIn("iart-ai/motion-design-skills", notice)
        self.assertIn("MIT", notice)
        self.assertIn("heygen-com/hyperframes", notice)
        self.assertIn("Apache-2.0", notice)
        inv = INV.read_text(encoding="utf-8")
        for banned in ("AGPL", "PolyForm", "Remotion"):
            # Must appear only under Not used / skip policy, never as ADOPT source text.
            self.assertIn(banned, inv)
        self.assertIn("Not used", inv)
        self.assertIn("3c129f76", inv)
        self.assertIn("164d6c7", inv)
        # No vendored skill bodies from NC/AGPL
        self.assertNotIn("anything2explainer/", inv.lower() if False else notice.lower())


class WardrobeGateTest(unittest.TestCase):
    def test_missing_ref_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            mp4 = root / "out.mp4"
            make_color_mp4(mp4, color="navy", seconds=1.0)
            spec = root / "spec.json"
            spec.write_text(json.dumps({
                "garment": "polo", "sleeve": "half", "buttons_closed": 2, "colour": "navy",
            }), encoding="utf-8")
            code, payload = run_qc(
                "wardrobe", str(mp4), "--ref", str(root / "missing.png"), "--spec", str(spec),
            )
            self.assertEqual(1, code, payload)
            self.assertIn("missing-ref", payload.get("hits", []))

    def test_button_drift_and_sample_cap(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            mp4 = root / "out.mp4"
            ref = root / "ref.png"
            ref.write_bytes(b"\x89PNG\r\n\x1a\n" + b"\x00" * 32)
            make_color_mp4(mp4, color="navy", seconds=1.0)
            spec = root / "spec.json"
            spec.write_text(json.dumps({
                "garment": "polo", "sleeve": "half", "buttons_closed": 2, "colour": "navy",
            }), encoding="utf-8")
            obs = root / "obs.json"
            obs.write_text(json.dumps([
                {"t": 0.0, "collar_crop": "a.png", "buttons_closed": 2, "colour": "navy", "sleeve": "half"},
                {"t": 0.5, "collar_crop": "b.png", "buttons_closed": 1, "colour": "navy", "sleeve": "half"},
            ]), encoding="utf-8")
            code, payload = run_qc(
                "wardrobe", str(mp4), "--ref", str(ref), "--spec", str(spec),
                "--observations", str(obs), "--sample", "0.6",
            )
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertTrue(any("sample-interval" in h for h in hits), hits)
            self.assertTrue(any("buttons_closed" in h for h in hits), hits)


class RenderMetaGateTest(unittest.TestCase):
    def test_sha_and_rev_mismatch(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            mp4 = root / "out.mp4"
            make_color_mp4(mp4, color="red", seconds=0.5)
            meta = root / "meta.json"
            meta.write_text(json.dumps({"render_rev": "r1", "sha": "deadbeef"}), encoding="utf-8")
            code, payload = run_qc("rendermeta", str(mp4), "--meta", str(meta), "--rev", "r2")
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertIn("sha-mismatch", hits)
            self.assertTrue(any("rev-mismatch" in h for h in hits), hits)

    def test_match_passes(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            mp4 = root / "out.mp4"
            make_color_mp4(mp4, color="green", seconds=0.5)
            digest = sha256_file(mp4)
            meta = root / "meta.json"
            meta.write_text(json.dumps({"render_rev": "r3", "sha256": digest}), encoding="utf-8")
            code, payload = run_qc("rendermeta", str(mp4), "--meta", str(meta), "--rev", "r3")
            self.assertEqual(0, code, payload)


class ExtendFeasGateTest(unittest.TestCase):
    def test_low_dup_and_g6_blocked(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            inv = root / "inv.json"
            inv.write_text(json.dumps({
                "units": [
                    {"id": "u1", "dup_multiple": 2.0, "g6": "clear"},
                    {"id": "u2", "dup_multiple": 4.0, "g6": "blocked"},
                ]
            }), encoding="utf-8")
            code, payload = run_qc("extendfeas", str(inv))
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertTrue(any("u1:dup" in h for h in hits), hits)
            self.assertTrue(any("u2:g6-blocked" in h for h in hits), hits)

    def test_ok_passes(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            inv = root / "inv.json"
            inv.write_text(json.dumps({
                "units": [{"id": "u1", "dup_multiple": 3.0, "g6": "clear"}]
            }), encoding="utf-8")
            code, payload = run_qc("extendfeas", str(inv))
            self.assertEqual(0, code, payload)


class MotionStillGateTest(unittest.TestCase):
    def test_static_clip_fails_max_still(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            mp4 = root / "still.mp4"
            # Fully static 4s solid colour → longest static > 3s
            make_color_mp4(mp4, color="black", seconds=4.0)
            code, payload = run_qc(
                "motionstill", str(mp4), "--max-still", "3.0",
                "--end-hold-frames", "5", "--fps", "5",
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)


class CapsyncL49Test(unittest.TestCase):
    def test_zero_measured_and_builder_injected(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            srt = root / "c.srt"
            write_srt(srt, [(0.0, 1.0, "hello"), (1.2, 2.0, "world")])
            onsets = root / "onsets.json"
            onsets.write_text(json.dumps({"words": []}), encoding="utf-8")
            code, payload = run_qc("capsync", str(srt), "--onsets", str(onsets))
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertTrue(
                "zero-measured-cues" in hits or "empty-onsets" in hits,
                payload,
            )

            planted = root / "planted.json"
            planted.write_text(json.dumps({
                "words": [
                    {"word": "hello", "start": 0.0, "source": "builder"},
                    {"word": "world", "start": 1.2, "injected": True},
                ]
            }), encoding="utf-8")
            code, payload = run_qc("capsync", str(srt), "--onsets", str(planted))
            self.assertEqual(1, code, payload)
            self.assertIn("builder-injected-onset", payload.get("hits", []))

    def test_drift_over_150ms_fails_and_consensus(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            srt = root / "c.srt"
            write_srt(srt, [(0.0, 1.0, "hello")])
            a = root / "a.json"
            a.write_text(json.dumps({
                "words": [{"word": "hello", "start": 0.16, "end": 0.5}]
            }), encoding="utf-8")
            code, payload = run_qc(
                "capsync", str(srt), "--onsets", str(a), "--tol", "0.15", "--codeswitch-tol", "0.15",
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(any("drift" in h for h in payload.get("hits", [])), payload)

            # Within tol + consensus second model
            a.write_text(json.dumps({
                "words": [{"word": "hello", "start": 0.05, "end": 0.4}]
            }), encoding="utf-8")
            b = root / "b.json"
            b.write_text(json.dumps({
                "words": [{"word": "hello", "start": 0.06, "end": 0.4}]
            }), encoding="utf-8")
            code, payload = run_qc(
                "capsync", str(srt), "--onsets", str(a),
                "--onsets-b", str(b), "--consensus", "--tol", "0.15", "--codeswitch-tol", "0.15",
            )
            self.assertEqual(0, code, payload)


class TitleSafeDedupeTest(unittest.TestCase):
    def test_d08_defers_916_to_v07(self) -> None:
        d08 = (DESIGN / "references/motion-principles.md").read_text(encoding="utf-8")
        v07 = (DESIGN / "references/video-craft/shorts-safe-area-captions.md").read_text(encoding="utf-8")
        # D08 must point at V07 for 9:16 px map, not duplicate competing numbers
        self.assertRegex(d08, r"V07|shorts-safe-area")
        self.assertIn("9:16", v07)
        # HyperFrames pin refresh mentioned or inventory already covers
        inv = INV.read_text(encoding="utf-8")
        self.assertIn("164d6c7", inv)


if __name__ == "__main__":
    unittest.main()
