"""L56–L59 content-pipeline gates + lesson cards."""
from __future__ import annotations

import json
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


def run_qc(*args: str) -> tuple[int, dict]:
    proc = subprocess.run(  # nosec B603
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


class LessonCardsTest(unittest.TestCase):
    def test_cards_and_index(self) -> None:
        for name, needles in (
            ("avatar-likeness-voice-gate.md", ("L56", "likenessvoice", "doubt", "subject")),
            ("youtube-studio-preupload-check.md", ("L57", "ytprecheck", "Studio")),
            ("inventory-yt-id-backfill.md", ("L58", "ytidbackfill", "yt_id")),
            ("retain-mp4-until-public-url.md", ("L59", "retainmp4", "public")),
            ("freeze-method-before-review.md", ("fixed sample set", "Independent sync")),
            ("avatar-wardrobe-gate.md", ("Per-button", "buttons")),
            ("video-signoff-viewer-frames.md", ("likenessvoice", "ytprecheck", "ytidbackfill", "retainmp4")),
        ):
            text = (LESSONS / name).read_text(encoding="utf-8")
            for needle in needles:
                self.assertIn(needle, text, msg=f"{name}: {needle}")
        inv = (DESIGN / "INVENTORY.md").read_text(encoding="utf-8")
        collapsed = " ".join(inv.split())
        self.assertIn("L01-L59", collapsed)
        self.assertIn("59 lesson", collapsed)
        idx = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        for needle in ("L56", "L57", "L58", "L59"):
            self.assertIn(needle, idx)


class LikenessVoiceGateTest(unittest.TestCase):
    def _write_mp4(self, root: Path) -> Path:
        mp4 = root / "out.mp4"
        mp4.write_bytes(b"\x00\x00\x00\x18ftypmp42")
        return mp4

    def test_missing_observations_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = self._write_mp4(root)
            # argparse requires --observations; invoke via missing file path
            obs = root / "missing.json"
            code, payload = run_qc("likenessvoice", str(mp4), "--observations", str(obs))
            self.assertEqual(1, code, payload)
            self.assertIn("missing-observations", payload.get("hits", []))

    def test_doubt_fails_and_pass_ok(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = self._write_mp4(root)
            bad = root / "bad.json"
            bad.write_text(
                json.dumps(
                    {
                        "independent_reviewer": True,
                        "blind": True,
                        "likeness": {
                            "verdict": "doubt",
                            "refs": ["ref.jpg"],
                            "side_by_side": True,
                        },
                        "voice": {
                            "verdict": "pass",
                            "refs": ["ref.wav"],
                            "side_by_side": True,
                        },
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("likenessvoice", str(mp4), "--observations", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("likeness-doubt" in h for h in payload.get("hits", [])), payload)
            good = root / "good.json"
            good.write_text(
                json.dumps(
                    {
                        "independent_reviewer": True,
                        "blind": True,
                        "builder_id": "builder-a",
                        "reviewer_id": "reviewer-b",
                        "likeness": {
                            "verdict": "pass",
                            "refs": ["ref.jpg"],
                            "side_by_side": True,
                        },
                        "voice": {
                            "verdict": "pass",
                            "refs": ["ref.wav"],
                            "side_by_side": True,
                        },
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("likenessvoice", str(mp4), "--observations", str(good))
            self.assertEqual(0, code, payload)


class YtPrecheckTest(unittest.TestCase):
    def test_existing_upload_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            obs = root / "obs.json"
            obs.write_text(
                json.dumps(
                    {
                        "studio_search": {
                            "queried": True,
                            "matches": [{"yt_id": "abc123", "title": "dup"}],
                        }
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("ytprecheck", "--observations", str(obs))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("existing-upload" in h for h in payload.get("hits", [])), payload)

    def test_clean_search_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            obs = root / "obs.json"
            obs.write_text(
                json.dumps({"studio_search": {"queried": True, "matches": []}}),
                encoding="utf-8",
            )
            code, payload = run_qc("ytprecheck", "--observations", str(obs))
            self.assertEqual(0, code, payload)


class YtIdBackfillTest(unittest.TestCase):
    def test_stale_null_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            inv = root / "inv.json"
            inv.write_text(
                json.dumps(
                    {
                        "units": [
                            {"id": "u1", "status": "scheduled", "yt_id": None},
                            {"id": "u2", "status": "draft", "yt_id": None},
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("ytidbackfill", str(inv))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("stale-null-yt-id:u1" in h for h in payload.get("hits", [])), payload)

    def test_backfilled_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            inv = root / "inv.json"
            inv.write_text(
                json.dumps(
                    {"units": [{"id": "u1", "status": "scheduled", "yt_id": "xyz"}]}
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("ytidbackfill", str(inv))
            self.assertEqual(0, code, payload)


class RetainMp4Test(unittest.TestCase):
    def test_dropped_before_proof_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            obs = root / "obs.json"
            obs.write_text(
                json.dumps({"final_mp4": str(root / "gone.mp4"), "mp4_present": False}),
                encoding="utf-8",
            )
            code, payload = run_qc("retainmp4", "--observations", str(obs))
            self.assertEqual(1, code, payload)
            self.assertIn("mp4-dropped-before-public-url", payload.get("hits", []))

    def test_present_or_public_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = root / "final.mp4"
            mp4.write_bytes(b"data")
            obs = root / "obs.json"
            obs.write_text(
                json.dumps({"final_mp4": str(mp4), "mp4_present": True}),
                encoding="utf-8",
            )
            code, payload = run_qc("retainmp4", "--observations", str(obs))
            self.assertEqual(0, code, payload)
            obs2 = root / "obs2.json"
            obs2.write_text(
                json.dumps(
                    {
                        "final_mp4": str(root / "gone.mp4"),
                        "mp4_present": False,
                        "public_url": "https://youtu.be/abc",
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("retainmp4", "--observations", str(obs2))
            self.assertEqual(0, code, payload)


class WardrobePerButtonTest(unittest.TestCase):
    def test_per_button_mismatch_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = root / "out.mp4"
            make_color_mp4(mp4, color="navy", seconds=1.0)
            ref = root / "ref.png"
            ref.write_bytes(b"PNG")
            spec = root / "spec.json"
            spec.write_text(
                json.dumps(
                    {
                        "garment": "polo",
                        "sleeve": "half",
                        "buttons_closed": 2,
                        "colour": "navy",
                        "buttons": [
                            {"id": "1", "closed": True},
                            {"id": "2", "closed": True},
                        ],
                        "ref_ok": True,
                    }
                ),
                encoding="utf-8",
            )
            obs = root / "obs.json"
            obs.write_text(
                json.dumps(
                    [
                        {
                            "collar_crop": "c0.png",
                            "buttons_closed": 2,
                            "buttons": [
                                {"id": "1", "closed": True},
                                {"id": "2", "closed": False},
                            ],
                            "colour": "navy",
                            "sleeve": "half",
                        }
                    ]
                ),
                encoding="utf-8",
            )
            code, payload = run_qc(
                "wardrobe",
                str(mp4),
                "--ref",
                str(ref),
                "--spec",
                str(spec),
                "--observations",
                str(obs),
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("button[2]" in h for h in payload.get("hits", [])),
                payload,
            )


if __name__ == "__main__":
    unittest.main()
