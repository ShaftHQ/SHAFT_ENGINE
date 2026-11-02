"""L39c / L52–L55 retro lessons + adjudic gate."""
from __future__ import annotations

import json
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"
CE = ROOT / "chaos-engine"


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
    def test_cards_exist(self) -> None:
        for name, needles in (
            ("per-cue-sync-adjudication.md", ("L52", "adjudic", "discard", "false_lead", "post_silence_rms_rise")),
            ("freeze-method-before-review.md", ("L53", "METHOD.md", "2 fix", "AMBIG", "fixed sample set")),
            ("checkpoint-generated-assets.md", ("L54", "checkpoint", "explicit")),
            ("signed-script-header.md", ("L55", "signed")),
            ("caption-min-cue-independent-sync.md", ("token-overlap", "--single-model")),
        ):
            text = (LESSONS / name).read_text(encoding="utf-8")
            for needle in needles:
                self.assertIn(needle, text, msg=f"{name}: {needle}")

    def test_index_extension_lists_l52_l55(self) -> None:
        text = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        for needle in ("L52", "L53", "L54", "L55", "per-cue-sync-adjudication"):
            self.assertIn(needle, text)

    def test_design_loop_mentions_method_cap(self) -> None:
        text = (CE / "references/design-loop.md").read_text(encoding="utf-8")
        self.assertIn("METHOD.md", text)
        self.assertIn("2 fix rounds", text)


class AdjudicGateTest(unittest.TestCase):
    def test_snap_to_clip_start_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "adjudication.json"
            path.write_text(
                json.dumps(
                    {
                        "cues": [
                            {
                                "index": 1,
                                "disputed": True,
                                "clip_start": 10.0,
                                "clip_end": 13.0,
                                "local_onset": 10.02,
                                "discard": False,
                                "rationale": "local FA near clip edge",
                            }
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("adjudic", str(path))
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("snap-to-clip-start" in h for h in payload.get("hits", [])),
                payload,
            )

    def test_discard_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "adjudication.json"
            path.write_text(
                json.dumps(
                    {
                        "cues": [
                            {
                                "index": 1,
                                "disputed": True,
                                "clip_start": 10.0,
                                "clip_end": 13.0,
                                "local_onset": 10.02,
                                "discard": True,
                                "rationale": "snap FA discarded",
                            }
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("adjudic", str(path))
            self.assertEqual(0, code, payload)

    def test_false_lead_and_bridge_and_rationale(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "adjudication.json"
            path.write_text(
                json.dumps(
                    {
                        "cues": [
                            {
                                "index": 2,
                                "disputed": True,
                                "false_lead": True,
                                "rejected": False,
                                "rationale": "wa from prev plate",
                            },
                            {
                                "index": 3,
                                "disputed": True,
                                "bridge_after_silence": {
                                    "used": True,
                                    "method": "early_asr_in_gap",
                                },
                                "rationale": "bad bridge",
                            },
                            {
                                "index": 4,
                                "disputed": True,
                                "rationale": "",
                            },
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("adjudic", str(path))
            self.assertEqual(1, code, payload)
            hits = payload.get("hits", [])
            self.assertTrue(any("false-lead-not-rejected" in h for h in hits), payload)
            self.assertTrue(any("bad-silence-bridge" in h for h in hits), payload)
            self.assertTrue(any("missing-rationale" in h for h in hits), payload)

    def test_good_bridge_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "adjudication.json"
            path.write_text(
                json.dumps(
                    {
                        "cues": [
                            {
                                "index": 5,
                                "disputed": True,
                                "bridge_after_silence": {
                                    "used": True,
                                    "method": "post_silence_rms_rise",
                                },
                                "rationale": "RMS rise after mute",
                            }
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_qc("adjudic", str(path))
            self.assertEqual(0, code, payload)


class L44TokenOverlapDocTest(unittest.TestCase):
    def test_single_model_token_overlap_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            srt = root / "cues.srt"
            srt.write_text(
                "1\n00:00:00,000 --> 00:00:01,500\nhello brave world\n\n",
                encoding="utf-8",
            )
            onsets = root / "onsets.json"
            onsets.write_text(
                json.dumps({
                    "words": [{"word": "hello", "start": 0.05, "end": 0.3}]
                }),
                encoding="utf-8",
            )
            code, payload = run_qc(
                "capsync", str(srt), "--onsets", str(onsets), "--single-model",
                "--tol", "0.15",
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("token-overlap" in h for h in payload.get("hits", [])),
                payload,
            )


if __name__ == "__main__":
    unittest.main()
