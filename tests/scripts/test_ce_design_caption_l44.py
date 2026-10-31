"""L44 caption min-cue duration + independent ASR/FA sync."""

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
CARD = "caption-min-cue-independent-sync.md"


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


def write_srt(path: Path, cues: list[tuple[float, float, str]]) -> None:
    blocks = []
    for index, (start, end, text) in enumerate(cues, 1):
        def stamp(s: float) -> str:
            ms = round(s * 1000)
            return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
        blocks.append(f"{index}\n{stamp(start)} --> {stamp(end)}\n{text}\n")
    path.write_text("\n".join(blocks), encoding="utf-8")


class L44LessonCardTest(unittest.TestCase):
    def test_card_shape(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertTrue(raw.startswith("---\nname:"))
        for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
            self.assertIn(section, raw)
        for needle in ("L44", "mincuedur", "capsync", "0.8", "independent", "codeswitch"):
            self.assertIn(needle, raw)
        self.assertNotRegex(raw.lower(), r"\bcoach\b|\bcarl\b")
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn(CARD, index)


class MinCueDurTest(unittest.TestCase):
    def test_short_cue_fails_until_merged(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            short = Path(folder) / "short.srt"
            write_srt(short, [(0.0, 0.4, "hi"), (1.0, 2.0, "ok")])
            code, payload = run_qc("mincuedur", str(short))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("0.400s" in h for h in payload.get("hits", [])))
            merged = Path(folder) / "merged.srt"
            write_srt(merged, [(0.0, 2.0, "hi ok")])
            code, payload = run_qc("mincuedur", str(merged))
            self.assertEqual(0, code, payload)


class CapSyncTest(unittest.TestCase):
    def test_echo_onsets_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            write_srt(cues, [(0.0, 1.0, "Hello"), (1.5, 3.0, "world")])
            onsets = Path(folder) / "o.json"
            onsets.write_text(
                json.dumps({"words": [
                    {"word": "Hello", "start": 0.0},
                    {"word": "world", "start": 1.5},
                ]}),
                encoding="utf-8",
            )
            code, payload = run_qc("capsync", str(cues), "--onsets", str(onsets), "--single-model")
            self.assertEqual(1, code, payload)
            self.assertIn("onsets-echo-cue-starts", payload.get("hits", []))

    def test_independent_onsets_pass_and_codeswitch_tol(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            write_srt(cues, [(0.0, 1.2, "Hello"), (2.0, 3.5, "API")])
            onsets = Path(folder) / "o.json"
            onsets.write_text(
                json.dumps({"words": [
                    {"word": "Hello", "start": 0.05},
                    {"word": "API", "start": 2.25},
                ]}),
                encoding="utf-8",
            )
            code, payload = run_qc("capsync", str(cues), "--onsets", str(onsets), "--single-model")
            self.assertEqual(0, code, payload)
            self.assertGreaterEqual(payload.get("max_drift_ms", 0), 50.0)

    def test_missing_onsets_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            cues = Path(folder) / "c.srt"
            write_srt(cues, [(0.0, 1.0, "Hello")])
            code, payload = run_qc(
                "capsync", str(cues), "--onsets", str(Path(folder) / "missing.json"),
                "--single-model",
            )
            self.assertEqual(1, code, payload)
            self.assertIn("missing-independent-onsets", payload.get("hits", []))


if __name__ == "__main__":
    unittest.main()
