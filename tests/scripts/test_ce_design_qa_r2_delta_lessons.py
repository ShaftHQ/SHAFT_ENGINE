"""Design QA r2 delta lessons L21–L22 + L15 cue>2s wording + viralpacing / G16 --type."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
QC = DESIGN / "scripts/design_qc.py"

CARDS = {
    "viral-pacing-flag.md": (
        "Advisory FLAG, not FAIL",
        "0.65",
        "design_qc.py viralpacing",
        "type=viral",
        "punchline_rel",
    ),
    "earlock-type-templates.md": (
        "Ear-lock the title",
        "type=viral",
        "مقطع:",
        "مقطع من الـ Q&A:",
        "metadatadesc",
        "titlepolarity",
    ),
}

def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run(  # nosec B603
        [sys.executable, str(QC), *args],
        capture_output=True, text=True, check=False,
    )
    lines = [ln for ln in result.stdout.strip().splitlines() if ln.strip()]
    payload = json.loads(lines[-1]) if lines else {
        "status": "error", "stdout": result.stdout, "stderr": result.stderr,
    }
    return result.returncode, payload

class ViralPacingFlagTest(unittest.TestCase):

    def test_bad_content_dur_errors(self) -> None:
        code, payload = run_qc(
            "viralpacing",
            "--content-dur", "nan",
            "--punchline-at", "10",
            "--type", "viral",
        )
        self.assertEqual(2, code, payload)
        self.assertEqual("error", payload.get("status"))

if __name__ == "__main__":
    unittest.main()
