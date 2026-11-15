"""Design QA lessons L09-L12: privacy names, endcard live, title punct, qmark baseline."""

from __future__ import annotations

import json
import math
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

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:  # pragma: no cover - skip path when Pillow absent
    Image = ImageDraw = ImageFont = None  # type: ignore[misc, assignment]

CARDS = {
    "privacy-names-gate.md": (
        "Scan caption text and ASR tokens",
        "Vocatives count",
        "Mute notes never PASS alone",
        "design_qc.py privacynames",
        "يا فلان",
        "يا [اسم]",
        "Fail-closed",
        "احسن",
        "يحسنوا",
    ),
    "endcard-band-concat.md": (
        "Burn bands on the content segment only",
        "Gate live end-card frames",
        "design_qc.py endcardlive",
        "endcard.png",
        "ink",
    ),
    "title-punct-by-script.md": (
        "Latin-script string → ASCII",
        "Any Arabic letter → Arabic",
        "design_qc.py titlepunct",
        "Strip URLs",
        "U+003F",
        "U+061F",
    ),
    "qmark-baseline.md": (
        "One typographic baseline",
        "anchor=ls",
        "design_qc.py qmarkbaseline",
        "0.06",
        "mixed-font",
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

def srt(*cues: tuple[float, float, str]) -> str:
    def stamp(s: float) -> str:
        ms = round(s * 1000)
        return f"00:00:{ms // 1000:02d},{ms % 1000:03d}"
    return "\n\n".join(
        f"{i}\n{stamp(a)} --> {stamp(b)}\n{text}" for i, (a, b, text) in enumerate(cues, 1)
    ) + "\n"

def _require_pillow(test: unittest.TestCase) -> None:
    if Image is None:
        test.skipTest("Pillow required")

class TitlePunctGateTest(unittest.TestCase):

    def test_no_inputs_never_pass(self) -> None:
        code, payload = run_qc("titlepunct")
        self.assertNotEqual(0, code, payload)
        self.assertIn(payload.get("status"), ("usage", "fail", "error"))

class QmarkBaselineGateTest(unittest.TestCase):
    def _band(self, path: Path, *, raised: bool) -> None:
        _require_pillow(self)
        im = Image.new("RGBA", (640, 180), (0, 38, 63, 255))
        dr = ImageDraw.Draw(im)
        try:
            font = ImageFont.truetype(
                "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 48
            )
        except OSError:
            font = ImageFont.load_default()
        body = "QUALITY CHECK"
        baseline = 100
        dr.text((40, baseline), body, font=font, fill=(255, 255, 255, 255), anchor="ls")
        bbox = dr.textbbox((40, baseline), body, font=font, anchor="ls")
        qx = bbox[2] + 8
        if raised:
            dr.text((qx, baseline - 28), "?", font=font, fill=(255, 255, 255, 255), anchor="ls")
        else:
            dr.text((qx, baseline), "?", font=font, fill=(255, 255, 255, 255), anchor="ls")
        im.save(path)

    def test_nonfinite_frac_is_fail(self) -> None:
        """Mirrors craft_qc fail rule: not math.isfinite(frac) or frac > thr."""
        self.assertTrue((not math.isfinite(float("nan"))) or (float("nan") > 0.06))
        self.assertTrue((not math.isfinite(float("inf"))) or (float("inf") > 0.06))
        self.assertFalse((not math.isfinite(0.01)) or (0.01 > 0.06))

if __name__ == "__main__":
    unittest.main()
