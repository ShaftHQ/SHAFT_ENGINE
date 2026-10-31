"""L33 LESS Custom CSS longhand grid + escaped calc (slash grid / bare calc gate)."""

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
CARD = "less-css-grid-calc.md"

NEEDLES = (
    "LESS 1.3.3",
    "grid-column-start",
    "grid-column-end",
    "lessgrid",
    '~"calc',
    "slash",
    "Custom CSS",
    "gutters",
)


def run_qc(*args: str) -> tuple[int, dict]:
    proc = subprocess.run(  # nosec B603 - argv list, no shell.
        [sys.executable, str(QC), *args],
        capture_output=True,
        text=True,
        check=False,
    )
    payload = {}
    if proc.stdout.strip():
        try:
            payload = json.loads(proc.stdout)
        except json.JSONDecodeError:
            payload = {"raw": proc.stdout, "stderr": proc.stderr}
    else:
        payload = {"stderr": proc.stderr}
    return proc.returncode, payload


class LessCssLessonCardTest(unittest.TestCase):
    def test_card_needles_budget_index(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        self.assertIn("## Why", raw)
        self.assertIn("## Verify", raw)
        text = " ".join(raw.split())
        for needle in NEEDLES:
            self.assertIn(needle, text, needle)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L33 ", index)
        self.assertIn(f"]({CARD})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("63 lesson", inv)
        self.assertIn("L01-L64", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("LESS CSS", craft)
        sq = (DESIGN / "references/site-builders/squarespace-7-1.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("L33", sq)
        self.assertIn("lessgrid", sq)
        web = DESIGN / "references/craft/web-design.md"
        self.assertLessEqual(len(web.read_text(encoding="utf-8").splitlines()), 120)

    def test_card_is_project_neutral(self) -> None:
        banned = (
            "automatest", "mohab", "hrsd", "elmenus", "stc", "price",
            "carl", "coach", *private_terms(),
        )
        lowered = (LESSONS / CARD).read_text(encoding="utf-8").lower()
        for word in banned:
            self.assertNotIn(word, lowered, word)


class LessGridGateTest(unittest.TestCase):
    """Riskiest assumption: slash grid-column + bare calc FAIL; longhand + escaped PASS."""

    def test_lessgrid_red_on_slash_and_bare_calc_green_on_longhand(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.css"
            good = Path(folder) / "good.css"
            bad.write_text(
                ".hero { grid-column: 1 / -1; padding-inline: calc(24px + 8px); }\n",
                encoding="utf-8",
            )
            good.write_text(
                ".hero {\n"
                "  grid-column-start: 1;\n"
                "  grid-column-end: -1;\n"
                '  padding-inline: ~"calc(24px + 8px)";\n'
                "}\n"
                "/* grid-column: 1 / -1 commented ok */\n",
                encoding="utf-8",
            )
            code, payload = run_qc("lessgrid", str(bad))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            hits = " ".join(payload.get("hits") or [])
            self.assertIn("slash-grid", hits, payload)
            self.assertIn("bare-calc", hits, payload)

            code, payload = run_qc("lessgrid", str(good))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_lessgrid_flags_grid_row_slash(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "row.css"
            bad.write_text(".x { grid-row: 2 / span 3; }\n", encoding="utf-8")
            code, payload = run_qc("lessgrid", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(any("slash-grid" in h for h in payload.get("hits") or []), payload)


if __name__ == "__main__":
    unittest.main()
