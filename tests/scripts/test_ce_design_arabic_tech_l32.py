"""L32 Arabic tech copy code-switch + bidiwao gate (Carl follow-up after #7563)."""

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
CARD = "arabic-tech-codeswitch.md"

NEEDLES = (
    "code-switch",
    "Fluent API",
    "bidiwao",
    "،",
    "headings",
    "card titles",
    "Fusha",
    "<bdi",
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


class ArabicTechLessonCardTest(unittest.TestCase):
    def test_card_needles_budget_index(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        self.assertIn("## Why", raw)
        self.assertIn("## Verify", raw)
        text = " ".join(raw.split())
        for needle in NEEDLES:
            self.assertIn(needle, text, needle)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L32 ", index)
        self.assertIn(f"]({CARD})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("45 lesson", inv)
        self.assertIn("L01-L45", inv)
        self.assertNotIn("L32 reserved", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("Arabic tech code-switch", craft)
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


class BidiWaoGateTest(unittest.TestCase):
    """Riskiest assumption: heading scope fails و+bdi in h3; body-only PASS."""

    def test_bidiwao_red_on_heading_wao_green_on_comma(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.html"
            good = Path(folder) / "good.html"
            # Red: و between two Latin bdi islands inside a card title (h3).
            bad.write_text(
                "<article class='card'>"
                "<h3><bdi lang='en'>Selenium</bdi> و <bdi lang='en'>Appium</bdi></h3>"
                "<p>body ok</p></article>",
                encoding="utf-8",
            )
            # Green: Arabic comma; and attached و before non-bdi Arabic is fine.
            good.write_text(
                "<article class='card'>"
                "<h3><bdi lang='en'>Selenium</bdi> ، <bdi lang='en'>Appium</bdi></h3>"
                "<h2><bdi lang='en'>Fluent API</bdi></h2>"
                "<p>سائقو <bdi lang='en'>Selenium</bdi> و <bdi lang='en'>Appium</bdi></p>"
                "</article>",
                encoding="utf-8",
            )
            code, payload = run_qc("bidiwao", str(bad))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertTrue(payload.get("hits"), payload)

            code, payload = run_qc("bidiwao", str(good))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)

    def test_bidiwao_body_only_passes_default_heading_scope(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            body = Path(folder) / "body.html"
            body.write_text(
                "<main><h2>عنوان نظيف</h2>"
                "<p><bdi lang='en'>Selenium</bdi> و <bdi lang='en'>Appium</bdi></p>"
                "</main>",
                encoding="utf-8",
            )
            code, payload = run_qc("bidiwao", str(body))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            # --all widens scope and must fail the same body pattern.
            code, payload = run_qc("bidiwao", str(body), "--all")
            self.assertEqual(1, code, payload)

    def test_bidiwao_attached_wao_before_bdi_fails_in_heading(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "attach.html"
            bad.write_text(
                "<h3>مع و<bdi lang='en'>SHAFT</bdi></h3>",
                encoding="utf-8",
            )
            code, payload = run_qc("bidiwao", str(bad))
            self.assertEqual(1, code, payload)


if __name__ == "__main__":
    unittest.main()
