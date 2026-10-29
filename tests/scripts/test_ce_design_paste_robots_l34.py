"""L34 paste robots noindex/nofollow + cmsmeta extension (red on main)."""

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
CARD = "paste-robots-noindex.md"

NEEDLES = (
    "noindex",
    "nofollow",
    "X-Robots-Tag",
    "cmsmeta",
    "liveseo",
    "Code-block",
    "product pages",
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


class PasteRobotsLessonCardTest(unittest.TestCase):
    def test_card_needles_budget_index(self) -> None:
        raw = (LESSONS / CARD).read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        self.assertIn("## Why", raw)
        self.assertIn("## Verify", raw)
        text = " ".join(raw.split())
        for needle in NEEDLES:
            self.assertIn(needle, text, needle)
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        self.assertIn("L34 ", index)
        self.assertIn(f"]({CARD})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("34 lesson", inv)
        self.assertIn("L01-L34", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("paste robots L34", craft)
        embed = (DESIGN / "references/site-builders/hosted-cms-embed.md").read_text(
            encoding="utf-8",
        )
        self.assertIn("L34", embed)
        self.assertIn("liveseo", embed)
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


class CmsmetaPasteRobotsGateTest(unittest.TestCase):
    """Riskiest assumption: cmsmeta fails closed on paste robots noindex (red on main)."""

    def test_cmsmeta_rejects_robots_noindex_and_x_robots(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad_meta = Path(folder) / "robots.html"
            bad_x = Path(folder) / "xrobots.html"
            bad_hdr = Path(folder) / "hdr.html"
            good = Path(folder) / "good.html"
            bad_meta.write_text(
                '<div><meta name="robots" content="noindex, nofollow"></div>',
                encoding="utf-8",
            )
            bad_x.write_text(
                '<div><meta http-equiv="X-Robots-Tag" content="noindex"></div>',
                encoding="utf-8",
            )
            bad_hdr.write_text(
                "<!--\nX-Robots-Tag: noindex, nofollow\n-->\n<div>ok</div>",
                encoding="utf-8",
            )
            good.write_text("<div><h1>Product</h1></div>", encoding="utf-8")
            for bad in (bad_meta, bad_x, bad_hdr):
                code, payload = run_qc("cmsmeta", str(bad))
                self.assertEqual(1, code, payload)
                self.assertEqual("fail", payload.get("status"), payload)
                self.assertGreaterEqual(payload.get("robots_block_hits", 0), 1, payload)
            code, payload = run_qc("cmsmeta", str(good))
            self.assertEqual((0, "pass"), (code, payload.get("status")), payload)
            self.assertEqual(0, payload.get("robots_block_hits", -1), payload)

    def test_cmsmeta_rejects_nofollow_only(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "nofollow.html"
            bad.write_text(
                '<meta name="robots" content="nofollow">',
                encoding="utf-8",
            )
            code, payload = run_qc("cmsmeta", str(bad))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)


if __name__ == "__main__":
    unittest.main()
