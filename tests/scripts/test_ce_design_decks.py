"""Deck review cards S05/S06 and the cxnglue, pptxparity, deckgap and slidediff checks (#7418)."""

from __future__ import annotations

import json
import shutil
import struct
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
import zlib
import zipfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
SLIDES = DESIGN / "references/slides"
QC = DESIGN / "scripts/design_qc.py"
HAS_FFMPEG = bool(shutil.which("ffmpeg") and shutil.which("ffprobe"))
CARDS = {
    "deck-qc-measurable.md": ("name: deck-qc-measurable", "zero rows", "rendered spans", "padding", "Ghost layout art", "pixel",
                              "design_qc.py cxnglue", "design_qc.py deckgap", "design_qc.py slidediff", "Waivers"),
    "deck-roundtrip-review.md": ("name: deck-roundtrip-review", "modified time", "speaker notes", "10 MB",
                                 "design_qc.py pptxparity", "reject", "delta", "re-check external state"),
}


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run([sys.executable, str(QC), *args], capture_output=True, text=True, check=False)  # nosec B603
    return result.returncode, json.loads(result.stdout.strip().splitlines()[-1])


def png(path: Path, width: int, height: int, paint) -> Path:
    rows = b"".join(b"\x00" + b"".join(bytes(paint(x, y)) for x in range(width)) for y in range(height))

    def chunk(kind: bytes, data: bytes) -> bytes:
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data))

    path.write_bytes(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
                     + chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))
    return path


def deck(path: Path, slides: list[dict]) -> Path:
    """slides: {connectors: [xml], notes: [text runs], links: [urls], font: name}."""
    with zipfile.ZipFile(path, "w") as z:
        for number, slide in enumerate(slides, 1):
            body = "".join(slide.get("connectors", []))
            z.writestr(f"ppt/slides/slide{number}.xml",
                       f'<p:sld xmlns:a="a" xmlns:p="p"><a:r><a:latin typeface="{slide.get("font", "Lato")}"/></a:r>{body}</p:sld>')
            rels = "".join(f'<Relationship Id="l{i}" Type="http://x/hyperlink" Target="{u}" TargetMode="External"/>'
                           for i, u in enumerate(slide.get("links", [])))
            if "notes" in slide:
                rels += f'<Relationship Id="n" Type="http://x/notesSlide" Target="../notesSlides/notesSlide{number}.xml"/>'
                runs = "".join(f"<a:t>{t}</a:t>" for t in slide["notes"])
                z.writestr(f"ppt/notesSlides/notesSlide{number}.xml",
                           '<p:notes xmlns:a="a" xmlns:p="p"><p:sp><p:nvSpPr><p:nvPr><p:ph type="sldImg"/></p:nvPr></p:nvSpPr>'
                           '<a:t>IMAGE</a:t></p:sp><p:sp><p:nvSpPr><p:nvPr><p:ph type="body" idx="1"/></p:nvPr></p:nvSpPr>'
                           f"<p:txBody><a:p>{runs}</a:p></p:txBody></p:sp></p:notes>")
            z.writestr(f"ppt/slides/_rels/slide{number}.xml.rels", f"<Relationships>{rels}</Relationships>")
    return path


def connector(glued: tuple[bool, bool], cx: int = 914400, cy: int = 0) -> str:
    st = '<a:stCxn id="2" idx="3"/>' if glued[0] else ""
    en = '<a:endCxn id="3" idx="1"/>' if glued[1] else ""
    return (f'<p:cxnSp><p:nvCxnSpPr><p:cNvPr id="9" name="Arrow 9"/><p:cNvCxnSpPr>{st}{en}</p:cNvCxnSpPr></p:nvCxnSpPr>'
            f'<p:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm></p:spPr></p:cxnSp>')


class DeckCardsTest(unittest.TestCase):
    def test_cards_exist_within_budget_with_their_rules(self) -> None:
        for name, needles in CARDS.items():
            raw = (SLIDES / name).read_text(encoding="utf-8")
            text = " ".join(raw.split())
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                for needle in needles:
                    self.assertIn(needle, text)

    def test_cards_are_linked_and_main_skill_untouched(self) -> None:
        linked = (DESIGN / "references/craft/training-decks.md").read_text(encoding="utf-8")
        system = (SLIDES / "deck-system.md").read_text(encoding="utf-8")
        for name in CARDS:
            self.assertIn(f"slides/{name}", linked)
        self.assertIn("deck-qc-measurable.md", system)
        self.assertLessEqual(len((DESIGN / "references/craft/training-decks.md").read_bytes()), 6144)
        self.assertLessEqual(len((DESIGN / "SKILL.md").read_bytes()), 4096)

    def test_cards_carry_no_project_data(self) -> None:
        for name in CARDS:
            text = (SLIDES / name).read_text(encoding="utf-8").lower()
            for term in ("mohab", "automatest", "carl", "coach", "trainee", "invoice", "price list"):
                self.assertNotIn(term, text)


class PdcaRulesTest(unittest.TestCase):
    def test_reviewer_evidence_and_artifact_version_rules(self) -> None:
        text = " ".join((ROOT / "chaos-engine/references/multi-agent-pdca.md").read_text(encoding="utf-8").split())
        for needle in ("never evidence", "location, the measured value and the expected value",
                       "names the version it reviewed", "modified time or hash"):
            self.assertIn(needle, text)
        for term in ("shaft", "grok", "selenium", "maven"):
            self.assertNotIn(term, text.lower())


class CxnGlueTest(unittest.TestCase):
    def check(self, slide_xml: list[str], code: int, *extra: str) -> dict:
        with tempfile.TemporaryDirectory() as folder:
            path = deck(Path(folder) / "d.pptx", [{"connectors": slide_xml}])
            got, payload = run_qc("cxnglue", str(path), *extra)
            self.assertEqual(code, got, payload)
            return payload

    def test_flags_each_unglued_end(self) -> None:
        self.assertEqual(1, len(self.check([connector((True, False))], 1)["hits"]))
        self.assertEqual(2, len(self.check([connector((False, False))], 1)["hits"]))
        self.check([connector((True, True))], 0)

    def test_flags_tiny_connectors_unless_disabled(self) -> None:
        tiny = connector((True, True), cx=76200, cy=0)  # 6 pt
        self.check([tiny], 1)
        self.check([tiny], 0, "--min-length", "0")

    def test_long_rotated_elbow_uses_longer_extent(self) -> None:
        self.check([connector((True, True), cx=0, cy=914400)], 0)  # vertical, 72 pt


class PptxParityTest(unittest.TestCase):
    def run_pair(self, before: list[dict], after: list[dict], *extra: str) -> tuple[int, dict]:
        with tempfile.TemporaryDirectory() as folder:
            a, b = deck(Path(folder) / "a.pptx", before), deck(Path(folder) / "b.pptx", after)
            return run_qc("pptxparity", str(b), "--before", str(a), *extra)

    def test_pptxparity_notes_split_runs_equal(self) -> None:  # the riskiest assumption of #7418
        base = {"notes": ["Next: open the demo"], "links": ["https://example.org/a"]}
        self.assertEqual(0, self.run_pair([base], [{**base, "notes": ["Next: open", " the demo"]}])[0])
        code, payload = self.run_pair([base], [{**base, "notes": ["Next: open the other demo"]}])
        self.assertEqual(1, code)
        self.assertIn("slide 1: speaker notes changed", payload["hits"])

    def test_dropped_link_or_slide_or_font_fails(self) -> None:
        base = {"notes": ["x"], "links": ["https://example.org/a"]}
        self.assertEqual(1, self.run_pair([base], [{**base, "links": []}])[0])
        self.assertEqual(1, self.run_pair([base, base], [base])[0])
        code, payload = self.run_pair([base], [{**base, "font": "Arial"}])
        self.assertEqual(1, code)
        self.assertEqual({"typeface added: Arial", "typeface removed: Lato"}, set(payload["hits"]))

    def test_allow_lists_a_slide_that_may_change(self) -> None:
        base = {"notes": ["x"]}
        self.assertEqual(0, self.run_pair([base], [{"notes": ["y"]}], "--allow", "1")[0])

    def test_export_size_cap_with_headroom(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            path = deck(Path(folder) / "a.pptx", [{}])
            size = path.stat().st_size
            self.assertEqual(0, run_qc("pptxparity", str(path), "--max-bytes", str(size * 2))[0])
            self.assertEqual(1, run_qc("pptxparity", str(path), "--max-bytes", str(size), "--headroom", "0.05")[0])


class DeckGapTest(unittest.TestCase):
    def check(self, data: dict, code: int, *extra: str) -> dict:
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "m.json"
            path.write_text(json.dumps(data), encoding="utf-8")
            got, payload = run_qc("deckgap", str(path), *extra)
            self.assertEqual(code, got, payload)
            return payload

    CARD = {"x": 0, "y": 0, "w": 200, "h": 100}

    def test_padding_floors_per_side(self) -> None:
        good = {"id": "card", "outer": self.CARD, "inner": {"x": 12, "y": 7, "w": 176, "h": 86}}
        self.check({"pads": [good]}, 0)
        bad = {**good, "inner": {"x": 12, "y": 7, "w": 176, "h": 90}}  # bottom 3
        self.assertIn("card: padding b 3.0 below 7", self.check({"pads": [bad]}, 1)["hits"])

    def test_item_override_and_symmetry(self) -> None:
        chip = {"id": "chip", "outer": self.CARD, "inner": {"x": 10, "y": 3.6, "w": 180, "h": 92.8}, "min": {"l": 10, "r": 10, "t": 3.6, "b": 3.6}}
        self.check({"pads": [chip]}, 0)
        lopsided = {**chip, "inner": {"x": 10, "y": 3.6, "w": 170, "h": 92.8}}
        self.check({"pads": [lopsided]}, 0)
        self.check({"pads": [lopsided]}, 1, "--sym", "2")

    def test_gap_and_chain_report_the_whole_table(self) -> None:
        title, image, caption = ({"id": n, "x": 0, "y": y, "w": 50, "h": 20} for n, y in (("title", 0), ("image", 26), ("caption", 60)))
        payload = self.check({"chains": [{"id": "card", "boxes": [title, image, caption]}]}, 0)
        self.assertEqual([6.0, 14.0], [row["gap"] for row in payload["table"]])
        touching = {**image, "y": 22}  # 2 pt under the title
        payload = self.check({"chains": [{"id": "card", "boxes": [title, touching, caption]}]}, 1)
        self.assertEqual(["card:title>image: gap 2.0 below 6"], payload["hits"])
        self.check({"gaps": [{"id": "g", "a": title, "b": image, "min": 7}]}, 1)

    def test_horizontal_axis_and_empty_file(self) -> None:
        a, b = {"x": 0, "y": 0, "w": 10, "h": 10}, {"x": 12, "y": 0, "w": 10, "h": 10}
        self.check({"gaps": [{"id": "h", "a": a, "b": b, "axis": "x", "min": 2}]}, 0)
        self.check({"gaps": [{"id": "h", "a": a, "b": b, "axis": "x", "min": 3}]}, 1)
        self.assertEqual(4, run_qc_file({}))


def run_qc_file(data: dict) -> int:
    with tempfile.TemporaryDirectory() as folder:
        path = Path(folder) / "m.json"
        path.write_text(json.dumps(data), encoding="utf-8")
        return run_qc("deckgap", str(path))[0]


@unittest.skipUnless(HAS_FFMPEG, "ffmpeg not installed")
class SlideDiffTest(unittest.TestCase):
    def rounds(self, folder: str, edits: dict[str, tuple[int, int]]) -> tuple[Path, Path]:
        before, after = Path(folder) / "r1", Path(folder) / "r2"
        before.mkdir()
        after.mkdir()
        for name in ("s1.png", "s2.png"):
            png(before / name, 40, 40, lambda x, y: (255, 255, 255))
            dot = edits.get(name)
            png(after / name, 40, 40, lambda x, y, d=dot: (0, 0, 0) if d and x < 10 and y < d[1] else (255, 255, 255))
        return before, after

    def test_unexpected_change_fails_and_expected_passes(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            before, after = self.rounds(folder, {"s2.png": (0, 10)})
            code, payload = run_qc("slidediff", str(before), str(after))
            self.assertEqual(1, code, payload)
            self.assertEqual(["s2.png: changed 6.25% of pixels, not expected"], payload["hits"])
            code, payload = run_qc("slidediff", str(before), str(after), "--expect", "s2")
            self.assertEqual((0, ["s2.png"]), (code, list(payload["changed"])), payload)

    def test_expected_slide_that_did_not_change_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            before, after = self.rounds(folder, {})
            code, payload = run_qc("slidediff", str(before), str(after), "--expect", "s1.png")
            self.assertEqual(1, code, payload)
            self.assertEqual(["s1.png: expected to change but did not"], payload["hits"])

    def test_small_or_faint_changes_are_below_the_threshold(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            before, after = self.rounds(folder, {})
            png(after / "s1.png", 40, 40, lambda x, y: (250, 255, 255))  # summed difference 5, under 30
            self.assertEqual(0, run_qc("slidediff", str(before), str(after))[0])

    def test_missing_slide_fails(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            before, after = self.rounds(folder, {})
            (after / "s2.png").unlink()
            self.assertEqual(1, run_qc("slidediff", str(before), str(after))[0])


class HelpTest(unittest.TestCase):
    def test_help_lists_the_checks(self) -> None:
        result = subprocess.run([sys.executable, str(QC), "--help"], capture_output=True, text=True, check=False)  # nosec B603
        for name in ("cxnglue", "pptxparity", "deckgap", "slidediff"):
            self.assertIn(name, result.stdout)


if __name__ == "__main__":
    unittest.main()
