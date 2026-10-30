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


class DesignQaR2DeltaLessonCardsTest(unittest.TestCase):
    def test_cards_needles_budget_index(self) -> None:
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        for name, needles in CARDS.items():
            raw = (LESSONS / name).read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                self.assertIn("## Why", raw)
                self.assertIn("## Verify", raw)
                text = " ".join(raw.split())
                for needle in needles:
                    self.assertIn(needle, text)
                self.assertIn(f"]({name})", index)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("39 lesson", inv)
        self.assertIn("L01-L39", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("viral pacing FLAG", craft)
        self.assertIn("ear-lock type templates", craft)
        mute = (LESSONS / "mute-span-gate.md").read_text(encoding="utf-8")
        self.assertIn("Cue longer than 2 s", mute)
        self.assertIn("contiguous-cut", mute)

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "mohab", "hrsd", "elmenus", "stc", "price", *private_terms())
        names = list(CARDS) + ["mute-span-gate.md"]
        for name in names:
            lowered = (LESSONS / name).read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class DesignQaR2DeltaDiscoverabilityTest(unittest.TestCase):
    def test_qc_docstring_documents_advisory_flag(self) -> None:
        doc = QC.read_text(encoding="utf-8").split('"""')[1]
        self.assertIn("viralpacing", doc)
        self.assertIn("status=flag with exit 0", doc)
        self.assertIn("never a hard fail", doc)

    def test_lessons_index_l15_mentions_contiguous_cut(self) -> None:
        row = next(
            line
            for line in (LESSONS / "index.md").read_text(encoding="utf-8").splitlines()
            if "[L15 " in line
        )
        self.assertIn("cue >2s", row)
        self.assertIn("contiguous cut", row)


class ViralPacingFlagTest(unittest.TestCase):
    def test_late_punchline_flags_not_fails(self) -> None:
        # Riskiest assumption: late punchline → status=flag exit 0 (not fail).
        code, payload = run_qc(
            "viralpacing",
            "--content-dur", "116.28",
            "--punchline-at", "97.0",
            "--type", "viral",
            "--threshold", "0.65",
        )
        self.assertEqual(0, code, payload)
        self.assertEqual("flag", payload.get("status"), payload)
        self.assertGreater(payload.get("punchline_rel", 0), 0.65)

    def test_early_punchline_passes(self) -> None:
        code, payload = run_qc(
            "viralpacing",
            "--content-dur", "60",
            "--punchline-at", "20",
            "--type", "viral",
        )
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_qa_type_passes_even_when_late(self) -> None:
        code, payload = run_qc(
            "viralpacing",
            "--content-dur", "100",
            "--punchline-at", "90",
            "--type", "qa",
        )
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_bad_content_dur_errors(self) -> None:
        code, payload = run_qc(
            "viralpacing",
            "--content-dur", "nan",
            "--punchline-at", "10",
            "--type", "viral",
        )
        self.assertEqual(2, code, payload)
        self.assertEqual("error", payload.get("status"))


class MetadataDescTypeTemplatesTest(unittest.TestCase):
    def _meta(self, title: str, line1: str) -> str:
        return (
            f"# YouTube metadata\n\n## Title (≤70)\n\n{title}\n\n"
            f"## Description (4 lines)\n\n"
            f"{line1}\n"
            "الإجابة كاملة: https://www.youtube.com/watch?v=OcgL3MwC014&t=2126s\n"
            "https://example.com\n"
            "#testing #shorts\n"
        )

    def test_viral_prefix_passes_qa_prefix_fails_for_viral_type(self) -> None:
        title = "claim only title"
        with tempfile.TemporaryDirectory() as folder:
            viral = Path(folder) / "viral.md"
            qa = Path(folder) / "qa.md"
            viral.write_text(self._meta(title, f"مقطع: {title}"), encoding="utf-8")
            qa.write_text(
                self._meta(title, f"مقطع من الـ Q&A: {title}"), encoding="utf-8",
            )
            code, payload = run_qc(
                "metadatadesc", str(viral),
                "--type", "viral",
                "--site", "https://example.com",
                "--tags", "#testing #shorts",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            self.assertEqual("viral", payload.get("type"))
            code, payload = run_qc(
                "metadatadesc", str(qa),
                "--type", "viral",
                "--site", "https://example.com",
                "--tags", "#testing #shorts",
            )
            self.assertEqual(1, code, payload)
            self.assertIn("line1_prefix", payload.get("errors", []))

    def test_default_type_remains_qa(self) -> None:
        title = "سؤال تجريبي؟"
        with tempfile.TemporaryDirectory() as folder:
            good = Path(folder) / "qa.md"
            good.write_text(
                self._meta(title, f"مقطع من الـ Q&A: {title}"), encoding="utf-8",
            )
            code, payload = run_qc(
                "metadatadesc", str(good),
                "--site", "https://example.com",
                "--tags", "#testing #shorts",
            )
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)
            self.assertEqual("qa", payload.get("type", "qa"))


if __name__ == "__main__":
    unittest.main()
