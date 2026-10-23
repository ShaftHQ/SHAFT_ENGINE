"""Design-skills lesson cards L01-L05 and the resource verdicts (#7333)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
CARDS = {
    "hook-speech-match": ("verbatim", "per-item end time", "monotonic", "repro that must fail", "Muted test"),
    "mixed-script-captions": ("LRI ... PDI", "`؟`", "before any wrapping markup", "2 lines at most",
                              "by rendering them", "WJ + thin space + WJ", "smallest size", "file that ships"),
    "talk-edit-and-layers": ("0.8 s", "No mid-sentence jump cuts", "pre-cut time", "RMS dip", "2 or more models",
                             "opaque base", "mask (blur)"),
    "signed-output-release": ("one opt-in flag", "byte-identical", "`framemd5`", "never re-render",
                              "failed before the fix", "10 fps", "CRLF", "shared profile"),
    "cross-surface-takeaways": ("at most 1 brand-colour key word", "1.05", "Explicit CTA end card",
                                "big cursor", "Masked speaker", "Pattern interrupt", "verb first", "reduced motion"),
}
RESOURCES = ("taste-skill", "ui-ux-pro-max-skill", "design-motion-principles", "getdesign.md", "design-md-chrome")


def _text(path: Path) -> str:
    return " ".join(path.read_text(encoding="utf-8").split())


class LessonCardsTest(unittest.TestCase):
    def test_cards_have_shape_rules_and_budget(self) -> None:
        for name, needles in CARDS.items():
            path = LESSONS / f"{name}.md"
            raw = path.read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertTrue(raw.startswith(f"---\nname: {name}\ndescription: Use when"))
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
                    self.assertIn(section, raw)
                for needle in needles:
                    self.assertIn(needle, _text(path))

    def test_index_lists_every_card_and_is_reachable_on_demand(self) -> None:
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        for name in CARDS:
            self.assertIn(f"]({name}.md)", index)
        self.assertIn("../lessons/index.md", (DESIGN / "references/craft/index.md").read_text(encoding="utf-8"))
        self.assertIn("lessons/mixed-script-captions.md",
                      (DESIGN / "references/captions-subtitles.md").read_text(encoding="utf-8"))
        self.assertIn("lessons/talk-edit-and-layers.md",
                      (DESIGN / "references/cutting-pacing.md").read_text(encoding="utf-8"))
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        self.assertNotIn("lessons/", router)  # on demand via craft index; router budget untouched

    def test_every_resource_has_licence_and_verdict_and_nothing_is_vendored(self) -> None:
        table = (DESIGN / "references/research/design-resources-2026-10.md").read_text(encoding="utf-8")
        for resource in RESOURCES:
            row = next((line for line in table.splitlines() if resource in line and line.startswith("|")), "")
            with self.subTest(resource=resource):
                self.assertRegex(row, r"\*\*(adopt|reference|skip)")
                self.assertRegex(row, r"MIT|Apache-2\.0|no clear licence")
        self.assertIn("design-resources-2026-10.md", (DESIGN / "INVENTORY.md").read_text(encoding="utf-8"))
        names = {p.name.lower() for p in DESIGN.rglob("*")}
        for vendored in ("taste-skill", "ui-ux-pro-max", "design-motion-principles", "design-md-chrome"):
            self.assertFalse(any(vendored in n for n in names), vendored)

    def test_lessons_are_project_neutral(self) -> None:
        banned = re.compile(r"automatest|shaft|mohab|hrsd|stc\b|#00335c|#ffb547|price|budget talk", re.I)
        for path in [*LESSONS.glob("*.md"), DESIGN / "references/research/design-resources-2026-10.md"]:
            with self.subTest(path=path.name):
                self.assertIsNone(banned.search(path.read_text(encoding="utf-8")))


if __name__ == "__main__":
    unittest.main()
