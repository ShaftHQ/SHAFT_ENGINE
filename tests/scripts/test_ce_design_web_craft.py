"""Design-skills web-craft cards W01-W04 and their resource verdicts (#7336)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
WEB = DESIGN / "references/web-craft"
CARDS = {
    "anti-generic-lint": ("Inter", "purple-to-blue gradient", "card-in-card", "grey-on-colour", "emoji icons",
                          "all visual surfaces", "`bolder`", "`quieter`", "`distill`", "`typeset`", "`layout`",
                          "`harden`", "`polish`", "`critique`", "`audit`", "`PRODUCT.md`", "`DESIGN.md`",
                          "Google DESIGN.md spec", "every script", "is not a finding", "Apache-2.0", "NOTICE"),
    "motion-tokens": ("CSS custom properties", "prefers-reduced-motion: reduce", "origin-aware", "transform-origin", "mirrors in `dir=\"rtl\"`", "inline-start/inline-end", "never a hard-coded left/right",
                      "Copy no text", "forbids redistributing"),
    "gsap-web-motion": ("official `gsap-scrolltrigger` skill", "Reduced motion is mandatory", "gsap.matchMedia()",
                        "D08 motion principles", "`markers`", "MIT"),
    "micro-polish": ("inner radius = outer radius - padding", "Optical alignment", "mirrors in RTL", "directional icons (arrows, play, chevrons) flip", "non-directional icons do not", "Layered shadows",
                     "Hit target >= 44px", "Hover and focus", "slides and stills", "MIT"),
}
RESOURCES = ("pbakaus/impeccable", "transitions.dev", "gsap-skills", "make-interfaces-feel-better")


def _text(path: Path) -> str:
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8"))


class WebCraftCardsTest(unittest.TestCase):
    def test_cards_have_shape_rules_and_budget(self) -> None:
        for name, needles in CARDS.items():
            path = WEB / f"{name}.md"
            raw = path.read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertTrue(raw.startswith(f"---\nname: {name}\ndescription: Use when"))
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
                    self.assertIn(section, raw)
                for needle in needles:
                    self.assertIn(needle, _text(path))

    def test_cards_are_reachable_on_demand_only(self) -> None:
        index = (WEB / "index.md").read_text(encoding="utf-8")
        for name in CARDS:
            self.assertIn(f"({name}.md)", index)
        self.assertIn("../web-craft/index.md", (DESIGN / "references/craft/index.md").read_text(encoding="utf-8"))
        self.assertIn("web-craft/motion-tokens.md", (DESIGN / "references/motion-principles.md").read_text(encoding="utf-8"))
        self.assertNotIn("web-craft/", (DESIGN / "SKILL.md").read_text(encoding="utf-8"))
        self.assertIn("16 web-craft cards", (DESIGN / "INVENTORY.md").read_text(encoding="utf-8"))

    def test_every_resource_has_licence_and_verdict_and_nothing_is_vendored(self) -> None:
        research = _text(DESIGN / "references/research/design-resources-2026-10.md")
        for resource in RESOURCES:
            with self.subTest(resource=resource):
                row = next(line for line in research.split(" | [") if resource in line)
                self.assertRegex(row, r"\*\*(adapt|reference)")
        self.assertIn("reference only", research)
        for vendored in ("impeccable", "transitions", "gsap-skills", "make-interfaces-feel-better"):
            self.assertFalse(any(vendored in p.name for p in DESIGN.rglob("*") if p.is_dir()))

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", *private_terms())
        for path in WEB.glob("*.md"):
            with self.subTest(path=path.name):
                lowered = path.read_text(encoding="utf-8").lower()
                for word in banned:
                    self.assertNotIn(word, lowered)


if __name__ == "__main__":
    unittest.main()
