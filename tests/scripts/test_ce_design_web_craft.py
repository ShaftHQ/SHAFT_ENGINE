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

    def test_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", *private_terms())
        for path in WEB.glob("*.md"):
            with self.subTest(path=path.name):
                lowered = path.read_text(encoding="utf-8").lower()
                for word in banned:
                    self.assertNotIn(word, lowered)

if __name__ == "__main__":
    unittest.main()
