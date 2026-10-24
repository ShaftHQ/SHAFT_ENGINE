"""Design-skills reel lessons: gate tiers, UI motion, hover-reveal, diagrams, slides, taste v2 (#7345)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
REF = DESIGN / "references"
GROUPS = {
    "web-craft/anti-generic-lint.md": (
        "**Floor** blocks delivery", "Invented facts are Floor on all surfaces", "**Reflex**",
        "overrule with a reason on record", "**Finish**: advise, never block", "Built from the job",
        "the page ends on it", "eyebrow label over every heading", "tracked all-caps labels",
        "caseless scripts", "each of the 7 tells", "Credit openly licensed photos", "Nutlope/hallmark", "MIT"),
    "web-craft/motion-tokens.md": (
        "Should it animate?", "high-frequency actions", "Never animate from `scale(0)`", "`scale(0.9)`-`scale(0.97)`",
        "interruptible", "`transform` and `opacity` only", "blur masks a rough swap",
        "subsequent peers open instantly", "springs", "`@media (hover: hover)`", "emilkowalski/skills", "MIT",
        "Rules 1-3 are reference only from transitions.dev", "Rules 4-11 are adapted"),
    "web-craft/micro-polish.md": (
        "Decorative only", "pixel-registered", "50% overlay", "pointer CSS vars", "`--x`, `--y`",
        "commercial and unbranded", "rAF-throttled", "`(hover: none)`", "never tap-gated",
        "not focusable", "`:focus-visible`", "mirrors in RTL", "reference only"),
    "web-craft/diagram-design.md": (
        "A table would do?", "one distinct idea", "co-travel", "Every edge carries information",
        "at most 2 focal", "r8 rounded elbows", ">= 12px", "fan out", "<= 14 characters", "6-10px mask gap",
        "Complexity budget", "overview plus detail", "Removal pass", "`role=\"img\"`", "flow direction mirrors",
        "cathrynlavery/diagram-design", "MIT"),
    "slides/deck-system.md": ("rendered previews", "never pick from adjectives", "zarazhangrui/frontend-slides", "MIT"),
    "web-visual-direction.md": (
        "from the subject", "two lines or fewer at every width", "five seconds", "Equal outer margins",
        "One signature move", "1440, 768 and 390"),
    "web-interface-audit.md": ("`overscroll-behavior: contain`", "`@media (pointer: fine)`"),
    "motion-principles.md": ("web-craft/motion-tokens.md", "should-it-animate"),
}
PINS = ("6010f91a9935f1c167dfc500fb1a475681892e6b", "434b7f91364665f2f733b310ec54809bf8f37937",
        "7ec868225c6caa040558c321ee0fefa0e73f83a8", "2d228630ba1446365f614a108cb8252f1feda202",
        "0b9b10cab3dbf695642fa1a6c4b4f60df9083689", "9906a34d640d2111f724544cbc50f7f130569ae1")


def _text(path: Path) -> str:
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8"))


class DesignReelLessonsTest(unittest.TestCase):
    def test_every_rule_group_has_its_needles_within_budget(self) -> None:
        for rel, needles in GROUPS.items():
            path = REF / rel
            with self.subTest(card=rel):
                self.assertLessEqual(len(path.read_bytes()), 6144)
                for needle in needles:
                    self.assertIn(needle, _text(path))

    def test_router_and_bot_entry_untouched_and_cards_reachable(self) -> None:
        self.assertNotIn("diagram-design", (DESIGN / "SKILL.md").read_text(encoding="utf-8"))
        self.assertLessEqual(len((DESIGN / "SKILL.md").read_bytes()), 4096)
        self.assertIn("(diagram-design.md)", (REF / "web-craft/index.md").read_text(encoding="utf-8"))
        self.assertIn("web-craft/diagram-design.md", (REF / "image-graphics.md").read_text(encoding="utf-8"))

    def test_pins_are_full_shas_and_old_pins_are_gone(self) -> None:
        inventory = (DESIGN / "INVENTORY.md").read_text(encoding="utf-8")
        for sha in PINS:
            self.assertIn(f"`{sha}`", inventory)
        for old in ("ce26fc25c0e5", "e3d624baaf29"):
            self.assertNotIn(old, inventory)
        self.assertIn("Unicorn Studio (proprietary", inventory)

    def test_addon_description_counts_five_web_craft_cards(self) -> None:
        desc = (DESIGN / "addon.json").read_text(encoding="utf-8")
        self.assertIn("five web-craft cards", desc)
        self.assertIn("diagram design", desc)
        self.assertNotIn("four web-craft", desc)

    def test_new_cards_are_project_neutral(self) -> None:
        banned = ("automatest", "shaft", "mohab", "price", *private_terms())
        for rel in GROUPS:
            lowered = (REF / rel).read_text(encoding="utf-8").lower()
            for word in banned:
                with self.subTest(card=rel, word=word):
                    self.assertNotIn(word, lowered)


if __name__ == "__main__":
    unittest.main()
