"""The delivery cycle is architect, at most 3 PDCA iterations, then two reviews."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
REFS = CE / "references"

CYCLE = "at most 3 PDCA"
REVIEWS = "two independent adversarial reviews"
LINK = "delivery-cycle.md"
OLD = ("second round only for", "One fresh-context review", "one reviewer, one round")
LESSON_HEADINGS = (
    "## Canonical record",
    "## Past approval",
    "## Unknown stays unknown",
    "## Restoration",
    "## Non-success is not green",
    "## Completed phase",
    "## Historical inventory",
    "## Identity acceptance",
    "## Leaving Investigate",
)


class DeliveryCycleContractTest(unittest.TestCase):
    def test_cycle_owns_the_cap_and_is_not_a_quota(self) -> None:
        text = (REFS / "delivery-cycle.md").read_text(encoding="utf-8")
        self.assertIn(CYCLE, text)
        self.assertIn(REVIEWS, text)
        self.assertIn("not a quota", text)
        self.assertIn("Architect", text)
        self.assertIn("investigate-lessons.md", text)
        self.assertNotIn("at least three", text.lower())
        self.assertNotIn("minimum 3", text.lower())

    def test_always_loaded_cards_point_here_and_drop_the_one_review_exception(self) -> None:
        skill = (CE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        kanban = (CE / "skills/kanban/SKILL.md").read_text(encoding="utf-8")
        router = (REFS / "router-contract.md").read_text(encoding="utf-8")
        delegation = (REFS / "delegation.md").read_text(encoding="utf-8")
        for text in (skill, kanban, router, delegation):
            self.assertIn(LINK, text)
            for old in OLD:
                self.assertNotIn(old, text)
        self.assertIn(CYCLE, skill)
        self.assertIn(REVIEWS, skill)

    def test_pdca_cap_matches_the_cycle(self) -> None:
        pdca = (REFS / "multi-agent-pdca.md").read_text(encoding="utf-8")
        self.assertIn("3 PDCA iterations", pdca)
        self.assertIn(LINK, pdca)
        self.assertNotIn("5 rounds without convergence", pdca)

    def test_lessons_are_generic(self) -> None:
        text = (REFS / "investigate-lessons.md").read_text(encoding="utf-8")
        for heading in LESSON_HEADINGS:
            self.assertIn(heading, text)
        lowered = text.lower()
        for term in ("mohab", "shaft", "coach", "carl", "automatest", "youtube", "linkedin"):
            self.assertNotIn(term, lowered)
        self.assertIn("Waiting on others", text)
        self.assertIn("Won't do", text)
