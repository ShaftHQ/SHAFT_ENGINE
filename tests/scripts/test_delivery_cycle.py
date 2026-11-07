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
ONE_REVIEW_CLOSE = (
    "The reviewer runs an acceptance review",
    "owner and reviewer both sign off",
    "final scope commit and automated finding batch",
    "before any local validation or review",
    "followed by extra local tests",
    "after independent review",
    "one acceptance review",
    "one independent review",
)
CALLERS = (
    "iterative-flow.md",
    "consult-first.md",
    "work-github-playbook.md",
    "design-review-video-checklist.md",
)
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
        self.assertIn("The author is neither reviewer", text)
        self.assertIn("Do not review an unchanged head a third time", text)
        self.assertIn("A new commit invalidates both passes", text)
        self.assertIn("after required checks are green", text)
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
        self.assertNotIn("](delivery-cycle.md)", skill)
        self.assertNotIn("](../../references/delivery-cycle.md)", skill)
        self.assertNotIn("optional terminal review", router)

    def test_callers_use_the_cycle_instead_of_the_old_caps(self) -> None:
        planning = (REFS / "work-github-planning.md").read_text(encoding="utf-8")
        streams = (REFS / "parallel-streams.md").read_text(encoding="utf-8")
        gates = (REFS / "delivery-phase-gates.md").read_text(encoding="utf-8")
        playbook = (
            ROOT / "shaft-skills/ce-pack/references/playbooks/agentic-pdca-loop.md"
        ).read_text(encoding="utf-8")
        sequential = (REFS / "sequential-wip.md").read_text(encoding="utf-8")
        web_design = (
            CE / "addons/design-skills/references/craft/web-design.md"
        ).read_text(encoding="utf-8")
        self.assertNotIn("recommend on", planning)
        self.assertIn(REVIEWS, planning)
        self.assertNotIn("five PDCA", streams)
        self.assertIn(CYCLE, streams)
        self.assertNotIn("Optional adversarial", gates)
        self.assertIn("delivery-cycle.md", gates)
        self.assertNotIn("for two passes", playbook)
        self.assertIn("Stop when Check accepts", playbook)
        self.assertIn(CYCLE, playbook)
        self.assertIn(REVIEWS, playbook)
        for text in (sequential, web_design):
            self.assertIn(REVIEWS, text)
            self.assertNotIn("one round", text)
            self.assertNotIn("returns to the reviewer who raised the blocker", text)

    def test_leftover_callers_keep_two_reviews_not_a_one_review_close(self) -> None:
        for name in CALLERS:
            raw = (REFS / name).read_text(encoding="utf-8")
            text = " ".join(raw.split())
            with self.subTest(file=name):
                self.assertIn(REVIEWS, raw)
                for old in ONE_REVIEW_CLOSE:
                    self.assertNotIn(old, text)
        flow = " ".join((REFS / "iterative-flow.md").read_text(encoding="utf-8").split())
        consult = " ".join((REFS / "consult-first.md").read_text(encoding="utf-8").split())
        playbook = " ".join((REFS / "work-github-playbook.md").read_text(encoding="utf-8").split())
        checklist = (REFS / "design-review-video-checklist.md").read_text(encoding="utf-8")
        self.assertIn("The author is neither reviewer", flow)
        self.assertIn("the cap is not a quota", flow)
        self.assertIn("architect, at most 3 PDCA", consult)
        self.assertIn("after required checks are green", consult)
        self.assertIn("Never force a review between earlier phases", consult)
        self.assertIn("local tests that are the required checks", playbook)
        self.assertIn("after required checks are green", playbook)
        self.assertIn("Do not review a red head", playbook)
        self.assertIn("then sign-off", checklist)

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
