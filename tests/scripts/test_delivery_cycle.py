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

    def test_remaining_callers_wait_for_green_checks(self) -> None:
        flow = (REFS / "iterative-flow.md").read_text(encoding="utf-8")
        consult = (REFS / "consult-first.md").read_text(encoding="utf-8")
        playbook = (REFS / "work-github-playbook.md").read_text(encoding="utf-8")
        checklist = (REFS / "design-review-video-checklist.md").read_text(encoding="utf-8")
        cycle = (REFS / "delivery-cycle.md").read_text(encoding="utf-8")
        registry = (
            ROOT / "tests/scripts/test_agent_router_contract.py"
        ).read_text(encoding="utf-8")
        ordered = "two independent adversarial reviews after required checks are green"
        sequential = (REFS / "sequential-wip.md").read_text(encoding="utf-8")
        for text in (flow, consult, playbook, checklist, sequential):
            self.assertIn("after required checks are green", text)
            self.assertIn("two independent adversarial reviews", text)
        for text in (flow, checklist, sequential):
            self.assertLess(text.index(CYCLE), text.index(ordered))
            self.assertIn(ordered, text)
        self.assertLess(consult.index(CYCLE), consult.index("two independent adversarial reviews"))
        self.assertIn(ordered, playbook)
        self.assertEqual(playbook.count(ordered), 1)
        self.assertNotIn("does the acceptance review", flow)
        self.assertIn("The author is neither reviewer", flow)
        self.assertIn("The author is neither reviewer", checklist)
        self.assertIn(CYCLE, flow)
        self.assertIn(CYCLE, consult)
        self.assertIn("architect ->", consult)
        self.assertNotIn("followed by extra local tests", playbook)
        self.assertNotIn("before any local\nvalidation or review", playbook)
        self.assertIn("## Session lift", cycle)
        self.assertIn("active session only", cycle)
        self.assertNotIn("planning-selected terminal adversarial review", registry)
        self.assertIn(
            "two independent adversarial reviews after required checks are green",
            registry,
        )

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
