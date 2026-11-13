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
        self.assertNotIn("before any local\nvalidation or review", playbook)
        self.assertNotIn("followed by extra local tests", playbook)
        self.assertIn("## Session lift", cycle)
        self.assertIn("active session only", cycle)
        self.assertNotIn("planning-selected terminal adversarial review", registry)
        self.assertIn(ordered, registry)

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
        flow_raw = (REFS / "iterative-flow.md").read_text(encoding="utf-8")
        streams = (REFS / "parallel-streams.md").read_text(encoding="utf-8")
        self.assertNotIn("does the acceptance review", flow_raw)
        self.assertNotIn("→ independent review →", flow_raw)
        self.assertNotIn("→ independent review →", streams)
        self.assertLess(
            flow_raw.index("At most 3 iterations"),
            flow_raw.index("Run two independent adversarial reviews"),
        )

    def test_complex_media_edit_keeps_a_standing_review_rule(self) -> None:
        cycle = (REFS / "delivery-cycle.md").read_text(encoding="utf-8")
        checklist = (REFS / "design-review-video-checklist.md").read_text(encoding="utf-8")
        self.assertIn("## Complex video or audio editing", checklist)
        self.assertIn("Each cycle has two independent adversarial reviewers", checklist)
        self.assertIn("A failed cycle is fixed before a later cycle counts", checklist)
        self.assertIn("not uploaded while a cycle is failed", checklist)
        self.assertIn("design-review-video-checklist.md", cycle)
        self.assertIn("not limited to the active session", cycle)
        self.assertNotIn("at least three", cycle.lower())
        self.assertNotIn("minimum 3", cycle.lower())
        self.assertIn("one accepted Check is not final", cycle)
        self.assertIn("Other work still stops when Check accepts", cycle)
        self.assertIn("Stop when Check accepts", cycle)
        section = checklist.split("## Complex video or audio editing", 1)[1]
        section = section.split("## Follow-up", 1)[0]
        for sentence in (
            "runs three PDCA cycles before it is called final",
            "Each cycle has two independent adversarial reviewers",
            "The author is neither reviewer",
            "A failed cycle is fixed before a later cycle counts",
            "not uploaded while a cycle is failed",
            "Required checks are green before those reviews start",
            "A new cut invalidates earlier passes",
            "When one span of an accepted recording is wrong, keep the correct audio and replace only that span.",
            "An owner recording used as a spoken token is the pronunciation.",
            "An automatic transcript spelling does not reject that token.",
            "Do not alter copied samples from an accepted recording to meet a silence floor.",
            "A spoken introduction follows [narration-review.md](details/narration-review.md).",
            "ask the owner whether to regenerate the whole file or patch the bad spans",
            "The standing preference is to regenerate so the read stays consistent.",
            "The two adversarial reviewers apply the narration review checklist and this section.",
            "They do not pass an item they did not measure.",
            "Do not spell a dropped word in another writing system to force the token.",
            "A spoken introduction is one synthesis of the whole script.",
            "Do not splice a louder take into it.",
            "Loudness is one gain for the whole file.",
            "The opening sentence is not louder than the rest.",
            "Short-term speech loudness stays within 3 LU of the integrated loudness.",
            "True peak stays at or below -1 dBTP.",
            "A pause of at least 200 ms is at or below -70 dBFS RMS.",
            "Do not zero a dip shorter than 200 ms.",
            "An automatic transcript catches gibberish, a torn word, and an interrupted word.",
            "A proper noun is judged against the owner's recording, not against transcript spelling.",
            "A breath or a pause sits at a clause boundary.",
            "A filler is not clustered: no two fillers inside eight seconds.",
        ):
            self.assertIn(sentence, section)
        narration = (REFS / "details" / "narration-review.md").read_text(encoding="utf-8")
        for sentence in (
            "One gain for the whole file",
            "no two fillers inside eight seconds",
            "one synthesis of the whole script",
            "150 to 190 words",
            "A proper noun is judged against the owner's recording, not against\n  transcript spelling.",
            "ask the owner whether to regenerate the whole file from scratch or to patch the bad spans",
            "The owner's standing preference is to regenerate the whole file",
            "The two adversarial reviewers load this file and the standing section",
            "Do not train a likeness, and do not patch a take, from a synthesized file.",
            "Do not spell a dropped word in another writing system to force the token.",
            "the private review listen is -20 LUFS ±2",
            "The gate is 140 to 175 words a minute.",
            "The gate is at or below 280.",
            "A full-file transcript can drop a real",
            "Do not time-stretch the file.",
            "Measure integrated loudness on the stereo programme, not on one channel",
            "Short-term loudness is the 3-second window defined in EBU",
            "Transcribe a 16 kHz resample, not the 48 kHz file samples.",
        ):
            self.assertIn(sentence, narration)
        flow = checklist.split("### Flow", 1)[1].split("###", 1)[0]
        self.assertIn("does not stop after one accepted cycle", flow)
        self.assertIn("does not sign off, publish, or share on that single pair", flow)
        self.assertIn("is not uploaded while a cycle is failed", flow)
        self.assertIn("shared only after the standing section is satisfied", flow)
        self.assertIn("#complex-video-or-audio-editing", flow)
        self.assertLess(
            flow.index("at most 3 PDCA"),
            flow.index("two independent adversarial reviews after required checks are green"),
        )
        callers = {
            "technical-video": CE / "addons/design-skills/references/pipelines/technical-video.md",
            "runbook": CE / "addons/design-skills/references/pipelines/runbook.md",
            "design-loop": REFS / "design-loop.md",
        }
        for name, path in callers.items():
            text = path.read_text(encoding="utf-8")
            with self.subTest(caller=name):
                self.assertIn("complex video or audio edit", text)
                self.assertIn("design-review-video-checklist.md", text)

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

    def test_large_delivery_opens_a_draft_at_the_first_commit(self) -> None:
        path = REFS / "work-github-playbook.md"
        raw = path.read_bytes()
        text = raw.decode("utf-8")
        phrases = (
            "draft pull request on the first commit",
            "pushes again after each locally green slice",
            "does not arm auto-merge",
            "where the playbook already requires it, waits until that exact head is green and the required reviews have passed",
            "one-file reversible fix is not a large delivery",
            "not while the pull request is still a draft or the head is red",
        )
        for phrase in phrases:
            self.assertEqual(1, text.count(phrase), phrase)
        self.assertLessEqual(len(raw), 16384)
        self.assertEqual(0, text.count("This rule does not auto-merge."))
