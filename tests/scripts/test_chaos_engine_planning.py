"""Focused planning-contract tests for keep-asking-then-unattended (#5248)."""

from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PLANNING = ROOT / "chaos-engine/references/work-github-planning.md"
RECEIPT = ROOT / "chaos-engine/references/research-receipt.md"
CONSULT = ROOT / "chaos-engine/references/consult-first.md"
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
FORBIDDEN = "Ask once, at the start, then go unattended"


def contract_violations(planning: str, receipt: str, consult: str, skill: str) -> list[str]:
    """Return violations of the #5248 planning contract for the given texts."""
    violations: list[str] = []
    if FORBIDDEN in planning:
        violations.append(
            "work-github-planning.md still says Ask once, at the start, then go unattended"
        )
    if FORBIDDEN in receipt or FORBIDDEN in consult or FORBIDDEN in skill:
        violations.append("aligned guidance restored Ask once, at the start, then go unattended")
    for required in (
        "keep asking follow-ups",
        "decision-ready",
        "consultant agent",
        "implementation clarifications",
        "never granted",
    ):
        if required not in planning:
            violations.append(f"work-github-planning.md missing {required!r}")
    if "keep asking follow-ups" not in receipt:
        violations.append("research-receipt.md missing keep-asking-during-planning rule")
    if "incomplete" not in consult.lower():
        violations.append("consult-first.md missing incomplete-without-those-questions sentence")
    plan_step = next(
        (line for line in skill.splitlines() if line.startswith("3. Plan by ")),
        "",
    )
    if "decision-ready" not in plan_step or "unattended" not in plan_step:
        violations.append("SKILL.md plan step missing decision-ready unattended contract")
    return violations


class ChaosEnginePlanningContractTest(unittest.TestCase):
    def test_live_guidance_keeps_asking_then_goes_unattended(self):
        violations = contract_violations(
            PLANNING.read_text(encoding="utf-8"),
            RECEIPT.read_text(encoding="utf-8"),
            CONSULT.read_text(encoding="utf-8"),
            SKILL.read_text(encoding="utf-8"),
        )
        self.assertEqual([], violations)

    def test_restoring_ask_once_sentence_fails(self):
        mutated = PLANNING.read_text(encoding="utf-8") + f"\n{FORBIDDEN}\n"
        violations = contract_violations(
            mutated,
            RECEIPT.read_text(encoding="utf-8"),
            CONSULT.read_text(encoding="utf-8"),
            SKILL.read_text(encoding="utf-8"),
        )
        self.assertTrue(
            any(FORBIDDEN in item for item in violations),
            violations,
        )

    def test_chaos_engine_references_do_not_restore_ask_once(self):
        hits = [
            str(path.relative_to(ROOT)).replace("\\", "/")
            for path in (ROOT / "chaos-engine").rglob("*.md")
            if FORBIDDEN in path.read_text(encoding="utf-8")
        ]
        self.assertEqual([], hits)


if __name__ == "__main__":
    unittest.main()
