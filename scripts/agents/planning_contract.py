"""Validate evidence-backed, implementation-owned plans."""

from __future__ import annotations

import argparse
import json
from datetime import date
from pathlib import Path
from typing import Any


def _text(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip())


def _texts(value: Any) -> bool:
    return isinstance(value, list) and bool(value) and all(_text(item) for item in value)


def _iso_date(value: Any) -> bool:
    if not _text(value):
        return False
    try:
        date.fromisoformat(value)
    except ValueError:
        return False
    return True


def _source_discoverable_question(value: str) -> bool:
    lowered = value.lower()
    return any(term in lowered for term in (
        "default branch", "which file", "what file", "current version", "existing test",
        "which module", "what module", "repository path", "current behavior",
    ))


def validate_plan(plan: object) -> list[str]:  # noqa: C901
    """Return deterministic violations for one consequential-work plan."""
    if not isinstance(plan, dict):
        return ["plan must be a JSON object"]
    violations: list[str] = []
    for field, label in (
        ("objective", "objective"), ("reasoning", "reasoning"),
        ("currentState", "current state"), ("decision", "decision"),
    ):
        value = plan.get(field)
        valid = _text(value) if field not in {"currentState"} else _texts(value)
        if not valid:
            violations.append(f"missing {label}")
    for field, label in (
        ("successCriteria", "success criteria"), ("audience", "audience"),
        ("constraints", "constraints"), ("callers", "callers"),
        ("proofCommands", "proof commands"), ("assumptions", "assumptions"),
    ):
        if not _texts(plan.get(field)):
            violations.append(f"missing {label}")
    scope = plan.get("scope")
    if not isinstance(scope, dict) or not _texts(scope.get("included")) or not isinstance(scope.get("excluded"), list):
        violations.append("missing included and excluded scope")

    intent = plan.get("intent")
    if not isinstance(intent, dict):
        violations.append("missing intent evidence, questions, unknowns, and confidence")
    else:
        if not _texts(intent.get("evidence")):
            violations.append("missing intent evidence")
        questions = intent.get("questionsAsked")
        answers = intent.get("answers")
        valid_questions = isinstance(questions, list) and all(_text(item) for item in questions)
        valid_answers = isinstance(answers, list) and all(_text(item) for item in answers)
        if not valid_questions:
            violations.append("invalid intent questions")
        elif any(_source_discoverable_question(item) for item in questions):
            violations.append("source-discoverable questions must be resolved from repository evidence, not asked of the user")
        if not valid_answers or not valid_questions or len(answers) != len(questions):
            violations.append("missing intent answers corresponding to every question")
        discoverable = intent.get("discoverableQuestionsResolvedFromSources")
        if not isinstance(discoverable, list) or any(not _text(item) for item in discoverable):
            violations.append("invalid discoverable questions record")
        unknowns = intent.get("unknowns")
        if not isinstance(unknowns, list):
            violations.append("missing material unknowns")
        elif unknowns:
            violations.append("material unknowns must be resolved before high-confidence implementation")
        if intent.get("confidence") not in {"high", "very high"}:
            violations.append("confidence must be high with evidence")
        if not _text(intent.get("confidenceRationale")):
            violations.append("missing confidence rationale")

    retrieval = plan.get("retrieval")
    for store in ("memory", "mempalace", "graphify"):
        receipt = retrieval.get(store) if isinstance(retrieval, dict) else None
        if not isinstance(receipt, dict) or not _text(receipt.get("query")) or not _texts(receipt.get("evidence")):
            violations.append(f"missing {store} retrieval receipt")

    research = plan.get("research")
    if not isinstance(research, list) or not research:
        violations.append("missing authoritative online research")
    else:
        if not any(
            isinstance(source, dict)
            and source.get("authority") == "primary"
            and _text(source.get("url"))
            and str(source.get("url")).startswith("https://")
            and _iso_date(source.get("accessed"))
            and _text(source.get("finding"))
            for source in research
        ):
            violations.append("missing dated primary source research")

    approaches = plan.get("approaches")
    if not isinstance(approaches, list) or len(approaches) < 2 or any(
        not isinstance(item, dict)
        or not _text(item.get("name"))
        or not _texts(item.get("pros"))
        or not _texts(item.get("cons"))
        for item in approaches or []
    ):
        violations.append("at least two complete approaches are required")
    if not _text(plan.get("rejectedApproachSteelman")):
        violations.append("missing rejected approach steelman")

    steps = plan.get("steps")
    if not isinstance(steps, list) or not steps or any(
        not isinstance(step, dict)
        or not isinstance(step.get("order"), int)
        or isinstance(step.get("order"), bool)
        or not _text(step.get("action"))
        or not _text(step.get("proof"))
        for step in steps or []
    ) or [step.get("order") for step in steps or []] != list(range(1, len(steps or []) + 1)):
        violations.append("missing ordered steps with proof")
    risks = plan.get("risks")
    if not isinstance(risks, list) or not risks or any(
        not isinstance(risk, dict) or not _text(risk.get("risk")) or not _text(risk.get("mitigation"))
        for risk in risks or []
    ):
        violations.append("missing risks and mitigations")
    ownership = plan.get("ownership")
    for phase in ("implementation", "review", "delivery"):
        if not isinstance(ownership, dict) or not _text(ownership.get(phase)):
            violations.append(f"missing {phase} ownership")

    diagram = plan.get("diagramDecision")
    if not isinstance(diagram, dict) or not isinstance(diagram.get("needed"), bool) or not _text(diagram.get("reason")):
        violations.append("missing conditional diagram decision")
    elif diagram["needed"] and not (
        _text(diagram.get("mermaid"))
        and any(
            diagram["mermaid"].lstrip().startswith(token)
            for token in (
                "flowchart", "graph", "sequenceDiagram", "stateDiagram", "classDiagram",
                "architecture-beta", "erDiagram", "journey", "gantt", "timeline",
            )
        )
    ):
        violations.append("needed diagram must contain a Mermaid dependency, component, or workflow diagram")
    return violations


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    arguments = parser.parse_args(argv)
    try:
        plan = json.loads(arguments.input.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        print(json.dumps({"valid": False, "violations": [f"cannot read plan: {error}"]}, sort_keys=True))
        return 1
    violations = validate_plan(plan)
    print(json.dumps({"valid": not violations, "violations": violations}, sort_keys=True))
    return 1 if violations else 0


if __name__ == "__main__":
    raise SystemExit(main())
