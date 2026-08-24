"""Executable planning-contract tests (#4775)."""

import copy
import unittest

from scripts.agents.planning_contract import validate_plan


def complete_plan() -> dict:
    return {
        "objective": "Prevent incomplete harness work from being reported as delivered.",
        "reasoning": "The user needs durable delivery, not an implementation-only checkpoint.",
        "successCriteria": ["Every owned authorized PR has a live mergedAt value."],
        "audience": ["SHAFT maintainers", "portable harness clients"],
        "scope": {"included": ["portable runtime"], "excluded": ["provider adapters"]},
        "constraints": ["provider-neutral", "repository-relative"],
        "currentState": ["Green checks do not prove feedback is resolved."],
        "intent": {
            "evidence": ["Issue #4766 requires delivery through merge."],
            "questionsAsked": ["Should related harness issues share one PR?"],
            "answers": ["Yes."],
            "discoverableQuestionsResolvedFromSources": ["Default branch: repository context."],
            "unknowns": [],
            "confidence": "high",
            "confidenceRationale": "The issues, live code, and user delivery instruction agree.",
        },
        "retrieval": {
            "memory": {"query": "harness delivery", "evidence": ["decision.delivery"]},
            "mempalace": {"query": "prior PR incident", "evidence": ["episode-4766"]},
            "graphify": {"query": "runtime callers", "evidence": ["guard -> CLI"]},
        },
        "research": [{
            "url": "https://docs.github.com/en/graphql/reference/objects#pullrequest",
            "title": "GitHub GraphQL PullRequest",
            "accessed": "2026-08-12",
            "authority": "primary",
            "finding": "Live pull-request state includes mergedAt.",
        }],
        "approaches": [
            {"name": "portable service", "pros": ["shared"], "cons": ["new module"]},
            {"name": "guard-only patch", "pros": ["small diff"], "cons": ["duplicates transport"]},
        ],
        "rejectedApproachSteelman": "A guard-only patch minimizes immediate integration work.",
        "decision": "Use a portable service and keep guard as a receipt consumer.",
        "callers": ["chaos_engine_cli.py", "guard.py", "MCP clients"],
        "steps": [{"order": 1, "action": "Add RED contract fixtures", "proof": "focused unittest"}],
        "risks": [{"risk": "stale audit", "mitigation": "bind receipt to head OID"}],
        "proofCommands": ["py -3 -m unittest tests.scripts.test_planning_contract"],
        "assumptions": ["GitHub CLI is authenticated for live delivery."],
        "ownership": {
            "implementation": "primary agent",
            "review": "independent adversarial reviewer",
            "delivery": "primary agent through authorized merge and scoped cleanup",
        },
        "diagramDecision": {
            "needed": True,
            "reason": "The change has multiple callers and a receipt workflow.",
            "mermaid": "flowchart LR\n  CLI --> Audit\n  Audit --> Receipt\n  Receipt --> Guard",
        },
    }


class PlanningContractTest(unittest.TestCase):
    def test_complete_consequential_plan_is_accepted(self):
        self.assertEqual([], validate_plan(complete_plan()))

    def test_each_evidence_and_ownership_dimension_is_required(self):
        mutations = {
            "objective": lambda plan: plan.pop("objective"),
            "success criteria": lambda plan: plan.pop("successCriteria"),
            "intent evidence": lambda plan: plan["intent"].update(evidence=[]),
            "confidence rationale": lambda plan: plan["intent"].update(confidenceRationale=""),
            "intent questions": lambda plan: plan["intent"].update(questionsAsked=[""]),
            "intent answers": lambda plan: plan["intent"].update(answers=[]),
            "discoverable questions": lambda plan: plan["intent"].update(
                discoverableQuestionsResolvedFromSources=[None]
            ),
            "memory": lambda plan: plan["retrieval"].pop("memory"),
            "mempalace": lambda plan: plan["retrieval"].pop("mempalace"),
            "graphify": lambda plan: plan["retrieval"].pop("graphify"),
            "online research": lambda plan: plan.update(research=[]),
            "primary source": lambda plan: plan["research"][0].update(authority="secondary"),
            "dated primary source": lambda plan: plan["research"][0].update(accessed="not-a-date"),
            "approaches": lambda plan: plan.update(approaches=plan["approaches"][:1]),
            "steelman": lambda plan: plan.pop("rejectedApproachSteelman"),
            "callers": lambda plan: plan.update(callers=[]),
            "ordered steps": lambda plan: plan["steps"][0].pop("order"),
            "risks": lambda plan: plan.update(risks=[]),
            "proof commands": lambda plan: plan.update(proofCommands=[]),
            "ownership": lambda plan: plan["ownership"].pop("delivery"),
        }
        for expected, mutate in mutations.items():
            with self.subTest(expected=expected):
                plan = copy.deepcopy(complete_plan())
                mutate(plan)
                self.assertTrue(
                    any(expected in violation.lower() for violation in validate_plan(plan)),
                    validate_plan(plan),
                )

    def test_material_unknown_prevents_high_confidence(self):
        plan = complete_plan()
        plan["intent"]["unknowns"] = ["Whether the user authorizes merge."]
        self.assertTrue(any("unknown" in item.lower() for item in validate_plan(plan)))

    def test_no_user_question_is_required_when_sources_resolve_intent(self):
        plan = complete_plan()
        plan["intent"]["questionsAsked"] = []
        plan["intent"]["answers"] = []
        self.assertEqual([], validate_plan(plan))

    def test_discoverable_question_cannot_be_recorded_as_a_user_question(self):
        plan = complete_plan()
        plan["intent"]["questionsAsked"] = ["What is the default branch?"]
        plan["intent"]["answers"] = ["main"]
        plan["intent"]["discoverableQuestionsResolvedFromSources"] = []
        self.assertTrue(any("discoverable" in item.lower() for item in validate_plan(plan)))

    def test_mermaid_is_conditional_not_decorative(self):
        plan = complete_plan()
        plan["diagramDecision"] = {
            "needed": False,
            "reason": "One isolated literal change has no dependency or state flow.",
        }
        self.assertEqual([], validate_plan(plan))

        plan["diagramDecision"] = {"needed": True, "reason": "workflow", "mermaid": "pretty box"}
        self.assertTrue(any("mermaid" in item.lower() for item in validate_plan(plan)))

        plan["diagramDecision"]["mermaid"] = "architecture-beta\n service cli\n service audit"
        self.assertEqual([], validate_plan(plan))

    def test_file_list_only_plan_is_rejected(self):
        violations = validate_plan({"steps": [{"order": 1, "action": "edit guard.py"}]})
        self.assertGreaterEqual(len(violations), 10)

    def test_explicit_used_skipped_and_degraded_retrieval_receipts_are_accepted(self):
        plan = complete_plan()
        plan["retrieval"] = {
            "memory": {
                "status": "used",
                "query": "harness delivery",
                "evidence": ["decision.delivery verified in live guidance"],
            },
            "mempalace": {
                "status": "skipped",
                "reason": "No cross-session history can answer the scoped ownership question.",
            },
            "graphify": {
                "status": "degraded",
                "operation": "graphify query callers of delivery receipts",
                "reason": "Cache revision marker is stale.",
                "issue": "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4995",
            },
        }
        self.assertEqual([], validate_plan(plan))

    def test_each_retrieval_status_requires_its_own_evidence_shape(self):
        mutations = (
            {"status": "used", "query": "scoped"},
            {"status": "skipped", "reason": ""},
            {"status": "degraded", "operation": "", "reason": "unavailable"},
            {"status": "unknown", "reason": "invented state"},
        )
        for receipt in mutations:
            with self.subTest(receipt=receipt):
                plan = complete_plan()
                plan["retrieval"]["memory"] = receipt
                violations = validate_plan(plan)
                self.assertTrue(
                    any("memory retrieval" in item for item in violations), violations
                )

    def test_legacy_query_and_evidence_remains_a_used_receipt(self):
        self.assertEqual([], validate_plan(complete_plan()))


if __name__ == "__main__":
    unittest.main()
