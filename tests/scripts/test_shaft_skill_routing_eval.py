"""Routing corpus, native compiler, and result evaluator tests (#4642)."""

import json
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.shaft_skill_routing_eval import (
        compile_claude_evals,
        compile_codex_cases,
        evaluate_results,
        package_decision,
        validate_corpus,
        validate_repository,
    )
except ImportError:
    compile_claude_evals = None
    compile_codex_cases = None
    evaluate_results = None
    package_decision = None
    validate_corpus = None
    validate_repository = None


ROOT = Path(__file__).resolve().parents[2]
CORPUS_PATH = ROOT / "agent-plugins/shaft-skills/evals/cases.json"
REVIEW_PATH = ROOT / "shaft-skills/quality-review.json"


class ShaftSkillRoutingEvalTest(unittest.TestCase):
    def setUp(self):
        self.review = {
            "skills": {
                "skill-a": {"confusion_with": ["skill-b"]},
                "skill-b": {"confusion_with": ["skill-a"]},
            }
        }
        self.corpus = {
            "schema_version": 1,
            "package": "shaft-skills",
            "thresholds": {"case_pass_rate": 1.0, "positive_skill_coverage": 1.0},  # nosec B105 - routing thresholds.
            "cases": [
                {
                    "id": "route-skill-a",
                    "prompt": "Design acceptance criteria and expose requirement gaps for checkout.",
                    "expected_skill": "skill-a",
                    "rejected_skills": ["skill-b"],
                },
                {
                    "id": "route-skill-b",
                    "prompt": "Create a risk-based test schedule with entry and exit criteria.",
                    "expected_skill": "skill-b",
                    "rejected_skills": ["skill-a"],
                },
            ],
        }

    def test_evaluation_api_is_available(self):
        for function in (
            compile_claude_evals,
            compile_codex_cases,
            evaluate_results,
            package_decision,
            validate_corpus,
            validate_repository,
        ):
            self.assertTrue(callable(function))

    def test_corpus_requires_one_positive_case_per_skill(self):
        self.corpus["cases"].pop()

        defects = validate_corpus(self.corpus, self.review)

        self.assertIn("positive-coverage", {defect["code"] for defect in defects})

    def test_corpus_requires_each_reviewed_confusion_direction(self):
        self.corpus["cases"][0]["rejected_skills"] = []

        defects = validate_corpus(self.corpus, self.review)

        self.assertIn("confusion-coverage", {defect["code"] for defect in defects})

    def test_prompts_cannot_supply_the_expected_skill_name(self):
        self.corpus["cases"][0]["prompt"] += " Choose skill-a."

        defects = validate_corpus(self.corpus, self.review)

        self.assertIn("vacuous-prompt", {defect["code"] for defect in defects})

    def test_native_compilers_share_the_canonical_case_ids_and_assertions(self):
        claude = compile_claude_evals(self.corpus)
        codex_lines, schema = compile_codex_cases(self.corpus)

        self.assertEqual(claude["skill_name"], "shaft-developer")
        self.assertEqual([row["id"] for row in claude["evals"]], [1, 2])
        self.assertTrue(all(row["files"] == [] for row in claude["evals"]))
        self.assertTrue(all(row["expectations"] for row in claude["evals"]))
        codex = [json.loads(line) for line in codex_lines.splitlines()]
        self.assertEqual([row["case_id"] for row in codex], [case["id"] for case in self.corpus["cases"]])
        self.assertEqual(schema["required"], ["chosen_skill"])
        self.assertFalse(schema["additionalProperties"])

    def test_changed_selection_fails_the_machine_readable_assertion(self):
        records = [
            {"case_id": "route-skill-a", "chosen_skill": "skill-b"},
            {"case_id": "route-skill-b", "chosen_skill": "skill-b"},
        ]

        report = evaluate_results(self.corpus, "Codex CLI", "0.146.0", records)

        by_id = {row["case_id"]: row for row in report["results"]}
        self.assertEqual(by_id["route-skill-a"]["verdict"], "fail")
        self.assertEqual(by_id["route-skill-b"]["verdict"], "pass")
        self.assertEqual(report["summary"]["case_pass_rate"], 0.5)

    def test_external_blocker_is_not_fabricated_as_a_routing_result(self):
        report = evaluate_results(
            self.corpus,
            "Claude Code",
            "2.1.223",
            [],
            external_blocker="ANTHROPIC_API_KEY is unavailable",
        )

        self.assertEqual({row["verdict"] for row in report["results"]}, {"external_blocker"})
        self.assertEqual(report["summary"]["failures"], 0)

    def test_client_failure_is_not_fabricated_as_a_routing_result(self):
        report = evaluate_results(
            self.corpus,
            "Claude Code",
            "2.1.223",
            [],
            client_failure="native client exited without a routing response",
        )

        self.assertEqual({row["verdict"] for row in report["results"]}, {"client_failure"})
        self.assertEqual(report["summary"]["failures"], 0)
        self.assertEqual(report["summary"]["client_failures"], len(self.corpus["cases"]))
        self.assertIsNone(report["summary"]["case_pass_rate"])

    def test_package_decision_uses_only_post_compression_failures_and_warnings(self):
        codex_passing = evaluate_results(
            self.corpus,
            "Codex CLI",
            "0.146.0",
            [
                {"case_id": case["id"], "chosen_skill": case["expected_skill"]}
                for case in self.corpus["cases"]
            ],
        )
        blocked = evaluate_results(
            self.corpus,
            "Claude Code",
            "2.1.223",
            [],
            external_blocker="credential unavailable",
        )
        claude_passing = evaluate_results(
            self.corpus,
            "Claude Code",
            "2.1.223",
            [
                {"case_id": case["id"], "chosen_skill": case["expected_skill"]}
                for case in self.corpus["cases"]
            ],
        )

        self.assertEqual(package_decision([])["decision"], "insufficient-evidence")
        self.assertEqual(
            package_decision([codex_passing, blocked])["decision"],
            "insufficient-evidence",
        )
        self.assertEqual(
            package_decision([codex_passing, claude_passing])["decision"],
            "retain-single-package",
        )
        codex_passing["context_budget_warnings"] = ["descriptions shortened"]
        self.assertEqual(
            package_decision([codex_passing, claude_passing])["decision"],
            "investigate-split-or-profile",
        )

    def test_late_external_blocker_preserves_completed_case_results(self):
        report = evaluate_results(
            self.corpus,
            "Claude Code",
            "2.1.223",
            [{"case_id": "route-skill-a", "chosen_skill": "skill-a"}],
            external_blocker="401 Unauthorized",
        )

        by_id = {row["case_id"]: row for row in report["results"]}
        self.assertEqual(by_id["route-skill-a"]["verdict"], "pass")
        self.assertEqual(by_id["route-skill-b"]["verdict"], "external_blocker")
        self.assertEqual(report["summary"]["passes"], 1)

    def test_live_repository_corpus_and_generated_outputs_are_current(self):
        defects = validate_repository(ROOT)

        self.assertEqual(defects, [])
        corpus = json.loads(CORPUS_PATH.read_text(encoding="utf-8"))
        review = json.loads(REVIEW_PATH.read_text(encoding="utf-8"))
        self.assertEqual(len(corpus["cases"]), 30)
        self.assertEqual(
            {case["expected_skill"] for case in corpus["cases"]},
            set(review["skills"]),
        )

    def test_generated_drift_is_reported(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "agent-plugins/shaft-skills/evals/claude").mkdir(parents=True)
            (root / "agent-plugins/shaft-skills/evals/codex").mkdir(parents=True)
            (root / "shaft-skills").mkdir(exist_ok=True)
            (root / "agent-plugins/shaft-skills/evals/cases.json").write_text(
                json.dumps(self.corpus), encoding="utf-8"
            )
            (root / "shaft-skills/quality-review.json").write_text(
                json.dumps(self.review), encoding="utf-8"
            )
            for name in self.review["skills"]:
                skill = root / "shaft-skills" / name
                skill.mkdir()
                (skill / "SKILL.md").write_text(
                    f"---\nname: {name}\ndescription: Use when testing {name}.\n---\n",
                    encoding="utf-8",
                )
            (root / "agent-plugins/shaft-skills/evals/claude/evals.json").write_text("{}", encoding="utf-8")
            (root / "agent-plugins/shaft-skills/evals/codex/cases.jsonl").write_text("", encoding="utf-8")
            (root / "agent-plugins/shaft-skills/evals/codex/output-schema.json").write_text("{}", encoding="utf-8")

            defects = validate_repository(root)

        self.assertIn("generated-drift", {defect["code"] for defect in defects})


if __name__ == "__main__":
    unittest.main()
