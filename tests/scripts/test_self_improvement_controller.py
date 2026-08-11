"""Fail-closed evaluation, promotion, and recovery for harness self-modification."""

from __future__ import annotations

import hashlib
import json
import tempfile
import unittest
from pathlib import Path

from scripts.agents import learning_loop


class SelfImprovementControllerTest(unittest.TestCase):
    def candidate(self, state: Path, risk_tier: str = "ordinary") -> dict:
        artifact = state / "red.txt"
        artifact.write_text("red", encoding="utf-8")
        digest = hashlib.sha256(b"red").hexdigest()
        learning_loop.record_signal(
            state, session_id="s", kind="guard_block", incident_id="r16",
            origin="agent", evidence=[{"kind": "test", "id": "red.txt", "sha256": digest}],
            evidence_root=state,
        )
        return learning_loop.assess(
            state, session_id="s", hypothesis="improve r16",
            owner="scripts/agents/guard.py", baseline_ref="a" * 40,
            allowed_paths=["scripts/agents/guard.py"], red_command="focused red",
            success_predicates=["target improves"], invariants=["no regressions"],
            risk_tier=risk_tier,
        )[0]

    @staticmethod
    def report(passed: bool, *, unmeasured: list[str] | None = None) -> dict:
        return {
            "episodes": {
                "learning-loop": {
                    "rule_ids": ["r16"],
                    "strict_episode_pass": passed,
                    "expectations": [{"kind": "requires", "passed": passed}],
                }
            },
            "rules": {},
            "guard_metrics": {"false_block_count": 0, "actionable_remedy_count": 1},
            "unmeasured_rule_ids": unmeasured or [],
        }

    def test_ordinary_candidate_requires_strict_targeted_improvement(self):
        evaluator = getattr(learning_loop, "evaluate_candidate", None)
        self.assertTrue(callable(evaluator), "self-improvement evaluator is missing")
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            result = evaluator(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            self.assertEqual(result["status"], "evaluated")

    def test_regressions_unmeasured_rules_and_path_escape_block_authorization(self):
        evaluator = learning_loop.evaluate_candidate
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            unknown = self.report(True)
            unknown["episodes"]["learning-loop"]["expectations"][0]["passed"] = None
            partial = self.report(True)
            partial["episodes"]["learning-loop"]["expectations"].append(
                {"kind": "forbids", "passed": None}
            )
            inconsistent = self.report(True)
            inconsistent["episodes"]["learning-loop"]["strict_episode_pass"] = False
            for report, changed_paths in (
                (self.report(True, unmeasured=["r16"]), ["scripts/agents/guard.py"]),
                (unknown, ["scripts/agents/guard.py"]),
                (partial, ["scripts/agents/guard.py"]),
                (inconsistent, ["scripts/agents/guard.py"]),
                (self.report(True), ["../outside.py"]),
            ):
                with self.subTest(report=report, changed_paths=changed_paths):
                    with self.assertRaises(ValueError):
                        evaluator(
                            state, candidate=candidate, baseline_report=self.report(False),
                            candidate_report=report, target_rule_ids=["r16"],
                            candidate_sha="b" * 40, corpus_sha256="c" * 64,
                            changed_paths=changed_paths, tests_passed=True,
                            reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                            run_ids=["run-1"],
                        )
            baseline_unknown = self.report(False)
            baseline_unknown["episodes"]["learning-loop"]["expectations"][0]["passed"] = None
            with self.assertRaises(ValueError):
                evaluator(
                    state, candidate=candidate, baseline_report=baseline_unknown,
                    candidate_report=self.report(True), target_rule_ids=["r16"],
                    candidate_sha="b" * 40, corpus_sha256="c" * 64,
                    changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                    reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                    run_ids=["run-1"],
                )

    def test_tests_passed_requires_the_boolean_true(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            for invalid in ("false", 1, {"value": True}):
                with self.subTest(invalid=invalid):
                    with self.assertRaises(ValueError):
                        learning_loop.evaluate_candidate(
                            state, candidate=candidate, baseline_report=self.report(False),
                            candidate_report=self.report(True), target_rule_ids=["r16"],
                            candidate_sha="b" * 40, corpus_sha256="c" * 64,
                            changed_paths=["scripts/agents/guard.py"], tests_passed=invalid,
                            reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                            run_ids=["run-1"],
                        )

    def test_kernel_candidate_requires_two_keys_three_lenses_and_two_runs(self):
        evaluator = learning_loop.evaluate_candidate
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state, "kernel")
            common = dict(
                state=state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
            )
            with self.assertRaises(ValueError):
                evaluator(
                    **common,
                    reviews=[{"key": "one", "lens": "correctness", "decision": "approve"}],
                    run_ids=["same", "same"],
                )
            result = evaluator(
                **common,
                reviews=[
                    {"key": "one", "lens": "correctness", "decision": "approve"},
                    {"key": "two", "lens": "reproduction", "decision": "approve"},
                    {"key": "two", "lens": "safety", "decision": "approve"},
                ],
                run_ids=["run-1", "run-2"],
            )
            self.assertEqual(result["status"], "evaluated")

    def test_promotion_requires_authorization_and_exact_evaluated_head(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            with self.assertRaises(ValueError):
                learning_loop.promote_candidate(
                    state, candidate["candidate_id"], head_sha="b" * 40,
                    branch="ChaosEngine/self-test", pr_number=1,
                )
            learning_loop.evaluate_candidate(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            with self.assertRaises(ValueError):
                learning_loop.promote_candidate(
                    state, candidate["candidate_id"], head_sha="d" * 40,
                    branch="ChaosEngine/self-test", pr_number=1,
                )
            promoted = learning_loop.promote_candidate(
                state, candidate["candidate_id"], head_sha="b" * 40,
                branch="ChaosEngine/self-test", pr_number=1,
            )
            self.assertEqual(promoted["status"], "promotion-intent")

    def test_tampered_evaluation_cannot_authorize_promotion(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            learning_loop.evaluate_candidate(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            path = next((state / "evaluations").glob("*.json"))
            evaluation = json.loads(path.read_text(encoding="utf-8"))
            evaluation["candidate_sha"] = "d" * 40
            path.write_text(json.dumps(evaluation), encoding="utf-8")
            with self.assertRaises(ValueError):
                learning_loop.promote_candidate(
                    state, candidate["candidate_id"], head_sha="d" * 40,
                    branch="ChaosEngine/self-test", pr_number=1,
                )

    def test_idempotent_evaluation_rejects_corrupt_existing_record(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            arguments = dict(
                state=state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            learning_loop.evaluate_candidate(**arguments)
            path = next((state / "evaluations").glob("*.json"))
            evaluation = json.loads(path.read_text(encoding="utf-8"))
            evaluation["status"] = "corrupt"
            path.write_text(json.dumps(evaluation), encoding="utf-8")
            with self.assertRaises(ValueError):
                learning_loop.evaluate_candidate(**arguments)

    def test_kernel_evaluation_cannot_be_rebound_as_ordinary(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state, "kernel")
            learning_loop.evaluate_candidate(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[
                    {"key": "one", "lens": "correctness", "decision": "approve"},
                    {"key": "two", "lens": "reproduction", "decision": "approve"},
                    {"key": "two", "lens": "safety", "decision": "approve"},
                ],
                run_ids=["run-1", "run-2"],
            )
            path = next((state / "evaluations").glob("*.json"))
            evaluation = json.loads(path.read_text(encoding="utf-8"))
            evaluation["risk_tier"] = "ordinary"
            identity = {key: evaluation[key] for key in learning_loop._EVALUATION_IDENTITY_KEYS}
            evaluation["evaluation_id"] = learning_loop._hash_text(
                learning_loop._canonical(identity)
            )
            path.write_text(json.dumps(evaluation), encoding="utf-8")
            with self.assertRaises(ValueError):
                learning_loop.promote_candidate(
                    state, candidate["candidate_id"], head_sha="b" * 40,
                    branch="ChaosEngine/self-test", pr_number=1,
                )

    def test_recovery_rejects_traversal_and_detached_promotion_state(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            with self.assertRaises(ValueError):
                learning_loop.repair_or_revert(state, "../outside")
            promotions = state / "promotions"
            promotions.mkdir(exist_ok=True)
            candidate_id = "a" * 64
            identity = {
                "candidate_id": candidate_id, "evaluation_id": "b" * 64,
                "candidate_sha": "c" * 40, "branch_hash": "d" * 64, "pr_number": 1,
            }
            detached = {
                "schema_version": 1,
                "promotion_id": learning_loop._hash_text(learning_loop._canonical(identity)),
                **identity, "status": "promotion-intent", "repairs_attempted": 0,
                "frozen": False, "updated_at": "2026-08-11T00:00:00+00:00",
            }
            (promotions / f"{candidate_id}.json").write_text(
                json.dumps(detached), encoding="utf-8"
            )
            with self.assertRaises(ValueError):
                learning_loop.repair_or_revert(state, candidate_id)

    def test_one_repair_then_revert_and_freeze_on_recurrence(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            learning_loop.evaluate_candidate(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            learning_loop.promote_candidate(
                state, candidate["candidate_id"], head_sha="b" * 40,
                branch="ChaosEngine/self-test", pr_number=1,
            )
            repairing = learning_loop.repair_or_revert(state, candidate["candidate_id"])
            self.assertEqual(repairing["status"], "repair-required")
            reverted = learning_loop.repair_or_revert(state, candidate["candidate_id"])
            self.assertEqual(reverted["status"], "revert-required")
            self.assertTrue(reverted["frozen"])

    def test_tampered_promotion_state_cannot_enter_recovery(self):
        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory)
            candidate = self.candidate(state)
            learning_loop.evaluate_candidate(
                state, candidate=candidate, baseline_report=self.report(False),
                candidate_report=self.report(True), target_rule_ids=["r16"],
                candidate_sha="b" * 40, corpus_sha256="c" * 64,
                changed_paths=["scripts/agents/guard.py"], tests_passed=True,
                reviews=[{"key": "reviewer-1", "lens": "correctness", "decision": "approve"}],
                run_ids=["run-1"],
            )
            learning_loop.promote_candidate(
                state, candidate["candidate_id"], head_sha="b" * 40,
                branch="ChaosEngine/self-test", pr_number=1,
            )
            path = next((state / "promotions").glob("*.json"))
            promotion = json.loads(path.read_text(encoding="utf-8"))
            promotion["repairs_attempted"] = "many"
            path.write_text(json.dumps(promotion), encoding="utf-8")
            with self.assertRaises(ValueError):
                learning_loop.repair_or_revert(state, candidate["candidate_id"])

    def test_controller_cli_exposes_evaluate_promote_and_recovery(self):
        parser = learning_loop.build_parser()
        evaluate = parser.parse_args(
            ["evaluate", "--candidate-id", "a" * 64, "--manifest", "evaluation.json"]
        )
        promote = parser.parse_args(
            ["promote", "--candidate-id", "a" * 64, "--head-sha", "b" * 40,
             "--branch", "ChaosEngine/self-test", "--pr-number", "1"]
        )
        recovery = parser.parse_args(["repair-or-revert", "--candidate-id", "a" * 64])
        self.assertEqual(
            [evaluate.command, promote.command, recovery.command],
            ["evaluate", "promote", "repair-or-revert"],
        )


if __name__ == "__main__":
    unittest.main()
