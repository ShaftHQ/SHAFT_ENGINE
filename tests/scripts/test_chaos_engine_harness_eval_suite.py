"""Harness eval suite: schema, pass@k report, and PR-gate wiring (#6519)."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

from scripts.ci.chaos_engine_harness_eval_suite import (
    evaluate_suite,
    load_manifest,
    pass_at_k,
    validate_manifest,
)
from scripts.ci.harness_pr_gate import CHECKS, SURFACE_CHECKS, SURFACE_PATTERNS, classify_paths

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "chaos-engine/references/harness-eval-suite.md"
MANIFEST = ROOT / "chaos-engine/evals/harness-suite/manifest.json"
CATALOG = ROOT / "chaos-engine/references/zero-llm-catalog.md"


class HarnessEvalSuiteTests(unittest.TestCase):
    def test_manifest_schema_and_coverage(self):
        document = load_manifest(MANIFEST)
        defects = validate_manifest(document)
        self.assertEqual([], defects, msg=defects)
        self.assertEqual(1, document["schema_version"])
        self.assertGreaterEqual(len(document["tasks"]), 20)
        sets = {row["set"] for row in document["tasks"]}
        self.assertEqual({"capability", "regression"}, sets)
        areas = {row["area"] for row in document["tasks"]}
        self.assertLessEqual({"installer", "doctor", "entry", "hooks", "retrieve"}, areas)
        regression_issues = {
            row["source_issue"] for row in document["tasks"] if row["set"] == "regression"
        }
        self.assertGreaterEqual(len(regression_issues), 12)

    def test_pass_at_k_helper(self):
        self.assertEqual(1.0, pass_at_k([True]))
        self.assertEqual(0.0, pass_at_k([False]))
        self.assertEqual(1.0, pass_at_k([False, True]))
        self.assertEqual(0.0, pass_at_k([]))

    def test_documented_runner_and_gate(self):
        text = DOC.read_text(encoding="utf-8")
        self.assertIn("scripts/ci/chaos_engine_harness_eval_suite.py", text)
        self.assertIn("pass@k", text)
        self.assertIn("capability", text)
        self.assertIn("regression", text)
        self.assertIn("harness-eval-suite-contract", text)
        catalog = CATALOG.read_text(encoding="utf-8")
        self.assertIn("chaos_engine_harness_eval_suite.py", catalog)

    def test_pr_gate_selects_suite_on_chaos_engine_change(self):
        self.assertIn("harness-eval-suite-contract", CHECKS)
        self.assertIn("harness-eval", SURFACE_CHECKS)
        self.assertIn("harness-eval-suite-contract", SURFACE_CHECKS["harness-eval"])
        self.assertIn("chaos-engine/**", SURFACE_PATTERNS["harness-eval"])
        plan = classify_paths(["chaos-engine/install.py"])
        self.assertIn("harness-eval", plan.surfaces)
        self.assertIn("harness-eval-suite-contract", [check.id for check in plan.checks])

    def test_suite_passes_under_live_modules(self):
        report = evaluate_suite(load_manifest(MANIFEST), root=ROOT, timeout=180)
        self.assertEqual([], report["defects"], msg=report["defects"])
        failed = [row for row in report["results"] if not row["passed"]]
        self.assertEqual([], failed, msg=json.dumps(failed, indent=2))
        self.assertEqual(1.0, report["pass_at_k"])
        self.assertEqual(1.0, report["case_pass_rate"])
        self.assertTrue(report["passed"])

    def test_validate_rejects_empty_corpus(self):
        document = {
            "schema_version": 1,
            "package": "chaos-engine",
            "thresholds": {
                "pass_at_k": 1.0,
                "default_k": 1,
                "min_tasks": 20,
                "min_capability": 6,
                "min_regression": 12,
            },
            "tasks": [],
        }
        defects = validate_manifest(document)
        self.assertTrue(any("tasks" in item for item in defects))

    def test_evaluate_suite_reports_failure(self):
        document = load_manifest(MANIFEST)
        tiny = {
            **document,
            "thresholds": {
                "pass_at_k": 1.0,
                "default_k": 1,
                "min_tasks": 1,
                "min_capability": 0,
                "min_regression": 1,
            },
            "tasks": [
                {
                    "id": "reg-fake",
                    "set": "regression",
                    "area": "doctor",
                    "source_issue": 1,
                    "k": 1,
                    "runner": "unittest",
                    "module": "tests.scripts.does_not_exist_module",
                }
            ],
        }
        with unittest.mock.patch(
            "scripts.ci.chaos_engine_harness_eval_suite._run_unittest",
            return_value=(False, "boom"),
        ):
            report = evaluate_suite(tiny, root=ROOT)
        self.assertFalse(report["passed"])
        self.assertEqual(0.0, report["pass_at_k"])


if __name__ == "__main__":
    unittest.main()
