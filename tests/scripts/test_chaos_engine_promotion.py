"""Scheduled ChaosEngine promotion evaluator contracts (#5301)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PROMOTION = ROOT / "scripts/ci/chaos_engine_promotion.py"
SPEC = importlib.util.spec_from_file_location("chaos_engine_promotion", PROMOTION)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("promotion evaluator could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def receipt(host: str, scenario: str, trial: int, variant: str) -> dict[str, object]:
    candidate = variant == "candidate"
    return {
        "schemaVersion": 1,
        "host": host,
        "scenario": scenario,
        "trial": trial,
        "variant": variant,
        "completed": True,
        "safe": True,
        "tokens": 40 if candidate else 100,
        "latencyMs": 70 if candidate else 100,
        "retries": 0,
        "denials": 0,
        "repeatedStates": 0,
        "terminalReason": "Complete",
    }


def complete_matrix() -> list[dict[str, object]]:
    return [
        receipt(host, scenario, trial, variant)
        for host in MODULE.HOSTS
        for scenario in MODULE.SCENARIOS
        for trial in range(1, MODULE.TRIALS + 1)
        for variant in MODULE.VARIANTS
    ]


def credentials() -> dict[str, str]:
    return {name: "present-but-never-rendered" for name in MODULE.CREDENTIALS.values()}


class ChaosEnginePromotionTest(unittest.TestCase):
    def test_manifest_resolves_the_issue_arithmetic_without_dropping_trials(self):
        manifest = MODULE.case_manifest()

        self.assertEqual(16, len(manifest["scenarios"]))
        self.assertEqual(5, len(manifest["hosts"]))
        self.assertEqual(5, manifest["trialsPerScenario"])
        self.assertEqual(400, manifest["pairedTrials"])
        self.assertEqual(800, manifest["individualRuns"])
        self.assertEqual(160, manifest["issueDeclaredRuns"])
        self.assertIn("omits its five-trial requirement", manifest["arithmeticResolution"])

    def test_complete_matrix_meets_all_host_and_global_thresholds(self):
        report = MODULE.evaluate(complete_matrix(), credentials())

        self.assertEqual(("Promoted", "complete"), (report["status"], report["terminalReason"]))
        self.assertEqual([], report["failures"])
        self.assertRegex(report["receiptSetSha256"], r"^[0-9a-f]{64}$")
        for metrics in [*report["metrics"]["hosts"].values(), report["metrics"]["global"]]:
            self.assertGreaterEqual(metrics["tokenReduction"], 0.5)
            self.assertGreaterEqual(metrics["medianLatencyImprovement"], 0.2)
            self.assertGreaterEqual(metrics["p95LatencyImprovement"], 0.2)

    def test_missing_credentials_block_without_rendering_secret_values(self):
        environment = credentials()
        environment.pop(MODULE.CREDENTIALS["gemini"])
        report = MODULE.evaluate([], environment)
        rendered = json.dumps(report, sort_keys=True)

        self.assertEqual("Blocked", report["status"])
        self.assertEqual(["gemini"], report["missingCredentialHosts"])
        self.assertNotIn("present-but-never-rendered", rendered)

    def test_completion_safety_and_loop_regressions_block_promotion(self):
        records = complete_matrix()
        candidate = next(item for item in records if item["variant"] == "candidate")
        candidate.update(
            completed=False,
            safe=False,
            repeatedStates=1,
            terminalReason="Blocked",
        )

        report = MODULE.evaluate(records, credentials())

        self.assertEqual("Blocked", report["status"])
        self.assertLessEqual(
            {"completion-regression", "safety-regression", "candidate-safety", "loop-or-deadlock"},
            set(report["failures"]),
        )

    def test_receipt_schema_rejects_transcripts_secrets_and_duplicate_runs(self):
        value = receipt("codex", MODULE.SCENARIOS[0], 1, "baseline")
        value["transcript"] = "do not persist"
        with self.assertRaisesRegex(ValueError, "fields"):
            MODULE.validate_receipt(value)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            valid = receipt("codex", MODULE.SCENARIOS[0], 1, "baseline")
            for name in ("one.json", "two.json"):
                (root / name).write_text(json.dumps(valid), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "duplicate"):
                MODULE.load_receipts(root)


if __name__ == "__main__":
    unittest.main()
