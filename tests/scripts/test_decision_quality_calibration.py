"""Behavioral regression for decision-quality public calibration (#5522)."""

from __future__ import annotations

import importlib.util
import json
import os
import re
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
ARTIFACT = ROOT / "chaos-engine/decision-quality-calibration.md"
MODULE_PATH = GAUGE / "decision_quality_calibration.py"


def load_module():
    spec = importlib.util.spec_from_file_location("decision_quality_calibration", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("decision_quality_calibration module is unavailable")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


MODULE = load_module()


class ArtifactContractTest(unittest.TestCase):
    def setUp(self):
        self.content = ARTIFACT.read_text(encoding="utf-8")

    def test_artifact_exists_and_is_dated(self):
        self.assertTrue(ARTIFACT.exists())
        self.assertRegex(self.content, r"Accessed:\s+\d{4}-\d{2}-\d{2}")

    def test_references_parent_and_siblings(self):
        for marker in ("#5514", "#5522", "#5520", "#5521", "decision-quality-baseline.md", "decision-quality-rubric.md"):
            self.assertIn(marker, self.content)

    def test_records_that_trials_did_not_run(self):
        self.assertIn("120 paid trials did not run", self.content)

    def test_unavailable_policy_and_privacy_gate(self):
        self.assertIn("UNAVAILABLE", self.content)
        lower = self.content.lower()
        self.assertNotIn("never zero", lower)
        for pattern in (r"model_id\s*:", r"provider_route\s*:", r"endpoint\s*:", r"anthropic\.com/", r"openai\.com/"):
            self.assertIsNone(re.search(pattern, self.content, re.I))


class CalibrationIdentityTest(unittest.TestCase):
    def setUp(self):
        self.manifest = MODULE.load_manifest()
        self.identity = MODULE.calibration_identity(self.manifest)

    def test_frozen_public_identity(self):
        self.assertEqual(5450, self.identity["seed"])
        self.assertEqual(12, self.identity["taskCount"])
        self.assertEqual(5, self.identity["attemptsPerTask"])
        self.assertEqual(120, self.identity["trialCount"])
        self.assertEqual(["control", "chaos-engine"], self.identity["arms"])
        self.assertEqual(12, len(self.identity["tasks"]))

    def test_public_task_names_unchanged(self):
        expected = [
            task["name"]
            for task in self.manifest["tasks"]
            if task["visibility"] == "public"
        ]
        self.assertEqual(expected, [task["name"] for task in self.identity["tasks"]])
        self.assertEqual(
            [task["sha256"] for task in self.manifest["tasks"] if task["visibility"] == "public"],
            [task["sha256"] for task in self.identity["tasks"]],
        )


class UnavailablePolicyTest(unittest.TestCase):
    def test_none_becomes_unavailable_not_zero(self):
        self.assertEqual(MODULE.UNAVAILABLE, MODULE.metric_or_unavailable(None))
        self.assertEqual(0, MODULE.metric_or_unavailable(0))
        self.assertEqual(1.5, MODULE.metric_or_unavailable(1.5))

    def test_blocked_evidence_uses_unavailable(self):
        evidence = MODULE.blocked_evidence(
            MODULE.load_manifest(),
            ["harbor==0.22.0", "docker-engine-access"],
        )
        self.assertEqual("blocked", evidence["status"])
        self.assertEqual({"planned": 120, "observed": 0}, evidence["trialAccounting"])
        for arm in MODULE.ARMS:
            for name, value in evidence["metrics"][arm].items():
                self.assertEqual(
                    MODULE.UNAVAILABLE,
                    value,
                    f"{arm}.{name} must stay UNAVAILABLE when blocked",
                )
                self.assertIsNot(value, 0)
                self.assertIsNotNone(value)

    def test_missing_token_provenance_stays_unavailable(self):
        comparison = {
            "schemaVersion": 1,
            "scoreVersion": "chaos-gauge-60-20-20-v1",
            "bootstrapIterations": 10000,
            "seed": 5450,
            "campaign": "calibration",
            "arms": {
                "control": {
                    "sampleSize": 60,
                    "successCount": 30,
                    "effectiveness": 0.5,
                    "reliability": 0.5,
                    "safetyEligible": True,
                    "verifierComplete": True,
                    "tokenProvenance": "unavailable",
                    "tokensPerSuccess": None,
                    "secondsPerSuccess": 12.0,
                    "costPerSuccess": None,
                    "efficiency": None,
                    "overallScore": None,
                    "equalWeightScore": None,
                },
                "chaos-engine": {
                    "sampleSize": 60,
                    "successCount": 36,
                    "effectiveness": 0.6,
                    "reliability": 0.6,
                    "safetyEligible": True,
                    "verifierComplete": True,
                    "tokenProvenance": "unavailable",
                    "tokensPerSuccess": None,
                    "secondsPerSuccess": 10.0,
                    "costPerSuccess": None,
                    "efficiency": None,
                    "overallScore": None,
                    "equalWeightScore": None,
                },
            },
            "scoreDelta": None,
            "confidenceInterval95": {"lower": None, "upper": None},
            "verdict": {"state": "insufficient evidence", "winner": None},
            "retries": {"control": 1, "chaos-engine": 2},
            "exclusions": [],
            "pairAccounting": {"planned": 60, "excluded": 0, "analyzed": 60},
        }
        evidence = MODULE.build_redacted_aggregate(MODULE.load_manifest(), comparison)
        self.assertEqual({"planned": 120, "observed": 0}, evidence["trialAccounting"])
        self.assertEqual("incomplete", evidence["status"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["tokens"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["cost_usd"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["actions"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["external_run_minutes"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["variance"])
        self.assertEqual(0.5, evidence["metrics"]["control"]["correctness"])
        self.assertEqual(1, evidence["metrics"]["control"]["retries"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["comparison"]["scoreDelta"])
        self.assertNotEqual(0, evidence["metrics"]["control"]["tokens"])

    def test_build_requires_collect_receipt_for_observed_trials(self):
        comparison = {
            "schemaVersion": 1,
            "scoreVersion": "chaos-gauge-60-20-20-v1",
            "bootstrapIterations": 10000,
            "seed": 5450,
            "campaign": "calibration",
            "arms": {
                "control": {
                    "sampleSize": 60,
                    "successCount": 30,
                    "effectiveness": 0.5,
                    "reliability": 0.5,
                    "safetyEligible": True,
                    "verifierComplete": True,
                    "tokenProvenance": "reported",
                    "tokensPerSuccess": 100,
                    "secondsPerSuccess": 12.0,
                    "costPerSuccess": 0.01,
                    "efficiency": 1.0,
                    "overallScore": 0.5,
                    "equalWeightScore": 0.5,
                },
                "chaos-engine": {
                    "sampleSize": 60,
                    "successCount": 36,
                    "effectiveness": 0.6,
                    "reliability": 0.6,
                    "safetyEligible": True,
                    "verifierComplete": True,
                    "tokenProvenance": "reported",
                    "tokensPerSuccess": 90,
                    "secondsPerSuccess": 10.0,
                    "costPerSuccess": 0.009,
                    "efficiency": 1.1,
                    "overallScore": 0.6,
                    "equalWeightScore": 0.6,
                },
            },
            "scoreDelta": 0.1,
            "confidenceInterval95": {"lower": 0.01, "upper": 0.2},
            "verdict": {"state": "significant", "winner": "chaos-engine"},
            "retries": {"control": 1, "chaos-engine": 2},
            "exclusions": [],
            "pairAccounting": {"planned": 60, "excluded": 0, "analyzed": 60},
        }
        without_receipt = MODULE.build_redacted_aggregate(MODULE.load_manifest(), comparison)
        self.assertEqual(0, without_receipt["trialAccounting"]["observed"])
        self.assertEqual("incomplete", without_receipt["status"])

        with_receipt = MODULE.build_redacted_aggregate(
            MODULE.load_manifest(),
            comparison,
            collect_receipt={"trialAccounting": {"planned": 120, "observed": 120}},
        )
        self.assertEqual({"planned": 120, "observed": 120}, with_receipt["trialAccounting"])
        self.assertEqual("complete", with_receipt["status"])
        self.assertEqual(100, with_receipt["metrics"]["control"]["tokens"])


class ProbeAndValidateTest(unittest.TestCase):
    def test_probe_reports_exact_missing_inputs(self):
        def fake_run(command):
            raise OSError("unavailable")

        with mock.patch.dict(os.environ, {}, clear=True):
            probe = MODULE.probe_runtime(run=fake_run)
        self.assertFalse(probe["ready"])
        joined = " | ".join(probe["missingInputs"])
        self.assertIn("harbor==0.22.0", joined)
        self.assertIn("docker-engine-access", joined)
        self.assertIn("OPENAI_API_KEY", joined)
        self.assertIn("CHAOS_GAUGE_PUBLIC_CALIBRATION_AUTHORIZED=1", joined)
        self.assertEqual(120, probe["plannedTrials"])

    def test_validate_rejects_null_metrics_and_privacy_leaks(self):
        evidence = MODULE.blocked_evidence(MODULE.load_manifest(), ["harbor==0.22.0"])
        evidence["metrics"]["control"]["tokens"] = None
        with self.assertRaisesRegex(ValueError, "UNAVAILABLE rather than null"):
            MODULE.validate_redacted_aggregate(evidence, MODULE.load_manifest())

        evidence = MODULE.blocked_evidence(MODULE.load_manifest(), ["harbor==0.22.0"])
        evidence["missingInputs"] = ["see openai.com/docs"]
        with self.assertRaisesRegex(ValueError, "privacy scan"):
            MODULE.validate_redacted_aggregate(evidence, MODULE.load_manifest())

    def test_cli_blocked_writes_json(self):
        import subprocess
        import tempfile

        with tempfile.TemporaryDirectory() as temporary:
            out = Path(temporary) / "blocked.json"
            completed = subprocess.run(
                [
                    "python3",
                    str(MODULE_PATH),
                    "blocked",
                    "--out",
                    str(out),
                ],
                check=True,
                capture_output=True,
                text=True,
                env={**os.environ, "OPENAI_API_KEY": "", "CHAOS_GAUGE_PUBLIC_CALIBRATION_AUTHORIZED": ""},
            )
            self.assertEqual(0, completed.returncode)
            payload = json.loads(out.read_text(encoding="utf-8"))
            self.assertEqual("blocked", payload["status"])
            self.assertEqual(0, payload["trialAccounting"]["observed"])
            self.assertGreaterEqual(len(payload["missingInputs"]), 1)


if __name__ == "__main__":
    unittest.main()
