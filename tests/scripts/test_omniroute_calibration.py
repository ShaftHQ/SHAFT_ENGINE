"""Focused regressions for OmniRoute 12-trial paired free-model calibration."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "scripts" / "ci" / "chaos_gauge" / "omniroute_calibration.py"
SPEC = importlib.util.spec_from_file_location("omniroute_calibration", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("omniroute calibration module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class IdentityAndPairingTest(unittest.TestCase):
    def test_campaign_identity_selects_unchanged_public_subset(self):
        identity = MODULE.campaign_identity(MODULE.load_manifest())
        self.assertEqual(5450, identity["seed"])
        self.assertEqual(3, identity["taskCount"])
        self.assertEqual(2, identity["attemptsPerTask"])
        self.assertEqual(12, identity["trialCount"])
        self.assertEqual(list(MODULE.ARMS), identity["arms"])
        names = [task["name"] for task in identity["tasks"]]
        self.assertEqual(list(MODULE.SELECTED_TASKS), names)
        manifest = MODULE.load_manifest()
        by_name = {
            task["name"]: task["sha256"]
            for task in manifest["tasks"]
            if isinstance(task, dict)
        }
        for task in identity["tasks"]:
            self.assertEqual(by_name[task["name"]], task["sha256"])

    def test_plan_pairs_emits_twelve_trials_with_shared_model_slots(self):
        pairs = MODULE.plan_pairs(model="nvidia/nemotron-3-ultra-550b-a55b")
        self.assertEqual(6, len(pairs))
        trials = [trial for pair in pairs for trial in pair["trials"]]
        self.assertEqual(12, len(trials))
        for pair in pairs:
            self.assertEqual(pair["model"], pair["trials"][0]["model"])
            self.assertEqual(pair["model"], pair["trials"][1]["model"])
            arms = {trial["arm"] for trial in pair["trials"]}
            self.assertEqual(set(MODULE.ARMS), arms)

    def test_pairing_invariant_rejects_mixed_models_inside_pair(self):
        pairs = MODULE.plan_pairs(model="nvidia/nemotron-3-ultra-550b-a55b")
        pairs[0]["trials"][1]["model"] = "other/provider-model"
        with self.assertRaisesRegex(ValueError, "pairing invariant"):
            MODULE.assert_pairing_invariant(pairs)


class UnavailableAndAggregateTest(unittest.TestCase):
    def test_metric_or_unavailable_never_coerces_absence_to_zero(self):
        self.assertEqual(MODULE.UNAVAILABLE, MODULE.metric_or_unavailable(None))
        self.assertEqual(MODULE.UNAVAILABLE, MODULE.metric_or_unavailable(float("nan")))
        self.assertEqual(0, MODULE.metric_or_unavailable(0))
        self.assertEqual(2.5, MODULE.metric_or_unavailable(2.5))
        with self.assertRaisesRegex(ValueError, "boolean"):
            MODULE.metric_or_unavailable(False)

    def test_aggregate_uses_unavailable_for_missing_telemetry(self):
        trials = []
        for task in MODULE.SELECTED_TASKS:
            for attempt in (1, 2):
                for arm in MODULE.ARMS:
                    trials.append(
                        {
                            "task": task,
                            "attempt": attempt,
                            "arm": arm,
                            "model": "nvidia/nemotron-3-ultra-550b-a55b",
                            "correctness": 1 if arm == "chaos-engine" else 0,
                            "tokens": None,
                            "latency_seconds": 1.5,
                            "external_run_minutes": None,
                            "actions": None,
                            "retries": 0,
                            "cost_usd": None,
                        }
                    )
        evidence = MODULE.build_redacted_aggregate(
            MODULE.load_manifest(),
            trials,
            models_used=["nvidia/nemotron-3-ultra-550b-a55b"],
            preferred_model="agy/gemini-3.7-flash-high",
            failover_events=[{"from": "agy/gemini-3.7-flash-high", "to": "nvidia/nemotron-3-ultra-550b-a55b", "reason": "429"}],
        )
        self.assertEqual({"planned": 12, "observed": 12}, evidence["trialAccounting"])
        self.assertEqual("complete", evidence["status"])
        self.assertEqual(["nvidia/nemotron-3-ultra-550b-a55b"], evidence["modelsUsed"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["tokens"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["cost_usd"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["actions"])
        self.assertEqual(MODULE.UNAVAILABLE, evidence["metrics"]["control"]["external_run_minutes"])
        self.assertNotEqual(0, evidence["metrics"]["control"]["tokens"])
        self.assertEqual(0.0, evidence["metrics"]["control"]["correctness"])
        self.assertEqual(1.0, evidence["metrics"]["chaos-engine"]["correctness"])
        MODULE.validate_redacted_aggregate(evidence, MODULE.load_manifest())

    def test_validate_rejects_null_metrics_and_privacy_leaks(self):
        trials = [
            {
                "task": task,
                "attempt": attempt,
                "arm": arm,
                "model": "nvidia/nemotron-3-ultra-550b-a55b",
                "correctness": 0,
                "tokens": 10,
                "latency_seconds": 1.0,
                "external_run_minutes": None,
                "actions": 1,
                "retries": 0,
                "cost_usd": None,
            }
            for task in MODULE.SELECTED_TASKS
            for attempt in (1, 2)
            for arm in MODULE.ARMS
        ]
        evidence = MODULE.build_redacted_aggregate(
            MODULE.load_manifest(),
            trials,
            models_used=["nvidia/nemotron-3-ultra-550b-a55b"],
            preferred_model="agy/gemini-3.7-flash-high",
            failover_events=[],
        )
        evidence["metrics"]["control"]["tokens"] = None
        with self.assertRaisesRegex(ValueError, "UNAVAILABLE rather than null"):
            MODULE.validate_redacted_aggregate(evidence, MODULE.load_manifest())

        evidence = MODULE.build_redacted_aggregate(
            MODULE.load_manifest(),
            trials,
            models_used=["nvidia/nemotron-3-ultra-550b-a55b"],
            preferred_model="agy/gemini-3.7-flash-high",
            failover_events=[],
        )
        evidence["missingInputs"] = ["see openai.com/docs"]
        with self.assertRaisesRegex(ValueError, "privacy scan"):
            MODULE.validate_redacted_aggregate(evidence, MODULE.load_manifest())


class ModelSelectionTest(unittest.TestCase):
    def test_prefer_primary_then_failover_skips_exhausted(self):
        candidates = [
            {"model": "agentrouter/claude-opus-4-8", "remaining": 100, "capability": "most-intelligent"},
            {"model": "agy/gemini-3.7-flash-high", "remaining": 100, "capability": "most-intelligent"},
            {"model": "nvidia/nemotron-3-ultra-550b-a55b", "remaining": 100, "capability": "most-intelligent"},
        ]
        self.assertEqual(
            "agy/gemini-3.7-flash-high",
            MODULE.select_model(candidates, preferred=MODULE.PREFERRED_MODEL, skipped=set()),
        )
        self.assertEqual(
            "nvidia/nemotron-3-ultra-550b-a55b",
            MODULE.select_model(
                candidates,
                preferred=MODULE.PREFERRED_MODEL,
                skipped={"agy/gemini-3.7-flash-high", "agentrouter/claude-opus-4-8"},
            ),
        )

    def test_rejects_paid_recharge_marker(self):
        with self.assertRaisesRegex(MODULE.PaidTransportError, "paid"):
            MODULE.ensure_free_transport_message("Please recharge. Insufficient balance")


class GateVerdictTest(unittest.TestCase):
    def test_gate_yes_requires_correctness_and_efficiency_without_regression(self):
        metrics = {
            "control": {
                "correctness": 0.25,
                "tokens": 200,
                "latency_seconds": 20.0,
                "external_run_minutes": MODULE.UNAVAILABLE,
                "actions": MODULE.UNAVAILABLE,
                "retries": 1,
                "cost_usd": MODULE.UNAVAILABLE,
                "variance": MODULE.UNAVAILABLE,
            },
            "chaos-engine": {
                "correctness": 0.75,
                "tokens": 150,
                "latency_seconds": 18.0,
                "external_run_minutes": MODULE.UNAVAILABLE,
                "actions": MODULE.UNAVAILABLE,
                "retries": 0,
                "cost_usd": MODULE.UNAVAILABLE,
                "variance": MODULE.UNAVAILABLE,
            },
        }
        self.assertEqual("YES", MODULE.gate_verdict(metrics)["verdict"])

    def test_gate_inconclusive_when_efficiency_unavailable(self):
        metrics = {
            "control": {
                "correctness": 0.5,
                "tokens": MODULE.UNAVAILABLE,
                "latency_seconds": MODULE.UNAVAILABLE,
                "external_run_minutes": MODULE.UNAVAILABLE,
                "actions": MODULE.UNAVAILABLE,
                "retries": MODULE.UNAVAILABLE,
                "cost_usd": MODULE.UNAVAILABLE,
                "variance": MODULE.UNAVAILABLE,
            },
            "chaos-engine": {
                "correctness": 0.75,
                "tokens": MODULE.UNAVAILABLE,
                "latency_seconds": MODULE.UNAVAILABLE,
                "external_run_minutes": MODULE.UNAVAILABLE,
                "actions": MODULE.UNAVAILABLE,
                "retries": MODULE.UNAVAILABLE,
                "cost_usd": MODULE.UNAVAILABLE,
                "variance": MODULE.UNAVAILABLE,
            },
        }
        self.assertEqual("INCONCLUSIVE", MODULE.gate_verdict(metrics)["verdict"])


class ApplyAndVerifyHelpersTest(unittest.TestCase):
    def test_parse_files_payload_and_verify_diagnosis_task(self):
        with tempfile.TemporaryDirectory() as temporary:
            sandbox = Path(temporary) / "app"
            MODULE.materialize_task("diagnosis-failure-trace", sandbox)
            payload = {
                "files": {
                    "source.txt": (
                        "from pathlib import Path\n\n"
                        "frames = [line.split(\"FRAME \", 1)[1] for line in "
                        "Path(\"trace.log\").read_text().splitlines() if line.startswith(\"FRAME \")]\n"
                        "print(f\"culprit={frames[-1]}\")\n"
                    )
                }
            }
            MODULE.apply_files_payload(sandbox, payload)
            reward = MODULE.run_verifier("diagnosis-failure-trace", sandbox)
            self.assertEqual(1, reward["correctness"])


if __name__ == "__main__":
    unittest.main()
