"""Excluded ChaosGauge canary contract (#5462)."""

from __future__ import annotations

import copy
import importlib.util
import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts" / "ci" / "chaos_gauge"
SPEC = importlib.util.spec_from_file_location("chaos_gauge_canary", GAUGE / "canary.py")
assert SPEC and SPEC.loader
CANARY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CANARY)
MANIFEST = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))


class CanaryContractTest(unittest.TestCase):
    def test_canary_is_one_excluded_public_two_arm_pair(self) -> None:
        planned = CANARY.plan(MANIFEST)

        self.assertEqual("canary", planned["campaign"])
        self.assertTrue(planned["excludedFromPilot"])
        self.assertEqual(2, planned["trials"])
        self.assertEqual(0, planned["pair"]["attempt"])
        self.assertEqual("diagnosis-config-precedence", planned["pair"]["task"])
        self.assertEqual({"control", "chaos-engine"}, set(planned["pair"]["arms"]))

        config = CANARY.job_config(MANIFEST)
        self.assertTrue(config["job_name"].startswith("chaos-gauge-canary-"))
        self.assertEqual(1, config["n_attempts"])
        self.assertEqual(2, config["n_concurrent_trials"])
        self.assertEqual(1, len(config["datasets"]))
        self.assertEqual([planned["pair"]["task"]], config["datasets"][0]["task_names"])
        self.assertEqual(
            ["codex" if arm == "control" else None for arm in planned["pair"]["arms"]],
            [agent.get("name") for agent in config["agents"]],
        )

    def test_receipt_requires_pinned_telemetry_isolation_and_cleanup(self) -> None:
        planned = CANARY.plan(MANIFEST)
        result = {
            "trial_results": [
                {
                    "task_name": planned["pair"]["task"],
                    "task_checksum": planned["pair"]["sha256"],
                    "trial_name": f"{planned['pair']['task']}__{'ctrl001' if arm == 'control' else 'chaos01'}",
                    "agent_info": {
                        "name": "codex", "version": "0.118.0",
                        "model_info": {"name": "gpt-5.6-terra", "provider": "openai"},
                    },
                    "agent_result": {"n_input_tokens": 10, "n_output_tokens": 20, "cost_usd": 0.01},
                    "agent_execution": {
                        "started_at": "2026-08-31T00:00:00+00:00",
                        "finished_at": "2026-08-31T00:00:01+00:00",
                    },
                    "verifier_environment_mode": "separate",
                    "verifier_result": {"rewards": {"correctness": 1.0, "safety": 1.0, "cleanup": 1.0}},
                }
                for arm in planned["pair"]["arms"]
            ]
        }
        receipt = CANARY.receipt(MANIFEST, planned, result, public_source_revision="f" * 40)

        self.assertTrue(receipt["excludedFromPilot"])
        self.assertEqual(2, receipt["trialAccounting"]["observed"])
        self.assertNotIn("trial_results", json.dumps(receipt, sort_keys=True))
        CANARY.validate_public_evidence(receipt)

        missing_tokens = copy.deepcopy(result)
        del missing_tokens["trial_results"][0]["agent_result"]["n_input_tokens"]
        with self.assertRaisesRegex(ValueError, "token telemetry"):
            CANARY.receipt(MANIFEST, planned, missing_tokens, public_source_revision="f" * 40)

        unsafe = copy.deepcopy(result)
        unsafe["trial_results"][0]["verifier_result"]["rewards"]["cleanup"] = 0.0
        with self.assertRaisesRegex(ValueError, "cleanup"):
            CANARY.receipt(MANIFEST, planned, unsafe, public_source_revision="f" * 40)

        leaked = copy.deepcopy(receipt)
        leaked["rawTrajectory"] = "forbidden"
        with self.assertRaisesRegex(ValueError, "public canary evidence"):
            CANARY.validate_public_evidence(leaked)

        leaked = copy.deepcopy(receipt)
        leaked["privatePackage"]["repository"] = "sk-private-value"
        with self.assertRaisesRegex(ValueError, "public canary evidence"):
            CANARY.validate_public_evidence(leaked)


if __name__ == "__main__":
    unittest.main()
