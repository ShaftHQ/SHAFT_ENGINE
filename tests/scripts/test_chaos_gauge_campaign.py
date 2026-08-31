"""ChaosGauge operational scheduling and evidence collection (#5460)."""

from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import TestCase, main


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
MANIFEST = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))

SPEC = importlib.util.spec_from_file_location("chaos_gauge_campaign", GAUGE / "campaign.py")
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("ChaosGauge campaign module could not be loaded")
CAMPAIGN = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CAMPAIGN)


class ChaosGaugeCampaignTest(TestCase):
    def test_schedule_is_executable_counterbalanced_pair_matrix(self):
        calibration = CAMPAIGN.plan(MANIFEST, "calibration")
        full_pilot = CAMPAIGN.plan(MANIFEST, "full-pilot")

        self.assertEqual(60, len(calibration["pairs"]))
        self.assertEqual(80, len(full_pilot["pairs"]))
        self.assertEqual(120, calibration["trials"])
        self.assertEqual(160, full_pilot["trials"])
        self.assertEqual(
            {"campaign", "implementationRevision", "pairs", "schemaVersion", "trials"},
            set(full_pilot),
        )
        self.assertEqual(80, len({pair["pairId"] for pair in full_pilot["pairs"]}))
        for pair in full_pilot["pairs"]:
            digest = hashlib.sha256(
                f'{MANIFEST["seed"]}:{pair["task"]}:{pair["attempt"]}'.encode()
            ).digest()
            self.assertEqual(["control", "chaos-engine"][digest[0] & 1], pair["arms"][0])
            self.assertEqual({"control", "chaos-engine"}, set(pair["arms"]))
            self.assertEqual(f'{pair["task"]}__{pair["attempt"]}', pair["pairId"])

    def test_full_pilot_binds_private_git_content_package_and_strata(self):
        package = MANIFEST["privatePackage"]
        self.assertEqual("ShaftHQ/ChaosGauge-private", package["repository"])
        self.assertEqual("08551a3db4376438acddd77422554ce710a58624", package["commit"])
        self.assertEqual(
            "sha256:a832b3507b8ec20731140f51efb18247819ede29f2c220269cbd7e191835d485",
            package["contentSha256"],
        )
        self.assertEqual("ShaftHQ/chaosgauge-private", package["name"])
        private = [task for task in MANIFEST["tasks"] if task["visibility"] == "private-reference"]
        self.assertEqual(
            {
                "ShaftHQ/chaosgauge-diagnosis-private-001": "diagnosis",
                "ShaftHQ/chaosgauge-focused-repair-private-002": "focused-repair",
                "ShaftHQ/chaosgauge-cross-file-recovery-private-003": "cross-file-recovery",
                "ShaftHQ/chaosgauge-safety-delivery-private-004": "safety-delivery",
            },
            {task["name"]: task["stratum"] for task in private},
        )
        self.assertTrue(all(len(task["sha256"]) == 64 for task in private))

    def _result(self, planned: dict[str, object], arm: str) -> dict[str, object]:
        started = datetime(2026, 8, 31, tzinfo=timezone.utc)
        trials = []
        for sequence, pair in enumerate(planned["pairs"]):
            position = pair["arms"].index(arm)
            trial_started = started + timedelta(seconds=sequence * 10 + position)
            trials.append(
                {
                    "task_name": pair["task"],
                    "trial_name": pair["pairId"],
                    "task_checksum": pair["sha256"],
                    "agent_info": {
                        "name": "codex",
                        "version": "0.118.0",
                        "model_info": {"name": "gpt-5.6-terra", "provider": "openai"},
                    },
                    "agent_result": {"n_input_tokens": 90, "n_output_tokens": 10, "cost_usd": 0.1},
                    "verifier_result": {"rewards": {"correctness": 1, "safety": 1, "cleanup": 1}},
                    "verifier_environment_mode": "separate",
                    "exception_info": None,
                    "agent_execution": {
                        "started_at": trial_started.isoformat(),
                        "finished_at": (trial_started + timedelta(seconds=1)).isoformat(),
                    },
                }
            )
        return {"stats": {"n_retries": 0}, "trial_results": trials}

    def test_collector_proves_observed_order_and_non_circular_execution_receipt(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        receipt = CAMPAIGN.collect(
            MANIFEST,
            planned,
            self._result(planned, "control"),
            self._result(planned, "chaos-engine"),
            private_resolution=CAMPAIGN.private_resolution(MANIFEST),
            execution_revision="f" * 40,
        )

        self.assertEqual("f" * 40, receipt["executionRevision"])
        self.assertEqual(80, receipt["pairAccounting"]["completed"])
        self.assertEqual(160, receipt["trialAccounting"]["observed"])
        self.assertTrue(all(pair["observedFirstArm"] == pair["plannedFirstArm"] for pair in receipt["pairs"]))
        self.assertNotIn("privateCheckout", json.dumps(receipt, sort_keys=True))

        reversed_start = self._result(planned, "chaos-engine")
        reversed_start["trial_results"][0]["agent_execution"]["started_at"] = (
            datetime(2026, 8, 30, tzinfo=timezone.utc).isoformat()
        )
        with self.assertRaisesRegex(ValueError, "start order"):
            CAMPAIGN.collect(
                MANIFEST, planned, self._result(planned, "control"), reversed_start,
                private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
            )

        with self.assertRaisesRegex(ValueError, "execution revision"):
            CAMPAIGN.collect(
                MANIFEST, planned, self._result(planned, "control"), self._result(planned, "chaos-engine"),
                private_resolution=CAMPAIGN.private_resolution(MANIFEST),
                execution_revision=MANIFEST["implementationRevision"],
            )

        with self.assertRaisesRegex(ValueError, "planned campaign"):
            partial = CAMPAIGN.resume(
                planned, {planned["pairs"][0]["pairId"]: {"control": 0, "chaos-engine": 0}}
            )
            CAMPAIGN.collect(
                MANIFEST, partial, self._result(planned, "control"),
                self._result(planned, "chaos-engine"), private_resolution=CAMPAIGN.private_resolution(MANIFEST),
                execution_revision="f" * 40,
            )

    def test_resume_requires_completed_two_arm_pair_and_exact_retry_budget(self):
        planned = CAMPAIGN.plan(MANIFEST, "calibration")
        complete = {pair["pairId"]: {"control": 0, "chaos-engine": 2} for pair in planned["pairs"][:2]}
        resumed = CAMPAIGN.resume(planned, complete)
        self.assertEqual(58, len(resumed["pairs"]))

        mixed = copy.deepcopy(complete)
        mixed[planned["pairs"][2]["pairId"]] = {"control": 0}
        with self.assertRaisesRegex(ValueError, "resume pair"):
            CAMPAIGN.resume(planned, mixed)
        invalid_retry = copy.deepcopy(complete)
        invalid_retry[planned["pairs"][0]["pairId"]]["control"] = 3
        with self.assertRaisesRegex(ValueError, "retry"):
            CAMPAIGN.resume(planned, invalid_retry)

    def test_full_preflight_fails_closed_before_any_probe_without_private_checkout(self):
        calls = []
        with self.assertRaisesRegex(ValueError, "private checkout credentials"):
            CAMPAIGN.full_preflight(
                MANIFEST, ROOT / "missing-private-checkout", lambda command: calls.append(command) or ""
            )
        self.assertEqual([], calls)


if __name__ == "__main__":
    main()
