"""ChaosGauge native Harbor campaign proof (#5460)."""

from __future__ import annotations

import copy
import importlib.util
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import TestCase, main

import yaml


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
MANIFEST = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))

SPEC = importlib.util.spec_from_file_location("chaos_gauge_campaign", GAUGE / "campaign.py")
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("ChaosGauge campaign module could not be loaded")
CAMPAIGN = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CAMPAIGN)


class ChaosGaugeCampaignTest(TestCase):
    def _jobs(self, campaign: str) -> dict[str, object]:
        prefix = "" if campaign == "calibration" else "full-pilot-"
        return {
            arm: yaml.safe_load((GAUGE / "job-configs" / f"{prefix}{arm}.yaml").read_text())
            for arm in CAMPAIGN.ARMS
        }

    def _native_name(self, task: str, number: int) -> str:
        return f"{task.rsplit('/', 1)[-1][:32].rstrip('_-')}__A{number:06d}"

    def _result(self, planned: dict[str, object], arm: str, *, retries: int = 0) -> dict[str, object]:
        jobs = self._jobs(str(planned["campaign"]))
        started = datetime(2026, 8, 31, tzinfo=timezone.utc)
        trials = []
        serial = 0
        for task in {pair["task"] for pair in planned["pairs"]}:
            pairs = [pair for pair in planned["pairs"] if pair["task"] == task]
            for pair in pairs:
                position = pair["arms"].index(arm)
                trials.append(
                    {
                        "task_name": pair["task"],
                        "trial_name": self._native_name(pair["task"], serial),
                        "task_checksum": pair["sha256"],
                        "config": {"agent": copy.deepcopy(jobs[arm]["agents"][0])},
                        "agent_info": {
                            "name": "codex",
                            "version": "0.118.0",
                            "model_info": {"name": "gpt-5.6-terra", "provider": "openai"},
                        },
                        "verifier_environment_mode": "separate",
                        "agent_execution": {
                            "started_at": (started + timedelta(seconds=serial * 10 + position)).isoformat(),
                        },
                    }
                )
                serial += 1
        return {"stats": {"n_retries": retries}, "trial_results": trials}

    def _lock(self, planned: dict[str, object], arm: str) -> dict[str, object]:
        agent = self._jobs(str(planned["campaign"]))[arm]["agents"][0]
        return {
            "schema_version": 3,
            "harbor": {"version": "0.22.0"},
            "n_concurrent_trials": 2,
            "retry": {"max_retries": 2, "include_exceptions": ["EnvironmentStartError", "EnvironmentBuildError"]},
            "trials": [
                {
                    "task": {"name": pair["task"], "digest": f"sha256:{pair['sha256']}"},
                    "agent": copy.deepcopy(agent),
                }
                for pair in planned["pairs"]
            ],
        }

    def _merged_git(self, revision: str):
        def run(command: list[str]) -> str:
            if "rev-parse" in command:
                return revision + "\n"
            if "rev-list" in command:
                return f"{revision} {'1' * 40} {'2' * 40}\n"
            if "merge-base" in command:
                return ""
            raise AssertionError(command)
        return run

    def _collect(self, planned: dict[str, object], *, retries: int = 0) -> dict[str, object]:
        revision = "f" * 40
        return CAMPAIGN.collect(
            MANIFEST,
            planned,
            self._result(planned, "control", retries=retries),
            self._result(planned, "chaos-engine", retries=retries),
            control_lock=self._lock(planned, "control"),
            candidate_lock=self._lock(planned, "chaos-engine"),
            private_resolution=CAMPAIGN.private_resolution(MANIFEST),
            execution_revision=revision,
            repository=ROOT,
            run=self._merged_git(revision),
        )

    def test_schedule_is_seeded_balanced_and_has_exact_pair_matrix(self):
        calibration = CAMPAIGN.plan(MANIFEST, "calibration")
        full_pilot = CAMPAIGN.plan(MANIFEST, "full-pilot")

        self.assertEqual((60, 120), (len(calibration["pairs"]), calibration["trials"]))
        self.assertEqual((80, 160), (len(full_pilot["pairs"]), full_pilot["trials"]))
        for plan in (calibration, full_pilot):
            first = [pair["arms"][0] for pair in plan["pairs"]]
            self.assertEqual(len(first) // 2, first.count("control"))
            self.assertEqual(len(first) // 2, first.count("chaos-engine"))
            for task in {pair["task"] for pair in plan["pairs"]}:
                task_first = [pair["arms"][0] for pair in plan["pairs"] if pair["task"] == task]
                self.assertIn(task_first.count("control"), (2, 3))
                self.assertIn(task_first.count("chaos-engine"), (2, 3))
        self.assertEqual(80, len({pair["pairId"] for pair in full_pilot["pairs"]}))

    def test_full_pilot_binds_private_git_content_package_and_strata(self):
        package = MANIFEST["privatePackage"]
        self.assertEqual("ShaftHQ/ChaosGauge-private", package["repository"])
        self.assertEqual("08551a3db4376438acddd77422554ce710a58624", package["commit"])
        self.assertEqual("ShaftHQ/chaosgauge-private", package["name"])
        private = [task for task in MANIFEST["tasks"] if task["visibility"] == "private-reference"]
        self.assertEqual(4, len(private))
        self.assertEqual(
            {"diagnosis", "focused-repair", "cross-file-recovery", "safety-delivery"},
            {task["stratum"] for task in private},
        )

    def test_collector_joins_native_harbor_names_and_preserves_job_wide_retries(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        receipt = self._collect(planned, retries=3)

        self.assertEqual(80, receipt["pairAccounting"]["completed"])
        self.assertEqual(160, receipt["trialAccounting"]["observed"])
        self.assertEqual({"control": 3, "chaos-engine": 3}, receipt["jobRetryAccounting"])
        self.assertTrue(all(pair["observedFirstArm"] == pair["plannedFirstArm"] for pair in receipt["pairs"]))
        self.assertTrue(all("nativeTrialNames" in pair for pair in receipt["pairs"]))

        bad = self._result(planned, "control")
        bad["trial_results"][0]["trial_name"] = planned["pairs"][0]["pairId"]
        with self.assertRaisesRegex(ValueError, "native trial identity"):
            CAMPAIGN.collect(
                MANIFEST, planned, bad, self._result(planned, "chaos-engine"),
                control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
                private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
                repository=ROOT, run=self._merged_git("f" * 40),
            )

    def test_collector_rejects_foreign_plan_arm_and_order(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        foreign = copy.deepcopy(planned)
        foreign["pairs"][0]["sha256"] = "0" * 64
        with self.assertRaisesRegex(ValueError, "planned campaign"):
            self._collect(foreign)

        control = self._result(planned, "control")
        candidate = self._result(planned, "chaos-engine")
        candidate["trial_results"][0]["config"]["agent"] = control["trial_results"][0]["config"]["agent"]
        with self.assertRaisesRegex(ValueError, "arm identity"):
            CAMPAIGN.collect(
                MANIFEST, planned, control, candidate,
                control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
                private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
                repository=ROOT, run=self._merged_git("f" * 40),
            )

        reversed_start = self._result(planned, "chaos-engine")
        target = next(
            pairs[0] for pairs in (
                [pair for pair in planned["pairs"] if pair["task"] == task]
                for task in {pair["task"] for pair in planned["pairs"]}
            ) if pairs[0]["arms"][0] == "control"
        )
        target_trial = next(
            trial for trial in reversed_start["trial_results"] if trial["task_name"] == target["task"]
        )
        target_trial["agent_execution"]["started_at"] = (
            datetime(2026, 8, 30, tzinfo=timezone.utc).isoformat()
        )
        with self.assertRaisesRegex(ValueError, "start order"):
            CAMPAIGN.collect(
                MANIFEST, planned, self._result(planned, "control"), reversed_start,
                control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
                private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
                repository=ROOT, run=self._merged_git("f" * 40),
            )

    def test_resume_requires_identity_bound_completed_evidence(self):
        planned = CAMPAIGN.plan(MANIFEST, "calibration")
        pair = planned["pairs"][0]
        completed = {
            pair["pairId"]: {
                "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"],
                "implementationRevision": planned["implementationRevision"],
                "arms": {
                    arm: {"treatmentSha256": next(item for item in MANIFEST["arms"] if item["name"] == arm)["treatmentSha256"]["calibration"], "nativeTrialName": self._native_name(pair["task"], number)}
                    for number, arm in enumerate(CAMPAIGN.ARMS)
                },
            }
        }
        self.assertEqual(59, len(CAMPAIGN.resume(MANIFEST, planned, completed)["pairs"]))
        stale = copy.deepcopy(completed)
        stale[pair["pairId"]]["sha256"] = "0" * 64
        with self.assertRaisesRegex(ValueError, "resume pair identity"):
            CAMPAIGN.resume(MANIFEST, planned, stale)
        with self.assertRaisesRegex(ValueError, "resume pair"):
            CAMPAIGN.resume(MANIFEST, planned, {pair["pairId"]: {"control": 0, "chaos-engine": 0}})

    def test_preflight_validates_live_root_and_provider_capability(self):
        calls = []

        class Validator:
            def validate_manifest(self, manifest, *, root):
                calls.append(("manifest", root))

            def load_jobs(self, root, campaign):
                calls.append(("jobs", root, campaign))
                return {"control": {}, "chaos-engine": {}}

            def validate_job_contracts(self, manifest, jobs, *, campaign, root):
                calls.append(("contracts", campaign, root))

        CAMPAIGN._validate_live_campaign(Validator(), MANIFEST, ROOT)
        self.assertEqual(("manifest", ROOT), calls[0])
        self.assertEqual({ROOT}, {call[-1] for call in calls if call[0] == "contracts"})

        probe = []
        self.assertTrue(CAMPAIGN.provider_capability_is_available(
            lambda command: probe.append(command) or "CHAOSGAUGE_CAPABILITY_OK\n"
        ))
        self.assertIn("gpt-5.6-terra", probe[0])
        self.assertFalse(CAMPAIGN.provider_capability_is_available(lambda command: "unauthorized"))

    def test_exact_codex_pin_and_merged_execution_proof_fail_closed(self):
        self.assertTrue(CAMPAIGN.codex_version_is_pinned("codex-cli 0.118.0"))
        for value in ("codex-cli 0.118.0-beta", "codex-cli 0.118.1", "codex-cli 10.118.0"):
            self.assertFalse(CAMPAIGN.codex_version_is_pinned(value))

        with self.assertRaisesRegex(ValueError, "execution revision"):
            CAMPAIGN.validate_execution_revision(
                ROOT, "f" * 40, MANIFEST["implementationRevision"], lambda command: "f" * 40
            )

    def test_full_preflight_fails_closed_before_any_probe_without_private_checkout(self):
        calls = []
        with self.assertRaisesRegex(ValueError, "private checkout credentials"):
            CAMPAIGN.full_preflight(
                MANIFEST, ROOT / "missing-private-checkout", lambda command: calls.append(command) or ""
            )
        self.assertEqual([], calls)


if __name__ == "__main__":
    main()
