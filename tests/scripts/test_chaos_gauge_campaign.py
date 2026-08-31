"""ChaosGauge native Harbor campaign proof (#5460)."""

from __future__ import annotations

import copy
import importlib.util
import json
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path
from tempfile import TemporaryDirectory
from types import ModuleType, SimpleNamespace
from unittest import TestCase, main
from unittest.mock import patch

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

    def _job_id(self, arm: str) -> str:
        return "00000000-0000-0000-0000-00000000000" + ("1" if arm == "control" else "2")

    def _result(self, planned: dict[str, object], arm: str, *, retries: int = 0) -> dict[str, object]:
        jobs = self._jobs(str(planned["campaign"]))
        started = datetime(2026, 8, 31, tzinfo=timezone.utc)
        trials = []
        serial = 0
        native_serial = 0 if arm == "control" else 10_000
        for task in {pair["task"] for pair in planned["pairs"]}:
            pairs = [pair for pair in planned["pairs"] if pair["task"] == task]
            for pair in pairs:
                position = pair["arms"].index(arm)
                trials.append(
                    {
                        "task_name": pair["task"],
                        "trial_name": self._native_name(pair["task"], native_serial),
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
                native_serial += 1
        return {"id": self._job_id(arm), "stats": {"n_retries": retries}, "trial_results": trials}

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

    def _bindings(self, planned: dict[str, object]) -> dict[str, object]:
        return CAMPAIGN.bind_resolved_jobs(
            MANIFEST,
            planned,
            self._resolved_jobs(planned),
            prepared_at="2026-08-29T00:00:00+00:00",
        )

    def _resolved_jobs(self, planned: dict[str, object]) -> dict[str, object]:
        class Task:
            def __init__(self, name):
                self.name = name

            def get_task_id(self):
                return self

            def get_name(self):
                return self.name

        return {
            arm: SimpleNamespace(
                id=self._job_id(arm),
                _trial_configs=[
                    SimpleNamespace(task=Task(trial["task_name"]), trial_name=trial["trial_name"])
                    for trial in self._result(planned, arm)["trial_results"]
                ],
            )
            for arm in CAMPAIGN.ARMS
        }

    def _pair_jobs(self, planned: dict[str, object]) -> dict[str, object]:
        class Task:
            def __init__(self, name):
                self.name = name

            def get_task_id(self):
                return self

            def get_name(self):
                return self.name

        jobs = {}
        for number, pair in enumerate(planned["pairs"], 1):
            jobs[pair["pairId"]] = SimpleNamespace(
                id=f"00000000-0000-0000-0000-{number:012d}",
                config=SimpleNamespace(job_name=CAMPAIGN._pair_job_name(planned["campaign"], pair["pairId"])),
                _trial_configs=[
                    SimpleNamespace(task=Task(pair["task"]), trial_name=self._native_name(pair["task"], number * 2 + index))
                    for index in range(2)
                ],
            )
        return jobs

    def _pair_evidence(self, planned: dict[str, object], bindings: dict[str, object], *, retries: int = 1) -> dict[str, object]:
        jobs = self._jobs(planned["campaign"])
        bound = {pair["pairId"]: pair for pair in bindings["pairs"]}
        started = datetime(2026, 8, 31, tzinfo=timezone.utc)
        evidence = {}
        for serial, pair in enumerate(planned["pairs"]):
            item = bound[pair["pairId"]]
            trials = []
            lock_trials = []
            for position, arm in enumerate(pair["arms"]):
                trials.append({
                    "task_name": pair["task"], "trial_name": item["arms"][arm], "task_checksum": pair["sha256"],
                    "config": {"agent": copy.deepcopy(jobs[arm]["agents"][0])},
                    "agent_info": {"name": "codex", "version": "0.118.0", "model_info": {"name": "gpt-5.6-terra", "provider": "openai"}},
                    "verifier_environment_mode": "separate",
                    "agent_execution": {"started_at": (started + timedelta(seconds=serial * 2 + position)).isoformat()},
                })
                lock_trials.append({"task": {"name": pair["task"], "digest": f"sha256:{pair['sha256']}"}, "agent": copy.deepcopy(jobs[arm]["agents"][0])})
            evidence[pair["pairId"]] = {
                "result": {"id": item["jobId"], "stats": {"n_retries": retries}, "trial_results": trials},
                "lock": {"schema_version": 3, "harbor": {"version": "0.22.0"}, "n_concurrent_trials": 2, "retry": {"max_retries": 2, "include_exceptions": ["EnvironmentStartError", "EnvironmentBuildError"]}, "trials": lock_trials},
            }
        return evidence

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
            native_bindings=self._bindings(planned),
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
        self.assertEqual(CAMPAIGN._bindings_digest(self._bindings(planned)), receipt["nativeBindingsSha256"])
        self.assertTrue(all(pair["observedFirstArm"] == pair["plannedFirstArm"] for pair in receipt["pairs"]))
        self.assertTrue(all("nativeTrialNames" in pair for pair in receipt["pairs"]))

        bad = self._result(planned, "control")
        bad["trial_results"][0]["trial_name"] = planned["pairs"][0]["pairId"]
        with self.assertRaisesRegex(ValueError, "native trial identity"):
            CAMPAIGN.collect(
                MANIFEST, planned, bad, self._result(planned, "chaos-engine"),
                control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
                native_bindings=self._bindings(planned),
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
                native_bindings=self._bindings(planned),
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
                native_bindings=self._bindings(planned),
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

    def test_resume_rejects_native_identity_reuse_across_attempts(self):
        planned = CAMPAIGN.plan(MANIFEST, "calibration")
        first, second = planned["pairs"][:2]

        def record(pair):
            return {
                "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"],
                "implementationRevision": planned["implementationRevision"],
                "arms": {
                    arm: {
                        "treatmentSha256": next(item for item in MANIFEST["arms"] if item["name"] == arm)["treatmentSha256"]["calibration"],
                        "nativeTrialName": self._native_name(pair["task"], number),
                    }
                    for number, arm in enumerate(CAMPAIGN.ARMS)
                },
            }

        reused = {first["pairId"]: record(first), second["pairId"]: record(second)}
        with self.assertRaisesRegex(ValueError, "native trial identity is reused"):
            CAMPAIGN.resume(MANIFEST, planned, reused)

    def test_collector_uses_preexecution_native_mapping_not_chronological_adjacency(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        bindings = self._bindings(planned)
        control, candidate = self._result(planned, "control"), self._result(planned, "chaos-engine")
        first, second = bindings["pairs"][:2]
        starts = [
            "2026-08-31T00:00:00+00:00", "2026-08-31T00:00:01+00:00",
            "2026-08-31T00:00:02+00:00", "2026-08-31T00:00:03+00:00",
        ]

        pairs = {pair["pairId"]: pair for pair in planned["pairs"]}
        for binding, first_start, second_start in ((first, starts[0], starts[2]), (second, starts[1], starts[3])):
            for arm, started in zip(pairs[binding["pairId"]]["arms"], (first_start, second_start)):
                result = control if arm == "control" else candidate
                trial = next(item for item in result["trial_results"] if item["trial_name"] == binding["arms"][arm])
                trial["agent_execution"]["started_at"] = started

        receipt = CAMPAIGN.collect(
            MANIFEST, planned, control, candidate,
            control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
            native_bindings=bindings, private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
            repository=ROOT, run=self._merged_git("f" * 40),
        )
        observed = {pair["pairId"]: pair["nativeTrialNames"] for pair in receipt["pairs"]}
        self.assertEqual(first["arms"], observed[first["pairId"]])
        self.assertEqual(second["arms"], observed[second["pairId"]])

    def test_binding_uses_exact_resolved_job_identity(self):
        planned = CAMPAIGN.plan(MANIFEST, "calibration")
        jobs = self._resolved_jobs(planned)
        bindings = CAMPAIGN.bind_resolved_jobs(
            MANIFEST, planned, jobs, prepared_at="2026-08-29T00:00:00+00:00"
        )
        self.assertEqual({arm: self._job_id(arm) for arm in CAMPAIGN.ARMS}, bindings["jobIds"])

        started = self._resolved_jobs(planned)
        started["control"]._job_result = object()
        with self.assertRaisesRegex(ValueError, "already started"):
            CAMPAIGN.bind_resolved_jobs(
                MANIFEST, planned, started, prepared_at="2026-08-29T00:00:00+00:00"
            )

        control = self._result(planned, "control")
        control["id"] = self._job_id("chaos-engine")
        with self.assertRaisesRegex(ValueError, "job identity"):
            CAMPAIGN.collect(
                MANIFEST, planned, control, self._result(planned, "chaos-engine"),
                control_lock=self._lock(planned, "control"), candidate_lock=self._lock(planned, "chaos-engine"),
                native_bindings=bindings, private_resolution=CAMPAIGN.private_resolution(MANIFEST), execution_revision="f" * 40,
                repository=ROOT, run=self._merged_git("f" * 40),
            )

    def test_launch_map_is_exclusive_durable_and_restores_same_job_resume(self):
        planned = CAMPAIGN.plan(MANIFEST, "calibration")
        jobs = self._resolved_jobs(planned)
        with TemporaryDirectory() as directory:
            path = Path(directory) / "launch-map.json"
            bindings = CAMPAIGN.bind_and_persist_resolved_jobs(
                MANIFEST, planned, jobs, path, prepared_at="2026-08-29T00:00:00+00:00"
            )
            self.assertEqual(bindings, json.loads(path.read_text(encoding="utf-8")))
            with self.assertRaisesRegex(ValueError, "already exist"):
                CAMPAIGN.write_native_bindings(path, bindings)

        binding = bindings["pairs"][0]
        pair = next(item for item in planned["pairs"] if item["pairId"] == binding["pairId"])
        completed = {
            pair["pairId"]: {
                "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"],
                "implementationRevision": planned["implementationRevision"],
                "arms": {
                    arm: {
                        "treatmentSha256": next(item for item in MANIFEST["arms"] if item["name"] == arm)["treatmentSha256"]["calibration"],
                        "nativeTrialName": binding["arms"][arm],
                    }
                    for arm in CAMPAIGN.ARMS
                },
            }
        }
        resumed = self._resolved_jobs(planned)
        for arm in CAMPAIGN.ARMS:
            resumed[arm]._remaining_trial_configs = [
                trial for trial in resumed[arm]._trial_configs if trial.trial_name != binding["arms"][arm]
            ]
        remaining = CAMPAIGN.resume_resolved_jobs(MANIFEST, planned, bindings, resumed, completed)
        self.assertEqual(59, len(remaining["pairs"]))
        expected = {
            arm: {pair["arms"][arm] for pair in bindings["pairs"] if pair["pairId"] != binding["pairId"]}
            for arm in CAMPAIGN.ARMS
        }
        for arm in CAMPAIGN.ARMS:
            self.assertEqual(expected[arm], {trial.trial_name for trial in resumed[arm]._remaining_trial_configs})

        resumed["control"].id = self._job_id("chaos-engine")
        with self.assertRaisesRegex(ValueError, "job identity"):
            CAMPAIGN.resume_resolved_jobs(MANIFEST, planned, bindings, resumed, completed)

    def test_pair_launcher_creates_all_native_jobs_before_runs_and_limits_parallel_pairs(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        configs = CAMPAIGN.pair_job_configs(MANIFEST, "full-pilot")
        self.assertEqual(80, len(configs))
        self.assertTrue(all(config["n_attempts"] == 1 and config["n_concurrent_trials"] == 2 for config in configs.values()))
        self.assertTrue(all(len(config["agents"]) == 2 and len(config["datasets"]) == 1 and len(config["datasets"][0]["task_names"]) == 1 for config in configs.values()))
        expected = {config["job_name"]: pair_id for pair_id, config in configs.items()}
        resolved = self._pair_jobs(planned)
        created, active, maximum, gates = [], 0, [0], []

        async def create(config):
            pair_id = expected[config["job_name"]]
            created.append(pair_id)
            job = resolved[pair_id]

            async def run():
                nonlocal active
                self.assertEqual(80, len(created))
                active += 1
                maximum[0] = max(maximum[0], active)
                await __import__("asyncio").sleep(0)
                active -= 1
                return {"id": job.id}

            job.run = run
            return job

        with TemporaryDirectory() as directory:
            path = Path(directory) / "pair-launch-map.json"
            launched = __import__("asyncio").run(CAMPAIGN.launch_pair_jobs(
                MANIFEST, "full-pilot", path, prepared_at="2026-08-29T00:00:00+00:00", create_job=create,
                install_gate=lambda job, first, second: gates.append((job.id, first, second)),
            ))
            self.assertEqual(80, len(launched["results"]))
            self.assertEqual(launched["bindings"], json.loads(path.read_text(encoding="utf-8")))
        self.assertEqual(80, len(created))
        self.assertLessEqual(maximum[0], 2)
        self.assertEqual(80, len(gates))

    def test_pair_start_gate_releases_only_after_first_native_agent_start_or_rejects(self):
        class Events:
            AGENT_START, END, CANCEL = object(), object(), object()

        hooks_module = ModuleType("harbor.trial.hooks")
        hooks_module.TrialEvent = Events
        harbor, trial = ModuleType("harbor"), ModuleType("harbor.trial")
        with patch.dict(sys.modules, {"harbor": harbor, "harbor.trial": trial, "harbor.trial.hooks": hooks_module}):
            hooks = {}
            job = SimpleNamespace(add_hook=lambda event, callback: hooks.__setitem__(event, callback))
            CAMPAIGN._install_pair_start_gate(job, "first", "second")

            async def prove_order():
                waiter = __import__("asyncio").create_task(hooks[Events.AGENT_START](SimpleNamespace(trial_name="second")))
                await __import__("asyncio").sleep(0)
                self.assertFalse(waiter.done())
                await hooks[Events.AGENT_START](SimpleNamespace(trial_name="first"))
                await waiter

            __import__("asyncio").run(prove_order())

            failed = {}
            failed_job = SimpleNamespace(add_hook=lambda event, callback: failed.__setitem__(event, callback))
            CAMPAIGN._install_pair_start_gate(failed_job, "first", "second")

            async def prove_failure():
                waiter = __import__("asyncio").create_task(failed[Events.AGENT_START](SimpleNamespace(trial_name="second")))
                await __import__("asyncio").sleep(0)
                await failed[Events.END](SimpleNamespace(trial_name="first"))
                with self.assertRaisesRegex(ValueError, "did not start"):
                    await waiter

            __import__("asyncio").run(prove_failure())

    def test_pair_collector_and_resume_use_global_prebound_native_identities(self):
        planned = CAMPAIGN.plan(MANIFEST, "full-pilot")
        bindings = CAMPAIGN.bind_pair_jobs(MANIFEST, planned, self._pair_jobs(planned), prepared_at="2026-08-29T00:00:00+00:00")
        evidence = self._pair_evidence(planned, bindings)
        receipt = CAMPAIGN.collect_pair_jobs(
            MANIFEST, planned, evidence, native_bindings=bindings, private_resolution=CAMPAIGN.private_resolution(MANIFEST),
            execution_revision="f" * 40, repository=ROOT, run=self._merged_git("f" * 40),
        )
        self.assertEqual(80, receipt["pairAccounting"]["completed"])
        self.assertEqual(80, receipt["jobRetryAccounting"]["pairJobs"])
        self.assertEqual(2, receipt["schemaVersion"])

        reused = copy.deepcopy(bindings)
        reused["pairs"][1]["arms"]["control"] = reused["pairs"][0]["arms"]["control"]
        with self.assertRaisesRegex(ValueError, "native trial identity is reused"):
            CAMPAIGN.collect_pair_jobs(
                MANIFEST, planned, evidence, native_bindings=reused, private_resolution=CAMPAIGN.private_resolution(MANIFEST),
                execution_revision="f" * 40, repository=ROOT, run=self._merged_git("f" * 40),
            )

        completed = receipt["completedPairs"]
        incomplete = self._pair_jobs(planned)
        first = planned["pairs"][0]["pairId"]
        incomplete.pop(first)
        for job in incomplete.values():
            job._remaining_trial_configs = job._trial_configs
        remaining = CAMPAIGN.resume_pair_jobs(MANIFEST, planned, bindings, incomplete, {first: completed[first]})
        self.assertEqual(79, len(remaining["pairs"]))
        drifted = self._pair_jobs(planned)
        drifted.pop(first)
        for job in drifted.values():
            job._remaining_trial_configs = job._trial_configs
        next(iter(drifted.values())).id = "00000000-0000-0000-0000-999999999999"
        with self.assertRaisesRegex(ValueError, "pair job identity"):
            CAMPAIGN.resume_pair_jobs(MANIFEST, planned, bindings, drifted, {first: completed[first]})

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
