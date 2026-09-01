"""ChaosGauge immutable Harbor experiment contracts (#5459)."""

from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
import shutil
import sys
import tempfile
import types
import tomllib
from pathlib import Path
from unittest import IsolatedAsyncioTestCase, main
from unittest.mock import AsyncMock

import yaml


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
VALIDATOR = GAUGE / "validate_experiment.py"
MANIFEST = GAUGE / "experiment.json"

SPEC = importlib.util.spec_from_file_location("validate_experiment", VALIDATOR)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("ChaosGauge experiment validator could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ChaosGaugeContractsTest(IsolatedAsyncioTestCase):
    def manifest(self) -> dict[str, object]:
        return json.loads(MANIFEST.read_text(encoding="utf-8"))

    def test_write_generated_refreshes_coupled_identities_idempotently(self):
        write_generated = getattr(MODULE, "write_generated", None)
        self.assertTrue(callable(write_generated))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            gauge = root / "scripts/ci/chaos_gauge"
            shutil.copytree(GAUGE, gauge)
            manifest_path = gauge / "experiment.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest["arms"][1]["harnessSha256"] = "0" * 64
            manifest_path.write_text(
                json.dumps(manifest, indent=2, sort_keys=True) + "\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(
                ValueError,
                "job harness treatment|digest mismatch|treatment digest",
            ):
                MODULE.validate_job_contracts(
                    json.loads(manifest_path.read_text(encoding="utf-8")),
                    MODULE.load_jobs(gauge, "calibration"),
                    root=root,
                    campaign="calibration",
                )

            write_generated(root)
            MODULE.validate_job_contracts(
                json.loads(manifest_path.read_text(encoding="utf-8")),
                MODULE.load_jobs(gauge, "calibration"),
                root=root,
                campaign="calibration",
            )
            MODULE.validate_job_contracts(
                json.loads(manifest_path.read_text(encoding="utf-8")),
                MODULE.load_jobs(gauge, "full-pilot"),
                root=root,
                campaign="full-pilot",
            )
            first = {
                path.relative_to(root).as_posix(): path.read_bytes()
                for path in (
                    manifest_path,
                    gauge / "job-configs/chaos-engine.yaml",
                    gauge / "job-configs/full-pilot-chaos-engine.yaml",
                )
            }
            write_generated(root)
            second = {
                path.relative_to(root).as_posix(): path.read_bytes()
                for path in (
                    manifest_path,
                    gauge / "job-configs/chaos-engine.yaml",
                    gauge / "job-configs/full-pilot-chaos-engine.yaml",
                )
            }
            self.assertEqual(first, second)

    def test_canonical_manifest_is_pinned_comparable_and_reproducible(self):
        manifest = self.manifest()

        first = MODULE.validate_manifest(manifest, root=ROOT)
        second = MODULE.validate_manifest(copy.deepcopy(manifest), root=ROOT)

        self.assertEqual("0.22.0", manifest["harbor"]["version"])
        lock = (GAUGE / "requirements.lock").read_text(encoding="utf-8")
        self.assertIn("harbor==0.22.0", lock)
        self.assertIn("--hash=sha256:", lock)
        self.assertIn("# via harbor", lock)
        self.assertEqual(16, len(manifest["tasks"]))
        self.assertEqual(5, manifest["attemptsPerTask"])
        self.assertEqual(160, len(manifest["tasks"]) * len(manifest["arms"]) * 5)
        self.assertEqual(first, second)
        self.assertRegex(first, r"^[0-9a-f]{64}$")

    def test_non_treatment_drift_fails_before_execution(self):
        mutations = {
            "model": lambda value: value["arms"][1].update(model="different"),
            "effort": lambda value: value["arms"][1].update(effort="high"),
            "agent": lambda value: value["arms"][1].update(agent="claude-code"),
            "repository": lambda value: value["arms"][1].update(repositoryRevision="f" * 40),
            "resources": lambda value: value["arms"][1]["resources"].update(cpus=3),
            "timeout": lambda value: value["arms"][1].update(timeoutSeconds=1801),
            "image": lambda value: value["arms"][1].update(
                imageDigest="python:3.12.11-slim@sha256:" + "f" * 64
            ),
        }
        for field, mutate in mutations.items():
            with self.subTest(field=field):
                manifest = self.manifest()
                mutate(manifest)
                with self.assertRaisesRegex(ValueError, f"arm {field}"):
                    MODULE.validate_manifest(manifest, root=ROOT)

    def test_mutable_or_incomplete_identities_fail_closed(self):
        mutations = {
            "Harbor source": lambda value: value["harbor"].update(source="harbor-framework/harbor@main"),
            "image digest": lambda value: value["arms"][0].update(imageDigest="ubuntu:latest"),
            "task digest": lambda value: value["tasks"][0].update(sha256="pending"),
            "harness digest": lambda value: value["arms"][1].update(harnessSha256=""),
            "duplicate task": lambda value: value["tasks"].append(copy.deepcopy(value["tasks"][0])),
        }
        for message, mutate in mutations.items():
            with self.subTest(message=message):
                manifest = self.manifest()
                mutate(manifest)
                with self.assertRaisesRegex(ValueError, message):
                    MODULE.validate_manifest(manifest, root=ROOT)

    def test_only_declared_harness_treatment_may_differ(self):
        manifest = self.manifest()
        control, candidate = manifest["arms"]

        self.assertEqual("none", control["harness"])
        self.assertEqual("chaos-engine", candidate["harness"])
        self.assertNotEqual(control["harnessSha256"], candidate["harnessSha256"])
        self.assertEqual(
            {"name", "harness", "harnessSha256", "treatmentSha256"},
            {
                key
                for key in control
                if control[key] != candidate[key]
            },
        )

    def test_generated_and_private_material_are_ignored(self):
        ignored = (ROOT / ".gitignore").read_text(encoding="utf-8")

        self.assertIn("/scripts/ci/chaos_gauge/jobs/", ignored)
        self.assertIn("/scripts/ci/chaos_gauge/reports/", ignored)
        self.assertIn("/scripts/ci/chaos_gauge/private/", ignored)

    def test_native_harbor_jobs_differ_only_by_harness_treatment(self):
        jobs = {
            name: yaml.safe_load(
                (GAUGE / f"job-configs/{name}.yaml").read_text(encoding="utf-8")
            )
            for name in ("control", "chaos-engine")
        }

        for name, job in jobs.items():
            self.assertEqual(5, job["n_attempts"])
            self.assertEqual(
                "scripts/ci/chaos_gauge/dataset", job["datasets"][0]["path"]
            )
            self.assertEqual("docker", job["environment"]["type"])
            self.assertTrue(job["environment"]["delete"])
            if name == "control":
                self.assertEqual("codex", job["agents"][0]["name"])
            else:
                self.assertEqual(
                    "scripts.ci.chaos_gauge.agent:ChaosEngineCodex",
                    job["agents"][0]["import_path"],
                )
            self.assertEqual("gpt-5.6-terra", job["agents"][0]["model_name"])
            self.assertEqual("medium", job["agents"][0]["kwargs"]["reasoning_effort"])
            self.assertEqual(2, job["retry"]["max_retries"])
        self.assertEqual("codex", jobs["control"]["agents"][0]["name"])
        self.assertNotIn("skills", jobs["control"]["agents"][0])
        self.assertEqual(
            "scripts.ci.chaos_gauge.agent:ChaosEngineCodex",
            jobs["chaos-engine"]["agents"][0]["import_path"],
        )
        self.assertNotIn("name", jobs["chaos-engine"]["agents"][0])
        self.assertNotIn("skills", jobs["chaos-engine"]["agents"][0])

        control = copy.deepcopy(jobs["control"])
        candidate = copy.deepcopy(jobs["chaos-engine"])
        for job in (control, candidate):
            job.pop("job_name")
            agent = job["agents"][0]
            agent.pop("name", None)
            agent.pop("import_path", None)
            for field in (
                "harness_source",
                "harness_commit",
                "harness_sha256",
                "adapter_sha256",
            ):
                agent["kwargs"].pop(field, None)
        self.assertEqual(control, candidate)
        identities = MODULE.validate_job_contracts(self.manifest(), jobs, root=ROOT)
        self.assertEqual({"control", "chaos-engine"}, set(identities))
        self.assertNotEqual(identities["control"], identities["chaos-engine"])
        drifted_manifest = self.manifest()
        drifted_manifest["arms"][1]["harnessSha256"] = "f" * 64
        with self.assertRaisesRegex(ValueError, "job harness treatment"):
            MODULE.validate_job_contracts(drifted_manifest, jobs, root=ROOT)

        drifted = copy.deepcopy(jobs)
        drifted["chaos-engine"]["agents"][0]["model_name"] = "different"
        with self.assertRaisesRegex(ValueError, "job model"):
            MODULE.validate_job_contracts(self.manifest(), drifted, root=ROOT)

        drifted = copy.deepcopy(jobs)
        drifted["chaos-engine"]["agents"][0]["import_path"] = "unsafe:Agent"
        with self.assertRaisesRegex(ValueError, "job harness"):
            MODULE.validate_job_contracts(self.manifest(), drifted, root=ROOT)

        drifted = copy.deepcopy(jobs)
        drifted["control"]["retry"]["max_retries"] = 3
        with self.assertRaisesRegex(ValueError, "retry budget"):
            MODULE.validate_job_contracts(self.manifest(), drifted, root=ROOT)

        unbound = copy.deepcopy(jobs)
        for name in ("control", "chaos-engine"):
            unbound[name]["agents"][0]["kwargs"]["version"] = "9.9.9"
        with self.assertRaisesRegex(ValueError, "job harness treatment"):
            MODULE.validate_job_contracts(self.manifest(), unbound)
        self.assertEqual("0.118.0", jobs["chaos-engine"]["agents"][0]["kwargs"]["version"])

    def test_all_harbor_job_arms_add_only_the_pinned_chroma_model_host(self):
        host = "chroma-onnx-models.s3.amazonaws.com"
        for name in (
            "control", "chaos-engine", "full-pilot-control", "full-pilot-chaos-engine",
        ):
            job = yaml.safe_load((GAUGE / f"job-configs/{name}.yaml").read_text())
            self.assertEqual([host], job["environment"].get("extra_allowed_hosts"))
            self.assertNotIn("extra_allowed_hosts", job["agents"][0])

    async def test_custom_agent_delegates_to_codex_and_installs_full_harness(self):
        calls = []

        class StubCodex:
            def __init__(self, *args, **kwargs):
                pass

            async def install(self, environment):
                calls.append(("codex", environment))

            async def ensure_system_dependencies(self, environment, dependencies):
                calls.append(("dependencies", dependencies))

            async def exec_as_agent(self, environment, command, **kwargs):
                calls.append(("exec", command, kwargs))

        harbor = types.ModuleType("harbor")
        agents = types.ModuleType("harbor.agents")
        installed = types.ModuleType("harbor.agents.installed")
        codex = types.ModuleType("harbor.agents.installed.codex")
        codex.Codex = StubCodex
        previous = {
            name: sys.modules.get(name)
            for name in ("harbor", "harbor.agents", "harbor.agents.installed", "harbor.agents.installed.codex")
        }
        sys.modules.update(
            {
                "harbor": harbor,
                "harbor.agents": agents,
                "harbor.agents.installed": installed,
                "harbor.agents.installed.codex": codex,
            }
        )
        try:
            spec = importlib.util.spec_from_file_location(
                "chaos_gauge_agent", GAUGE / "agent.py"
            )
            self.assertIsNotNone(spec)
            self.assertIsNotNone(spec.loader)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            environment = types.SimpleNamespace(upload_dir=AsyncMock())
            agent = module.ChaosEngineCodex(
                harness_source=str(ROOT / "chaos-engine"),
                harness_commit="0481767def7c31fe144bc20543dfe937b8ffd4d5",
                harness_sha256=MODULE._tree_sha256(ROOT / "chaos-engine"),
                adapter_sha256=MODULE._file_sha256(GAUGE / "agent.py"),
            )

            await agent.install(environment)
        finally:
            for name, value in previous.items():
                if value is None:
                    sys.modules.pop(name, None)
                else:
                    sys.modules[name] = value

        self.assertEqual("codex", calls[0][0])
        self.assertIn(("dependencies", ("git", "python3")), calls)
        environment.upload_dir.assert_awaited_once()
        command = next(call[1] for call in calls if call[0] == "exec")
        self.assertIn("install_with_dependencies", command)
        self.assertNotIn("provisioner=", command)
        self.assertIn("doctor_with_dependencies", command)
        self.assertIn("activate_detected_plugins", command)
        self.assertIn(".chaos-engine-hosts.json", command)
        self.assertIn(".codex/hooks.json", command)
        self.assertIn("AGENTS.md", command)

    def test_counterbalanced_schedule_covers_every_planned_trial_once(self):
        schedule = json.loads((GAUGE / "schedule.json").read_text(encoding="utf-8"))
        self.assertEqual(5450, schedule["seed"])
        self.assertEqual(
            "sha256(seed:task)-balanced-2-or-3-control-first;sha256(seed:task:attempt)-rank",
            schedule["algorithm"],
        )
        self.assertEqual(["control", "chaos-engine"], schedule["arms"])
        self.assertEqual(
            {"tasks": 12, "trials": 120}, schedule["campaigns"]["publicCalibration"]
        )
        self.assertEqual(160, schedule["campaigns"]["fullPilot"]["trials"])
        self.assertTrue(
            schedule["campaigns"]["fullPilot"]["requiresPrivatePackageResolution"]
        )
        tasks = self.manifest()["tasks"]
        higher = {
            task["name"] for task in sorted(
                tasks,
                key=lambda task: hashlib.sha256(f'{schedule["seed"]}:task:{task["name"]}'.encode()).digest(),
            )[: len(tasks) // 2]
        }
        rows = []
        for task in tasks:
            attempts = sorted(
                range(1, schedule["attemptsPerTask"] + 1),
                key=lambda attempt: hashlib.sha256(f'{schedule["seed"]}:{task["name"]}:{attempt}'.encode()).digest(),
            )
            control = set(attempts[:3 if task["name"] in higher else 2])
            for attempt in range(1, schedule["attemptsPerTask"] + 1):
                first = "control" if attempt in control else "chaos-engine"
                rows.extend([(task["name"], attempt, first), (task["name"], attempt, "chaos-engine" if first == "control" else "control")])
        self.assertEqual(160, len(rows))
        self.assertEqual(80, len({(task, attempt) for task, attempt, _ in rows}))
        self.assertEqual(40, sum(1 for _, _, arm in rows[::2] if arm == "control"))

    def test_calibration_is_120_trials_and_full_pilot_requires_resolved_private_package(self):
        manifest = self.manifest()
        public = [task for task in manifest["tasks"] if task["visibility"] == "public"]
        self.assertEqual(120, len(public) * 2 * manifest["attemptsPerTask"])
        with self.assertRaisesRegex(ValueError, "private Harbor package is unresolved"):
            MODULE.validate_private_package(manifest, GAUGE / "private/resolution.json")

        for name in ("control", "chaos-engine"):
            job = yaml.safe_load(
                (GAUGE / f"job-configs/full-pilot-{name}.yaml").read_text(encoding="utf-8")
            )
            self.assertEqual(2, len(job["datasets"]))
            self.assertEqual("scripts/ci/chaos_gauge/dataset", job["datasets"][0]["path"])
            self.assertEqual("ShaftHQ/chaosgauge-private", job["datasets"][1]["name"])
            self.assertEqual(
                "sha256:7db9c6399f126edbaa60226e9eda09b5742b7302e7badea663c365ec7b2dce10",
                job["datasets"][1]["ref"],
            )
        full = MODULE.validate_job_contracts(
            manifest, MODULE.load_jobs(GAUGE, "full-pilot"), campaign="full-pilot", root=ROOT
        )
        self.assertEqual({"control", "chaos-engine"}, set(full))

    def test_configs_match_vendored_harbor_v0220_schema_facts(self):
        facts = json.loads(
            (GAUGE / "harbor-v0.22.0-contract.json").read_text(encoding="utf-8")
        )
        self.assertEqual(self.manifest()["harbor"]["commit"], facts["commit"])
        for path in (GAUGE / "job-configs").glob("*.yaml"):
            job = yaml.safe_load(path.read_text(encoding="utf-8"))
            self.assertLessEqual(set(job), set(facts["jobFields"]))
            self.assertTrue(set(job).isdisjoint(facts["deprecatedJobFields"]))
            agent = job["agents"][0]
            self.assertTrue(set(agent) & set(facts["agentIdentityFields"]))
        dataset = tomllib.loads((GAUGE / "dataset/dataset.toml").read_text())
        self.assertEqual(facts["datasetSchemaVersion"], dataset["schema_version"])
        for task in (GAUGE / "dataset").glob("*/task.toml"):
            self.assertEqual(
                facts["taskSchemaVersion"],
                tomllib.loads(task.read_text(encoding="utf-8"))["schema_version"],
            )


if __name__ == "__main__":
    main()
