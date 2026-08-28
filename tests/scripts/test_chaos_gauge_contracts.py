"""ChaosGauge immutable Harbor experiment contracts (#5459)."""

from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
import sys
import types
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
            {"name", "harness", "harnessSha256"},
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
        MODULE.validate_job_contracts(self.manifest(), jobs)

        drifted = copy.deepcopy(jobs)
        drifted["chaos-engine"]["agents"][0]["model_name"] = "different"
        with self.assertRaisesRegex(ValueError, "job model"):
            MODULE.validate_job_contracts(self.manifest(), drifted)

        drifted = copy.deepcopy(jobs)
        drifted["chaos-engine"]["agents"][0]["import_path"] = "unsafe:Agent"
        with self.assertRaisesRegex(ValueError, "job harness"):
            MODULE.validate_job_contracts(self.manifest(), drifted)

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
                harness_commit="76772e015d149e66e00bbad2e8d9ee8953248c9c",
                harness_sha256="03bd340d88d26177951551397b977d7454546739f34e0d6ad3154110abf27625",
                adapter_sha256="ef83517d7a1483de9c96da1039cbb11b3c9b2ae7e1dc5a467757f7015d034a7c",
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
        self.assertIn("provisioner=lambda", command)
        self.assertIn("activate_detected_plugins", command)
        self.assertIn(".chaos-engine-hosts.json", command)
        self.assertIn(".codex/hooks.json", command)
        self.assertIn("AGENTS.md", command)

    def test_counterbalanced_schedule_covers_every_planned_trial_once(self):
        schedule = json.loads((GAUGE / "schedule.json").read_text(encoding="utf-8"))
        self.assertEqual(5450, schedule["seed"])
        self.assertEqual("sha256(seed:task:attempt)-low-bit-first-arm", schedule["algorithm"])
        self.assertEqual(["control", "chaos-engine"], schedule["arms"])
        rows = []
        for task in self.manifest()["tasks"]:
            for attempt in range(1, schedule["attemptsPerTask"] + 1):
                digest = hashlib.sha256(
                    f'{schedule["seed"]}:{task["name"]}:{attempt}'.encode()
                ).digest()
                first = schedule["arms"][digest[0] & 1]
                second = schedule["arms"][1 - (digest[0] & 1)]
                rows.extend([(task["name"], attempt, first), (task["name"], attempt, second)])
        self.assertEqual(160, len(rows))
        self.assertEqual(80, len({(task, attempt) for task, attempt, _ in rows}))


if __name__ == "__main__":
    main()
