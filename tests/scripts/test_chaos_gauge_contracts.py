"""ChaosGauge immutable Harbor experiment contracts (#5459)."""

from __future__ import annotations

import copy
import importlib.util
import json
from pathlib import Path
from unittest import TestCase, main


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
VALIDATOR = GAUGE / "validate_experiment.py"
MANIFEST = GAUGE / "experiment.json"

SPEC = importlib.util.spec_from_file_location("validate_experiment", VALIDATOR)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("ChaosGauge experiment validator could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ChaosGaugeContractsTest(TestCase):
    def manifest(self) -> dict[str, object]:
        return json.loads(MANIFEST.read_text(encoding="utf-8"))

    def test_canonical_manifest_is_pinned_comparable_and_reproducible(self):
        manifest = self.manifest()

        first = MODULE.validate_manifest(manifest, root=ROOT)
        second = MODULE.validate_manifest(copy.deepcopy(manifest), root=ROOT)

        self.assertEqual("0.22.0", manifest["harbor"]["version"])
        self.assertEqual("harbor==0.22.0", (GAUGE / "requirements.lock").read_text().strip())
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


if __name__ == "__main__":
    main()
