"""Semantic ownership contract for the ChaosEngine harness."""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import yaml

from scripts.ci import validate_agent_ownership


ROOT = Path(__file__).resolve().parents[2]


def agent_guidance_paths(workflow: str) -> list[str]:
    document = yaml.safe_load(workflow)
    filter_step = next(
        step for step in document["jobs"]["changes"]["steps"]
        if step.get("id") == "filter"
    )
    filters = yaml.safe_load(filter_step["with"]["filters"])
    return filters["agent_guidance"]


class AgentOwnershipValidationTest(unittest.TestCase):
    def test_repository_manifest_has_one_owner_for_every_harness_duty(self):
        self.assertEqual([], validate_agent_ownership.validate(ROOT))

    def test_semantic_duplicate_owner_is_rejected_even_when_prose_differs(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "owner.json").write_text(json.dumps({
                "duties": {"lifecycle": {"owner": "core.py", "markers": ["dispatch lifecycle events"]}},
                "adapters": [],
            }), encoding="utf-8")
            (root / "core.py").write_text("# dispatch lifecycle events\n", encoding="utf-8")
            (root / "other.md").write_text("This file owns lifecycle event dispatch.\n", encoding="utf-8")

            errors = validate_agent_ownership.validate(root, root / "owner.json")

        self.assertTrue(any("lifecycle" in error and "other.md" in error for error in errors), errors)

    def test_pr_gate_agent_path_filter_has_no_literal_duplicates(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        manifest = json.loads((ROOT / "scripts/ci/agent_ownership.json").read_text(encoding="utf-8"))
        paths = agent_guidance_paths(workflow)
        self.assertEqual(len(paths), len(set(paths)), paths)
        self.assertIn("agent-plugins/**", paths)
        self.assertIn("plugin manifests", manifest["duties"])

    def test_path_parser_detects_duplicate_after_reachability_comment(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        mutated = workflow.replace(
            "              - 'scripts/ci/watch_pr_checks.py'",
            "              - 'scripts/ci/watch_pr_checks.py'\n              - 'agent-plugins/**'",
            1,
        )
        paths = agent_guidance_paths(mutated)
        self.assertNotEqual(len(paths), len(set(paths)), paths)

    def test_required_duty_cannot_be_deleted_from_manifest(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "owner.json").write_text(json.dumps({"duties": {}, "adapters": []}), encoding="utf-8")
            errors = validate_agent_ownership.validate(root, root / "owner.json")
        self.assertTrue(any("required duties missing" in error for error in errors), errors)

    def test_thin_adapter_cannot_claim_canonical_policy(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "owner.json").write_text(json.dumps({
                "duties": {"router": {"owner": "core.md", "markers": ["canonical policy owner"]}},
                "adapters": ["adapter.md"],
            }), encoding="utf-8")
            (root / "core.md").write_text("Canonical policy owner.\n", encoding="utf-8")
            (root / "adapter.md").write_text("Canonical policy owner is this adapter.\n", encoding="utf-8")

            errors = validate_agent_ownership.validate(root, root / "owner.json")

        self.assertTrue(any("router" in error and "adapter.md" in error for error in errors), errors)


if __name__ == "__main__":
    unittest.main()
