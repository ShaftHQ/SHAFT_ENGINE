"""#7035: deploy-shaft-mcp never checks out a workflow_run head directly."""
from __future__ import annotations

import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]


class DeployShaftMcpCheckoutTest(unittest.TestCase):
    def test_checkout_uses_trusted_ref_then_verified_switch(self) -> None:
        wf = yaml.safe_load((ROOT / ".github/workflows/deploy-shaft-mcp.yml").read_text(encoding="utf-8"))
        steps = wf["jobs"]["deploy-fly"]["steps"]
        for step in steps:
            if str(step.get("uses", "")).startswith("actions/checkout"):
                self.assertNotIn("ref", step.get("with", {}))
        switch = next(s for s in steps if s.get("name") == "Switch to the released commit")
        self.assertIn("git tag --points-at", switch["run"])
        self.assertIn("RUN_REPO", switch["run"])


if __name__ == "__main__":
    unittest.main()
