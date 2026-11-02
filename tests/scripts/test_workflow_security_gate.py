"""Tests for scripts/ci/workflow_security_gate.py (#7000)."""
from __future__ import annotations

import collections
import importlib.util
import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("workflow_security_gate", ROOT / "scripts/ci/workflow_security_gate.py")
gate = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(gate)


def finding(path: str, ident: str) -> dict:
    return {"ident": ident, "locations": [{"symbolic": {"key": {"Local": {"verbatim_path": path}}}}]}


class WorkflowSecurityGateTest(unittest.TestCase):
    def test_new_finding_above_baseline_regresses(self) -> None:
        current = gate.finding_keys([finding(".github/workflows/a.yml", "template-injection")] * 2)
        self.assertEqual(["a.yml::template-injection: 2 > baseline 1"],
                         gate.regressions(current, {"a.yml::template-injection": 1}))

    def test_baselined_or_fixed_findings_pass(self) -> None:
        self.assertEqual([], gate.regressions(collections.Counter({"a.yml::x": 1}), {"a.yml::x": 3}))

    def test_baseline_is_committed_and_job_is_in_summary(self) -> None:
        self.assertIsInstance(json.loads((ROOT / ".github/zizmor-baseline.json").read_text()), dict)
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("scripts/ci/workflow_security_gate.py", workflow)
        self.assertIn("needs.workflow-security.result", workflow)


if __name__ == "__main__":
    unittest.main()
