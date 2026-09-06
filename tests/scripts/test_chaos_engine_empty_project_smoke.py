"""Contracts for empty-project install → doctor smoke (#5575)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/chaos_engine_empty_project_smoke.py"


def load():
    spec = importlib.util.spec_from_file_location("ce_empty_smoke", SCRIPT)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class EmptyProjectSmokeTests(unittest.TestCase):
    def test_issue_cta_mentions_doctor_fix_next_and_github(self):
        module = load()
        text = module.issue_cta("[error] hooks\n  fix-next: reinstall")
        self.assertIn("fix-next", text)
        self.assertIn("GitHub issue", text)
        self.assertIn("doctor", text.casefold())

    def test_skip_tools_fixture_passes_when_core_lands(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "evidence.json"
            source = ROOT / "chaos-engine"
            code = module.main([
                "--source", str(source),
                "--commit", "a" * 40,
                "--skip-tools",
                "--budget-seconds", "300",
                "--output", str(output),
            ])
            self.assertEqual(0, code)
            evidence = json.loads(output.read_text(encoding="utf-8"))
            self.assertEqual("core-fixture-passed", evidence["status"])
            self.assertTrue(evidence["corePresent"])
            self.assertLessEqual(evidence["elapsedSeconds"], 300)

    def test_budget_exceeded_returns_nonzero_and_writes_evidence(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "evidence.json"
            source = ROOT / "chaos-engine"
            code = module.main([
                "--source", str(source),
                "--commit", "b" * 40,
                "--skip-tools",
                "--budget-seconds", "0",
                "--output", str(output),
            ])
            self.assertEqual(1, code)
            evidence = json.loads(output.read_text(encoding="utf-8"))
            self.assertEqual("budget-exceeded", evidence["status"])

    def test_install_docs_document_stopwatch_sla(self):
        text = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("Empty-project smoke", text)
        self.assertIn("300 seconds", text)
        self.assertIn("fix-next", text)

    def test_live_acceptance_records_empty_project_smoke_evidence(self):
        text = (ROOT / "scripts/ci/chaos_engine_live_installer_acceptance.py").read_text(
            encoding="utf-8"
        )
        self.assertIn("emptyProjectSmoke", text)
        self.assertIn("fresh-account-candidate-wrapper", text)
        self.assertIn("budgetSeconds", text)


if __name__ == "__main__":
    unittest.main()
