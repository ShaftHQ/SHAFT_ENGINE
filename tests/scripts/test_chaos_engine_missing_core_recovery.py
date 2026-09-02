from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path


INSTALL = Path(__file__).resolve().parents[2] / "chaos-engine" / "install.py"


class MissingCoreRecoveryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        import importlib.util

        spec = importlib.util.spec_from_file_location("chaos_engine_install", INSTALL)
        assert spec is not None and spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        cls.install = module

    def test_detects_installed_receipt_without_core(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps({"phase": "installed", "schemaVersion": 1}),
                encoding="utf-8",
            )
            self.assertTrue(self.install.missing_core_with_installed_hosts(project))
            status = self.install.missing_core_recovery_status(project)
            self.assertEqual("recovery-required", status["status"])
            self.assertEqual("CE_CORE_MISSING", status["diagnosticCode"])

    def test_absent_receipt_is_not_missing_core(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertFalse(self.install.missing_core_with_installed_hosts(project))
