from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock


INSTALL = Path(__file__).resolve().parents[2] / "chaos-engine" / "install.py"
SOURCE = INSTALL.parent
TEST_COMMIT = "1" * 40


class MissingCoreRecoveryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        import importlib.util

        spec = importlib.util.spec_from_file_location("chaos_engine_install", INSTALL)
        if spec is None or spec.loader is None:
            raise AssertionError("failed to load chaos-engine install module")
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
            self.assertEqual("CE_CORE_MISSING", status["components"]["core"]["code"])
            detail = status["components"]["core"]["detail"]
            self.assertIn("rerun", detail.casefold())
            self.assertIn("restore", detail.casefold())

    def test_absent_receipt_is_not_missing_core(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertFalse(self.install.missing_core_with_installed_hosts(project))

    def test_quarantine_moves_orphaned_receipt(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            receipt = project / ".chaos-engine-hosts.json"
            receipt.write_text(
                json.dumps({"phase": "installed", "schemaVersion": 1}),
                encoding="utf-8",
            )
            moved = self.install.quarantine_orphaned_host_receipt(project)
            self.assertIsNotNone(moved)
            assert moved is not None
            self.assertTrue(moved.is_file())
            self.assertFalse(receipt.exists())
            self.assertFalse(self.install.missing_core_with_installed_hosts(project))

    def test_install_rematerializes_core_under_orphaned_receipt(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps({"phase": "installed", "schemaVersion": 1}),
                encoding="utf-8",
            )
            self.assertTrue(self.install.missing_core_with_installed_hosts(project))
            target = self.install.install(project, SOURCE, TEST_COMMIT)
            self.assertEqual(project / ".chaos-engine", target)
            self.assertTrue((project / ".chaos-engine" / "install.py").is_file())
            self.assertFalse(self.install.missing_core_with_installed_hosts(project))

    def test_install_with_dependencies_quarantines_before_provision(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps({"phase": "installed", "schemaVersion": 1}),
                encoding="utf-8",
            )
            reporter = mock.Mock()
            # Source validates first; quarantine runs under the project lock,
            # then install() rematerializes core. Stop at install() to prove
            # the receipt was already moved aside.
            with mock.patch.object(
                self.install,
                "install",
                side_effect=RuntimeError("stop-after-quarantine"),
            ) as mocked_install:
                with self.assertRaisesRegex(RuntimeError, "stop-after-quarantine"):
                    self.install.install_with_dependencies(
                        project,
                        SOURCE,
                        TEST_COMMIT,
                        reporter=reporter,
                    )
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())
            quarantined = list(
                (project / ".chaos-engine-state").glob("orphaned-hosts-receipt*.json")
            )
            self.assertEqual(1, len(quarantined))
            mocked_install.assert_called_once()
            reporter.trace.assert_called()


if __name__ == "__main__":
    unittest.main()
