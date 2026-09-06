from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


INSTALL = Path(__file__).resolve().parents[2] / "chaos-engine" / "install.py"
SOURCE = INSTALL.parent
DEPENDENCIES = SOURCE / "dependencies.py"
TOOL = SOURCE / "tool.py"
TEST_COMMIT = "1" * 40
OTHER_COMMIT = "2" * 40


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

        dep_spec = importlib.util.spec_from_file_location(
            "chaos_engine_dependencies", DEPENDENCIES
        )
        if dep_spec is None or dep_spec.loader is None:
            raise AssertionError("failed to load chaos-engine dependencies module")
        dep_module = importlib.util.module_from_spec(dep_spec)
        dep_spec.loader.exec_module(dep_module)
        cls.dependencies = dep_module

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

    def test_missing_core_quarantine_also_moves_stale_anchors(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps({"phase": "installed", "schemaVersion": 1}),
                encoding="utf-8",
            )
            stale = "b" * 64
            (project / f".chaos-engine-hosts.active-{stale}").write_bytes(b"")
            moved = self.install.quarantine_orphaned_host_receipt(project)
            self.assertIsNotNone(moved)
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())
            self.assertFalse((project / f".chaos-engine-hosts.active-{stale}").exists())
            self.assertTrue(
                list(
                    (project / ".chaos-engine-state").glob(
                        "orphaned-.chaos-engine-hosts.active-*"
                    )
                )
            )

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
            # install() quarantines before rematerialize so stale hosts do not linger
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())

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

    def test_stale_host_receipt_after_rematerialized_core(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            target = self.install.install(project, SOURCE, TEST_COMMIT)
            manifest = json.loads(
                (target / "manifest.json").read_text(encoding="utf-8")
            )
            host_token = manifest["hostToken"]
            stale_token = "a" * 64
            # Authenticated-looking receipt is not required: anchor token mismatch
            # against the rematerialized core hostToken is the wipe-class signal.
            # coreCommit mismatch alone must NOT quarantine (upgrade path).
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "phase": "installed",
                        "schemaVersion": 1,
                        "coreCommit": OTHER_COMMIT,
                    }
                ),
                encoding="utf-8",
            )
            self.assertFalse(self.install.stale_host_state_after_wiped_runtime(project))
            (project / f".chaos-engine-hosts.active-{stale_token}").write_bytes(b"")
            self.assertTrue(self.install.account_dependency_receipt_missing(project))
            self.assertTrue(self.install.stale_host_state_after_wiped_runtime(project))
            self.assertTrue(self.install.wiped_runtime_recovery_needed(project))
            status = self.install.wiped_runtime_recovery_status(project)
            self.assertEqual("CE_WIPED_RUNTIME", status["components"]["hosts"]["code"])
            self.assertEqual(
                "CE_DEPENDENCY_RECEIPT_MISSING",
                status["components"]["tools"]["code"],
            )
            reporter = mock.Mock()
            moved = self.install.quarantine_orphaned_host_receipt(
                project, reporter=reporter
            )
            self.assertIsNotNone(moved)
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())
            self.assertFalse(
                (project / f".chaos-engine-hosts.active-{stale_token}").exists()
            )
            self.assertTrue(
                list(
                    (project / ".chaos-engine-state").glob(
                        "orphaned-.chaos-engine-hosts.active-*"
                    )
                )
            )
            self.assertNotEqual(stale_token, host_token)
            reporter.trace.assert_called()
            self.assertFalse(self.install.wiped_runtime_recovery_needed(project))

    def test_healthy_hosts_with_deps_receipt_are_not_quarantined(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            target = self.install.install(project, SOURCE, TEST_COMMIT)
            manifest = json.loads(
                (target / "manifest.json").read_text(encoding="utf-8")
            )
            commit = manifest["source"]["commit"]
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 2,
                        "components": {},
                        "commands": {},
                        "checkedAt": "2026-01-01T00:00:00+00:00",
                        "scope": "project",
                    }
                ),
                encoding="utf-8",
            )
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "phase": "installed",
                        "schemaVersion": 1,
                        "coreCommit": commit,
                    }
                ),
                encoding="utf-8",
            )
            # deps present → never auto-quarantine even if verify would fail
            self.assertFalse(self.install.stale_host_state_after_wiped_runtime(project))
            self.assertIsNone(self.install.quarantine_orphaned_host_receipt(project))
            self.assertTrue((project / ".chaos-engine-hosts.json").exists())

    def test_active_dispatch_missing_pointer_prints_fix_next(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with self.assertRaises(ValueError) as raised:
                self.dependencies.active_dispatch(project, "mempalace", ["--version"])
            message = str(raised.exception)
            self.assertIn("dependency pointer is missing", message.casefold())
            self.assertIn("fix-next:", message.casefold())
            self.assertIn("install", message.casefold())
            self.assertIn("doctor", message.casefold())

    def test_tool_py_exits_nonzero_with_heal_path(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            # Invoke source tool.py against an empty project (no receipt/pointer).
            # tool.py resolves project as parent of installed_root for non-monorepo.
            env = os.environ.copy()
            env["PYTHONDONTWRITEBYTECODE"] = "1"
            completed = subprocess.run(
                [sys.executable, str(TOOL), "mempalace", "--version"],
                cwd=project,
                capture_output=True,
                text=True,
                env=env,
                check=False,
            )
            # tool.py uses Path(__file__).parent as installed_root; project becomes
            # shared_project_root(parent of chaos-engine) = repo root, not cwd.
            # Instead call active_dispatch-style via a copy of tool in the temp tree.
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            runtime = project / ".chaos-engine"
            runtime.mkdir()
            for name in ("tool.py", "dependencies.py", "hosts.py"):
                (runtime / name).write_text(
                    (SOURCE / name).read_text(encoding="utf-8"),
                    encoding="utf-8",
                )
            completed = subprocess.run(
                [sys.executable, str(runtime / "tool.py"), "mempalace", "--version"],
                cwd=project,
                capture_output=True,
                text=True,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
                check=False,
            )
            self.assertNotEqual(0, completed.returncode)
            combined = (completed.stdout + completed.stderr).casefold()
            self.assertIn("fix-next:", combined)
            self.assertTrue(
                "dependency pointer is missing" in combined
                or "install" in combined
            )

    def test_component_fix_next_for_wiped_runtime(self) -> None:
        fix = self.install.component_fix_next(
            "hosts",
            {
                "status": "recovery-required",
                "code": "CE_WIPED_RUNTIME",
                "taskImpact": "required",
            },
        )
        self.assertIsNotNone(fix)
        assert fix is not None
        lowered = fix.casefold()
        self.assertIn("install", lowered)
        self.assertIn("quarantine", lowered)
        self.assertIn("dependencies.json", lowered)


if __name__ == "__main__":
    unittest.main()
