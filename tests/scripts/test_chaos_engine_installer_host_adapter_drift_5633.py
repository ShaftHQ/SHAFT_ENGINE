"""Installer self-heal for host adapter drift with deps+core present (#5633)."""

from __future__ import annotations

import importlib.util
import json
import shutil
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
INSTALL = ROOT / "chaos-engine" / "install.py"
SOURCE = ROOT / "chaos-engine"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class AccountDependencyController:
    """Retain generation helpers while replacing only account provisioning."""

    def __init__(self, controller):
        self._controller = controller

    def __getattr__(self, name):
        return getattr(self._controller, name)

    def install_account_dependencies(self, project, _specification, **_kwargs):
        receipt = {
            "schemaVersion": 2,
            "scope": "user",
            "components": {
                name: {"status": "healthy", "action": "reused"}
                for name in (
                    "uv",
                    "python",
                    "node",
                    "java",
                    "mempalace",
                    "graphify",
                    "memory",
                    "context7",
                )
            },
            "commands": {
                name: str(Path(sys.executable).resolve())
                for name in ("python3", "node", "memory-mcp", "mempalace-mcp")
            },
        }
        project.joinpath(".chaos-engine-dependencies.json").write_text(
            json.dumps(receipt), encoding="utf-8"
        )
        return receipt


class HostAdapterDrift5633Test(unittest.TestCase):
    def test_corecommit_mismatch_with_deps_quarantines(self):
        install = load(INSTALL, "chaos_engine_install_5633_corecommit")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine"
            core.mkdir()
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "phase": "installed",
                        "coreCommit": "a" * 40,
                    }
                ),
                encoding="utf-8",
            )
            (project / f".chaos-engine-hosts.active-{'c' * 64}").write_bytes(b"")
            manifest = {
                "source": {"commit": "b" * 40},
                "distribution": {"id": "repository", "policySha256": "0" * 64},
            }
            with mock.patch.object(
                install, "try_verify_install", return_value=manifest
            ), mock.patch.object(
                install, "peek_host_token", return_value="c" * 64
            ), mock.patch.object(
                install, "load_installed_controller"
            ) as loaded:
                self.assertTrue(install.upgrade_host_receipt_drift_needed(project))
                self.assertTrue(install.wiped_runtime_recovery_needed(project))
                moved = install.quarantine_orphaned_host_receipt(project)
            loaded.assert_not_called()
            self.assertIsNotNone(moved)
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())
            self.assertFalse(
                (project / f".chaos-engine-hosts.active-{'c' * 64}").exists()
            )
            self.assertTrue(
                (project / ".chaos-engine-state" / "orphaned-hosts-receipt.json").exists()
            )

    def test_token_mismatch_with_deps_quarantines_without_receipt_commit_check(self):
        install = load(INSTALL, "chaos_engine_install_5633_token")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "phase": "installed",
                        "coreCommit": "b" * 40,
                    }
                ),
                encoding="utf-8",
            )
            (project / f".chaos-engine-hosts.active-{'d' * 64}").write_bytes(b"")
            manifest = {
                "source": {"commit": "b" * 40},
                "distribution": {"id": "repository", "policySha256": "0" * 64},
            }
            with mock.patch.object(
                install, "try_verify_install", return_value=manifest
            ), mock.patch.object(
                install, "peek_host_token", return_value="c" * 64
            ):
                self.assertTrue(install.upgrade_host_receipt_drift_needed(project))
                moved = install.quarantine_orphaned_host_receipt(project)
            self.assertIsNotNone(moved)
            self.assertFalse(
                (project / f".chaos-engine-hosts.active-{'d' * 64}").exists()
            )

    def test_healthy_matching_deps_core_not_quarantined(self):
        install = load(INSTALL, "chaos_engine_install_5633_healthy")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "phase": "installed",
                        "coreCommit": "b" * 40,
                    }
                ),
                encoding="utf-8",
            )
            token = "c" * 64
            (project / f".chaos-engine-hosts.active-{token}").write_bytes(b"")
            manifest = {
                "source": {"commit": "b" * 40},
                "distribution": {"id": "repository", "policySha256": "0" * 64},
            }
            healthy = mock.Mock(return_value={"status": "healthy"})
            controller = SimpleNamespace(verify=healthy)
            with mock.patch.object(
                install, "try_verify_install", return_value=manifest
            ), mock.patch.object(
                install, "peek_host_token", return_value=token
            ), mock.patch.object(
                install, "load_installed_controller", return_value=controller
            ):
                self.assertFalse(install.upgrade_host_receipt_drift_needed(project))
                self.assertIsNone(install.quarantine_orphaned_host_receipt(project))
            self.assertTrue((project / ".chaos-engine-hosts.json").exists())
            self.assertTrue((project / f".chaos-engine-hosts.active-{token}").exists())

    def test_adapter_verify_drift_with_deps_quarantines(self):
        install = load(INSTALL, "chaos_engine_install_5633_verify")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            (project / ".chaos-engine-hosts.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "phase": "installed",
                        "coreCommit": "b" * 40,
                    }
                ),
                encoding="utf-8",
            )
            token = "c" * 64
            (project / f".chaos-engine-hosts.active-{token}").write_bytes(b"")
            manifest = {
                "source": {"commit": "b" * 40},
                "distribution": {"id": "repository", "policySha256": "0" * 64},
            }

            def boom(project_arg, core_commit=None):
                del project_arg, core_commit
                raise ValueError(
                    f"ChaosEngine host adapter drift detected: {project / 'x'}"
                )

            controller = SimpleNamespace(verify=boom)
            with mock.patch.object(
                install, "try_verify_install", return_value=manifest
            ), mock.patch.object(
                install, "peek_host_token", return_value=token
            ), mock.patch.object(
                install, "load_installed_controller", return_value=controller
            ):
                self.assertTrue(install.upgrade_host_receipt_drift_needed(project))
                moved = install.quarantine_orphaned_host_receipt(project)
            self.assertIsNotNone(moved)
            self.assertFalse((project / ".chaos-engine-hosts.json").exists())

    def test_repair_hosts_heals_live_adapter_drift_preserving_foreign_mcp(self):
        install = load(INSTALL, "chaos_engine_install_5633_repair")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            project.mkdir()
            source = root / "chaos-engine-source"
            shutil.copytree(
                SOURCE, source, ignore=shutil.ignore_patterns("__pycache__", "*.pyc")
            )

            load_controller = install.load_dependency_controller

            def account_loader(installed_root: Path):
                return AccountDependencyController(load_controller(installed_root))

            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                install.install_with_dependencies(project, source, "1" * 40)

            mcp_path = project / ".mcp.json"
            before_mcp = (
                mcp_path.read_text(encoding="utf-8") if mcp_path.is_file() else "{}\n"
            )
            payload = json.loads(before_mcp) if before_mcp.strip() else {}
            if not isinstance(payload, dict):
                payload = {}
            servers = payload.setdefault("mcpServers", {})
            if not isinstance(servers, dict):
                servers = {}
                payload["mcpServers"] = servers
            servers["user-foreign-5633"] = {
                "command": "keep-me",
                "args": ["--foreign"],
            }
            mcp_path.write_text(
                json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            # Rebind once so receipt after images include the foreign MCP server.
            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                repaired = install.repair_component(project, "hosts")
            self.assertEqual("repaired", repaired["status"])

            marketplace = project / ".claude-plugin" / "marketplace.json"
            self.assertTrue(marketplace.is_file())
            drifted = json.loads(marketplace.read_text(encoding="utf-8"))
            drifted["name"] = "drifted-by-git-restore-5633"
            marketplace.write_text(
                json.dumps(drifted, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )

            self.assertTrue(install.upgrade_host_receipt_drift_needed(project))
            # Pre-heal repair path used to fail closed; must succeed now.
            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                result = install.repair_component(project, "hosts")
            self.assertEqual("repaired", result["status"])
            self.assertEqual("rebind", result["action"])
            self.assertFalse(install.upgrade_host_receipt_drift_needed(project))
            self.assertTrue((project / ".chaos-engine-hosts.json").exists())
            healed_mcp = json.loads(mcp_path.read_text(encoding="utf-8"))
            self.assertIn("user-foreign-5633", healed_mcp.get("mcpServers", {}))
            self.assertEqual(
                "keep-me",
                healed_mcp["mcpServers"]["user-foreign-5633"]["command"],
            )

    def test_reinstall_heals_drift_without_manual_quarantine(self):
        install = load(INSTALL, "chaos_engine_install_5633_reinstall")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            project.mkdir()
            source = root / "chaos-engine-source"
            shutil.copytree(
                SOURCE, source, ignore=shutil.ignore_patterns("__pycache__", "*.pyc")
            )
            load_controller = install.load_dependency_controller

            def account_loader(installed_root: Path):
                return AccountDependencyController(load_controller(installed_root))

            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                install.install_with_dependencies(project, source, "1" * 40)

            marketplace = project / ".claude-plugin" / "marketplace.json"
            drifted = json.loads(marketplace.read_text(encoding="utf-8"))
            drifted["name"] = "drifted-upgrade-5633"
            marketplace.write_text(
                json.dumps(drifted, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            self.assertTrue(install.upgrade_host_receipt_drift_needed(project))

            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                install.install_with_dependencies(project, source, "2" * 40)

            self.assertFalse(install.upgrade_host_receipt_drift_needed(project))
            receipt = json.loads(
                (project / ".chaos-engine-hosts.json").read_text(encoding="utf-8")
            )
            self.assertEqual("2" * 40, receipt.get("coreCommit"))




class OrphanCoreMissingHosts5636Test(unittest.TestCase):
    def test_recover_clears_account_journal_when_hosts_receipt_absent(self):
        install = load(INSTALL, "chaos_engine_install_5636_journal")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine"
            core.mkdir()
            (core / "manifest.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "source": {"commit": "b" * 40, "kind": "local"},
                        "distribution": {"id": "portable", "policySha256": "0" * 64},
                        "files": {},
                        "hostToken": "c" * 64,
                    }
                ),
                encoding="utf-8",
            )
            journal_dir = project / ".chaos-engine-account-rollback"
            journal_dir.mkdir()
            (journal_dir / "journal.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "desiredCommit": "a" * 40,
                        "priorCommit": "b" * 40,
                        "priorHostReceipt": None,
                        "priorAccountReceipt": None,
                        "priorMempalaceState": {
                            "before": {"exists": False, "files": {}},
                            "after": {"exists": False, "files": {}},
                        },
                        "integritySha256": "0" * 64,
                    }
                ),
                encoding="utf-8",
            )
            # Bypass integrity of journal via patching read
            pending = {
                "desiredCommit": "a" * 40,
                "priorCommit": "b" * 40,
                "priorHostReceipt": None,
                "priorAccountReceipt": None,
                "priorMempalaceState": {
                    "before": {"exists": False, "files": {}},
                    "after": {"exists": False, "files": {}},
                },
            }
            with mock.patch.object(
                install, "read_account_rollback_journal", return_value=pending
            ), mock.patch.object(
                install, "remove_account_rollback_journal"
            ) as removed, mock.patch.object(
                install, "try_verify_install", return_value={
                    "source": {"commit": "b" * 40},
                    "distribution": {"id": "portable", "policySha256": "0" * 64},
                }
            ):
                install.recover_account_rollback_journal(project)
            removed.assert_called_once_with(project)

    def test_orphan_core_gate_and_repair_writes_hosts_receipt(self):
        install = load(INSTALL, "chaos_engine_install_5636_repair")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            project.mkdir()
            source = root / "chaos-engine-source"
            shutil.copytree(
                SOURCE, source, ignore=shutil.ignore_patterns("__pycache__", "*.pyc")
            )
            load_controller = install.load_dependency_controller

            def account_loader(installed_root: Path):
                return AccountDependencyController(load_controller(installed_root))

            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ):
                install.install_with_dependencies(project, source, "1" * 40)

            # Simulate #5631 keep-core + lost hosts receipt + stale account journal.
            hosts = project / ".chaos-engine-hosts.json"
            self.assertTrue(hosts.is_file())
            hosts.unlink()
            for anchor in project.glob(".chaos-engine-hosts.active-*"):
                anchor.unlink()
            journal_dir = project / ".chaos-engine-account-rollback"
            journal_dir.mkdir(exist_ok=True)
            (journal_dir / "journal.json").write_text("{}", encoding="utf-8")
            self.assertTrue(install.orphan_core_without_hosts_receipt(project))

            with mock.patch.object(
                install, "load_dependency_controller", side_effect=account_loader
            ), mock.patch.object(
                install,
                "read_account_rollback_journal",
                return_value={
                    "desiredCommit": "2" * 40,
                    "priorCommit": "1" * 40,
                    "priorHostReceipt": None,
                    "priorAccountReceipt": None,
                    "priorMempalaceState": {
                        "before": {"exists": False, "files": {}},
                        "after": {"exists": False, "files": {}},
                    },
                },
            ):
                result = install.repair_component(project, "hosts")
            self.assertEqual("repaired", result["status"])
            self.assertTrue((project / ".chaos-engine-hosts.json").is_file())
            self.assertFalse(install.orphan_core_without_hosts_receipt(project))
            receipt = json.loads(
                (project / ".chaos-engine-hosts.json").read_text(encoding="utf-8")
            )
            self.assertEqual("installed", receipt.get("phase"))
            self.assertEqual("1" * 40, receipt.get("coreCommit"))


if __name__ == "__main__":
    unittest.main()
