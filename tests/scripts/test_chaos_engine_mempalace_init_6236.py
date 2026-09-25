"""#6236: account-mode install and `repair --component mempalace` initialize MemPalace.

On a consumer repository (AhmedZoOoM/portfolio#39) doctor stayed
`recovery-required` / `mempalace-state` forever: account-mode install skipped
`initialize_mempalace_runtime`, repair never called it, and the doctor row did
not name a command that fixes it.
"""

from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess  # nosec B404 - git init on a temp fixture.
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path

from tests.scripts.test_chaos_engine_installer import (
    MODULE,
    REAL_REPAIR_COMPONENT,
    SOURCE,
    AccountDependencyController,
)

ROOT = Path(__file__).resolve().parents[2]
PALACE = ".chaos-engine-state/mempalace"


def load_hosts():
    spec = importlib.util.spec_from_file_location("ce_hosts_6236", SOURCE / "hosts.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class MemPalaceInitializationTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name).resolve() / "consumer"
        self.project.mkdir()
        # The portfolio#39 shape: a Git consumer repository, where the account
        # flow's shared palace is not the project palace doctor checks.
        subprocess.run(["git", "init", "-q", str(self.project)], check=True)  # nosec B603 B607
        self.hosts = load_hosts()
        load_controller = MODULE.load_dependency_controller
        patcher = mock.patch.object(
            MODULE,
            "load_dependency_controller",
            side_effect=lambda root: AccountDependencyController(load_controller(root)),
        )
        patcher.start()
        self.addCleanup(patcher.stop)

    def account_install(self):
        MODULE.install_with_dependencies(self.project, SOURCE, "1" * 40)

    def palace_status(self):
        return self.hosts.mempalace_directory_status(self.project / PALACE)["status"]

    def mcp_status(self):
        with mock.patch.dict(os.environ, {self.hosts.WITH_MCP_ENV: "1"}):
            return self.hosts.mcp_runtime_status(self.project, None, {})

    def repair(self):
        with mock.patch.object(MODULE, "_refresh_shared_store", return_value="skipped"), mock.patch.object(
            MODULE, "_install_store_schedule"
        ):
            return REAL_REPAIR_COMPONENT(self.project, "mempalace", runner=mock.Mock())

    def test_uninitialized_palace_is_named_with_a_working_fix_next(self):
        status = self.mcp_status()
        self.assertEqual("recovery-required", status["status"])
        self.assertEqual("mempalace-state", status["detail"])
        self.assertEqual("CE_MEMPALACE_UNINITIALIZED", status["code"])
        fix_next = MODULE.component_fix_next("mcps", {"status": "recovery-required", **status})
        self.assertIn("install.py repair --project . --component mempalace", fix_next)

    def test_account_mode_install_initializes_the_project_palace(self):
        self.account_install()
        self.assertEqual("healthy", self.palace_status())
        self.assertNotEqual("mempalace-state", self.mcp_status().get("detail"))

    def test_repair_initializes_a_missing_palace_and_doctor_leaves_mempalace_state(self):
        self.account_install()
        shutil.rmtree(self.project / PALACE, ignore_errors=True)
        self.assertEqual("mempalace-state", self.mcp_status().get("detail"))
        result = self.repair()
        self.assertEqual("repaired", result["status"])
        self.assertEqual("healthy", result["palace"])
        self.assertEqual("healthy", self.palace_status())
        self.assertNotEqual("mempalace-state", self.mcp_status().get("detail"))

    def test_repair_never_creates_a_checkout_palace_when_the_resolver_is_present(self):
        self.account_install()
        shutil.rmtree(self.project / PALACE, ignore_errors=True)
        resolver = self.project / "tools/repository-map/resolve_mempalace.py"
        resolver.parent.mkdir(parents=True)
        resolver.write_text("print('')\n", encoding="utf-8")
        self.repair()
        self.assertFalse((self.project / PALACE).exists())

    def test_account_mode_outside_git_leaves_the_palace_to_the_account_flow(self):
        plain = Path(self.temporary.name).resolve() / "plain"
        plain.mkdir()
        host = mock.Mock()
        controller = mock.Mock(mempalace_project_palace=lambda project: project / PALACE)
        self.assertFalse(MODULE.initialize_account_project_palace(plain, controller, host))
        host.initialize_mempalace_runtime.assert_not_called()
        shared = mock.Mock(mempalace_project_palace=lambda project: project / ".git/chaos-engine/mempalace")
        self.assertTrue(MODULE.initialize_account_project_palace(plain, shared, host))
        host.initialize_mempalace_runtime.assert_called_once_with(plain)

    def test_legacy_chroma_state_stays_recovery_required_without_the_init_code(self):
        palace = self.project / PALACE
        palace.mkdir(parents=True)
        (palace / "chroma.sqlite3").write_bytes(b"not sqlite")
        status = self.mcp_status()
        self.assertEqual("recovery-required", status["status"])
        self.assertNotIn("code", status)

    def test_opted_out_mcp_is_unchanged(self):
        with mock.patch.dict(os.environ, {self.hosts.WITH_MCP_ENV: "0"}):
            self.assertEqual("mcp-opted-out", self.hosts.mcp_runtime_status(self.project, None, {})["detail"])


if __name__ == "__main__":
    unittest.main()
