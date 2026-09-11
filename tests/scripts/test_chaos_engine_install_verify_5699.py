"""Install-verify health vs host-environment advisory (#5699)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "1" * 40


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


INSTALL = load(SOURCE / "install.py", "ce_install_verify_5699")
POLICY = load(SOURCE / "mcp_policy.py", "ce_mcp_policy_5699")
BOOTSTRAP = load(SOURCE / "bootstrap.py", "ce_bootstrap_5699")


class AccountDependencyController:
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


class InstallVerifyHealth5699Tests(unittest.TestCase):
    def _install_portable(self, project: Path) -> None:
        load_controller = INSTALL.load_dependency_controller

        def load_account_controller(installed_root):
            return AccountDependencyController(load_controller(installed_root))

        with mock.patch.object(
            INSTALL, "load_dependency_controller", side_effect=load_account_controller
        ):
            INSTALL.install_with_dependencies(project, SOURCE, TEST_COMMIT)

    def _doctor_with_probes(
        self,
        project: Path,
        *,
        hook_healthy: bool,
        home: Path | None = None,
    ) -> dict:
        hosts = INSTALL.load_installed_controller(project / ".chaos-engine", "hosts")
        original_load = INSTALL.load_installed_controller
        env = {**os.environ}
        if home is not None:
            env["HOME"] = str(home)
            env["USERPROFILE"] = str(home)
            env["XDG_CONFIG_HOME"] = str(home / ".config")
            for key in (
                "CLAUDE_CONFIG",
                "CODEX_HOME",
                "GROK_HOME",
                "GEMINI_HOME",
                "COPILOT_HOME",
            ):
                env.pop(key, None)
        with mock.patch.dict(os.environ, env, clear=False), mock.patch.object(
            hosts, "retrieval_runtime_status", return_value={"status": "healthy"}
        ), mock.patch.object(
            hosts,
            "mcp_runtime_status",
            return_value={"status": "healthy"},
        ), mock.patch.object(
            hosts, "hook_runtime_healthy", return_value=hook_healthy
        ), mock.patch.object(
            hosts, "grok_runtime_status", return_value={"status": "not-detected"}
        ), mock.patch.object(
            INSTALL,
            "load_installed_controller",
            side_effect=lambda root, name: (
                hosts if name == "hosts" else original_load(root, name)
            ),
        ):
            return INSTALL.doctor_with_dependencies(project, verify_clients=False)

    def test_portable_verify_ignores_user_mcp_aliases_and_nonowned_hook_probe(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            home = root / "home"
            project.mkdir()
            home.mkdir()
            self._install_portable(project)

            (home / ".claude.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "github": {"command": "npx"},
                            "github-gh": {"command": "npx"},
                        }
                    }
                ),
                encoding="utf-8",
            )

            doctor = self._doctor_with_probes(
                project, hook_healthy=False, home=home
            )
            hooks = doctor["components"]["hooks"]
            mcps = doctor["components"]["mcps"]
            self.assertEqual("healthy", hooks["status"])
            self.assertEqual("healthy", mcps["status"])
            self.assertEqual(
                "sync-advisory",
                hooks.get("hostEnvironment", {}).get("status"),
            )
            self.assertNotEqual("recovery-required", mcps.get("status"))
            self.assertFalse(BOOTSTRAP._required_install_unhealthy(doctor))
            rendered = INSTALL.format_health_report(doctor, kind="doctor")
            self.assertIn("Prefer gh for GitHub", POLICY.HEAL_PROMPT)
            self.assertNotIn("recovery-required", rendered.lower() or "healthy")
            self.assertTrue((project / ".chaos-engine/hooks/guard.py").is_file())
            self.assertTrue((project / ".mcp.json").is_file())

    def test_missing_owned_hook_or_project_mcp_still_fails_verify(self):
        missing_hook = {
            "status": "recovery-required",
            "commit": TEST_COMMIT,
            "kernel": {"status": "healthy"},
            "hosts": {"status": "healthy"},
            "dependencies": {"status": "healthy"},
            "components": {
                "hooks": {"status": "absent", "taskImpact": "required"},
                "mcps": {"status": "healthy", "taskImpact": "required"},
            },
        }
        missing_mcp = {
            "status": "recovery-required",
            "commit": TEST_COMMIT,
            "kernel": {"status": "healthy"},
            "hosts": {"status": "healthy"},
            "dependencies": {"status": "healthy"},
            "components": {
                "hooks": {"status": "healthy", "taskImpact": "required"},
                "mcps": {"status": "absent", "taskImpact": "required"},
            },
        }
        self.assertTrue(BOOTSTRAP._required_install_unhealthy(missing_hook))
        self.assertTrue(BOOTSTRAP._required_install_unhealthy(missing_mcp))

    def test_project_level_github_mcp_is_left_unchanged_and_does_not_fail_doctor(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "github": {"command": "npx"},
                            "github-gh": {"command": "npx"},
                            "maven-tools-mcp": {"command": "java"},
                        }
                    }
                ),
                encoding="utf-8",
            )
            self.assertIsNone(POLICY.project_mcp_policy_error(project))

            doctor = {
                "status": "healthy",
                "components": {
                    "mcps": {"status": "healthy", "taskImpact": "required"},
                },
            }
            INSTALL.apply_mcp_policy_doctor(doctor, doctor["components"], project, POLICY)
            mcps = doctor["components"]["mcps"]
            self.assertEqual("healthy", mcps["status"])
            self.assertEqual("healthy", doctor["status"])
            self.assertFalse(BOOTSTRAP._required_install_unhealthy(doctor))


if __name__ == "__main__":
    unittest.main()
