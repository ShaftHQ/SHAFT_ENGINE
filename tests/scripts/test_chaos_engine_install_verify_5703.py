"""#5703: installer Python is a verify interpreter; dual hooks+mcps is not enough to fail."""

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


INSTALL = load(SOURCE / "install.py", "ce_install_verify_5703")
BOOTSTRAP = load(SOURCE / "bootstrap.py", "ce_bootstrap_verify_5703")


class InstallVerify5703Tests(unittest.TestCase):
    def test_resolve_managed_python_falls_back_to_installer_interpreter(self):
        resolved = INSTALL.resolve_managed_python(None, None, windows=True)
        self.assertEqual(Path(sys.executable).resolve(), resolved)

    def test_doctor_does_not_fail_verify_when_only_installer_python_exists(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            INSTALL.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            hosts = INSTALL.load_installed_controller(project / ".chaos-engine", "hosts")
            original_load = INSTALL.load_installed_controller
            with mock.patch.object(
                hosts, "retrieval_runtime_status", return_value={"status": "healthy"}
            ), mock.patch.object(
                hosts,
                "mcp_runtime_status",
                return_value={"status": "healthy"},
            ) as mcp_probe, mock.patch.object(
                hosts, "hook_runtime_healthy", return_value=True
            ) as hook_probe, mock.patch.object(
                INSTALL,
                "load_installed_controller",
                side_effect=lambda root, name: (
                    hosts if name == "hosts" else original_load(root, name)
                ),
            ):
                result = INSTALL.doctor_with_dependencies(project, verify_clients=False)

            hooks = result["components"]["hooks"]
            mcps = result["components"]["mcps"]
            self.assertNotEqual("managed-python-missing", hooks.get("detail"))
            self.assertNotEqual("managed-python-missing", mcps.get("detail"))
            self.assertNotEqual("recovery-required", hooks.get("status"))
            self.assertNotEqual("recovery-required", mcps.get("status"))
            self.assertFalse(BOOTSTRAP._component_blocks_health(hooks))
            self.assertFalse(BOOTSTRAP._component_blocks_health(mcps))
            self.assertEqual(
                str(Path(sys.executable).resolve()),
                str(mcp_probe.call_args.args[1]),
            )
            self.assertEqual(
                str(Path(sys.executable).resolve()),
                str(hook_probe.call_args.args[1]),
            )


if __name__ == "__main__":
    unittest.main()
