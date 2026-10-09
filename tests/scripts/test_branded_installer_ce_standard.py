"""SHAFT-branded installers follow the ChaosEngine installer standard (installer-program.md)."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load(relative: str, name: str):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


INSTALLERS = {
    "agentic": ("scripts/mcp/install_shaft_agentic_tools.py",
                ("scripts/mcp/install-shaft-agentic-tools.sh", "scripts/mcp/install-shaft-agentic-tools.ps1")),
    "upgrader": ("shaft-upgrader/upgrade_to_modular_shaft.py",
                 ("shaft-upgrader/upgrade.sh", "shaft-upgrader/upgrade.ps1")),
}


class BrandedInstallerCeStandardTest(unittest.TestCase):
    def test_standard_is_documented(self):
        text = (ROOT / "chaos-engine/references/installer-program.md").read_text(encoding="utf-8")
        self.assertIn("## Branded installers", text)

    def test_each_installer_ships_the_full_lifecycle(self):
        for key, (script, wrappers) in INSTALLERS.items():
            with self.subTest(installer=key):
                module = load(script, f"branded_installer_{key}")
                self.assertEqual(module.BRAND_NAME, "SHAFT Engine")
                commands = set(module.LIFECYCLE_COMMANDS)
                self.assertTrue({"status", "doctor", "rollback"} <= commands, commands)
                self.assertTrue(commands & {"install", "upgrade"})
                for helper in ("write_heal_handoff", "clear_heal_handoff", "heal_handoff_path", "doctor_report",
                               "read_receipt", "write_receipt"):
                    self.assertTrue(callable(getattr(module, helper, None)), f"{key} lacks {helper}")
                self.assertEqual(module.HEAL_HANDOFF_NAME, "heal-handoff.md")
                for wrapper in wrappers:
                    text = (ROOT / wrapper).read_text(encoding="utf-8")
                    self.assertIn("sys.version_info >= (3, 9)", text, wrapper)

    def test_agentic_installer_probes_live_host_configuration(self):
        module = load(INSTALLERS["agentic"][0], "branded_installer_agentic_live")
        for helper in ("host_config_drift", "remove_host_entry", "merge_with_previous_receipt"):
            self.assertTrue(callable(getattr(module, helper, None)), helper)


if __name__ == "__main__":
    unittest.main()
