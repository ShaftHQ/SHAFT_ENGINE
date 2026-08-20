"""Public SHAFT MCP/CLI/skills one-liners match the ChaosEngine install shape (#5093)."""

from pathlib import Path
from unittest import TestCase, main


ROOT = Path(__file__).resolve().parents[2]
WINDOWS = (
    'irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.ps1" | iex'
)
POSIX = (
    'curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.sh" | bash'
)


class ShaftOneLinerInstallerTest(TestCase):
    def test_documented_one_liners_install_into_the_current_directory(self):
        path = ROOT / "CONTRIBUTING.md"
        self.assertTrue(path.is_file(), path)
        document = path.read_text(encoding="utf-8")
        self.assertIn(WINDOWS, document)
        self.assertIn(POSIX, document)
        self.assertRegex(document, r"Change into the target project|cd into the target project")

    def test_public_scripts_fetch_the_current_installer_without_a_second_copy(self):
        powershell_path = ROOT / "scripts/mcp/install.ps1"
        shell_path = ROOT / "scripts/mcp/install.sh"
        self.assertTrue(powershell_path.is_file(), powershell_path)
        self.assertTrue(shell_path.is_file(), shell_path)
        powershell = powershell_path.read_text(encoding="utf-8")
        shell = shell_path.read_text(encoding="utf-8")
        self.assertIn("install-shaft-agentic-tools.ps1", powershell)
        self.assertIn("install-shaft-agentic-tools.sh", shell)
        self.assertIn("Invoke-WebRequest", powershell)
        self.assertIn("curl -fsSL", shell)
        for body in (powershell, shell):
            self.assertNotIn("python-build-standalone", body)
            self.assertNotIn("cpython-3.13", body)
            self.assertIn("install-shaft-agentic-tools", body)
            self.assertNotIn("install-shaft-mcp.ps1", body)
            self.assertNotIn("install-shaft-mcp.sh", body)


if __name__ == "__main__":
    main()
