import importlib.util
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "ci" / "validate_installer_contract.py"
SPEC = importlib.util.spec_from_file_location("validate_installer_contract", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
SPEC.loader.exec_module(MODULE)


class ValidateInstallerContractTest(unittest.TestCase):
    def test_valid_contract_passes(self):
        """In-sync targets, Java mapping, and scripts should pass."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "scripts" / "mcp").mkdir(parents=True, exist_ok=True)
            (root / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui").mkdir(
                parents=True, exist_ok=True)

            # Write matching Python TARGETS
            py_installer = root / "scripts/mcp/install_shaft_mcp.py"
            py_installer.write_text(
                'TARGETS = ("codex", "claude", "claude-desktop", "copilot", "copilot-intellij", "intellij-plugin")\n',
                encoding="utf-8"
            )

            # Write matching Java installerArgumentFor
            java_setup = root / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"
            java_setup.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE",
                    "CLAUDE_DESKTOP",
                    "COPILOT_CLI",
                    "COPILOT_INTELLIJ",
                    "INTELLIJ_PLUGIN"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        case "CLAUDE_DESKTOP" -> "claude-desktop";
                        case "COPILOT_CLI" -> "copilot";
                        case "COPILOT_INTELLIJ" -> "copilot-intellij";
                        case "INTELLIJ_PLUGIN" -> "intellij-plugin";
                        default -> "codex";
                    };
                }
                ''',
                encoding="utf-8"
            )

            # Write installer scripts
            (root / "scripts/mcp/install-shaft-mcp.ps1").write_text("# PowerShell installer", encoding="utf-8")
            (root / "scripts/mcp/install-shaft-mcp.sh").write_text("#!/bin/bash\n# Shell installer", encoding="utf-8")

            # Write a minimal Java file referencing the installer command
            java_setup.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE",
                    "CLAUDE_DESKTOP",
                    "COPILOT_CLI",
                    "COPILOT_INTELLIJ",
                    "INTELLIJ_PLUGIN"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        case "CLAUDE_DESKTOP" -> "claude-desktop";
                        case "COPILOT_CLI" -> "copilot";
                        case "COPILOT_INTELLIJ" -> "copilot-intellij";
                        case "INTELLIJ_PLUGIN" -> "intellij-plugin";
                        default -> "codex";
                    };
                }

                private static String installerCommandFor(String target) {
                    String url = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main"
                            + "/scripts/mcp/install-shaft-mcp";
                    if (isWindows()) {
                        return "powershell ...";
                    }
                    return "curl ... " + url + ".sh";
                }
                ''',
                encoding="utf-8"
            )

            errors = MODULE.validate(root)
            self.assertEqual([], errors, f"Unexpected errors: {errors}")

    def test_target_missing_from_targets_fails(self):
        """A target in Java mapping but missing from TARGETS should fail."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "scripts" / "mcp").mkdir(parents=True, exist_ok=True)
            (root / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui").mkdir(
                parents=True, exist_ok=True)

            # Write Python TARGETS missing "extra-target"
            py_installer = root / "scripts/mcp/install_shaft_mcp.py"
            py_installer.write_text(
                'TARGETS = ("codex", "claude", "claude-desktop")\n',
                encoding="utf-8"
            )

            # Write Java with extra target that maps to something
            java_setup = root / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"
            java_setup.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE",
                    "CLAUDE_DESKTOP",
                    "COPILOT_CLI"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        case "CLAUDE_DESKTOP" -> "claude-desktop";
                        case "COPILOT_CLI" -> "copilot";
                        default -> "codex";
                    };
                }

                private static String installerCommandFor(String target) {
                    return "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp";
                }
                ''',
                encoding="utf-8"
            )

            # Write installer scripts
            (root / "scripts/mcp/install-shaft-mcp.ps1").write_text("# PowerShell", encoding="utf-8")
            (root / "scripts/mcp/install-shaft-mcp.sh").write_text("#!/bin/bash", encoding="utf-8")

            errors = MODULE.validate(root)
            self.assertTrue(any("copilot" in e.lower() and "missing from TARGETS" in e for e in errors),
                            f"Expected error about missing copilot target in TARGETS, got: {errors}")

    def test_target_missing_from_java_mapping_fails(self):
        """A target in TARGETS but missing from Java mapping should fail."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "scripts" / "mcp").mkdir(parents=True, exist_ok=True)
            (root / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui").mkdir(
                parents=True, exist_ok=True)

            # Write Python TARGETS with extra target
            py_installer = root / "scripts/mcp/install_shaft_mcp.py"
            py_installer.write_text(
                'TARGETS = ("codex", "claude", "missing-target")\n',
                encoding="utf-8"
            )

            # Write Java missing the mapping
            java_setup = root / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"
            java_setup.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        default -> "codex";
                    };
                }

                private static String installerCommandFor(String target) {
                    return "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp";
                }
                ''',
                encoding="utf-8"
            )

            # Write installer scripts
            (root / "scripts/mcp/install-shaft-mcp.ps1").write_text("# PowerShell", encoding="utf-8")
            (root / "scripts/mcp/install-shaft-mcp.sh").write_text("#!/bin/bash", encoding="utf-8")

            errors = MODULE.validate(root)
            self.assertTrue(any("missing-target" in e.lower() and "missing from installerArgumentFor" in e for e in errors),
                            f"Expected error about missing-target missing from Java mapping, got: {errors}")

    def test_missing_installer_script_fails(self):
        """Missing .ps1 or .sh installer script should fail."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "scripts" / "mcp").mkdir(parents=True, exist_ok=True)
            (root / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui").mkdir(
                parents=True, exist_ok=True)

            # Write valid matching Python and Java
            py_installer = root / "scripts/mcp/install_shaft_mcp.py"
            py_installer.write_text(
                'TARGETS = ("codex", "claude")\n',
                encoding="utf-8"
            )

            java_setup = root / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"
            java_setup.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        default -> "codex";
                    };
                }

                private static String installerCommandFor(String target) {
                    return "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp";
                }
                ''',
                encoding="utf-8"
            )

            # Only write .ps1, not .sh
            (root / "scripts/mcp/install-shaft-mcp.ps1").write_text("# PowerShell", encoding="utf-8")

            errors = MODULE.validate(root)
            self.assertTrue(any("install-shaft-mcp.sh" in e and "missing" in e.lower() for e in errors),
                            f"Expected error about missing .sh script, got: {errors}")

    def test_parse_targets_extracts_all_targets(self):
        """Parsing TARGETS should extract all quoted strings from tuple."""
        with tempfile.TemporaryDirectory() as tmp:
            py_file = Path(tmp) / "install.py"
            py_file.write_text(
                'TARGETS = ("target1", "target2", "target3")\n',
                encoding="utf-8"
            )

            targets = MODULE.parse_targets_from_python(py_file)
            self.assertEqual({"target1", "target2", "target3"}, targets)

    def test_parse_installer_targets_handles_default_arm(self):
        """Parsing Java should handle default -> "codex" arm without literal CODEX case."""
        with tempfile.TemporaryDirectory() as tmp:
            java_file = Path(tmp) / "Setup.java"
            java_file.write_text(
                '''
                private static final String[] INSTALLER_TARGETS = {
                    "CODEX",
                    "CLAUDE_CODE"
                };

                private static String installerArgumentFor(String target) {
                    return switch (normalize(target, "CODEX")) {
                        case "CLAUDE_CODE" -> "claude";
                        default -> "codex";
                    };
                }
                ''',
                encoding="utf-8"
            )

            mapping = MODULE.parse_installer_targets_from_java(java_file)
            # CODEX should be mapped to "codex" via the default arm
            self.assertEqual("codex", mapping.get("CODEX"))
            self.assertEqual("claude", mapping.get("CLAUDE_CODE"))


if __name__ == "__main__":
    unittest.main()
