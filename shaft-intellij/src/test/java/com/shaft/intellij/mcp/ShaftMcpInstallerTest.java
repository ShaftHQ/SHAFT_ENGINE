package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpInstallerTest {
    @Test
    void runStreamsInstallerOutputToConsumer() {
        boolean isWindows = System.getProperty("os.name", "").toLowerCase().contains("win");
        List<String> command = isWindows
                ? List.of("cmd", "/c", "echo first & echo second")
                : List.of("sh", "-c", "printf 'first\\nsecond\\n'");

        List<String> lines = new java.util.ArrayList<>();
        ShaftMcpInstallResult result = ShaftMcpInstaller.run(command, false, lines::add);

        assertTrue(result.success());
        assertEquals(List.of("first", "second"), lines.stream().map(String::trim).toList());
    }

    @Test
    void installerJsonBuildsQuotedPluginCommand() {
        String command = ShaftMcpInstaller.commandLineFromJson("""
                installer banner
                {"client":"intellij-plugin","server":"shaft-mcp","version":"1.0.0","command":"C:\\\\Program Files\\\\Java\\\\bin\\\\java.exe","args":["@C:\\\\Users\\\\me\\\\shaft-mcp.args"],"userGuide":"https://shafthq.github.io/docs/agentic/mcp"}
                """);

        assertEquals("\"C:/Program Files/Java/bin/java.exe\" \"@C:/Users/me/shaft-mcp.args\"", command);
        assertEquals(List.of("C:/Program Files/Java/bin/java.exe", "@C:/Users/me/shaft-mcp.args"),
                ShaftCommandLine.parse(command));
    }

    @Test
    void commandParserPreservesWindowsBackslashes() {
        List<String> command = ShaftCommandLine.parse(
                "\"C:\\Program Files\\Java\\bin\\java.exe\" \"@C:\\Users\\me\\shaft-mcp.args\"");

        assertEquals(List.of("C:\\Program Files\\Java\\bin\\java.exe", "@C:\\Users\\me\\shaft-mcp.args"),
                command);
    }

    @Test
    void pluginInstallCommandRequestsJsonInstallerOutput() {
        String command = String.join(" ", ShaftMcpInstaller.installCommand("intellij-plugin", true));

        assertTrue(command.contains("intellij-plugin"));
        assertTrue(command.contains("json"));
    }

    @Test
    void installerUrlDefaultsToMainBranchScripts() {
        assertEquals("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp.ps1",
                ShaftMcpInstaller.installerUrl("install-shaft-mcp.ps1", "main"));
    }

    @Test
    void installerRefSanitizerFallsBackForBlankOrUnsafeRefs() {
        assertEquals("main", ShaftMcpInstaller.sanitizeInstallerRef(null));
        assertEquals("main", ShaftMcpInstaller.sanitizeInstallerRef("   "));
        assertEquals("codex/intellij-plugin-recording-flow",
                ShaftMcpInstaller.sanitizeInstallerRef(" codex/intellij-plugin-recording-flow "));
        assertEquals("main", ShaftMcpInstaller.sanitizeInstallerRef("feature;Invoke-Expression"));
    }

    @Test
    void selectedAgentInstallCommandCanReturnJsonForPluginSettings() {
        String command = String.join(" ", ShaftMcpInstaller.installCommand("codex", true));

        assertTrue(command.contains("codex"));
        assertTrue(command.contains("json"));
    }

    @Test
    void diagnosticInstallCommandUsesJsonInstallerCommand() {
        String command = ShaftMcpInstaller.diagnosticInstallCommand("copilot-intellij");

        assertTrue(command.contains("copilot-intellij"));
        assertTrue(command.contains("json"));
        assertTrue(command.contains(isWindows() ? "-Command \"irm " : "sh -c \"curl "));
    }

    @Test
    void genericClientConfigurationUsesExistingInstallerTargets() {
        String command = String.join(" ", ShaftMcpInstaller.installCommand("claude-desktop", false));

        assertTrue(command.contains("claude-desktop"));
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase().contains("win");
    }
}
