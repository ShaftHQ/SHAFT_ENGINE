package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpInstallerTest {
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
    void genericClientConfigurationUsesExistingInstallerTargets() {
        String command = String.join(" ", ShaftMcpInstaller.installCommand("claude-desktop", false));

        assertTrue(command.contains("claude-desktop"));
    }
}
