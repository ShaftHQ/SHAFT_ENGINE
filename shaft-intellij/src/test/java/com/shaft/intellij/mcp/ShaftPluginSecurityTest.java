package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginSecurityTest {
    @Test
    void pluginSourcesDoNotDownloadAndExecuteRemoteInstallerScripts() throws IOException {
        String source = sourceUnder(Path.of("src/main/java/com/shaft/intellij"));
        String setupPanel = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"));
        // Official Grok and Antigravity installers are clipboard text in SetupPrerequisites.
        // The plugin copies them into the IDE terminal; it does not download or run them.
        String executed = source.replace(Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/ui/SetupPrerequisites.java")), "");
        String prerequisites = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/ui/SetupPrerequisites.java"));

        assertFalse(source.contains("ShaftMcpInstaller"));
        assertFalse(source.contains("installForPluginAndClient"));
        assertFalse(source.contains("Invoke-Expression"));
        assertFalse(executed.contains("| iex"));
        assertFalse(executed.contains("curl -fsSL"));
        assertFalse(prerequisites.contains("ProcessBuilder"));
        assertFalse(prerequisites.contains("Runtime.getRuntime"));
        assertTrue(prerequisites.contains("curl -fsSL https://x.ai/cli/install.sh | bash"));
        assertTrue(prerequisites.contains("irm https://x.ai/cli/install.ps1 | iex"));
        assertTrue(setupPanel.contains("INSTALLER_BRANCH = \"main\""));
        assertFalse(setupPanel.contains("SHAFT_MCP_INSTALLER_REF"));
        assertFalse(setupPanel.contains("ProcessBuilder"));
        assertFalse(setupPanel.contains("Runtime.getRuntime"));
        assertFalse(setupPanel.contains("GeneralCommandLine"));
    }

    private static String sourceUnder(Path root) throws IOException {
        StringBuilder source = new StringBuilder();
        try (var paths = Files.walk(root)) {
            for (Path path : paths.filter(path -> path.toString().endsWith(".java")).toList()) {
                source.append(Files.readString(path)).append('\n');
            }
        }
        return source.toString();
    }
}
