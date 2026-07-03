package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;

class ShaftPluginSecurityTest {
    @Test
    void pluginSourcesDoNotDownloadAndExecuteRemoteInstallerScripts() throws IOException {
        String source = sourceUnder(Path.of("src/main/java/com/shaft/intellij"));

        assertFalse(source.contains("ShaftMcpInstaller"));
        assertFalse(source.contains("installForPluginAndClient"));
        assertFalse(source.contains("raw.githubusercontent.com"));
        assertFalse(source.contains("install-shaft-mcp.ps1"));
        assertFalse(source.contains("install-shaft-mcp.sh"));
        assertFalse(source.contains("SHAFT_MCP_INSTALLER_REF"));
        assertFalse(source.contains("Invoke-Expression"));
        assertFalse(source.contains("| iex"));
        assertFalse(source.contains("curl -fsSL"));
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
