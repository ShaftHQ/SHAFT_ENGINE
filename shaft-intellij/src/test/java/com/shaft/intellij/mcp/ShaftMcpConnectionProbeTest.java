package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpConnectionProbeTest {
    @TempDir
    Path temporary;

    @Test
    void connectionProbeReportsEffectiveProjectWorkspace() throws Exception {
        Path project = temporary.resolve("project").toAbsolutePath().normalize();
        Files.createDirectories(project);
        String command = quote(javaExecutable()) + " -cp " + quote(System.getProperty("java.class.path"))
                + " " + FakeMcpServer.class.getName() + " stdoutNoiseThenToolsList";

        ShaftMcpToolResult result = ShaftMcpConnectionProbe.test(
                command,
                new ShaftSettingsState.Settings(),
                project).get(5, TimeUnit.SECONDS);

        String output = result.output();
        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(output.contains("MCP workspace: " + project)),
                () -> assertTrue(output.contains("user.dir: " + project)),
                () -> assertTrue(output.contains("shaft.mcp.workspaceRoot: " + project)),
                () -> assertTrue(output.contains("SHAFT_MCP_WORKSPACE_ROOT: " + project)),
                () -> assertFalse(output.contains("ShaftHQ/shaft-mcp/work")));
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return Paths.get(javaHome, "bin", windows ? "java.exe" : "java").toString();
    }

    private static String quote(String value) {
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }
}
