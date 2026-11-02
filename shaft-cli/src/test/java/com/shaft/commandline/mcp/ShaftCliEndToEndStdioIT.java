package com.shaft.commandline.mcp;

import com.shaft.commandline.testsupport.FakeMcpServer;
import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.node.ObjectNode;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * End-to-end exercise of the real stdio process path: spawns {@link FakeMcpServer} as an actual child
 * JVM via {@link StdioMcpClient#spawn} and drives the full handshake. This covers real process I/O and
 * stderr draining that the in-process pipe test cannot. Deterministic, headless, localhost-free.
 */
class ShaftCliEndToEndStdioIT {

    @Test
    void spawnsRealChildAndRoundTrips(@TempDir Path tempDir) throws IOException {
        List<String> command = command(System.getProperty("java.class.path"), tempDir);

        try (StdioMcpClient client = StdioMcpClient.spawn(command)) {
            assertEquals("shaft-mcp", client.initialize().serverName());

            List<String> names = client.listTools().stream().map(Tool::name).toList();
            assertTrue(names.contains(FakeMcpServer.ARBITRARY_TOOL));

            ObjectNode args = Json.newObject();
            args.put("value", "roundtrip");
            CallToolResult result = client.callTool(FakeMcpServer.ARBITRARY_TOOL, args);
            assertFalse(result.isError());
            assertTrue(result.text().contains("roundtrip"));
        }
    }

    /**
     * Regression for #3881: shaft-cli's own test classpath (now pulling shaft-capture, and with it
     * shaft-engine's whole dependency tree) brushes Windows' ~32,767-character CreateProcess limit; on
     * the {@code windows-2025} CI runner (longer {@code runneradmin} home path) it tips over, failing
     * with {@code CreateProcess error=206, The filename or extension is too long}. Pad the classpath
     * further here so the failure reproduces deterministically on any machine, regardless of username
     * length.
     */
    @Test
    void spawnsRealChildEvenWhenClasspathExceedsWindowsCommandLineLimit(@TempDir Path tempDir) throws IOException {
        String longClasspath = System.getProperty("java.class.path")
                + File.pathSeparator + "x".repeat(40_000);
        List<String> command = command(longClasspath, tempDir);

        try (StdioMcpClient client = StdioMcpClient.spawn(command)) {
            assertEquals("shaft-mcp", client.initialize().serverName());
        }
    }

    /**
     * Builds the child command as {@code [java, @argsFile]} rather than inlining {@code -cp
     * <classpath>}: an arbitrarily long classpath is safe in an {@code @argfile} but can blow past
     * Windows' CreateProcess command-line limit if passed directly (mirrors
     * {@code McpLauncherLocator#writeDevArgsFile}).
     */
    private static List<String> command(String classpath, Path tempDir) throws IOException {
        Path argsFile = tempDir.resolve("shaft-cli-it-classpath.args");
        String content = "-cp" + System.lineSeparator()
                + '"' + classpath.replace("\\", "/").replace("\"", "\\\"") + '"' + System.lineSeparator()
                + FakeMcpServer.class.getName() + System.lineSeparator();
        Files.writeString(argsFile, content, StandardCharsets.UTF_8);
        return List.of(javaExecutable(), "@" + argsFile);
    }

    private static String javaExecutable() {
        return ProcessHandle.current().info().command()
                .filter(command -> command.toLowerCase().contains("java"))
                .orElseGet(() -> {
                    String executable = System.getProperty("os.name").toLowerCase().contains("win")
                            ? "java.exe" : "java";
                    return Path.of(System.getProperty("java.home"), "bin", executable).toString();
                });
    }
}
