package com.shaft.commandline.mcp;

import com.shaft.commandline.testsupport.FakeMcpServer;
import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.node.ObjectNode;

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
    void spawnsRealChildAndRoundTrips() {
        List<String> command = List.of(
                javaExecutable(),
                "-cp",
                System.getProperty("java.class.path"),
                FakeMcpServer.class.getName());

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
