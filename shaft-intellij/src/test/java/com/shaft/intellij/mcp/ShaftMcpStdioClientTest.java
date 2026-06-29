package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.time.Duration;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ShaftMcpStdioClientTest {
    @Test
    void initializeTimesOutForUnresponsiveServer() {
        long start = System.nanoTime();
        IOException exception = assertThrows(IOException.class, () ->
                withClient("hang", client -> client.initializeOnly(Duration.ofMillis(150))));
        long elapsed = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);

        assertTrue(exception.getMessage().contains("Timed out waiting for SHAFT MCP response."));
        assertTrue(elapsed < 2_500L);
    }

    @Test
    void initializeFailuresSurfaceStderrAndExitDiagnostics() {
        IOException exception = assertThrows(IOException.class, () ->
                withClient("stderrAndExit", client -> client.initializeOnly(Duration.ofMillis(300))));

        String message = exception.getMessage();
        assertTrue(message.contains("fake MCP failed during startup"));
        assertTrue(message.contains("process exit code"));
    }

    @Test
    void listToolsReturnsRawJsonResult() throws IOException {
        JsonElement result = withClient("toolsList", client -> client.listTools(Duration.ofSeconds(2)));

        assertEquals("fake_tool", result.getAsJsonObject()
                .getAsJsonArray("tools").get(0).getAsJsonObject().get("name").getAsString());
    }

    @Test
    void callToolPreservesNonObjectJsonResults() throws IOException {
        JsonElement array = withClient("toolResultArray",
                client -> client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(2)));
        JsonElement string = withClient("toolResultString",
                client -> client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(2)));

        assertEquals("first", array.getAsJsonArray().get(0).getAsString());
        assertEquals("plain text result", string.getAsString());
    }

    private interface ClientAction<T> {
        T call(ShaftMcpStdioClient client) throws IOException;
    }

    private static <T> T withClient(String mode, ClientAction<T> action) throws IOException {
        List<String> command = List.of(javaExecutable(), "-cp", System.getProperty("java.class.path"),
                FakeMcpServer.class.getName(), mode);
        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, Path.of("."), Map.of())) {
            return action.call(client);
        }
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return Paths.get(javaHome, "bin", windows ? "java.exe" : "java").toString();
    }
}
