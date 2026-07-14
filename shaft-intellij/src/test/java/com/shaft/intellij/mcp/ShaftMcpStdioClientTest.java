package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.time.Duration;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
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
        // Generous timeout on purpose: the server exits immediately, so the deterministic
        // process-exit path must win. A short timeout races JVM spawn under parallel suite load
        // and fails with empty diagnostics (#3491).
        IOException exception = assertThrows(IOException.class, () ->
                withClient("stderrAndExit", client -> client.initializeOnly(Duration.ofSeconds(30))));

        String message = exception.getMessage();
        assertTrue(message.contains("fake MCP failed during startup"));
        assertTrue(message.contains("process exit code"));
    }

    @Test
    void initializeIgnoresNonProtocolStdoutBeforeJsonRpcMessage() throws Exception {
        String result = withClient("stdoutNoiseThenToolsList",
                client -> client.initializeOnly(Duration.ofSeconds(2)));

        assertEquals("SHAFT MCP connection is ready.", result);
    }

    @Test
    void listToolsReturnsRawJsonResult() throws Exception {
        JsonElement result = withClient("toolsList", client -> client.listTools(Duration.ofSeconds(2)));

        assertEquals("fake_tool", result.getAsJsonObject()
                .getAsJsonArray("tools").get(0).getAsJsonObject().get("name").getAsString());
    }

    @Test
    void clientSurvivesAcrossSequentialToolCalls() throws Exception {
        withClient("echoTool", client -> {
            JsonElement first = client.callTool("capture_start", new JsonObject(), Duration.ofSeconds(5));
            assertTrue(client.isAlive(),
                    "The server process must stay alive between tool calls: session-starting tools "
                            + "(capture_start) keep their live session inside it.");
            JsonElement second = client.callTool("capture_status", new JsonObject(), Duration.ofSeconds(5));

            assertEquals("capture_start", first.getAsJsonObject().get("echoedTool").getAsString());
            assertEquals("capture_status", second.getAsJsonObject().get("echoedTool").getAsString());
            assertTrue(client.isAlive());
            return null;
        });
    }

    @Test
    void concurrentToolCallsReceiveTheirOwnResponses() throws Exception {
        withClient("echoTool", client -> {
            client.initializeOnly(Duration.ofSeconds(5));
            CompletableFuture<JsonElement> first = CompletableFuture.supplyAsync(() -> callQuietly(
                    client, "capture_status"));
            CompletableFuture<JsonElement> second = CompletableFuture.supplyAsync(() -> callQuietly(
                    client, "shaft_guide_search"));

            assertEquals("capture_status", first.join()
                    .getAsJsonObject().get("echoedTool").getAsString());
            assertEquals("shaft_guide_search", second.join()
                    .getAsJsonObject().get("echoedTool").getAsString());
            return null;
        });
    }

    @Test
    void cancelAbandonsInFlightRequestWithoutKillingTheServerProcess() throws Exception {
        withClient("silentToolCalls", client -> {
            client.initializeOnly(Duration.ofSeconds(5));
            CompletableFuture<Throwable> failure = CompletableFuture.supplyAsync(() -> {
                try {
                    client.callTool("never_answered", new JsonObject(), Duration.ofSeconds(30));
                    return null;
                } catch (IOException | RuntimeException exception) {
                    return exception;
                }
            });
            // The tool call registers asynchronously; keep cancelling until it has been abandoned.
            for (int attempt = 0; attempt < 100 && !failure.isDone(); attempt++) {
                client.cancel();
                Thread.sleep(50);
            }

            Throwable outcome = failure.join();
            assertTrue(outcome instanceof CancellationException,
                    "Cancel must abandon the pending request, got: " + outcome);
            assertTrue(client.isAlive(),
                    "Cancel must not terminate the shared server process; live sessions "
                            + "(an active recording) must keep running.");
            return null;
        });
    }

    private static JsonElement callQuietly(ShaftMcpStdioClient client, String toolName) {
        try {
            return client.callTool(toolName, new JsonObject(), Duration.ofSeconds(5));
        } catch (IOException exception) {
            throw new java.io.UncheckedIOException(exception);
        }
    }

    @Test
    void callToolPreservesNonObjectJsonResults() throws Exception {
        JsonElement array = withClient("toolResultArray",
                client -> client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(2)));
        JsonElement string = withClient("toolResultString",
                client -> client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(2)));

        assertEquals("first", array.getAsJsonArray().get(0).getAsString());
        assertEquals("plain text result", string.getAsString());
    }

    @Test
    void callToolStreamsProgressNotificationsToRegisteredCallback() throws Exception {
        withClient("progressStream", client -> {
            client.initializeOnly(Duration.ofSeconds(5));
            List<ShaftMcpProgress> updates = Collections.synchronizedList(new ArrayList<>());

            JsonElement result = client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(5), updates::add);

            assertTrue(result.getAsJsonObject().get("ok").getAsBoolean());
            // Only the frame carrying this call's own progressToken reaches the callback: the
            // unregistered-token frame and the unrelated notifications/message frame the fake
            // server also sends must both be silent no-ops.
            assertEquals(1, updates.size());
            assertEquals(0.5, updates.get(0).progress());
            assertEquals(1.0, updates.get(0).total());
            assertEquals("halfway there", updates.get(0).message());
            return null;
        });
    }

    @Test
    void callToolWithoutProgressCallbackIgnoresProgressNotifications() throws Exception {
        withClient("progressStream", client -> {
            client.initializeOnly(Duration.ofSeconds(5));

            // No callback: no progressToken is sent, so this exercises an id-less
            // notifications/progress frame for a token nobody registered, plus an unrelated id-less
            // notification, arriving on a call that never opted into progress at all. Neither may
            // throw or otherwise disrupt the normal tools/call response.
            JsonElement result = client.callTool("fake_tool", new JsonObject(), Duration.ofSeconds(5));

            assertTrue(result.getAsJsonObject().get("ok").getAsBoolean());
            return null;
        });
    }

    private interface ClientAction<T> {
        T call(ShaftMcpStdioClient client) throws Exception;
    }

    private static <T> T withClient(String mode, ClientAction<T> action) throws Exception {
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
