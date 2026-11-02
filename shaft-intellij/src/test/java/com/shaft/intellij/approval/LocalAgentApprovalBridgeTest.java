package com.shaft.intellij.approval;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises {@link LocalAgentApprovalBridge}'s real HTTP endpoint with a real loopback {@link
 * HttpClient}, covering exactly the minimal MCP-over-HTTP contract validated against the actual
 * {@code claude} CLI: no session-id tracking, a single synchronous JSON response per request,
 * {@code 202} for notifications, {@code 405} for GET.
 */
class LocalAgentApprovalBridgeTest {

    @Test
    void initializeEchoesRequestedProtocolVersionAndAdvertisesToolsCapability() throws Exception {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        try {
            JsonObject params = new JsonObject();
            params.addProperty("protocolVersion", "2099-01-01");
            JsonObject request = jsonRpc(1, "initialize", params);

            JsonObject result = call(bridge, request);

            assertEquals("2099-01-01", result.get("protocolVersion").getAsString());
            assertTrue(result.getAsJsonObject("capabilities").has("tools"));
        } finally {
            bridge.close();
        }
    }

    @Test
    void toolsListReturnsTheApprovalPromptTool() throws Exception {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        try {
            JsonObject result = call(bridge, jsonRpc(2, "tools/list", new JsonObject()));

            JsonObject tool = result.getAsJsonArray("tools").get(0).getAsJsonObject();
            assertEquals(LocalAgentApprovalBridge.TOOL_NAME, tool.get("name").getAsString());
        } finally {
            bridge.close();
        }
    }

    @Test
    void toolsCallAllowReturnsAllowBehaviorWithEchoedInput() throws Exception {
        LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(
                (toolName, input) -> CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow()),
                Duration.ofSeconds(5));
        try {
            JsonObject input = new JsonObject();
            input.addProperty("file_path", "a.txt");
            JsonObject payload = callToolsCall(bridge, "Write", input);

            assertEquals("allow", payload.get("behavior").getAsString());
            assertEquals("a.txt", payload.getAsJsonObject("updatedInput").get("file_path").getAsString());
        } finally {
            bridge.close();
        }
    }

    @Test
    void toolsCallDenyReturnsDenyBehaviorWithMessage() throws Exception {
        LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(
                (toolName, input) -> CompletableFuture.completedFuture(
                        LocalAgentApprovalBridge.Decision.deny("The user denied this tool call.")),
                Duration.ofSeconds(5));
        try {
            JsonObject payload = callToolsCall(bridge, "Bash", new JsonObject());

            assertEquals("deny", payload.get("behavior").getAsString());
            assertEquals("The user denied this tool call.", payload.get("message").getAsString());
        } finally {
            bridge.close();
        }
    }

    @Test
    void toolsCallForwardsTheRequestedToolNameToTheHandler() throws Exception {
        AtomicReference<String> receivedToolName = new AtomicReference<>();
        LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(
                (toolName, input) -> {
                    receivedToolName.set(toolName);
                    return CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow());
                },
                Duration.ofSeconds(5));
        try {
            callToolsCall(bridge, "Bash", new JsonObject());
            assertEquals("Bash", receivedToolName.get());
        } finally {
            bridge.close();
        }
    }

    @Test
    void notificationWithNoIdReceivesA202WithNoBody() throws Exception {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        try {
            JsonObject notification = new JsonObject();
            notification.addProperty("jsonrpc", "2.0");
            notification.addProperty("method", "notifications/initialized");

            HttpResponse<String> response = post(bridge, notification.toString());

            assertEquals(202, response.statusCode());
            assertTrue(response.body() == null || response.body().isEmpty());
        } finally {
            bridge.close();
        }
    }

    @Test
    void getRequestsAreRejectedWith405() throws Exception {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        try {
            HttpClient client = HttpClient.newHttpClient();
            HttpRequest request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + bridge.port() + "/mcp"))
                    .GET()
                    .timeout(Duration.ofSeconds(5))
                    .build();

            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            assertEquals(405, response.statusCode());
        } finally {
            bridge.close();
        }
    }

    @Test
    void closeDeniesAPendingInFlightApprovalRequest() throws Exception {
        CountDownLatch requested = new CountDownLatch(1);
        CompletableFuture<LocalAgentApprovalBridge.Decision> neverAnswered = new CompletableFuture<>();
        LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(
                (toolName, input) -> {
                    requested.countDown();
                    return neverAnswered;
                },
                Duration.ofSeconds(10));
        ExecutorService client = Executors.newSingleThreadExecutor();
        try {
            Future<JsonObject> inFlight = client.submit(() -> callToolsCall(bridge, "Write", new JsonObject()));
            assertTrue(requested.await(5, TimeUnit.SECONDS), "Expected the handler to be invoked");

            bridge.close();

            JsonObject payload = inFlight.get(5, TimeUnit.SECONDS);
            assertEquals("deny", payload.get("behavior").getAsString());
        } finally {
            client.shutdownNow();
            bridge.close();
        }
    }

    @Test
    void closeDeletesTheScratchConfigFileAndIsIdempotent() {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        Path configFile = Path.of(bridge.mcpConfigArgument());
        assertTrue(Files.exists(configFile), "Expected the scratch mcp-config file to exist while running");

        bridge.close();
        bridge.close();

        assertFalse(Files.exists(configFile), "Expected the scratch mcp-config file to be deleted on close");
        assertTrue(bridge.isClosed());
    }

    @Test
    void handlerTimeoutResultsInADenyDecision() throws Exception {
        LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(
                (toolName, input) -> new CompletableFuture<>(), // never completes
                Duration.ofMillis(200));
        try {
            JsonObject payload = callToolsCall(bridge, "Bash", new JsonObject());
            assertEquals("deny", payload.get("behavior").getAsString());
        } finally {
            bridge.close();
        }
    }

    @Test
    void permissionPromptToolNameFollowsTheMcpDoubleUnderscoreConvention() {
        LocalAgentApprovalBridge bridge = allowAllBridge();
        try {
            assertEquals("mcp__shaft-approval__approval_prompt", bridge.permissionPromptToolName());
        } finally {
            bridge.close();
        }
    }

    private static LocalAgentApprovalBridge allowAllBridge() {
        try {
            return LocalAgentApprovalBridge.start(
                    (toolName, input) -> CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow()),
                    Duration.ofSeconds(5));
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }
    }

    private static JsonObject callToolsCall(LocalAgentApprovalBridge bridge, String toolName, JsonObject input)
            throws IOException, InterruptedException {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("tool_name", toolName);
        arguments.add("input", input);
        JsonObject params = new JsonObject();
        params.addProperty("name", LocalAgentApprovalBridge.TOOL_NAME);
        params.add("arguments", arguments);

        JsonObject result = call(bridge, jsonRpc(3, "tools/call", params));
        String text = result.getAsJsonArray("content").get(0).getAsJsonObject().get("text").getAsString();
        return JsonParser.parseString(text).getAsJsonObject();
    }

    private static JsonObject call(LocalAgentApprovalBridge bridge, JsonObject request)
            throws IOException, InterruptedException {
        HttpResponse<String> response = post(bridge, request.toString());
        assertEquals(200, response.statusCode());
        JsonElement parsed = JsonParser.parseString(response.body());
        return parsed.getAsJsonObject().getAsJsonObject("result");
    }

    private static HttpResponse<String> post(LocalAgentApprovalBridge bridge, String body)
            throws IOException, InterruptedException {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + bridge.port() + "/mcp"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .timeout(Duration.ofSeconds(10))
                .build();
        return client.send(request, HttpResponse.BodyHandlers.ofString());
    }

    private static JsonObject jsonRpc(int id, String method, JsonObject params) {
        JsonObject request = new JsonObject();
        request.addProperty("jsonrpc", "2.0");
        request.addProperty("id", id);
        request.addProperty("method", method);
        request.add("params", params);
        return request;
    }
}
