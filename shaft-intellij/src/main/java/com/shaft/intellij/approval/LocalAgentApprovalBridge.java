package com.shaft.intellij.approval;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Hosts a minimal MCP-over-HTTP server, loopback-only, that a local Claude Code CLI subprocess can
 * be pointed at via {@code --permission-prompt-tool} to ask SHAFT for a real, interactive per-tool
 * approval decision mid-run, instead of the launch-time-only acceptEdits/plan choice.
 *
 * <p>The wire contract implemented here was validated empirically against the real {@code claude}
 * CLI (v2.1.201): a single {@code POST /mcp} JSON-RPC endpoint answering {@code initialize}, {@code
 * notifications/initialized}, {@code tools/list}, and {@code tools/call} is sufficient -- no {@code
 * Mcp-Session-Id} tracking or SSE streaming is required, since the CLI never requested a session id
 * in testing and every exchange here is a single synchronous request/response. A stray {@code GET}
 * to the endpoint (the CLI probing for a server-push channel this server doesn't offer) is answered
 * {@code 405} and does not affect the rest of the exchange. The CLI's own built-in safety classifier
 * decides which tool calls are obviously safe enough to auto-allow without ever reaching this
 * server -- only genuinely mutating/risky calls trigger a {@code tools/call} here, matching what an
 * interactive user would see.
 */
public final class LocalAgentApprovalBridge implements AutoCloseable {

    public static final String SERVER_NAME = "shaft-approval";
    public static final String TOOL_NAME = "approval_prompt";

    private static final String CONTEXT_PATH = "/mcp";
    private static final String FALLBACK_PROTOCOL_VERSION = "2025-03-26";

    /**
     * A user's answer to a mid-run tool-approval request, translated into the {@code
     * behavior:allow|deny} JSON contract the CLI's permission-prompt-tool expects.
     */
    public record Decision(boolean allowed, String message) {
        public static Decision allow() {
            return new Decision(true, null);
        }

        public static Decision deny(String message) {
            return new Decision(false, message);
        }
    }

    /**
     * Resolves a pending approval request to a {@link Decision}. Implementations are called on one
     * of the bridge's own HTTP-handling threads, never the caller's EDT/UI thread -- implementations
     * that need to show UI must marshal onto the appropriate thread themselves and return a future
     * that completes once the user (or a timeout) decides.
     */
    @FunctionalInterface
    public interface ApprovalRequestHandler {
        CompletableFuture<Decision> requestApproval(String toolName, JsonObject toolInput);
    }

    private final HttpServer server;
    private final ExecutorService executor;
    private final Path mcpConfigFile;
    private final Duration decisionTimeout;
    private final ApprovalRequestHandler handler;
    private final Gson gson = new Gson();
    private final Set<CompletableFuture<Decision>> pending = ConcurrentHashMap.newKeySet();
    private final AtomicBoolean closed = new AtomicBoolean();
    private final AtomicInteger pendingCount = new AtomicInteger();
    private final AtomicLong accumulatedPendingMillis = new AtomicLong();
    private volatile Instant pendingSince;

    private LocalAgentApprovalBridge(
            HttpServer server,
            ExecutorService executor,
            Path mcpConfigFile,
            Duration decisionTimeout,
            ApprovalRequestHandler handler) {
        this.server = server;
        this.executor = executor;
        this.mcpConfigFile = mcpConfigFile;
        this.decisionTimeout = decisionTimeout;
        this.handler = handler;
    }

    /**
     * Starts the bridge: binds a loopback HTTP server on an ephemeral port, writes a scratch {@code
     * --mcp-config} JSON file pointing at it, and begins serving requests. Callers must {@link
     * #close()} the returned bridge once the local-agent run that needed it has finished, to avoid
     * leaking a listening socket and a temp file per run.
     */
    public static LocalAgentApprovalBridge start(ApprovalRequestHandler handler, Duration decisionTimeout)
            throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
        ExecutorService executor = Executors.newCachedThreadPool(LocalAgentApprovalBridge::newDaemonThread);
        Path mcpConfigFile = writeMcpConfig(server.getAddress().getPort());
        LocalAgentApprovalBridge bridge =
                new LocalAgentApprovalBridge(server, executor, mcpConfigFile, decisionTimeout, handler);
        server.createContext(CONTEXT_PATH, bridge::handle);
        server.setExecutor(executor);
        server.start();
        return bridge;
    }

    public int port() {
        return server.getAddress().getPort();
    }

    /** Path of the scratch {@code --mcp-config} JSON file to pass on the CLI command line. */
    public String mcpConfigArgument() {
        return mcpConfigFile.toString();
    }

    /** Value to pass as {@code --permission-prompt-tool}. */
    public String permissionPromptToolName() {
        return "mcp__" + SERVER_NAME + "__" + TOOL_NAME;
    }

    public boolean isClosed() {
        return closed.get();
    }

    /**
     * Idempotent teardown: denies every still-pending approval request (so a blocked HTTP-handler
     * thread never hangs forever), stops the HTTP server, shuts down its executor, and deletes the
     * scratch config file. Safe to call more than once and safe to call while a request is in flight.
     */
    @Override
    public void close() {
        if (!closed.compareAndSet(false, true)) {
            return;
        }
        for (CompletableFuture<Decision> future : pending) {
            future.complete(Decision.deny("SHAFT Assistant run ended before a decision was made."));
        }
        // A non-zero delay lets an exchange that was just unblocked by the deny-completion above finish
        // writing its HTTP response before the listening socket closes; stop() returns as soon as
        // outstanding exchanges finish rather than always waiting the full delay, so this only slows
        // down close() in the rare case where a response is still in flight.
        server.stop(2);
        executor.shutdownNow();
        try {
            Files.deleteIfExists(mcpConfigFile);
        } catch (IOException ignored) {
            // Best-effort cleanup of a scratch temp file; a leftover file in the OS temp directory is
            // harmless and will be cleared on the next reboot/temp-cleanup cycle.
        }
    }

    private void handle(HttpExchange exchange) throws IOException {
        try {
            if (!"POST".equalsIgnoreCase(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }
            JsonObject request = readRequest(exchange);
            if (request == null) {
                exchange.sendResponseHeaders(400, -1);
                return;
            }
            JsonElement id = request.get("id");
            if (id == null) {
                // A notification (e.g. notifications/initialized): acknowledged, no JSON-RPC reply.
                exchange.sendResponseHeaders(202, -1);
                return;
            }
            String method = stringField(request, "method");
            JsonObject response = switch (method == null ? "" : method) {
                case "initialize" -> initializeResult(request, id);
                case "tools/list" -> toolsListResult(id);
                case "tools/call" -> toolsCallResult(request, id);
                default -> errorResult(id, -32601, "Method not found: " + method);
            };
            writeJson(exchange, 200, response);
        } finally {
            exchange.close();
        }
    }

    private JsonObject initializeResult(JsonObject request, JsonElement id) {
        JsonObject params = objectField(request, "params");
        String protocolVersion = firstNonBlank(stringField(params, "protocolVersion"), FALLBACK_PROTOCOL_VERSION);
        JsonObject capabilities = new JsonObject();
        capabilities.add("tools", new JsonObject());
        JsonObject serverInfo = new JsonObject();
        serverInfo.addProperty("name", SERVER_NAME);
        serverInfo.addProperty("version", "1");
        JsonObject result = new JsonObject();
        result.addProperty("protocolVersion", protocolVersion);
        result.add("capabilities", capabilities);
        result.add("serverInfo", serverInfo);
        return envelope(id, result);
    }

    private JsonObject toolsListResult(JsonElement id) {
        JsonObject toolNameProperty = new JsonObject();
        toolNameProperty.addProperty("type", "string");
        JsonObject inputProperty = new JsonObject();
        inputProperty.addProperty("type", "object");
        JsonObject properties = new JsonObject();
        properties.add("tool_name", toolNameProperty);
        properties.add("input", inputProperty);
        JsonObject inputSchema = new JsonObject();
        inputSchema.addProperty("type", "object");
        inputSchema.add("properties", properties);

        JsonObject tool = new JsonObject();
        tool.addProperty("name", TOOL_NAME);
        tool.addProperty("description", "Ask the SHAFT Assistant user for approval to run a tool call.");
        tool.add("inputSchema", inputSchema);

        JsonArray tools = new JsonArray();
        tools.add(tool);
        JsonObject result = new JsonObject();
        result.add("tools", tools);
        return envelope(id, result);
    }

    private JsonObject toolsCallResult(JsonObject request, JsonElement id) {
        JsonObject params = objectField(request, "params");
        String calledTool = stringField(params, "name");
        if (!TOOL_NAME.equals(calledTool)) {
            return errorResult(id, -32601, "Unknown tool: " + calledTool);
        }
        JsonObject arguments = objectField(params, "arguments");
        String toolName = firstNonBlank(stringField(arguments, "tool_name"), "(unknown)");
        JsonObject input = objectField(arguments, "input");
        if (input == null) {
            input = new JsonObject();
        }

        Decision decision = awaitDecision(toolName, input);
        JsonObject payload = new JsonObject();
        if (decision.allowed()) {
            payload.addProperty("behavior", "allow");
            payload.add("updatedInput", input);
        } else {
            payload.addProperty("behavior", "deny");
            payload.addProperty("message", firstNonBlank(decision.message(), "Denied by the SHAFT user."));
        }
        JsonObject textBlock = new JsonObject();
        textBlock.addProperty("type", "text");
        textBlock.addProperty("text", gson.toJson(payload));
        JsonArray content = new JsonArray();
        content.add(textBlock);
        JsonObject result = new JsonObject();
        result.add("content", content);
        return envelope(id, result);
    }

    private Decision awaitDecision(String toolName, JsonObject input) {
        CompletableFuture<Decision> future = handler.requestApproval(toolName, input);
        pending.add(future);
        beginPendingWindow();
        try {
            return future.get(decisionTimeout.toMillis(), TimeUnit.MILLISECONDS);
        } catch (TimeoutException exception) {
            return Decision.deny("No user decision arrived before the run's timeout.");
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return Decision.deny("Interrupted while awaiting a user decision.");
        } catch (ExecutionException exception) {
            return Decision.deny("Approval failed: " + exception.getMessage());
        } finally {
            pending.remove(future);
            endPendingWindow();
        }
    }

    /**
     * Starts (or extends, if another request is already pending) a pending-approval window: only the
     * transition from zero to one concurrently pending requests records a new {@link #pendingSince},
     * so overlapping/parallel tool-approval requests don't double-count the same wall-clock interval.
     */
    private synchronized void beginPendingWindow() {
        if (pendingCount.getAndIncrement() == 0) {
            pendingSince = Instant.now();
        }
    }

    private synchronized void endPendingWindow() {
        if (pendingCount.decrementAndGet() == 0) {
            Instant since = pendingSince;
            pendingSince = null;
            if (since != null) {
                accumulatedPendingMillis.addAndGet(Duration.between(since, Instant.now()).toMillis());
            }
        }
    }

    /**
     * When a decision is currently pending (a tool-approval prompt is on screen awaiting the user),
     * the instant the current pending window began; {@code null} when nothing is pending right now.
     */
    public Instant pendingSince() {
        return pendingSince;
    }

    /**
     * Total wall-clock time spent awaiting user approval decisions so far during this bridge's
     * lifetime, including any decision that is still pending right now. Used by the runner to extend
     * its own run-timeout deadline so deliberation time isn't charged against the run budget.
     */
    public long accumulatedPendingMillis() {
        long total = accumulatedPendingMillis.get();
        Instant since = pendingSince;
        if (since != null) {
            total += Duration.between(since, Instant.now()).toMillis();
        }
        return total;
    }

    private JsonObject readRequest(HttpExchange exchange) {
        try (InputStream body = exchange.getRequestBody()) {
            String raw = new String(body.readAllBytes(), StandardCharsets.UTF_8);
            if (raw.isBlank()) {
                return null;
            }
            JsonElement parsed = JsonParser.parseString(raw);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (IOException | JsonParseException exception) {
            return null;
        }
    }

    private void writeJson(HttpExchange exchange, int status, JsonObject body) throws IOException {
        byte[] bytes = gson.toJson(body).getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream out = exchange.getResponseBody()) {
            out.write(bytes);
        }
    }

    private static JsonObject envelope(JsonElement id, JsonObject result) {
        JsonObject response = new JsonObject();
        response.addProperty("jsonrpc", "2.0");
        response.add("id", id);
        response.add("result", result);
        return response;
    }

    private static JsonObject errorResult(JsonElement id, int code, String message) {
        JsonObject error = new JsonObject();
        error.addProperty("code", code);
        error.addProperty("message", message);
        JsonObject response = new JsonObject();
        response.addProperty("jsonrpc", "2.0");
        response.add("id", id);
        response.add("error", error);
        return response;
    }

    private static String stringField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()
                ? value.getAsString()
                : null;
    }

    private static JsonObject objectField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonObject() ? value.getAsJsonObject() : null;
    }

    private static String firstNonBlank(String value, String fallback) {
        return value == null || value.isBlank() ? fallback : value;
    }

    private static Thread newDaemonThread(Runnable runnable) {
        Thread thread = new Thread(runnable, "shaft-local-agent-approval");
        thread.setDaemon(true);
        return thread;
    }

    private static Path writeMcpConfig(int port) throws IOException {
        JsonObject serverEntry = new JsonObject();
        serverEntry.addProperty("type", "http");
        serverEntry.addProperty("url", "http://127.0.0.1:" + port + CONTEXT_PATH);
        JsonObject servers = new JsonObject();
        servers.add(SERVER_NAME, serverEntry);
        JsonObject config = new JsonObject();
        config.add("mcpServers", servers);

        Path file = Files.createTempFile("shaft-approval-mcp-", ".json");
        Files.writeString(file, config.toString(), StandardCharsets.UTF_8);
        return file;
    }
}
