package com.shaft.intellij.mcp;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Minimal JSON-RPC MCP stdio client used by the IntelliJ shell.
 *
 * <p>The client is safe to keep open across many tool calls: a dedicated reader thread routes
 * responses to their callers by JSON-RPC id, so concurrent requests never consume each other's
 * responses. Long-lived server-side sessions (an active SHAFT Capture recording, an initialized
 * WebDriver) only survive while their server process is alive, so callers that start such
 * sessions must reuse one client instead of closing it after every call.</p>
 */
final class ShaftMcpStdioClient implements AutoCloseable {
    private static final Gson GSON = new Gson();
    private static final String PROTOCOL_VERSION = "2024-11-05";
    private static final Duration GRACEFUL_EXIT_WAIT = Duration.ofSeconds(5);

    private final Process process;
    private final BufferedReader input;
    private final OutputStream output;
    private final AtomicInteger id = new AtomicInteger(1);
    private final CompletableFuture<String> stderr;
    private final AtomicBoolean closed = new AtomicBoolean();
    private final ExecutorService ioExecutor;
    private final Map<Integer, CompletableFuture<JsonObject>> pendingRequests = new ConcurrentHashMap<>();
    private final Object initializationLock = new Object();
    private final Object sendLock = new Object();
    private volatile boolean initialized;

    ShaftMcpStdioClient(List<String> command, Path workingDirectory, Map<String, String> environment)
            throws IOException {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(workingDirectory.toFile());
        builder.environment().putAll(environment);
        process = builder.start();
        input = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
        output = process.getOutputStream();
        ioExecutor = Executors.newFixedThreadPool(2, daemonThreadFactory());
        stderr = CompletableFuture.supplyAsync(() -> readAll(process.getErrorStream()), ioExecutor);
        ioExecutor.execute(this::readResponses);
    }

    /**
     * Reports whether the server process is still running.
     *
     * @return true while the process is alive and the client has not been closed
     */
    boolean isAlive() {
        return !closed.get() && process.isAlive();
    }

    String initializeOnly(Duration timeout) throws IOException {
        initialize(timeout);
        return "SHAFT MCP connection is ready.";
    }

    JsonElement listTools(Duration timeout) throws IOException {
        initialize(timeout);
        int requestId = id.getAndIncrement();
        JsonObject request = request(requestId, "tools/list");
        return awaitResult(sendRequest(requestId, request), timeout);
    }

    JsonElement callTool(String toolName, JsonObject arguments, Duration timeout) throws IOException {
        initialize(timeout);
        int requestId = id.getAndIncrement();
        JsonObject request = request(requestId, "tools/call");
        JsonObject params = new JsonObject();
        params.addProperty("name", toolName);
        params.add("arguments", arguments);
        request.add("params", params);
        return awaitResult(sendRequest(requestId, request), timeout);
    }

    private void initialize(Duration timeout) throws IOException {
        synchronized (initializationLock) {
            if (initialized) {
                return;
            }
            int requestId = id.getAndIncrement();
            JsonObject request = request(requestId, "initialize");
            JsonObject params = new JsonObject();
            params.addProperty("protocolVersion", PROTOCOL_VERSION);
            params.add("capabilities", new JsonObject());
            JsonObject clientInfo = new JsonObject();
            clientInfo.addProperty("name", "shaft-intellij");
            clientInfo.addProperty("version", pluginVersion());
            params.add("clientInfo", clientInfo);
            request.add("params", params);
            awaitResult(sendRequest(requestId, request), timeout);

            JsonObject initializedNotification = new JsonObject();
            initializedNotification.addProperty("jsonrpc", "2.0");
            initializedNotification.addProperty("method", "notifications/initialized");
            send(initializedNotification);
            initialized = true;
        }
    }

    private static String pluginVersion() {
        try {
            String version = ResourceBundle.getBundle("messages.ShaftBundle").getString("shaft.plugin.version");
            return version == null || version.isBlank() ? "dev" : version;
        } catch (MissingResourceException exception) {
            return "dev";
        }
    }

    private static JsonObject request(int requestId, String method) {
        JsonObject request = new JsonObject();
        request.addProperty("jsonrpc", "2.0");
        request.addProperty("id", requestId);
        request.addProperty("method", method);
        return request;
    }

    private void send(JsonObject payload) throws IOException {
        byte[] body = GSON.toJson(payload).getBytes(StandardCharsets.UTF_8);
        synchronized (sendLock) {
            output.write(body);
            output.write('\n');
            output.flush();
        }
    }

    private PendingRequest sendRequest(int requestId, JsonObject request) throws IOException {
        CompletableFuture<JsonObject> response = new CompletableFuture<>();
        pendingRequests.put(requestId, response);
        try {
            send(request);
        } catch (IOException exception) {
            pendingRequests.remove(requestId);
            throw withDiagnostics("Failed to send SHAFT MCP request.", exception);
        } catch (RuntimeException exception) {
            pendingRequests.remove(requestId);
            throw exception;
        }
        return new PendingRequest(requestId, response);
    }

    private JsonElement awaitResult(PendingRequest pending, Duration timeout) throws IOException {
        try {
            JsonObject message = pending.response().get(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (message.has("error")) {
                throw withDiagnostics("SHAFT MCP returned an error response.",
                        new IOException(message.get("error").toString()));
            }
            JsonElement result = message.get("result");
            return result == null || result.isJsonNull() ? new JsonObject() : result;
        } catch (TimeoutException exception) {
            throw requestTimedOut();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw withDiagnostics("Interrupted while waiting for SHAFT MCP response.", exception);
        } catch (CancellationException exception) {
            throw exception;
        } catch (ExecutionException exception) {
            Throwable cause = exception.getCause();
            if (cause instanceof CancellationException cancellation) {
                throw cancellation;
            }
            throw withDiagnostics("Failed to read SHAFT MCP response.", cause == null ? exception : cause);
        } finally {
            pendingRequests.remove(pending.requestId());
        }
    }

    /**
     * Continuously reads JSON-RPC frames from the server and completes the matching pending
     * request. Runs on a dedicated daemon thread for the lifetime of the process.
     */
    private void readResponses() {
        try {
            String line;
            while ((line = input.readLine()) != null) {
                String trimmed = line.trim();
                if (trimmed.isEmpty()) {
                    continue;
                }
                dispatch(trimmed);
            }
        } catch (IOException ignored) {
            // Stream closed with the process; pending requests fail below.
        } finally {
            IOException processExited = withDiagnostics("SHAFT MCP process exited.", (Throwable) null,
                    failureDetails());
            pendingRequests.values().forEach(pending -> pending.completeExceptionally(processExited));
            pendingRequests.clear();
        }
    }

    private void dispatch(String line) {
        JsonObject message;
        try {
            JsonElement parsed = JsonParser.parseString(line);
            if (!parsed.isJsonObject()) {
                return;
            }
            message = parsed.getAsJsonObject();
        } catch (JsonSyntaxException | IllegalStateException exception) {
            // Spring Boot and tooling can write startup logs to stdout before MCP frames.
            // Ignore those non-protocol lines and keep waiting for JSON-RPC messages.
            return;
        }
        Integer requestId = messageId(message);
        if (requestId == null) {
            return;
        }
        CompletableFuture<JsonObject> pending = pendingRequests.remove(requestId);
        if (pending != null) {
            pending.complete(message);
        }
    }

    private static Integer messageId(JsonObject message) {
        if (!message.has("id")) {
            return null;
        }
        try {
            return message.get("id").getAsInt();
        } catch (IllegalStateException | ClassCastException | NumberFormatException
                 | UnsupportedOperationException exception) {
            return null;
        }
    }

    private IOException requestTimedOut() {
        return withDiagnostics("Timed out waiting for SHAFT MCP response.", (Throwable) null, failureDetails());
    }

    private IOException withDiagnostics(String message, Throwable cause) {
        return withDiagnostics(message, cause, failureDetails());
    }

    private IOException withDiagnostics(String message, Throwable cause, String details) {
        String composed = message;
        if (details != null && !details.isBlank()) {
            composed = message + "\n" + details;
        }
        return cause == null ? new IOException(composed) : new IOException(composed, cause);
    }

    private static String readAll(InputStream stream) {
        try {
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            return "";
        }
    }

    private String failureDetails() {
        StringBuilder details = new StringBuilder();
        Integer exitCode = exitCode();
        String stderrOutput = readStandardError();
        if (!stderrOutput.isBlank()) {
            details.append("stderr: ").append(stderrOutput.trim());
        }
        if (exitCode != null) {
            if (details.length() > 0) {
                details.append("; ");
            }
            details.append("process exit code: ").append(exitCode);
        }
        return details.toString();
    }

    private String readStandardError() {
        try {
            return stderr.get(250, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            return "";
        }
    }

    private Integer exitCode() {
        try {
            if (!process.waitFor(250, TimeUnit.MILLISECONDS)) {
                return null;
            }
            return process.exitValue();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return null;
        } catch (IllegalThreadStateException exception) {
            return null;
        }
    }

    @Override
    public void close() {
        close(false, true);
    }

    /**
     * Abandons every in-flight request without terminating the server process, so an active
     * server-side session (for example a live SHAFT Capture recording) keeps running.
     */
    void cancel() {
        pendingRequests.values().forEach(pending ->
                pending.completeExceptionally(new CancellationException("Operation cancelled")));
        pendingRequests.clear();
    }

    void kill() {
        close(true, false);
    }

    private void close(boolean force, boolean awaitExit) {
        if (!closed.compareAndSet(false, true)) {
            if (force) {
                process.destroyForcibly();
                ioExecutor.shutdownNow();
            }
            return;
        }
        cancel();
        if (force) {
            process.destroyForcibly();
        } else {
            // Closing stdin signals end-of-session to the stdio transport, letting the server
            // shut down gracefully and release live sessions (an active SHAFT Capture recording
            // quits its managed browser). Process.destroy() on Windows terminates immediately
            // and would orphan those sessions, so it is only the fallback.
            closeQuietly(output);
        }
        try {
            // A server that never completed the handshake has no live sessions to release, so a
            // long graceful wait would only delay probes of hung or misconfigured commands.
            long exitWaitMillis = initialized ? GRACEFUL_EXIT_WAIT.toMillis() : 500;
            if (awaitExit && !process.waitFor(exitWaitMillis, TimeUnit.MILLISECONDS)) {
                process.destroyForcibly();
            } else if (!awaitExit && !force) {
                process.destroy();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            process.destroyForcibly();
        } finally {
            ioExecutor.shutdownNow();
        }
    }

    private static void closeQuietly(OutputStream stream) {
        try {
            stream.close();
        } catch (IOException ignored) {
            // The process may already have exited.
        }
    }

    private static ThreadFactory daemonThreadFactory() {
        return runnable -> {
            Thread thread = new Thread(runnable, "shaft-mcp-stdio");
            thread.setDaemon(true);
            return thread;
        };
    }

    private record PendingRequest(int requestId, CompletableFuture<JsonObject> response) {
    }
}
