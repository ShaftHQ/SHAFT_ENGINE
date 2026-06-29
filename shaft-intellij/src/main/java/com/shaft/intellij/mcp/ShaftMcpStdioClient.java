package com.shaft.intellij.mcp;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Minimal JSON-RPC MCP stdio client used by the IntelliJ shell.
 */
final class ShaftMcpStdioClient implements AutoCloseable {
    private static final Gson GSON = new Gson();
    private static final String PROTOCOL_VERSION = "2024-11-05";

    private final Process process;
    private final InputStream input;
    private final OutputStream output;
    private final AtomicInteger id = new AtomicInteger(1);
    private final CompletableFuture<String> stderr;
    private final AtomicBoolean closed = new AtomicBoolean();

    ShaftMcpStdioClient(List<String> command, Path workingDirectory, Map<String, String> environment)
            throws IOException {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(workingDirectory.toFile());
        builder.environment().putAll(environment);
        process = builder.start();
        input = process.getInputStream();
        output = process.getOutputStream();
        stderr = CompletableFuture.supplyAsync(() -> readAll(process.getErrorStream()));
    }

    String initializeOnly(Duration timeout) throws IOException {
        initialize(timeout);
        return "SHAFT MCP connection is ready.";
    }

    JsonObject listTools(Duration timeout) throws IOException {
        initialize(timeout);
        int requestId = id.getAndIncrement();
        JsonObject request = request(requestId, "tools/list");
        send(request);
        return awaitResult(requestId, timeout);
    }

    JsonObject callTool(String toolName, JsonObject arguments, Duration timeout) throws IOException {
        initialize(timeout);
        int requestId = id.getAndIncrement();
        JsonObject request = request(requestId, "tools/call");
        JsonObject params = new JsonObject();
        params.addProperty("name", toolName);
        params.add("arguments", arguments);
        request.add("params", params);
        send(request);
        return awaitResult(requestId, timeout);
    }

    private void initialize(Duration timeout) throws IOException {
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
        send(request);
        awaitResult(requestId, timeout);

        JsonObject initialized = new JsonObject();
        initialized.addProperty("jsonrpc", "2.0");
        initialized.addProperty("method", "notifications/initialized");
        send(initialized);
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
        byte[] header = ("Content-Length: " + body.length + "\r\n\r\n").getBytes(StandardCharsets.US_ASCII);
        output.write(header);
        output.write(body);
        output.flush();
    }

    private JsonObject awaitResult(int requestId, Duration timeout) throws IOException {
        long deadline = System.nanoTime() + timeout.toNanos();
        JsonObject response = null;
        boolean waiting = true;
        while (waiting && response == null && System.nanoTime() < deadline) {
            long timeoutNanos = deadline - System.nanoTime();
            JsonObject message = readMessage(timeoutNanos);
            if (message == null) {
                waiting = process.isAlive();
            } else if (message.has("id") && matchesId(message, requestId)) {
                if (message.has("error")) {
                    throw withDiagnostics("SHAFT MCP returned an error response.",
                            new IOException(message.get("error").toString()));
                }
                JsonElement result = message.get("result");
                response = result == null || !result.isJsonObject() ? new JsonObject() : result.getAsJsonObject();
            }
        }
        if (response != null) {
            return response;
        }
        throw requestTimedOut();
    }

    private boolean matchesId(JsonObject message, int requestId) {
        try {
            return message.get("id").getAsInt() == requestId;
        } catch (IllegalStateException | ClassCastException exception) {
            return false;
        }
    }

    private JsonObject readMessage(long timeoutNanos) throws IOException {
        if (timeoutNanos <= 0) {
            return null;
        }
        CompletableFuture<JsonObject> future = CompletableFuture.supplyAsync(() -> {
            try {
                return readMessage();
            } catch (IOException exception) {
                throw new UncheckedIOException(exception);
            }
        });
        long timeoutMillis = Math.max(1, TimeUnit.NANOSECONDS.toMillis(timeoutNanos));
        try {
            return future.get(timeoutMillis, TimeUnit.MILLISECONDS);
        } catch (TimeoutException exception) {
            future.cancel(true);
            close();
            throw requestTimedOut();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw withDiagnostics("Interrupted while waiting for SHAFT MCP response.", exception);
        } catch (ExecutionException exception) {
            if (exception.getCause() instanceof UncheckedIOException ioException) {
                throw withDiagnostics("Failed to read SHAFT MCP response.", ioException.getCause());
            }
            throw withDiagnostics("Failed to read SHAFT MCP response.", exception);
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

    private JsonObject readMessage() throws IOException {
        int contentLength = readContentLength();
        if (contentLength <= 0) {
            return null;
        }
        byte[] body = input.readNBytes(contentLength);
        if (body.length != contentLength) {
            return null;
        }
        return JsonParser.parseString(new String(body, StandardCharsets.UTF_8)).getAsJsonObject();
    }

    private int readContentLength() throws IOException {
        ByteArrayOutputStream header = new ByteArrayOutputStream();
        int previous = -1;
        int current;
        while ((current = input.read()) != -1) {
            header.write(current);
            String value = header.toString(StandardCharsets.US_ASCII);
            if ((previous == '\n' && current == '\n') || value.endsWith("\r\n\r\n")) {
                break;
            }
            previous = current;
        }
        String[] lines = header.toString(StandardCharsets.US_ASCII).split("\\r?\\n");
        for (String line : lines) {
            int separator = line.indexOf(':');
            if (separator > 0 && "content-length".equalsIgnoreCase(line.substring(0, separator).trim())) {
                return Integer.parseInt(line.substring(separator + 1).trim());
            }
        }
        return -1;
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
            return process.exitValue();
        } catch (IllegalThreadStateException exception) {
            return null;
        }
    }

    @Override
    public void close() {
        if (!closed.compareAndSet(false, true)) {
            return;
        }
        process.destroy();
        try {
            if (!process.waitFor(2, TimeUnit.SECONDS)) {
                process.destroyForcibly();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            process.destroyForcibly();
        }
    }

    void cancel() {
        close();
    }
}
