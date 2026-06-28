package com.shaft.intellij.mcp;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

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
        clientInfo.addProperty("version", "10.2.20260627-beta.0");
        params.add("clientInfo", clientInfo);
        request.add("params", params);
        send(request);
        awaitResult(requestId, timeout);

        JsonObject initialized = new JsonObject();
        initialized.addProperty("jsonrpc", "2.0");
        initialized.addProperty("method", "notifications/initialized");
        send(initialized);
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
        while (System.nanoTime() < deadline) {
            JsonObject message = readMessage();
            if (message == null) {
                break;
            }
            if (!message.has("id") || message.get("id").getAsInt() != requestId) {
                continue;
            }
            if (message.has("error")) {
                throw new IOException(message.get("error").toString());
            }
            JsonElement result = message.get("result");
            return result == null || !result.isJsonObject() ? new JsonObject() : result.getAsJsonObject();
        }
        throw new IOException("Timed out waiting for SHAFT MCP response.");
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

    @Override
    public void close() {
        process.destroy();
        try {
            if (!process.waitFor(2, TimeUnit.SECONDS)) {
                process.destroyForcibly();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            process.destroyForcibly();
        }
        stderr.cancel(true);
    }
}
