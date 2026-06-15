package com.shaft.mcp.install;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

final class InstallerStdioProbe {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Duration RESPONSE_TIMEOUT = Duration.ofSeconds(45);

    private InstallerStdioProbe() {
        throw new IllegalStateException("Utility class");
    }

    static void verify(Path javaExecutable, Path jar) throws IOException {
        Process process = new ProcessBuilder(
                javaExecutable.toString(),
                "-jar",
                jar.toAbsolutePath().normalize().toString())
                .redirectError(ProcessBuilder.Redirect.DISCARD)
                .start();
        BlockingQueue<String> lines = new LinkedBlockingQueue<>();
        Thread readerThread = Thread.startVirtualThread(() -> {
            try (var reader = process.inputReader(StandardCharsets.UTF_8)) {
                String line;
                while ((line = reader.readLine()) != null) {
                    lines.put(line);
                }
            } catch (IOException ignored) {
                // Process shutdown closes the stream after the required responses are read.
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
            }
        });
        try (var writer = new BufferedWriter(
                new OutputStreamWriter(process.getOutputStream(), StandardCharsets.UTF_8))) {
            send(writer, """
                    {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"shaft-mcp-installer","version":"1.0"}}}
                    """);
            JsonNode initialize = await(lines, process, 1);
            if (!"shaft-mcp".equals(initialize.path("result").path("serverInfo").path("name").asText())) {
                throw new IOException("Installed JAR returned an unexpected MCP server identity.");
            }

            send(writer, """
                    {"jsonrpc":"2.0","method":"notifications/initialized","params":{}}
                    """);
            send(writer, """
                    {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
                    """);
            JsonNode tools = await(lines, process, 2);
            if (!tools.path("result").path("tools").isArray() || tools.path("result").path("tools").isEmpty()) {
                throw new IOException("Installed JAR returned no MCP tools.");
            }
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while probing the installed shaft-mcp JAR.", exception);
        } finally {
            process.destroy();
            try {
                if (!process.waitFor(5, TimeUnit.SECONDS)) {
                    process.destroyForcibly();
                    process.waitFor(5, TimeUnit.SECONDS);
                }
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                process.destroyForcibly();
            }
            try {
                readerThread.join(Duration.ofSeconds(5));
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private static void send(BufferedWriter writer, String message) throws IOException {
        writer.write(message.strip());
        writer.newLine();
        writer.flush();
    }

    private static JsonNode await(BlockingQueue<String> lines, Process process, int requestId)
            throws IOException, InterruptedException {
        long deadline = System.nanoTime() + RESPONSE_TIMEOUT.toNanos();
        while (System.nanoTime() < deadline) {
            String line = lines.poll(250, TimeUnit.MILLISECONDS);
            if (line != null) {
                JsonNode response;
                try {
                    response = JSON.readTree(line);
                } catch (IOException exception) {
                    throw new IOException("shaft-mcp wrote non-JSON data to stdout during the installer probe.",
                            exception);
                }
                if (response.path("id").asInt(-1) == requestId) {
                    if (response.has("error")) {
                        throw new IOException("shaft-mcp installer probe failed: " + response.path("error"));
                    }
                    return response;
                }
            } else if (!process.isAlive()) {
                throw new IOException("shaft-mcp exited before completing the installer probe.");
            }
        }
        throw new IOException("Timed out while probing the installed shaft-mcp JAR.");
    }
}
