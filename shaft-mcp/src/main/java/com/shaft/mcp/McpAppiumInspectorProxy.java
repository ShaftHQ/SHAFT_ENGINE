package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Local Appium Inspector wrapper and reverse proxy.
 */
final class McpAppiumInspectorProxy implements AutoCloseable {
    private final ObjectMapper mapper = new ObjectMapper();
    private final HttpClient client = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(15))
            .followRedirects(HttpClient.Redirect.NORMAL)
            .build();
    private final URI backend;
    private final McpAppiumCommandRecorder commandRecorder;
    private final Controller controller;
    private final String capabilitiesJson;
    private final ExecutorService executor;
    private HttpServer server;

    McpAppiumInspectorProxy(
            URI backend,
            McpAppiumCommandRecorder commandRecorder,
            Controller controller,
            String capabilitiesJson) {
        this.backend = backend;
        this.commandRecorder = commandRecorder;
        this.controller = controller;
        this.capabilitiesJson = capabilitiesJson == null ? "{}" : capabilitiesJson;
        this.executor = Executors.newCachedThreadPool(task -> {
            Thread thread = new Thread(task, "shaft-appium-inspector-proxy");
            thread.setDaemon(true);
            return thread;
        });
    }

    String start(int port) {
        try {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0);
            server.createContext("/shaft-inspector/control", this::handleControl);
            server.createContext("/shaft-inspector/status", this::handleStatus);
            server.createContext("/shaft-inspector", this::handleWrapper);
            server.createContext("/", this::handleProxy);
            server.setExecutor(executor);
            server.start();
            return "http://127.0.0.1:" + server.getAddress().getPort() + "/shaft-inspector";
        } catch (IOException exception) {
            throw new IllegalStateException("Appium Inspector proxy could not start.", exception);
        }
    }

    private void handleWrapper(HttpExchange exchange) throws IOException {
        if (!"GET".equalsIgnoreCase(exchange.getRequestMethod())) {
            send(exchange, 405, "text/plain", "Method not allowed");
            return;
        }
        send(exchange, 200, "text/html; charset=utf-8", wrapperHtml());
    }

    private void handleStatus(HttpExchange exchange) throws IOException {
        sendJson(exchange, controller.control("status", ""));
    }

    private void handleControl(HttpExchange exchange) throws IOException {
        if (!"POST".equalsIgnoreCase(exchange.getRequestMethod())) {
            send(exchange, 405, "text/plain", "Method not allowed");
            return;
        }
        JsonNode body = mapper.readTree(exchange.getRequestBody());
        String action = body.path("action").asText("status");
        String checkpoint = body.path("checkpoint").asText("");
        McpMobileInspectorRecordingStatus status = controller.control(action, checkpoint);
        sendJson(exchange, status);
        if ("stop".equalsIgnoreCase(action) || "discard".equalsIgnoreCase(action)) {
            CompletableFuture.runAsync(() -> {
                sleep();
                close();
            });
        }
    }

    private void handleProxy(HttpExchange exchange) throws IOException {
        byte[] requestBody = exchange.getRequestBody().readAllBytes();
        URI target = backend.resolve(exchange.getRequestURI().toString());
        HttpRequest.Builder request = HttpRequest.newBuilder(target)
                .timeout(Duration.ofMinutes(3))
                .method(exchange.getRequestMethod(), HttpRequest.BodyPublishers.ofByteArray(requestBody));
        copyRequestHeaders(exchange.getRequestHeaders(), request);
        try {
            HttpResponse<byte[]> response = client.send(request.build(), HttpResponse.BodyHandlers.ofByteArray());
            copyResponseHeaders(response.headers().map(), exchange.getResponseHeaders());
            exchange.sendResponseHeaders(response.statusCode(), response.body().length);
            exchange.getResponseBody().write(response.body());
            capture(exchange, requestBody, response);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            send(exchange, 502, "text/plain", "Appium proxy interrupted.");
        } catch (Exception exception) {
            send(exchange, 502, "text/plain", "Appium proxy failed: " + exception.getMessage());
        } finally {
            exchange.close();
        }
    }

    private void capture(HttpExchange exchange, byte[] requestBody, HttpResponse<byte[]> response) {
        String requestText = new String(requestBody, StandardCharsets.UTF_8);
        String responseText = new String(response.body(), StandardCharsets.UTF_8);
        commandRecorder.capture(
                exchange.getRequestMethod(),
                exchange.getRequestURI().getPath(),
                requestText,
                response.statusCode(),
                responseText);
    }

    private void copyRequestHeaders(Headers source, HttpRequest.Builder request) {
        for (Map.Entry<String, List<String>> entry : source.entrySet()) {
            String name = entry.getKey();
            if (skipRequestHeader(name)) {
                continue;
            }
            for (String value : entry.getValue()) {
                request.header(name, value);
            }
        }
    }

    private void copyResponseHeaders(Map<String, List<String>> source, Headers target) {
        for (Map.Entry<String, List<String>> entry : source.entrySet()) {
            String name = entry.getKey();
            if (skipResponseHeader(name)) {
                continue;
            }
            target.put(name, entry.getValue());
        }
    }

    private boolean skipRequestHeader(String name) {
        String normalized = name.toLowerCase(java.util.Locale.ROOT);
        return normalized.equals("host")
                || normalized.equals("content-length")
                || normalized.equals("connection")
                || normalized.equals("accept-encoding");
    }

    private boolean skipResponseHeader(String name) {
        String normalized = name.toLowerCase(java.util.Locale.ROOT);
        return normalized.equals("content-length")
                || normalized.equals("transfer-encoding")
                || normalized.equals("connection")
                || normalized.equals("content-encoding")
                || normalized.equals("x-frame-options")
                || normalized.equals("content-security-policy");
    }

    private void sendJson(HttpExchange exchange, Object value) throws IOException {
        send(exchange, 200, "application/json; charset=utf-8",
                mapper.writerWithDefaultPrettyPrinter().writeValueAsString(value));
    }

    private void send(HttpExchange exchange, int statusCode, String contentType, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", contentType);
        exchange.sendResponseHeaders(statusCode, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }

    private String wrapperHtml() {
        String escapedCapabilities = capabilitiesJson
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
        return """
                <!doctype html>
                <html lang="en">
                <head>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <title>SHAFT Appium Inspector Recorder</title>
                  <style>
                    :root { color-scheme: light dark; font-family: Inter, Segoe UI, Arial, sans-serif; }
                    body { margin: 0; height: 100vh; display: grid; grid-template-rows: auto 1fr; background: #101418; color: #f7f7f2; }
                    header { display: flex; align-items: center; gap: 8px; padding: 8px 10px; background: #1b2026; border-bottom: 1px solid #303840; }
                    button { min-width: 34px; height: 32px; border: 1px solid #55606b; border-radius: 6px; background: #252c33; color: #f7f7f2; cursor: pointer; }
                    button:hover { background: #303944; }
                    .spacer { flex: 1; }
                    .status { font-size: 12px; color: #cbd2d9; }
                    details { max-width: min(680px, 45vw); font-size: 12px; }
                    summary { cursor: pointer; color: #cbd2d9; }
                    pre { max-height: 120px; overflow: auto; margin: 4px 0 0; padding: 8px; background: #0b0e11; border: 1px solid #303840; border-radius: 6px; }
                    iframe { width: 100%; height: 100%; border: 0; background: #fff; }
                  </style>
                </head>
                <body>
                  <header>
                    <button id="pause" title="Pause recording" aria-label="Pause recording">||</button>
                    <button id="resume" title="Resume recording" aria-label="Resume recording">&gt;</button>
                    <button id="checkpoint" title="Add checkpoint" aria-label="Add checkpoint">*</button>
                    <button id="stop" title="Stop recording" aria-label="Stop recording">[]</button>
                    <button id="discard" title="Discard recording" aria-label="Discard recording">x</button>
                    <span class="status" id="status">Recorder ready</span>
                    <span class="spacer"></span>
                    <details>
                      <summary>Capabilities</summary>
                      <pre>%s</pre>
                    </details>
                  </header>
                  <iframe src="/inspector" title="Appium Inspector"></iframe>
                  <script>
                    async function control(action) {
                      const checkpoint = action === 'checkpoint' ? prompt('Checkpoint name', '') || '' : '';
                      const response = await fetch('/shaft-inspector/control', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({action, checkpoint})
                      });
                      const status = await response.json();
                      document.getElementById('status').textContent =
                        status.active ? (status.paused ? 'Paused' : 'Recording') + ' - ' + status.actionCount + ' actions'
                                      : 'Stopped - ' + status.actionCount + ' actions';
                    }
                    for (const action of ['pause', 'resume', 'checkpoint', 'stop', 'discard']) {
                      document.getElementById(action).addEventListener('click', () => control(action));
                    }
                    setInterval(async () => {
                      try {
                        const response = await fetch('/shaft-inspector/status');
                        const status = await response.json();
                        document.getElementById('status').textContent =
                          status.active ? (status.paused ? 'Paused' : 'Recording') + ' - ' + status.actionCount + ' actions'
                                        : 'Stopped - ' + status.actionCount + ' actions';
                      } catch {}
                    }, 2500);
                  </script>
                </body>
                </html>
                """.formatted(escapedCapabilities);
    }

    @Override
    public void close() {
        if (server != null) {
            server.stop(0);
        }
        executor.shutdownNow();
    }

    private static void sleep() {
        try {
            Thread.sleep(250);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    interface Controller {
        McpMobileInspectorRecordingStatus control(String action, String checkpoint);
    }
}
