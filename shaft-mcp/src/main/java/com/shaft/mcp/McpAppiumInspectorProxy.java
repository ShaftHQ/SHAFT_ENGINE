package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
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
                    :root {
                      color-scheme: light dark;
                      --shaft-primary: #006ec0;
                      --shaft-primary-rgb: 0, 110, 192;
                      --shaft-deep: #102a31;
                      --shaft-deep-alt: #181f2a;
                      --shaft-muted: #c8d6e7;
                      --shaft-on-dark: #ffffff;
                      --shaft-bg: #f7f9fb;
                      --shaft-surface: #ffffff;
                      --shaft-text: #17202a;
                      --shaft-text-muted: #5f6f81;
                      --shaft-border: #d9e2ec;
                    }
                    * { box-sizing: border-box; }
                    html, body {
                      width: 100%;
                      min-width: 0;
                      height: 100%;
                      margin: 0;
                      overflow: hidden;
                      background: var(--shaft-bg);
                      color: var(--shaft-text);
                      font-family: "Segoe UI", system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
                      font-size: 13px;
                    }
                    body { display: grid; grid-template-rows: auto minmax(0, 1fr); }
                    header {
                      display: flex;
                      flex-wrap: wrap;
                      align-items: center;
                      gap: 8px;
                      min-width: 0;
                      padding: 8px 10px;
                      background: linear-gradient(135deg, var(--shaft-deep), var(--shaft-deep-alt));
                      color: var(--shaft-on-dark);
                      border-bottom: 1px solid rgba(var(--shaft-primary-rgb), .36);
                    }
                    .brand-mark {
                      width: 32px;
                      height: 32px;
                      display: inline-grid;
                      place-items: center;
                      flex: 0 0 auto;
                      border: 1px solid rgba(var(--shaft-primary-rgb), .42);
                      border-radius: 8px;
                      background: rgba(var(--shaft-primary-rgb), .18);
                      color: var(--shaft-on-dark);
                      font-weight: 700;
                    }
                    button {
                      min-width: 34px;
                      height: 32px;
                      border: 1px solid var(--shaft-border);
                      border-radius: 6px;
                      background: var(--shaft-surface);
                      color: var(--shaft-primary);
                      cursor: pointer;
                      font-weight: 700;
                    }
                    button:hover { background: rgba(var(--shaft-primary-rgb), .08); }
                    .spacer { flex: 1; }
                    .status-chip {
                      display: inline-flex;
                      align-items: center;
                      max-width: 100%;
                      min-height: 26px;
                      padding: 4px 9px;
                      border: 1px solid rgba(var(--shaft-primary-rgb), .36);
                      border-radius: 999px;
                      background: rgba(var(--shaft-primary-rgb), .16);
                      color: var(--shaft-on-dark);
                      font-size: 12px;
                      font-weight: 700;
                      overflow-wrap: anywhere;
                    }
                    details {
                      max-width: min(680px, 100%);
                      min-width: 0;
                      font-size: 12px;
                    }
                    summary { cursor: pointer; color: var(--shaft-muted); }
                    pre {
                      max-width: 100%;
                      max-height: 120px;
                      overflow-y: auto;
                      overflow-x: hidden;
                      margin: 4px 0 0;
                      padding: 8px;
                      border: 1px solid rgba(var(--shaft-primary-rgb), .24);
                      border-radius: 8px;
                      background: rgba(0, 0, 0, .32);
                      color: var(--shaft-on-dark);
                      white-space: pre-wrap;
                      overflow-wrap: anywhere;
                    }
                    iframe {
                      width: 100%;
                      min-width: 0;
                      height: 100%;
                      border: 0;
                      background: #fff;
                    }
                  </style>
                </head>
                <body>
                  <header>
                    <span class="brand-mark" aria-hidden="true">S</span>
                    <button id="pause" title="Pause recording" aria-label="Pause recording">||</button>
                    <button id="resume" title="Resume recording" aria-label="Resume recording">&gt;</button>
                    <button id="checkpoint" title="Add checkpoint" aria-label="Add checkpoint">*</button>
                    <button id="stop" title="Stop recording" aria-label="Stop recording">[]</button>
                    <button id="discard" title="Discard recording" aria-label="Discard recording">x</button>
                    <span class="status-chip" id="status">Recorder ready</span>
                    <span class="spacer"></span>
                    <details>
                      <summary>Capabilities</summary>
                      <pre>${CAPABILITIES}</pre>
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
                """.replace("${CAPABILITIES}", escapedCapabilities);
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
