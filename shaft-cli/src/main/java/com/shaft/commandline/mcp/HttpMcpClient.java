package com.shaft.commandline.mcp;

import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Optional;

/**
 * MCP client speaking streamable HTTP to a persistent shaft-mcp daemon (session mode).
 *
 * <p>Observed transport contract (live-probed): {@code initialize} returns HTTP 200
 * {@code application/json} plus an {@code Mcp-Session-Id} response header that must be echoed on
 * every subsequent request; {@code notifications/initialized} returns 202; {@code tools/list} and
 * {@code tools/call} return HTTP 200 {@code text/event-stream} (SSE-framed). {@link #close()} is a
 * no-op: the daemon and its live browser state outlive the CLI invocation.
 */
public final class HttpMcpClient extends AbstractMcpClient {

    private final HttpClient httpClient;
    private final URI endpoint;
    private String sessionId;

    /**
     * @param endpoint the daemon MCP endpoint, e.g. {@code http://127.0.0.1:8081/mcp}
     */
    public HttpMcpClient(URI endpoint) {
        this.endpoint = endpoint;
        this.httpClient = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(10))
                .build();
    }

    @Override
    protected JsonNode rpc(ObjectNode request) {
        HttpResponse<String> response = send(request);
        int status = response.statusCode();
        if (status < 200 || status >= 300) {
            throw new McpException("HTTP " + status + " from " + endpoint + ": "
                    + truncate(response.body()));
        }
        captureSessionId(response);
        String contentType = response.headers().firstValue("Content-Type").orElse("");
        String body = response.body();
        String payload = contentType.contains("text/event-stream") ? SseParser.extractData(body) : body;
        if (payload.isBlank()) {
            throw new McpException("Empty response body from " + endpoint);
        }
        return Json.MAPPER.readTree(payload);
    }

    @Override
    protected void sendNotification(ObjectNode notification) {
        HttpResponse<String> response = send(notification);
        captureSessionId(response);
        int status = response.statusCode();
        if (status < 200 || status >= 300) {
            throw new McpException("HTTP " + status + " for notification "
                    + notification.path("method").asText("") + " from " + endpoint);
        }
    }

    private HttpResponse<String> send(ObjectNode message) {
        HttpRequest.Builder builder = HttpRequest.newBuilder(endpoint)
                .timeout(Duration.ofSeconds(120))
                .header("Content-Type", "application/json")
                .header("Accept", "application/json, text/event-stream")
                .POST(HttpRequest.BodyPublishers.ofString(Json.MAPPER.writeValueAsString(message),
                        StandardCharsets.UTF_8));
        if (sessionId != null) {
            builder.header("Mcp-Session-Id", sessionId);
        }
        try {
            return httpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
        } catch (java.io.IOException exception) {
            throw new McpException("Failed to reach shaft-mcp daemon at " + endpoint
                    + " (is it still running? try `shaft-cli session status`)", exception);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new McpException("Interrupted while calling shaft-mcp daemon at " + endpoint, exception);
        }
    }

    private void captureSessionId(HttpResponse<String> response) {
        Optional<String> header = response.headers().firstValue("Mcp-Session-Id");
        if (header.isPresent() && !header.get().isBlank()) {
            sessionId = header.get();
        }
    }

    private static String truncate(String body) {
        if (body == null) {
            return "";
        }
        return body.length() > 500 ? body.substring(0, 500) + "..." : body;
    }

    @Override
    public void close() {
        // Intentional no-op: the daemon persists across CLI invocations so browser/device state
        // survives. Only `shaft-cli session stop` terminates the daemon.
    }
}
