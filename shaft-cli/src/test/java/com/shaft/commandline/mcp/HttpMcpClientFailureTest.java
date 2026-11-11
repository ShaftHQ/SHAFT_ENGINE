package com.shaft.commandline.mcp;

import com.shaft.commandline.util.Json;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Failure paths for {@link HttpMcpClient}: non-success HTTP, an empty body, a rejected
 * initialized notification, and a daemon that is not listening.
 */
class HttpMcpClientFailureTest {

    @Test
    void httpErrorIncludesStatusAndTruncatesALongBody() throws IOException {
        String body = "x".repeat(600);
        try (ServerClient started = clientReturning(503, body, "text/plain")) {
            McpException failure = assertThrows(McpException.class, started.client()::initialize);
            assertTrue(failure.getMessage().contains("HTTP 503"), failure.getMessage());
            assertTrue(failure.getMessage().endsWith("..."), failure.getMessage());
            assertFalse(failure.getMessage().contains("x".repeat(501)), failure.getMessage());
        }
    }

    @Test
    void emptySuccessBodyIsRejected() throws IOException {
        try (ServerClient started = clientReturning(200, "", "application/json")) {
            McpException failure = assertThrows(McpException.class, started.client()::initialize);
            assertTrue(failure.getMessage().contains("Empty response body"), failure.getMessage());
        }
    }

    @Test
    void failedInitializedNotificationReportsTheMethod() throws IOException {
        HttpServer failing = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        failing.createContext("/mcp", exchange -> {
            String request = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            String method = Json.MAPPER.readTree(request).path("method").asText("");
            if ("notifications/initialized".equals(method)) {
                byte[] payload = "nope".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(500, payload.length);
                exchange.getResponseBody().write(payload);
                exchange.close();
                return;
            }
            byte[] payload = initializePayload();
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, payload.length);
            exchange.getResponseBody().write(payload);
            exchange.close();
        });
        failing.start();
        try (HttpMcpClient client = new HttpMcpClient(endpoint(failing))) {
            McpException failure = assertThrows(McpException.class, client::initialize);
            assertTrue(failure.getMessage().contains("HTTP 500 for notification notifications/initialized"),
                    failure.getMessage());
        } finally {
            failing.stop(0);
        }
    }

    @Test
    void unreachableDaemonReportsTransportFailure() throws IOException {
        HttpServer closed = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        URI gone = endpoint(closed);
        closed.start();
        closed.stop(0);

        try (HttpMcpClient client = new HttpMcpClient(gone)) {
            McpException failure = assertThrows(McpException.class, client::initialize);
            assertTrue(failure.getMessage().contains("Failed to reach shaft-mcp daemon"), failure.getMessage());
            assertTrue(failure.getMessage().contains("shaft-cli session status"), failure.getMessage());
        }
    }

    private static ServerClient clientReturning(int status, String body, String contentType) throws IOException {
        HttpServer oneShot = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        oneShot.createContext("/mcp", exchange -> {
            byte[] payload = body.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", contentType);
            exchange.sendResponseHeaders(status, payload.length);
            exchange.getResponseBody().write(payload);
            exchange.close();
        });
        oneShot.start();
        return new ServerClient(oneShot, new HttpMcpClient(endpoint(oneShot)));
    }

    private record ServerClient(HttpServer server, HttpMcpClient client) implements AutoCloseable {
        @Override
        public void close() {
            client.close();
            server.stop(0);
        }
    }

    private static byte[] initializePayload() {
        ObjectNode response = Json.newObject();
        response.put("jsonrpc", "2.0");
        response.put("id", 1);
        ObjectNode result = Json.newObject();
        result.put("protocolVersion", "2025-03-26");
        ObjectNode serverInfo = Json.newObject();
        serverInfo.put("name", "shaft-mcp");
        serverInfo.put("version", "http-1.0");
        result.set("serverInfo", serverInfo);
        response.set("result", result);
        return Json.MAPPER.writeValueAsString(response).getBytes(StandardCharsets.UTF_8);
    }

    private static URI endpoint(HttpServer server) {
        return URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
    }
}
