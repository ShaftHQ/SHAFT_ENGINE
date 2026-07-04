package com.shaft.capture.runtime;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import com.shaft.capture.collector.BrowserSignal;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.function.Consumer;

/**
 * Loopback sink used by the WebDriver fallback listener before navigation can clear page memory.
 */
final class BrowserEventSink implements AutoCloseable {
    private static final int MAX_REQUEST_BYTES = 131_072;

    private final Consumer<BrowserSignal> signalConsumer;
    private final Consumer<String> warningConsumer;
    private final ObjectMapper mapper = JsonMapper.builder().build();
    private final String token = token();
    private final HttpServer server;
    private int port;

    BrowserEventSink(
            Consumer<BrowserSignal> signalConsumer,
            Consumer<String> warningConsumer) {
        if (signalConsumer == null || warningConsumer == null) {
            throw new IllegalArgumentException("Browser event sink consumers are required.");
        }
        this.signalConsumer = signalConsumer;
        this.warningConsumer = warningConsumer;
        try {
            server = HttpServer.create(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0), 0);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture browser event sink could not start.", exception);
        }
        server.createContext("/event", this::handle);
    }

    String start() {
        server.start();
        port = server.getAddress().getPort();
        return endpoint();
    }

    String endpoint() {
        return port == 0 ? "" : "http://127.0.0.1:" + port + "/event";
    }

    String eventToken() {
        return token;
    }

    @Override
    public void close() {
        server.stop(0);
    }

    private void handle(HttpExchange exchange) throws IOException {
        try (exchange) {
            exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
            if (!"POST".equalsIgnoreCase(exchange.getRequestMethod())) {
                send(exchange, 405);
                return;
            }
            byte[] body = exchange.getRequestBody().readNBytes(MAX_REQUEST_BYTES + 1);
            if (body.length > MAX_REQUEST_BYTES) {
                send(exchange, 413);
                return;
            }
            accept(body);
            send(exchange, 204);
        }
    }

    private void accept(byte[] body) {
        try {
            JsonNode root = mapper.readTree(body);
            if (!authorized(root.path("token").asText())) {
                return;
            }
            JsonNode payload = root.path("payload");
            if (!payload.isObject()) {
                return;
            }
            signalConsumer.accept(BrowserSignal.fromJson(mapper.writeValueAsString(payload), "loopback"));
        } catch (JacksonException exception) {
            warningConsumer.accept("A malformed browser interaction signal was ignored.");
        }
    }

    private boolean authorized(String candidate) {
        return MessageDigest.isEqual(
                token.getBytes(StandardCharsets.UTF_8),
                candidate.getBytes(StandardCharsets.UTF_8));
    }

    private static void send(HttpExchange exchange, int status) throws IOException {
        exchange.sendResponseHeaders(status, -1);
    }

    private static String token() {
        byte[] bytes = new byte[32];
        new SecureRandom().nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }
}
