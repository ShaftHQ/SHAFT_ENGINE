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
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Loopback sink used by the WebDriver fallback listener before navigation can clear page memory.
 * Also answers a token-authenticated GET with the current session snapshot, so the in-page
 * recorder can rehydrate its panel state after a cross-origin navigation resets page-scoped
 * storage.
 */
final class BrowserEventSink implements AutoCloseable {
    private static final int MAX_REQUEST_BYTES = 131_072;

    private final Consumer<BrowserSignal> signalConsumer;
    private final Consumer<String> warningConsumer;
    private final Supplier<Map<String, Object>> stateSupplier;
    private final ObjectMapper mapper = JsonMapper.builder().build();
    private final String token = token();
    private final HttpServer server;
    private int port;

    BrowserEventSink(
            Consumer<BrowserSignal> signalConsumer,
            Consumer<String> warningConsumer,
            Supplier<Map<String, Object>> stateSupplier) {
        if (signalConsumer == null || warningConsumer == null || stateSupplier == null) {
            throw new IllegalArgumentException("Browser event sink consumers are required.");
        }
        this.signalConsumer = signalConsumer;
        this.warningConsumer = warningConsumer;
        this.stateSupplier = stateSupplier;
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
            if ("GET".equalsIgnoreCase(exchange.getRequestMethod())) {
                handleState(exchange);
                return;
            }
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

    private void handleState(HttpExchange exchange) throws IOException {
        if (!authorized(queryParameter(exchange.getRequestURI().getRawQuery(), "token"))) {
            send(exchange, 403);
            return;
        }
        byte[] body = mapper.writeValueAsBytes(stateSupplier.get());
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, body.length);
        exchange.getResponseBody().write(body);
    }

    private static String queryParameter(String rawQuery, String name) {
        if (rawQuery == null || rawQuery.isBlank()) {
            return "";
        }
        for (String pair : rawQuery.split("&")) {
            int separator = pair.indexOf('=');
            String key = separator < 0 ? pair : pair.substring(0, separator);
            if (name.equals(URLDecoder.decode(key, StandardCharsets.UTF_8))) {
                return separator < 0 ? "" : URLDecoder.decode(pair.substring(separator + 1), StandardCharsets.UTF_8);
            }
        }
        return "";
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
