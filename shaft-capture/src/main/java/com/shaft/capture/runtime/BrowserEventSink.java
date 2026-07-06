package com.shaft.capture.runtime;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.model.CaptureStep;
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
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Loopback sink used by the WebDriver fallback listener before navigation can clear page memory.
 * Also serves the authoritative recorded step list so the recorder UI can rehydrate its step
 * list from the server-side session store across navigations, including cross-origin ones,
 * instead of relying on page-scoped storage.
 */
final class BrowserEventSink implements AutoCloseable {
    private static final int MAX_REQUEST_BYTES = 131_072;

    private final Consumer<BrowserSignal> signalConsumer;
    private final Consumer<String> warningConsumer;
    private final ObjectMapper mapper = JsonMapper.builder().build();
    private final String token = token();
    private final HttpServer server;
    private volatile Supplier<List<CaptureStep>> stepsSupplier = List::of;
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
        server.createContext("/steps", this::handleSteps);
    }

    String start() {
        server.start();
        port = server.getAddress().getPort();
        return endpoint();
    }

    String endpoint() {
        return port == 0 ? "" : "http://127.0.0.1:" + port + "/event";
    }

    String stepsEndpoint() {
        return port == 0 ? "" : "http://127.0.0.1:" + port + "/steps";
    }

    String eventToken() {
        return token;
    }

    /**
     * Wires the authoritative step supplier backing {@code GET /steps}.
     *
     * @param stepsSupplier current session step supplier
     */
    void stepsSupplier(Supplier<List<CaptureStep>> stepsSupplier) {
        this.stepsSupplier = stepsSupplier == null ? List::of : stepsSupplier;
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

    private void handleSteps(HttpExchange exchange) throws IOException {
        try (exchange) {
            exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
            if (!"GET".equalsIgnoreCase(exchange.getRequestMethod())) {
                send(exchange, 405);
                return;
            }
            if (!authorized(queryParameter(exchange, "token"))) {
                send(exchange, 401);
                return;
            }
            List<CaptureStep> steps;
            try {
                steps = stepsSupplier.get();
            } catch (RuntimeException exception) {
                steps = List.of();
            }
            byte[] body = mapper.writeValueAsBytes(steps);
            exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
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
        return candidate != null && MessageDigest.isEqual(
                token.getBytes(StandardCharsets.UTF_8),
                candidate.getBytes(StandardCharsets.UTF_8));
    }

    private static String queryParameter(HttpExchange exchange, String name) {
        String query = exchange.getRequestURI().getRawQuery();
        if (query == null || query.isBlank()) {
            return "";
        }
        for (String pair : query.split("&")) {
            int separator = pair.indexOf('=');
            if (separator < 0) {
                continue;
            }
            if (pair.substring(0, separator).equals(name)) {
                return URLDecoder.decode(pair.substring(separator + 1), StandardCharsets.UTF_8);
            }
        }
        return "";
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
