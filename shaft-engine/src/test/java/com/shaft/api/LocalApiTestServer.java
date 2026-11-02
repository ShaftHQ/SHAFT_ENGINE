package com.shaft.api;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

final class LocalApiTestServer {
    private final HttpServer server;

    private LocalApiTestServer(HttpServer server) {
        this.server = server;
    }

    static LocalApiTestServer start() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        LocalApiTestServer fixture = new LocalApiTestServer(server);
        server.createContext("/user/string", fixture::handleUserByUsername);
        server.createContext("/user/createWithList", fixture::handleCreateWithList);
        server.start();
        return fixture;
    }

    String baseUrl() {
        return "http://localhost:" + server.getAddress().getPort();
    }

    void stop() {
        server.stop(0);
    }

    private void handleUserByUsername(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendJson(exchange, 405, "{\"error\":\"method not allowed\"}");
            return;
        }
        sendJson(exchange, 200, "{\"id\":1,\"username\":\"string\"}");
    }

    private void handleCreateWithList(HttpExchange exchange) throws IOException {
        if (!"POST".equals(exchange.getRequestMethod())) {
            sendJson(exchange, 405, "{\"error\":\"method not allowed\"}");
            return;
        }

        String requestBody = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        if (requestBody.contains("\"email\": 1")) {
            sendJson(exchange, 400, "{\"error\":\"invalid email type\"}");
            return;
        }
        sendJson(exchange, 200, "{\"username\":\"string\"}");
    }

    private void sendJson(HttpExchange exchange, int statusCode, String body) throws IOException {
        byte[] bodyBytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.sendResponseHeaders(statusCode, bodyBytes.length);
        try (OutputStream responseBody = exchange.getResponseBody()) {
            responseBody.write(bodyBytes);
        }
    }
}
