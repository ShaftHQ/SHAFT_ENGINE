package com.shaft.commandline.mcp;

import com.shaft.commandline.testsupport.FakeMcpServer;
import com.shaft.commandline.util.Json;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies {@link HttpMcpClient} against a JDK {@link HttpServer} reproducing the live-probed shaft-mcp
 * contract: initialize returns application/json plus an {@code Mcp-Session-Id} header, and later calls
 * return SSE-framed bodies. Also asserts the client echoes the session id back.
 */
class HttpMcpClientTest {

    private static final String SESSION_ID = "test-session-123";

    private HttpServer server;
    private URI endpoint;
    private final AtomicBoolean sessionEchoedOnList = new AtomicBoolean(false);

    @BeforeEach
    void startServer() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/mcp", this::handle);
        server.start();
        endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
    }

    @AfterEach
    void stopServer() {
        server.stop(0);
    }

    @Test
    void initializeCapturesSessionIdAndParsesJson() {
        try (HttpMcpClient client = new HttpMcpClient(endpoint)) {
            InitializeResult result = client.initialize();
            assertEquals("shaft-mcp", result.serverName());
        }
    }

    @Test
    void listToolsParsesSseAndEchoesSession() {
        try (HttpMcpClient client = new HttpMcpClient(endpoint)) {
            List<String> names = client.listTools().stream().map(Tool::name).toList();
            assertTrue(names.contains(FakeMcpServer.ARBITRARY_TOOL));
            assertTrue(sessionEchoedOnList.get(), "tools/list must echo the Mcp-Session-Id header");
        }
    }

    @Test
    void callToolParsesSseResult() {
        try (HttpMcpClient client = new HttpMcpClient(endpoint)) {
            ObjectNode args = Json.newObject();
            args.put("value", "x");
            CallToolResult result = client.callTool(FakeMcpServer.ECHO_TOOL, args);
            assertFalse(result.isError());
            assertTrue(result.text().contains("called " + FakeMcpServer.ECHO_TOOL));
        }
    }

    private void handle(HttpExchange exchange) throws IOException {
        String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        JsonNode request = Json.MAPPER.readTree(body);
        String method = request.path("method").asText("");
        String incomingSession = exchange.getRequestHeaders().getFirst("Mcp-Session-Id");

        if ("notifications/initialized".equals(method)) {
            exchange.sendResponseHeaders(202, -1);
            exchange.close();
            return;
        }

        ObjectNode response = Json.newObject();
        response.put("jsonrpc", "2.0");
        response.set("id", request.path("id"));
        ObjectNode result = Json.newObject();

        if ("initialize".equals(method)) {
            result.put("protocolVersion", "2025-03-26");
            result.set("capabilities", Json.newObject());
            ObjectNode serverInfo = Json.newObject();
            serverInfo.put("name", "shaft-mcp");
            serverInfo.put("version", "http-1.0");
            result.set("serverInfo", serverInfo);
            response.set("result", result);
            byte[] payload = Json.MAPPER.writeValueAsString(response).getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.getResponseHeaders().set("Mcp-Session-Id", SESSION_ID);
            exchange.sendResponseHeaders(200, payload.length);
            exchange.getResponseBody().write(payload);
            exchange.close();
            return;
        }

        if ("tools/list".equals(method)) {
            sessionEchoedOnList.set(SESSION_ID.equals(incomingSession));
            result.set("tools", toolCatalog());
        } else if ("tools/call".equals(method)) {
            String name = request.path("params").path("name").asText("");
            var content = Json.MAPPER.createArrayNode();
            ObjectNode block = Json.newObject();
            block.put("type", "text");
            block.put("text", "called " + name);
            content.add(block);
            result.set("content", content);
            result.put("isError", false);
        }
        response.set("result", result);

        String sse = "id:" + SESSION_ID + "\nevent:message\ndata:"
                + Json.MAPPER.writeValueAsString(response) + "\n\n";
        byte[] payload = sse.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "text/event-stream");
        exchange.sendResponseHeaders(200, payload.length);
        exchange.getResponseBody().write(payload);
        exchange.close();
    }

    private static tools.jackson.databind.node.ArrayNode toolCatalog() {
        var tools = Json.MAPPER.createArrayNode();
        for (String name : List.of(FakeMcpServer.ECHO_TOOL, FakeMcpServer.ARBITRARY_TOOL)) {
            ObjectNode tool = Json.newObject();
            tool.put("name", name);
            tool.put("description", "desc");
            tools.add(tool);
        }
        return tools;
    }
}
