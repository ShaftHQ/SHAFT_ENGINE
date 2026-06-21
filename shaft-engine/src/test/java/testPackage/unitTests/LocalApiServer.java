package testPackage.unitTests;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;

public final class LocalApiServer implements AutoCloseable {
    private static final String FIXTURE_ROOT = "/testDataFiles/api/local-api/";
    private final HttpServer server;
    private final String baseUrl;

    private LocalApiServer(HttpServer server) {
        this.server = server;
        this.baseUrl = "http://127.0.0.1:" + server.getAddress().getPort();
    }

    public static LocalApiServer start() {
        try {
            HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            registerJsonResponse(server, "/posts", "posts.json");
            registerJsonResponse(server, "/posts/1", "post-1.json");
            registerJsonResponse(server, "/todos", "todos.json");
            registerJsonResponse(server, "/users", "users.json");
            registerJsonResponse(server, "/us/90210", "zip-90210.json");
            registerCreateUserResponse(server, "/api/users");
            registerRawResponse(server, "/xml/items",
                    "<items><item><name>alpha</name><id>1</id></item><item><name>beta</name><id>2</id></item></items>",
                    "application/xml");
            registerMethodResponse(server, "/method/patch", "PATCH");
            registerMethodResponse(server, "/method/put", "PUT");
            registerMethodResponse(server, "/method/delete", "DELETE");
            registerInspectResponse(server, "/inspect");
            registerGraphQlResponse(server, "/graphql");
            server.start();
            return new LocalApiServer(server);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to start local API fixture server", e);
        }
    }

    public String baseUrl() {
        return baseUrl;
    }

    @Override
    public void close() {
        server.stop(0);
    }

    private static void registerJsonResponse(HttpServer server, String path, String fixtureName) {
        server.createContext(path, exchange -> {
            if (!"GET".equalsIgnoreCase(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\":\"method not allowed\"}");
                return;
            }
            sendResponse(exchange, 200, readFixture(fixtureName));
        });
    }

    private static void registerCreateUserResponse(HttpServer server, String path) {
        server.createContext(path, exchange -> {
            if (!"POST".equalsIgnoreCase(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\":\"method not allowed\"}");
                return;
            }
            String requestBody = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            sendResponse(exchange, 201, requestBody);
        });
    }

    private static void registerRawResponse(HttpServer server, String path, String body, String contentType) {
        server.createContext(path, exchange -> {
            sendResponse(exchange, 200, body, contentType);
        });
    }

    private static void registerMethodResponse(HttpServer server, String path, String methodName) {
        server.createContext(path, exchange -> {
            if (!methodName.equalsIgnoreCase(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\":\"method not allowed\"}");
                return;
            }
            sendResponse(exchange, 200, String.format("{\"method\":\"%s\"}", methodName),
                    "application/json");
        });
    }

    private static void registerInspectResponse(HttpServer server, String path) {
        server.createContext(path, exchange -> {
            if (!"GET".equalsIgnoreCase(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\":\"method not allowed\"}");
                return;
            }
            String customHeader = exchange.getRequestHeaders().getFirst("x-custom-header");
            String cookieHeader = exchange.getRequestHeaders().getFirst("Cookie");
            if (customHeader == null) {
                customHeader = "";
            }
            if (cookieHeader == null) {
                cookieHeader = "";
            }
            String payload = Map.of(
                    "header", customHeader,
                    "cookie", cookieHeader
            ).entrySet()
                    .stream()
                    .map(entry -> String.format("\"%s\":\"%s\"", entry.getKey(),
                            escapeJson(entry.getValue())))
                    .collect(Collectors.joining(","));
            sendResponse(exchange, 200, String.format("{%s}", payload), "application/json");
        });
    }

    private static void registerGraphQlResponse(HttpServer server, String path) {
        server.createContext(path, exchange -> {
            if (!"POST".equalsIgnoreCase(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\":\"method not allowed\"}");
                return;
            }
            sendResponse(exchange, 200, "{\"data\":{\"ping\":\"pong\"}}", "application/json");
        });
    }

    private static String escapeJson(String rawValue) {
        if (rawValue == null) {
            return "";
        }
        return rawValue.replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    private static String readFixture(String fixtureName) {
        try (InputStream inputStream = LocalApiServer.class.getResourceAsStream(FIXTURE_ROOT + fixtureName)) {
            if (inputStream == null) {
                throw new IllegalStateException("Missing local API fixture: " + fixtureName);
            }
            return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to read local API fixture: " + fixtureName, e);
        }
    }

    private static void sendResponse(HttpExchange exchange, int statusCode, String body) throws IOException {
        sendResponse(exchange, statusCode, body, "application/json");
    }

    private static void sendResponse(HttpExchange exchange, int statusCode, String body, String contentType) throws IOException {
        byte[] responseBody = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", contentType);
        exchange.sendResponseHeaders(statusCode, responseBody.length);
        try (OutputStream outputStream = exchange.getResponseBody()) {
            outputStream.write(responseBody);
        }
    }
}
