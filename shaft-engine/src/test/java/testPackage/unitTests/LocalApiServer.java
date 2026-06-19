package testPackage.unitTests;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

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
        byte[] responseBody = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(statusCode, responseBody.length);
        try (OutputStream outputStream = exchange.getResponseBody()) {
            outputStream.write(responseBody);
        }
    }
}
