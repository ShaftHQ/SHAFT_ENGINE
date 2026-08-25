package testPackage;

import com.shaft.driver.SHAFT;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.concurrent.Executors;

public final class TestPageServer {
    private static final long MAX_REQUESTED_DELAY_MILLIS = 5_000L;
    private static final Object LOCK = new Object();
    private static volatile HttpServer server;
    private static volatile int port;

    private TestPageServer() {
        // utility class
    }

    public static String url(String fixtureName) {
        ensureStarted();
        return "http://" + browserHost() + ":" + port + "/" + fixtureName;
    }

    /**
     * URL that accepts TCP and never writes an HTTP response, so a driver that
     * actually waits for page load cannot complete navigation.
     */
    public static String neverRespondUrl() {
        ensureStarted();
        return "http://" + browserHost() + ":" + port + "/__never_respond";
    }

    /**
     * Page with a download link whose target is served as {@code Content-Disposition: attachment}.
     */
    public static String downloadPageUrl(String fileName) {
        ensureStarted();
        return "http://" + browserHost() + ":" + port + "/__download_page?file="
                + URLEncoder.encode(safeDownloadFileName(fileName), StandardCharsets.UTF_8);
    }

    private static void serveDownloadPage(HttpExchange exchange) throws IOException {
        try (exchange) {
            String fileName = requestedDownloadFileName(exchange);
            String href = "/__download?file=" + URLEncoder.encode(fileName, StandardCharsets.UTF_8);
            String html = "<!doctype html><html><body>"
                    + "<a id=\"download-link\" href=\"" + href + "\">download</a>"
                    + "</body></html>";
            send(exchange, 200, html.getBytes(StandardCharsets.UTF_8), "text/html; charset=utf-8");
        }
    }

    private static void serveDownload(HttpExchange exchange) throws IOException {
        try (exchange) {
            String fileName = requestedDownloadFileName(exchange);
            byte[] body = "SHAFT silent download payload\n".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Disposition", "attachment; filename=\"" + fileName + "\"");
            send(exchange, 200, body, "application/octet-stream");
        }
    }

    private static String requestedDownloadFileName(HttpExchange exchange) {
        String query = exchange.getRequestURI().getQuery();
        if (query != null) {
            for (String param : query.split("&")) {
                String[] keyValue = param.split("=", 2);
                if (keyValue.length == 2 && "file".equals(keyValue[0])) {
                    return safeDownloadFileName(URLDecoder.decode(keyValue[1], StandardCharsets.UTF_8));
                }
            }
        }
        return "shaft-download.bin";
    }

    private static String safeDownloadFileName(String fileName) {
        Path name = Path.of(fileName == null ? "" : fileName).getFileName();
        String sanitized = name == null ? "" : name.toString().replaceAll("[^A-Za-z0-9._-]", "_");
        return sanitized.isBlank() ? "shaft-download.bin" : sanitized;
    }

    private static void neverRespond(HttpExchange exchange) {
        try {
            Thread.sleep(Long.MAX_VALUE);
        } catch (InterruptedException interruptedException) {
            Thread.currentThread().interrupt();
        } finally {
            exchange.close();
        }
    }

    private static void ensureStarted() {
        if (server != null) {
            return;
        }
        synchronized (LOCK) {
            if (server != null) {
                return;
            }
            try {
                var newServer = HttpServer.create(new InetSocketAddress("0.0.0.0", 0), 0);
                var root = Path.of(SHAFT.Properties.paths.testData()).toAbsolutePath().normalize();
                newServer.createContext("/", exchange -> serveFixture(exchange, root));
                newServer.createContext("/__never_respond", TestPageServer::neverRespond);
                newServer.createContext("/__download", TestPageServer::serveDownload);
                newServer.createContext("/__download_page", TestPageServer::serveDownloadPage);
                newServer.setExecutor(Executors.newCachedThreadPool(runnable -> {
                    Thread thread = new Thread(runnable, "test-page-server");
                    thread.setDaemon(true);
                    return thread;
                }));
                newServer.start();
                server = newServer;
                port = newServer.getAddress().getPort();
                Runtime.getRuntime().addShutdownHook(new Thread(() -> newServer.stop(0), "test-page-server-shutdown"));
            } catch (IOException e) {
                throw new RuntimeException("Failed to start test page server.", e);
            }
        }
    }

    private static void serveFixture(HttpExchange exchange, Path root) throws IOException {
        try (exchange) {
            String requestedPath = URLDecoder.decode(exchange.getRequestURI().getPath(), StandardCharsets.UTF_8);
            if (requestedPath.startsWith("/")) {
                requestedPath = requestedPath.substring(1);
            }

            Path fixture = root.resolve(requestedPath).normalize();
            if (!fixture.startsWith(root) || !Files.isRegularFile(fixture)) {
                send(exchange, 404, "Not found".getBytes(StandardCharsets.UTF_8), "text/plain; charset=utf-8");
                return;
            }

            applyRequestedDelay(exchange.getRequestURI().getQuery());
            send(exchange, 200, Files.readAllBytes(fixture), contentType(fixture));
        }
    }

    /**
     * Fixtures that need to prove readiness waits hold for a genuinely in-flight request (rather
     * than a JS-only trick that resolves instantly) can append {@code ?delayMs=N} to a
     * same-file/self-fetch URL to make this server hold the response open for {@code N}
     * milliseconds (capped at {@link #MAX_REQUESTED_DELAY_MILLIS}) before replying. Requests
     * without the parameter are served immediately, unaffected.
     */
    private static void applyRequestedDelay(String query) {
        if (query == null || query.isBlank()) {
            return;
        }
        for (String param : query.split("&")) {
            String[] keyValue = param.split("=", 2);
            if (keyValue.length == 2 && "delayMs".equals(keyValue[0])) {
                try {
                    long delayMillis = Math.min(MAX_REQUESTED_DELAY_MILLIS, Math.max(0L, Long.parseLong(keyValue[1])));
                    Thread.sleep(delayMillis);
                } catch (NumberFormatException ignored) {
                    // malformed delayMs; serve immediately
                } catch (InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                }
                return;
            }
        }
    }

    private static void send(HttpExchange exchange, int statusCode, byte[] body, String contentType) throws IOException {
        exchange.getResponseHeaders().set("Content-Type", contentType);
        exchange.sendResponseHeaders(statusCode, body.length);
        try (OutputStream responseBody = exchange.getResponseBody()) {
            responseBody.write(body);
        }
    }

    private static String contentType(Path fixture) {
        String fileName = fixture.getFileName().toString().toLowerCase(Locale.ROOT);
        if (fileName.endsWith(".html")) {
            return "text/html; charset=utf-8";
        }
        if (fileName.endsWith(".png")) {
            return "image/png";
        }
        if (fileName.endsWith(".pdf")) {
            return "application/pdf";
        }
        return "application/octet-stream";
    }

    private static String browserHost() {
        String override = System.getProperty("shaft.testPageServer.host");
        if (override != null && !override.isBlank()) {
            return override;
        }

        String executionAddress = SHAFT.Properties.platform.executionAddress();
        if (executionAddress != null
                && (executionAddress.contains("localhost:4444")
                || executionAddress.contains("127.0.0.1:4444")
                || executionAddress.contains("0.0.0.0:4444"))) {
            return "host.docker.internal";
        }
        return "127.0.0.1";
    }
}
