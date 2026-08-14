package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Explicit installed-Chromium acceptance for the complete automatic browser-capture pipeline. */
public class BrowserCaptureAcceptanceTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final String SECRET = "acceptance-secret";

    @Test(groups = "browser-capture-acceptance")
    public void chromiumFailureTraceShouldPersistBoundedRedactedBrowserEvidence() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                .traceIncludeDomSnapshots(true).traceIncludeScreenshots(true)
                .traceIncludeNetwork(true).traceIncludeConsole(true).traceIncludeFullPageSnapshots(true)
                .traceIncludeNativePageSource(true);
        BrowserObservabilityRecorder.ObservationSession observationSession = BrowserObservabilityRecorder.startSession();
        try (LoopbackWebSocketServer webSocket = new LoopbackWebSocketServer();
             LoopbackPageServer page = new LoopbackPageServer(webSocket.port())) {
            ChromeOptions options = new ChromeOptions();
            options.setBinary(chromeExecutable().toFile());
            options.addArguments("--headless=new", "--disable-gpu", "--no-sandbox", "--window-size=1200,800",
                    "--disable-features=LocalNetworkAccessChecks,LocalNetworkAccessPermissionPrompt");
            options.setCapability("goog:loggingPrefs", Map.of("browser", "ALL"));
            ChromeDriver driver = new ChromeDriver(options);
            DriverFactoryHelper helper = new DriverFactoryHelper();
            helper.initializeDriver(driver);
            TestExecutionInfo info = info();
            Path traceDirectory = FailureTraceReporter.traceDirectory(info);
            try {
                FailureTraceReporter.registerSensitiveSourceValue(SECRET);
                driver.get(page.url());
                var action = TraceEventRecorder.start("element", "CLICK", By.id("change"), driver);
                TraceEventRecorder.recordScreenshot(action,
                        ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES));
                driver.findElement(By.id("change")).click();
                new WebDriverWait(driver, Duration.ofSeconds(10)).until(ignored ->
                        "after-action".equals(driver.findElement(By.id("state")).getText()));
                Object browserState = ((org.openqa.selenium.JavascriptExecutor) driver).executeScript(
                        "return {complete:window.captureComplete, message:window.wsMessage, error:window.wsError}");
                Assert.assertEquals(((Map<?, ?>) browserState).get("complete"), true,
                        browserState + "; server=" + webSocket.failure());
                new WebDriverWait(driver, Duration.ofSeconds(10)).until(ignored -> {
                    List<BrowserObservabilityRecorder.WebSocketSnapshotEntry> events =
                            BrowserObservabilityRecorder.snapshotWebSockets(observationSession);
                    return events.stream().anyMatch(event -> "created".equals(event.type()))
                            && events.stream().anyMatch(event -> "closed".equals(event.type()));
                });
                List<BrowserObservabilityRecorder.WebSocketSnapshotEntry> capturedSockets =
                        BrowserObservabilityRecorder.snapshotWebSockets(observationSession);
                Assert.assertTrue(capturedSockets.stream().anyMatch(event -> "created".equals(event.type())),
                        capturedSockets.toString());
                Assert.assertTrue(capturedSockets.stream().anyMatch(event -> "closed".equals(event.type())),
                        capturedSockets.toString());
                TraceEventRecorder.finish(action, "passed", "changed", null, Map.of(), List.of());

                FailureTraceReporter.attachOnFailure(info, "browser capture acceptance", List.of());
                assertArchive(traceDirectory.resolve("shaft-trace.zip"));
            } finally {
                helper.closeDriver(driver);
                TraceEventRecorder.clear();
                BrowserObservabilityRecorder.clear();
                Properties.clearForCurrentThread();
                deleteDirectory(traceDirectory);
            }
        }
    }

    private static void assertArchive(Path archive) throws Exception {
        Assert.assertTrue(Files.isRegularFile(archive), archive.toString());
        try (ZipFile zip = new ZipFile(archive.toFile())) {
            var entries = zip.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                if (!entry.isDirectory() && isTextEntry(entry.getName())) {
                    String content = read(zip, entry.getName());
                    Assert.assertFalse(content.contains(SECRET), entry.getName());
                }
            }
            JsonNode root = JSON.readTree(read(zip, "shaft-trace.json"));
            JsonNode evidence = root.path("evidence");
            JsonNode action = evidence.path("actions").get(0);
            Assert.assertEquals(action.path("name").asText(), "CLICK", action.toPrettyString());
            List<String> leakedPaths = new java.util.ArrayList<>();
            collectSecretPaths(root, "$", leakedPaths);
            Assert.assertTrue(leakedPaths.isEmpty(), leakedPaths.toString());
            Assert.assertEquals(root.path("snapshot").path("provider").asText(), "webdriver",
                    root.path("snapshot").toPrettyString());
            Assert.assertEquals(root.path("snapshot").path("fidelity").asText(), "structural");
            Assert.assertTrue(root.path("snapshot").path("reason").asText().contains("enforceable"),
                    root.path("snapshot").toPrettyString());

            JsonNode artifacts = root.path("session").path("artifacts");
            JsonNode before = artifact(artifacts, "dom-snapshot", "before");
            JsonNode after = artifact(artifacts, "dom-snapshot", "after");
            Assert.assertNotNull(before, artifacts.toPrettyString());
            Assert.assertNotNull(after, artifacts.toPrettyString());
            Assert.assertTrue(read(zip, before.path("path").asText()).contains("before-action"));
            Assert.assertTrue(read(zip, after.path("path").asText()).contains("after-action"));
            Assert.assertTrue(hasArtifact(artifacts, "screenshot", ""), artifacts.toPrettyString());
            for (JsonNode artifact : artifacts) {
                String path = artifact.path("path").asText();
                if (("dom-snapshot".equals(artifact.path("kind").asText())
                        || "screenshot".equals(artifact.path("kind").asText())) && !artifact.path("omitted").asBoolean()) {
                    Assert.assertNotNull(zip.getEntry(path), path);
                }
            }

            JsonNode observability = evidence.path("browserObservability");
            Assert.assertTrue(observability.path("webSockets").size() >= 2, observability.toPrettyString());
            Assert.assertTrue(stream(observability.path("webSockets"), "type").contains("created"));
            Assert.assertTrue(stream(observability.path("webSockets"), "type").contains("closed"));
            Assert.assertTrue(stream(observability.path("webSockets"), "type").contains("frame"),
                    observability.path("webSockets").toPrettyString());
            Assert.assertTrue(observability.path("webSockets").toString().contains("********"),
                    observability.path("webSockets").toPrettyString());
            Assert.assertTrue(evidence.path("network").toString().contains("/api"), evidence.path("network").toPrettyString());
            Assert.assertTrue(evidence.path("console").toString().contains("browser acceptance"),
                    evidence.path("console").toPrettyString());
        }
    }

    private static List<String> stream(JsonNode array, String field) {
        java.util.ArrayList<String> values = new java.util.ArrayList<>();
        array.forEach(node -> values.add(node.path(field).asText()));
        return values;
    }

    private static void collectSecretPaths(JsonNode node, String path, List<String> leakedPaths) {
        if (node.isTextual() && node.asText().contains(SECRET)) {
            leakedPaths.add(path);
        } else if (node.isArray()) {
            for (int index = 0; index < node.size(); index++) collectSecretPaths(node.get(index), path + "[" + index + "]", leakedPaths);
        } else if (node.isObject()) {
            node.properties().forEach(entry -> collectSecretPaths(entry.getValue(), path + "." + entry.getKey(), leakedPaths));
        }
    }

    private static boolean hasArtifact(JsonNode artifacts, String kind, String phase) {
        return artifact(artifacts, kind, phase) != null;
    }

    private static JsonNode artifact(JsonNode artifacts, String kind, String phase) {
        for (JsonNode artifact : artifacts) {
            if (kind.equals(artifact.path("kind").asText())
                    && (phase.isEmpty() || phase.equals(artifact.path("metadata").path("phase").asText()))) return artifact;
        }
        return null;
    }

    private static String read(ZipFile zip, String path) throws IOException {
        ZipEntry entry = zip.getEntry(path);
        Assert.assertNotNull(entry, path);
        try (var input = zip.getInputStream(entry)) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static boolean isTextEntry(String name) {
        String normalized = name.toLowerCase(java.util.Locale.ROOT);
        return normalized.endsWith(".json") || normalized.endsWith(".html") || normalized.endsWith(".har")
                || normalized.endsWith(".txt") || normalized.endsWith(".log");
    }

    private static TestExecutionInfo info() throws Exception {
        Method marker = BrowserCaptureAcceptanceTest.class.getDeclaredMethod(
                "chromiumFailureTraceShouldPersistBoundedRedactedBrowserEvidence");
        return new TestExecutionInfo("browser-capture-acceptance", "customer.BrowserCaptureTest",
                "browserCapture", "browserCapture", "browser capture acceptance", marker,
                new AssertionError("browser capture acceptance"), false);
    }

    private static Path chromeExecutable() {
        return Path.of(System.getProperty("shaft.trace.viewer.chrome",
                "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe"));
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (!Files.exists(directory)) return;
        try (var paths = Files.walk(directory)) {
            for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }

    private static final class LoopbackPageServer implements AutoCloseable {
        private final HttpServer server;

        private LoopbackPageServer(int webSocketPort) throws IOException {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/", exchange -> respond(exchange, "text/html", page(webSocketPort)));
            server.createContext("/style.css", exchange -> respond(exchange, "text/css", "#state{color:rgb(1,2,3)}"));
            server.createContext("/api", exchange -> respond(exchange, "application/json", "{\"ok\":true}"));
            server.createContext("/frame", exchange -> respond(exchange, "text/html", "<p>frame evidence</p>"));
            server.setExecutor(Executors.newVirtualThreadPerTaskExecutor());
            server.start();
        }

        private String url() { return "http://127.0.0.1:" + server.getAddress().getPort() + "/"; }

        private static void respond(HttpExchange exchange, String contentType, String body) throws IOException {
            byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", contentType + "; charset=utf-8");
            exchange.sendResponseHeaders(200, bytes.length);
            try (OutputStream output = exchange.getResponseBody()) { output.write(bytes); }
        }

        private static String page(int webSocketPort) {
            return """
                    <!doctype html><html><head><link rel="stylesheet" href="/style.css"></head><body>
                    <main id="state">before-action</main><button id="change">change</button>
                    <iframe src="/frame"></iframe><div id="shadow"></div>
                    <script>
                    shadow.attachShadow({mode:'open'}).innerHTML='<span>shadow evidence</span>';
                    const ws = new WebSocket('ws://127.0.0.1:%d/socket');
                    let wsDone=false, fetchDone=false;
                    ws.onopen=()=>ws.send('client %s');
                    ws.onmessage=e=>{window.wsMessage=e.data;wsDone=true;ws.close();};
                    ws.onerror=()=>window.wsError='websocket-error';
                    fetch('/api',{headers:{Authorization:'Bearer %s'}}).then(r=>r.json()).then(()=>fetchDone=true);
                    console.error('browser acceptance token=%s');
                    change.onclick=()=>{state.textContent='after-action';window.captureComplete=wsDone&&fetchDone;};
                    setInterval(()=>{window.captureComplete=state.textContent==='after-action'&&wsDone&&fetchDone;},20);
                    </script></body></html>
                    """.formatted(webSocketPort, SECRET, SECRET, SECRET);
        }

        @Override public void close() { server.stop(0); }
    }

    private static final class LoopbackWebSocketServer implements AutoCloseable {
        private final ServerSocket server;
        private final Thread worker;
        private volatile String failure = "none";

        private LoopbackWebSocketServer() throws IOException {
            server = new ServerSocket();
            server.bind(new InetSocketAddress("127.0.0.1", 0));
            worker = Thread.ofVirtual().name("browser-capture-websocket").start(this::serve);
        }

        private int port() { return server.getLocalPort(); }
        private String failure() { return failure; }

        private void serve() {
            try (Socket socket = server.accept();
                 BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream(),
                         StandardCharsets.US_ASCII))) {
                String key = "";
                for (String line; (line = reader.readLine()) != null && !line.isEmpty(); ) {
                    if (line.regionMatches(true, 0, "Sec-WebSocket-Key:", 0, 18)) key = line.substring(18).trim();
                }
                String accept = Base64.getEncoder().encodeToString(MessageDigest.getInstance("SHA-1")
                        .digest((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes(StandardCharsets.US_ASCII)));
                failure = "key=" + key + ";accept=" + accept;
                OutputStream output = socket.getOutputStream();
                output.write(("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
                        + "Sec-WebSocket-Accept: " + accept + "\r\n\r\n").getBytes(StandardCharsets.US_ASCII));
                output.flush();
                byte[] payload = ("server " + SECRET).getBytes(StandardCharsets.UTF_8);
                output.write(0x81);
                output.write(payload.length);
                output.write(payload);
                output.flush();
                Thread.sleep(500);
            } catch (Exception exception) {
                // Closing the fixture is an expected way to stop an idle accept/read.
                if (!server.isClosed()) failure = exception.toString();
            }
        }

        @Override public void close() throws Exception {
            server.close();
            worker.join(Duration.ofSeconds(2));
        }
    }
}
