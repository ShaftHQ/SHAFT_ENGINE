package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import lombok.SneakyThrows;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * Real-time test execution dashboard server for SHAFT_ENGINE.
 * <p>
 * Hosts a local HTTP server that serves a Kanban-style dashboard reflecting live
 * test execution state. The dashboard is automatically opened in the user's default
 * browser at the start of the test run.
 *
 * <p>The server is only launched when all of the following conditions are met:
 * <ul>
 *   <li>The {@code reporting.realtimeReport.enabled} property is {@code true}.</li>
 *   <li>Execution is not inside a CI/CD environment.</li>
 * </ul>
 *
 * <p>Only one instance of the server runs at a time. Starting a new test run tears down
 * the previous server and starts a fresh one.
 */
public class RealtimeReporter {

    /** Default port for the real-time dashboard server. */
    private static final int DEFAULT_PORT = 1111;
    private static final String DASHBOARD_URL = "http://localhost:" + DEFAULT_PORT;
    private static final Logger logger = LogManager.getLogger(RealtimeReporter.class);
    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm").withZone(ZoneId.systemDefault());
    private static final DateTimeFormatter FULL_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss").withZone(ZoneId.systemDefault());

    // Server state
    private static HttpServer httpServer;
    private static final AtomicBoolean serverRunning = new AtomicBoolean(false);

    // Test data
    private static final ConcurrentMap<String, TestCard> testCards = new ConcurrentLinkedHashMap<>();
    private static volatile String suiteName = "Test Execution";

    // SSE clients (each is an OutputStream of an open SSE connection)
    private static final List<OutputStream> sseClients = Collections.synchronizedList(new ArrayList<>());

    // Per-test console log buffer  (testId → list of log lines)
    private static final ConcurrentMap<String, List<String>> consoleLogs = new ConcurrentHashMap<>();

    // Per-test attachment buffer  (testId → list of attachments)
    private static final ConcurrentMap<String, List<AttachmentInfo>> attachments = new ConcurrentHashMap<>();

    // Global attachment data store  (attachmentId → raw bytes)
    private static final ConcurrentMap<String, byte[]> attachmentData = new ConcurrentHashMap<>();

    private static final AtomicLong attachmentIdCounter = new AtomicLong(0);

    private RealtimeReporter() {
        throw new IllegalStateException("Utility class");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Public lifecycle API
    // ─────────────────────────────────────────────────────────────────────────

    /**
     * Initialises the real-time report for a new test run.
     * <p>
     * If the feature is disabled or the execution is in CI, this method is a no-op.
     *
     * @param runSuiteName the name to display as the run title on the dashboard
     */
    public static void initialize(String runSuiteName) {
        if (!shouldLaunch()) return;
        stopServer();
        suiteName = runSuiteName != null ? runSuiteName : "Test Execution";
        testCards.clear();
        consoleLogs.clear();
        attachments.clear();
        attachmentData.clear();
        attachmentIdCounter.set(0);
        startServer();
        openBrowser();
    }

    /**
     * Registers all tests that are planned to run in the current suite.
     * These are placed in the "Todo" swim lane.
     *
     * @param cards list of {@link TestCard} objects representing planned tests
     */
    public static void onTestsPlanned(List<TestCard> cards) {
        if (!serverRunning.get()) return;
        cards.forEach(card -> {
            card.status = TestStatus.TODO;
            testCards.put(card.id, card);
        });
        broadcastFullState();
    }

    /**
     * Moves a test from the "Todo" lane to the "In Progress" lane.
     *
     * @param testId the unique identifier of the test (see {@link #buildTestId})
     */
    public static void onTestStarted(String testId) {
        if (!serverRunning.get()) return;
        TestCard card = testCards.computeIfAbsent(testId, id -> new TestCard(id, id, id, ""));
        card.status = TestStatus.IN_PROGRESS;
        card.startTime = Instant.now().toEpochMilli();
        consoleLogs.computeIfAbsent(testId, k -> Collections.synchronizedList(new ArrayList<>()));
        broadcastEvent("testStarted", card);
    }

    /**
     * Moves a test from the "In Progress" lane to the "Done" lane with the given status.
     *
     * @param testId    the unique identifier of the test
     * @param status    the final status (PASSED / FAILED / BROKEN / SKIPPED)
     * @param throwable the failure throwable, or {@code null} for passing/skipped tests
     */
    public static void onTestFinished(String testId, TestStatus status, Throwable throwable) {
        if (!serverRunning.get()) return;
        TestCard card = testCards.get(testId);
        if (card == null) return;
        card.status = status;
        card.endTime = Instant.now().toEpochMilli();
        if (throwable != null) {
            card.errorMessage = throwable.getMessage();
        }
        broadcastEvent("testFinished", card);
    }

    /**
     * Appends a console log line to the buffer for the currently active test.
     * The line is forwarded to all SSE clients watching the test details page.
     *
     * @param testId  the unique identifier of the test (may be {@code null} if no test is active)
     * @param message the log line to append (ANSI codes are stripped before storage)
     */
    public static void appendConsoleLog(String testId, String message) {
        if (!serverRunning.get() || testId == null || message == null) return;
        List<String> logs = consoleLogs.computeIfAbsent(testId,
                k -> Collections.synchronizedList(new ArrayList<>()));
        String clean = stripAnsi(message);
        logs.add(clean);
        // push incremental log update
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("testId", testId);
        payload.put("line", clean);
        broadcastRawEvent("consoleLog", toJson(payload));
    }

    /**
     * Appends a step entry to the test card.
     *
     * @param testId   the unique identifier of the test
     * @param stepName the human-readable step description
     * @param stepStatus the step status string (e.g. "PASSED", "FAILED")
     */
    public static void appendStep(String testId, String stepName, String stepStatus) {
        if (!serverRunning.get() || testId == null || stepName == null) return;
        TestCard card = testCards.get(testId);
        if (card == null) return;
        StepInfo step = new StepInfo();
        step.name = stepName;
        step.status = stepStatus != null ? stepStatus : "PASSED";
        step.timestamp = Instant.now().toEpochMilli();
        card.steps.add(step);
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("testId", testId);
        payload.put("step", stepToMap(step));
        broadcastRawEvent("stepAdded", toJson(payload));
    }

    /**
     * Appends an attachment (screenshot, API response, etc.) to a test card.
     * The raw bytes are stored in memory and served via {@code /api/attachment/<id>}.
     *
     * @param testId         the unique identifier of the test
     * @param attachmentType type label (e.g. "Screenshot", "API Response Body")
     * @param attachmentName human-readable file name
     * @param contentType    MIME type (e.g. "image/png", "text/json")
     * @param content        the raw bytes of the attachment
     */
    public static void appendAttachment(String testId, String attachmentType, String attachmentName,
                                        String contentType, byte[] content) {
        if (!serverRunning.get() || testId == null || content == null || content.length == 0) return;
        String attId = "att-" + attachmentIdCounter.incrementAndGet();
        attachmentData.put(attId, content);

        AttachmentInfo info = new AttachmentInfo();
        info.id = attId;
        info.type = attachmentType != null ? attachmentType : "file";
        info.name = attachmentName != null ? attachmentName : "attachment";
        info.contentType = contentType != null ? contentType : "application/octet-stream";
        info.size = content.length;
        info.timestamp = Instant.now().toEpochMilli();

        attachments.computeIfAbsent(testId, k -> Collections.synchronizedList(new ArrayList<>())).add(info);

        // Mark card as having attachments
        TestCard card = testCards.get(testId);
        if (card != null) card.hasAttachments = true;

        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("testId", testId);
        payload.put("attachment", attachmentInfoToMap(info));
        broadcastRawEvent("attachmentAdded", toJson(payload));
    }

    /**
     * Called when the execution finishes. Closes the server if the Allure auto-open
     * flag is enabled; otherwise the server remains running so the user can review
     * the final state.
     */
    public static void onExecutionFinished() {
        if (!serverRunning.get()) return;
        broadcastRawEvent("executionFinished", "{}");
        boolean allureAutoOpen = false;
        try {
            allureAutoOpen = SHAFT.Properties.allure.automaticallyOpen();
        } catch (Exception ignored) {
            // properties may not be available in all contexts
        }
        if (allureAutoOpen) {
            // Give the browser a moment to receive the final event before stopping
            try {
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            stopServer();
        }
    }

    /**
     * Forcefully stops the HTTP server and clears all state. Safe to call even if
     * the server is not running.
     */
    public static void stopServer() {
        if (httpServer != null) {
            try {
                httpServer.stop(0);
            } catch (Exception e) {
                logger.debug("[RealtimeReporter] Error stopping server: {}", e.getMessage());
            }
            httpServer = null;
        }
        serverRunning.set(false);
        sseClients.clear();
        attachmentData.clear();
    }

    /**
     * Returns {@code true} if the server is currently running and serving requests.
     */
    public static boolean isRunning() {
        return serverRunning.get();
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Launch guard
    // ─────────────────────────────────────────────────────────────────────────

    /**
     * Determines whether the real-time report should be launched.
     * The report launches when the flag is enabled and not running in CI.
     * Both headless and non-headless local modes are supported.
     *
     * @return {@code true} when the flag is enabled and not in CI
     */
    public static boolean shouldLaunch() {
        try {
            if (!SHAFT.Properties.reporting.realtimeReport()) return false;
        } catch (Exception e) {
            return false;
        }
        return !isRunningInCI();
    }

    /**
     * Detects common CI/CD environment variables.
     *
     * @return {@code true} if any known CI marker is present in the environment
     */
    public static boolean isRunningInCI() {
        return System.getenv("CI") != null
                || System.getenv("JENKINS_URL") != null
                || System.getenv("GITHUB_ACTIONS") != null
                || System.getenv("GITLAB_CI") != null
                || System.getenv("TRAVIS") != null
                || System.getenv("CIRCLECI") != null
                || System.getenv("TF_BUILD") != null
                || System.getenv("BAMBOO_BUILDNUMBER") != null
                || System.getenv("TEAMCITY_VERSION") != null
                || System.getenv("BUILD_ID") != null;
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Unique test ID helpers
    // ─────────────────────────────────────────────────────────────────────────

    /**
     * Builds a stable unique identifier for a test from its fully-qualified class name
     * and method name. Safe to call from both TestNG and JUnit listener hooks.
     *
     * @param className  fully-qualified class name (e.g. {@code com.example.MyTest})
     * @param methodName the test method name
     * @return identifier string in the form {@code className#methodName}
     */
    public static String buildTestId(String className, String methodName) {
        return className + "#" + methodName;
    }

    /**
     * Converts a fully-qualified class name into a relative source-file path.
     *
     * @param qualifiedClassName e.g. {@code com.example.tests.LoginTest}
     * @return e.g. {@code src/test/java/com/example/tests/LoginTest.java}
     */
    public static String classNameToFilePath(String qualifiedClassName) {
        return "src/test/java/" + qualifiedClassName.replace('.', '/') + ".java";
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Server management
    // ─────────────────────────────────────────────────────────────────────────

    @SneakyThrows
    private static void startServer() {
        httpServer = HttpServer.create(new InetSocketAddress("localhost", DEFAULT_PORT), 0);
        httpServer.createContext("/", RealtimeReporter::handleRoot);
        httpServer.createContext("/api/state", RealtimeReporter::handleState);
        httpServer.createContext("/api/events", RealtimeReporter::handleSse);
        httpServer.createContext("/api/test/", RealtimeReporter::handleTestDetails);
        httpServer.createContext("/api/attachment/", RealtimeReporter::handleAttachment);
        httpServer.setExecutor(Executors.newCachedThreadPool());
        httpServer.start();
        serverRunning.set(true);
        logger.info("[RealtimeReporter] Dashboard started at {}", DASHBOARD_URL);
    }

    private static void openBrowser() {
        try {
            if (SystemUtils.IS_OS_WINDOWS) {
                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + DASHBOARD_URL);
            } else if (SystemUtils.IS_OS_MAC) {
                Runtime.getRuntime().exec(new String[]{"open", DASHBOARD_URL});
            } else {
                Runtime.getRuntime().exec(new String[]{"xdg-open", DASHBOARD_URL});
            }
        } catch (Exception e) {
            logger.warn("[RealtimeReporter] Could not open browser automatically: {}", e.getMessage());
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // HTTP handlers
    // ─────────────────────────────────────────────────────────────────────────

    private static void handleRoot(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendResponse(exchange, 405, "text/plain", "Method Not Allowed");
            return;
        }
        String html = loadDashboardHtml();
        sendResponse(exchange, 200, "text/html; charset=utf-8", html);
    }

    private static void handleState(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendResponse(exchange, 405, "text/plain", "Method Not Allowed");
            return;
        }
        addCorsHeaders(exchange);
        String json = buildFullStateJson();
        sendResponse(exchange, 200, "application/json; charset=utf-8", json);
    }

    private static void handleSse(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendResponse(exchange, 405, "text/plain", "Method Not Allowed");
            return;
        }
        addCorsHeaders(exchange);
        exchange.getResponseHeaders().set("Content-Type", "text/event-stream");
        exchange.getResponseHeaders().set("Cache-Control", "no-cache");
        exchange.getResponseHeaders().set("Connection", "keep-alive");
        exchange.getResponseHeaders().set("X-Accel-Buffering", "no");
        exchange.sendResponseHeaders(200, 0);
        OutputStream os = exchange.getResponseBody();
        sseClients.add(os);
        // Send initial heartbeat
        try {
            os.write(":ok\n\n".getBytes(StandardCharsets.UTF_8));
            os.flush();
        } catch (IOException ignored) {
            sseClients.remove(os);
        }
        // Keep the connection open; the OutputStream is closed by stopServer or client disconnect
    }

    private static void handleTestDetails(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendResponse(exchange, 405, "text/plain", "Method Not Allowed");
            return;
        }
        addCorsHeaders(exchange);
        String path = exchange.getRequestURI().getPath();
        // path is like /api/test/<encoded-id>
        String encoded = path.substring("/api/test/".length());
        String testId = java.net.URLDecoder.decode(encoded, StandardCharsets.UTF_8);
        TestCard card = testCards.get(testId);
        if (card == null) {
            sendResponse(exchange, 404, "application/json", "{\"error\":\"not found\"}");
            return;
        }
        List<String> logs = consoleLogs.getOrDefault(testId, Collections.emptyList());
        List<AttachmentInfo> atts = attachments.getOrDefault(testId, Collections.emptyList());
        String json = buildTestDetailsJson(card, logs, atts);
        sendResponse(exchange, 200, "application/json; charset=utf-8", json);
    }

    private static void handleAttachment(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            sendResponse(exchange, 405, "text/plain", "Method Not Allowed");
            return;
        }
        addCorsHeaders(exchange);
        String path = exchange.getRequestURI().getPath();
        String attId = path.substring("/api/attachment/".length());
        byte[] data = attachmentData.get(attId);
        if (data == null) {
            sendResponse(exchange, 404, "text/plain", "Not found");
            return;
        }
        // Infer content type from the corresponding AttachmentInfo
        String ct = "application/octet-stream";
        for (List<AttachmentInfo> list : attachments.values()) {
            for (AttachmentInfo ai : list) {
                if (ai.id.equals(attId)) {
                    ct = ai.contentType;
                    break;
                }
            }
        }
        exchange.getResponseHeaders().set("Content-Type", ct);
        addCorsHeaders(exchange);
        exchange.sendResponseHeaders(200, data.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(data);
        }
    }

    private static void sendResponse(HttpExchange exchange, int code, String contentType, String body)
            throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", contentType);
        addCorsHeaders(exchange);
        exchange.sendResponseHeaders(code, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private static void addCorsHeaders(HttpExchange exchange) {
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // SSE broadcasting
    // ─────────────────────────────────────────────────────────────────────────

    private static void broadcastFullState() {
        broadcastRawEvent("fullState", buildFullStateJson());
    }

    private static void broadcastEvent(String eventType, TestCard card) {
        broadcastRawEvent(eventType, cardToJson(card));
    }

    private static void broadcastRawEvent(String eventType, String jsonData) {
        String sseMessage = "event: " + eventType + "\ndata: " + jsonData + "\n\n";
        byte[] bytes = sseMessage.getBytes(StandardCharsets.UTF_8);
        List<OutputStream> dead = new ArrayList<>();
        synchronized (sseClients) {
            for (OutputStream os : sseClients) {
                try {
                    os.write(bytes);
                    os.flush();
                } catch (IOException e) {
                    dead.add(os);
                }
            }
        }
        sseClients.removeAll(dead);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // JSON serialisation helpers
    // ─────────────────────────────────────────────────────────────────────────

    private static String buildFullStateJson() {
        StringBuilder sb = new StringBuilder();
        sb.append("{\"suiteName\":").append(jsonString(suiteName)).append(",");
        sb.append("\"tests\":[");
        List<TestCard> cards = new ArrayList<>(testCards.values());
        for (int i = 0; i < cards.size(); i++) {
            sb.append(cardToJson(cards.get(i)));
            if (i < cards.size() - 1) sb.append(",");
        }
        sb.append("]}");
        return sb.toString();
    }

    private static String buildTestDetailsJson(TestCard card, List<String> logs, List<AttachmentInfo> atts) {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append("\"card\":").append(cardToJson(card)).append(",");
        sb.append("\"consoleLogs\":[");
        List<String> snapshot = new ArrayList<>(logs);
        for (int i = 0; i < snapshot.size(); i++) {
            sb.append(jsonString(snapshot.get(i)));
            if (i < snapshot.size() - 1) sb.append(",");
        }
        sb.append("],");
        sb.append("\"attachments\":[");
        List<AttachmentInfo> attSnapshot = new ArrayList<>(atts);
        for (int i = 0; i < attSnapshot.size(); i++) {
            sb.append(toJson(attachmentInfoToMap(attSnapshot.get(i))));
            if (i < attSnapshot.size() - 1) sb.append(",");
        }
        sb.append("]}");
        return sb.toString();
    }

    private static String cardToJson(TestCard card) {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append("\"id\":").append(jsonString(card.id)).append(",");
        sb.append("\"className\":").append(jsonString(card.className)).append(",");
        sb.append("\"methodName\":").append(jsonString(card.methodName)).append(",");
        sb.append("\"filePath\":").append(jsonString(card.filePath)).append(",");
        sb.append("\"description\":").append(jsonString(card.description)).append(",");
        sb.append("\"status\":").append(jsonString(card.status.name())).append(",");
        sb.append("\"startTime\":").append(card.startTime).append(",");
        sb.append("\"endTime\":").append(card.endTime).append(",");
        sb.append("\"errorMessage\":").append(jsonString(card.errorMessage)).append(",");
        sb.append("\"hasAttachments\":").append(card.hasAttachments).append(",");
        sb.append("\"startTimeFormatted\":").append(jsonString(card.startTime > 0
                ? TIME_FORMATTER.format(Instant.ofEpochMilli(card.startTime)) : "")).append(",");
        sb.append("\"steps\":[");
        for (int i = 0; i < card.steps.size(); i++) {
            sb.append(toJson(stepToMap(card.steps.get(i))));
            if (i < card.steps.size() - 1) sb.append(",");
        }
        sb.append("]}");
        return sb.toString();
    }

    private static Map<String, Object> stepToMap(StepInfo step) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("name", step.name);
        m.put("status", step.status);
        m.put("timestamp", step.timestamp);
        m.put("timestampFormatted", FULL_FORMATTER.format(Instant.ofEpochMilli(step.timestamp)));
        return m;
    }

    private static Map<String, Object> attachmentInfoToMap(AttachmentInfo att) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("id", att.id);
        m.put("type", att.type);
        m.put("name", att.name);
        m.put("contentType", att.contentType);
        m.put("size", att.size);
        m.put("timestamp", att.timestamp);
        m.put("url", "/api/attachment/" + att.id);
        return m;
    }

    /** Minimal JSON serialiser for Map&lt;String,Object&gt; (string/number values only). */
    static String toJson(Map<String, Object> map) {
        StringBuilder sb = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<String, Object> e : map.entrySet()) {
            if (!first) sb.append(",");
            first = false;
            sb.append(jsonString(e.getKey())).append(":");
            Object v = e.getValue();
            if (v instanceof Number) {
                sb.append(v);
            } else {
                sb.append(jsonString(v != null ? v.toString() : null));
            }
        }
        sb.append("}");
        return sb.toString();
    }

    static String jsonString(String s) {
        if (s == null) return "null";
        return "\"" + s
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t")
                + "\"";
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Dashboard HTML loading
    // ─────────────────────────────────────────────────────────────────────────

    private static String loadDashboardHtml() {
        try (InputStream is = RealtimeReporter.class.getResourceAsStream("/realtime/dashboard.html")) {
            if (is != null) {
                return new String(is.readAllBytes(), StandardCharsets.UTF_8);
            }
        } catch (IOException e) {
            logger.warn("[RealtimeReporter] Could not load dashboard.html resource: {}", e.getMessage());
        }
        // Inline fallback (should not normally be reached)
        return "<html><body><h1>SHAFT Real-Time Dashboard</h1><p>dashboard.html resource not found.</p></body></html>";
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Utility
    // ─────────────────────────────────────────────────────────────────────────

    private static String stripAnsi(String text) {
        if (text == null) return "";
        return text.replaceAll("\u001B\\[[;\\d]*m", "");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Data model
    // ─────────────────────────────────────────────────────────────────────────

    /** Status of a test on the Kanban board. */
    public enum TestStatus {
        TODO, IN_PROGRESS, PASSED, FAILED, BROKEN, SKIPPED
    }

    /** Represents a single test card displayed on the Kanban board. */
    public static class TestCard {
        public String id;
        public String className;
        public String methodName;
        public String filePath;
        public String description;
        public TestStatus status = TestStatus.TODO;
        public long startTime = 0;
        public long endTime = 0;
        public String errorMessage = null;
        public boolean hasAttachments = false;
        public final List<StepInfo> steps = Collections.synchronizedList(new ArrayList<>());

        public TestCard(String id, String className, String methodName, String filePath) {
            this.id = id;
            this.className = className;
            this.methodName = methodName;
            this.filePath = filePath;
        }
    }

    /** Represents a single step within a test execution. */
    public static class StepInfo {
        public String name;
        public String status;
        public long timestamp;
    }

    /** Represents an attachment (screenshot, API response, etc.) associated with a test. */
    public static class AttachmentInfo {
        public String id;
        public String type;
        public String name;
        public String contentType;
        public int size;
        public long timestamp;
    }

    /**
     * Thread-safe insertion-ordered map backed by a {@link ConcurrentHashMap} and a
     * separate {@link CopyOnWriteArrayList} for ordering. Used to keep card insertion
     * order stable for rendering.
     */
    private static class ConcurrentLinkedHashMap<K, V> extends AbstractMap<K, V>
            implements ConcurrentMap<K, V> {

        private final ConcurrentMap<K, V> map = new ConcurrentHashMap<>();
        private final CopyOnWriteArrayList<K> order = new CopyOnWriteArrayList<>();

        @Override
        public V put(K key, V value) {
            V old = map.put(key, value);
            if (old == null) order.add(key);
            return old;
        }

        @Override
        public V putIfAbsent(K key, V value) {
            V existing = map.putIfAbsent(key, value);
            if (existing == null) order.addIfAbsent(key);
            return existing;
        }

        @Override
        public V computeIfAbsent(K key, java.util.function.Function<? super K, ? extends V> mappingFunction) {
            V existing = map.get(key);
            if (existing != null) return existing;
            V newVal = map.computeIfAbsent(key, mappingFunction);
            order.addIfAbsent(key);
            return newVal;
        }

        @Override
        public boolean replace(K key, V oldValue, V newValue) {
            return map.replace(key, oldValue, newValue);
        }

        @Override
        public V replace(K key, V value) {
            return map.replace(key, value);
        }

        @Override
        public boolean remove(Object key, Object value) {
            boolean removed = map.remove(key, value);
            if (removed) order.remove(key);
            return removed;
        }

        @Override
        public V get(Object key) {
            return map.get(key);
        }

        @Override
        public V remove(Object key) {
            V v = map.remove(key);
            if (v != null) order.remove(key);
            return v;
        }

        @Override
        public void clear() {
            map.clear();
            order.clear();
        }

        @Override
        public Set<Entry<K, V>> entrySet() {
            return order.stream()
                    .filter(map::containsKey)
                    .map(k -> new AbstractMap.SimpleEntry<>(k, map.get(k)))
                    .collect(Collectors.toCollection(LinkedHashSet::new));
        }

        @Override
        public Collection<V> values() {
            return order.stream()
                    .filter(map::containsKey)
                    .map(map::get)
                    .collect(Collectors.toList());
        }
    }
}
