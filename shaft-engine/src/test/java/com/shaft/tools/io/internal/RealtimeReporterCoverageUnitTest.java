package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpPrincipal;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

public class RealtimeReporterCoverageUnitTest {
    private static final Object REPORTER_LOCK = new Object();

    @BeforeMethod(alwaysRun = true)
    public void resetBeforeMethod() throws Exception {
        synchronized (REPORTER_LOCK) {
            resetRealtimeReporterState();
        }
    }

    @AfterMethod(alwaysRun = true)
    public void resetAfterMethod() throws Exception {
        synchronized (REPORTER_LOCK) {
            resetRealtimeReporterState();
            Properties.clearForCurrentThread();
        }
    }

    @Test
    public void initializationAndTeardownPathsRespectLaunchGuardsAndResetState() throws Exception {
        SHAFT.Properties.reporting.set().realtimeReport(false);
        RealtimeReporter.initialize("Disabled Suite");
        Assert.assertFalse(RealtimeReporter.isRunning());
        Assert.assertEquals(invokeBuildFullStateJson(), "{\"suiteName\":\"Test Execution\",\"tests\":[]}");

        startServerReflectively();
        Assert.assertTrue(RealtimeReporter.isRunning(), "Private server startup should enable the reporter for lifecycle coverage.");
        RealtimeReporter.onTestsPlanned(List.of(new RealtimeReporter.TestCard("old#test", "old", "test", "old.java")));
        Assert.assertTrue(invokeBuildFullStateJson().contains("old#test"));

        RealtimeReporter.stopServer();
        SHAFT.Properties.reporting.set().realtimeReport(true);
        SHAFT.Properties.platform.set().executionAddress("remote-grid");
        SHAFT.Properties.web.set().headlessExecution(false);
        RealtimeReporter.initialize("Fresh Suite");
        if (!RealtimeReporter.isRunning()) {
            throw new SkipException("RealtimeReporter HTTP server could not start in this environment.");
        }
        String stateJson = request("GET", "/api/state").body();
        Assert.assertTrue(stateJson.contains("Fresh Suite"));
        Assert.assertFalse(stateJson.contains("old#test"));

        SHAFT.Properties.allure.set().automaticallyOpen(false);
        RealtimeReporter.onExecutionFinished();
        Assert.assertTrue(RealtimeReporter.isRunning(), "Execution finish should keep the server open when Allure auto-open is disabled.");
        RealtimeReporter.stopServer();
        Assert.assertFalse(RealtimeReporter.isRunning());
        Assert.assertEquals(((AtomicLong) getField("attachmentBytesInMemory")).get(), 0L);
    }

    @Test
    public void plannedStartedPassedFailedSkippedAndBrokenReportingPathsUpdateCards() throws Exception {
        forceServerRunning(true);
        String passingId = RealtimeReporter.buildTestId("com.example.PassingTest", "passes");
        String failingId = RealtimeReporter.buildTestId("com.example.FailingTest", "fails");
        String skippedId = RealtimeReporter.buildTestId("com.example.SkippedTest", "skips");
        String brokenId = RealtimeReporter.buildTestId("com.example.ConfigLikeSetup", "beforeMethod");

        List<RealtimeReporter.TestCard> plannedCards = new ArrayList<>();
        plannedCards.add(new RealtimeReporter.TestCard(passingId, "com.example.PassingTest", "passes", RealtimeReporter.classNameToFilePath("com.example.PassingTest")));
        plannedCards.add(new RealtimeReporter.TestCard(failingId, "com.example.FailingTest", "fails", RealtimeReporter.classNameToFilePath("com.example.FailingTest")));
        plannedCards.add(new RealtimeReporter.TestCard(skippedId, "com.example.SkippedTest", "skips", RealtimeReporter.classNameToFilePath("com.example.SkippedTest")));
        plannedCards.add(new RealtimeReporter.TestCard(brokenId, "com.example.ConfigLikeSetup", "beforeMethod", "src/test/java/com/example/ConfigLikeSetup.java"));
        plannedCards.get(0).description = "A passing test with \"quotes\" and tabs\t";
        RealtimeReporter.onTestsPlanned(plannedCards);

        RealtimeReporter.onTestStarted(passingId);
        RealtimeReporter.appendConsoleLog(passingId, "plain \u001B[31mred\u001B[0m log");
        RealtimeReporter.appendStep(passingId, "open page", null);
        RealtimeReporter.appendAttachment(passingId, null, null, null, "small attachment".getBytes(StandardCharsets.UTF_8));
        RealtimeReporter.onTestFinished(passingId, RealtimeReporter.TestStatus.PASSED, null);

        RealtimeReporter.onTestStarted(failingId);
        RealtimeReporter.appendStep(failingId, "assert error", "FAILED");
        RealtimeReporter.onTestFinished(failingId, RealtimeReporter.TestStatus.FAILED, new AssertionError("expected failure"));

        RealtimeReporter.onTestStarted(skippedId);
        RealtimeReporter.onTestFinished(skippedId, RealtimeReporter.TestStatus.SKIPPED, null);

        RealtimeReporter.onTestStarted(brokenId);
        RealtimeReporter.onTestFinished(brokenId, RealtimeReporter.TestStatus.BROKEN, new RuntimeException("configuration failed"));

        RealtimeReporter.onTestStarted("com.example.DynamicTest#createdAtRuntime");
        RealtimeReporter.onTestFinished("missing#id", RealtimeReporter.TestStatus.FAILED, new RuntimeException("ignored"));
        RealtimeReporter.appendConsoleLog(null, "ignored");
        RealtimeReporter.appendConsoleLog(passingId, null);
        RealtimeReporter.appendStep(null, "ignored", "PASSED");
        RealtimeReporter.appendStep("unknown#id", "ignored", "PASSED");
        RealtimeReporter.appendAttachment(passingId, "empty", "empty", "text/plain", new byte[0]);
        RealtimeReporter.appendAttachment(null, "ignored", "ignored", "text/plain", "ignored".getBytes(StandardCharsets.UTF_8));

        String stateJson = invokeBuildFullStateJson();
        Assert.assertTrue(stateJson.contains("\"PASSED\""));
        Assert.assertTrue(stateJson.contains("\"FAILED\""));
        Assert.assertTrue(stateJson.contains("\"SKIPPED\""));
        Assert.assertTrue(stateJson.contains("\"BROKEN\""));
        Assert.assertTrue(stateJson.contains("expected failure"));
        Assert.assertTrue(stateJson.contains("configuration failed"));
        Assert.assertTrue(stateJson.contains("com.example.DynamicTest#createdAtRuntime"));

        RealtimeReporter.TestCard passingCard = getTestCards().get(passingId);
        String detailsJson = invokeBuildTestDetailsJson(passingCard, getConsoleLogs().get(passingId), getAttachments().get(passingId));
        Assert.assertTrue(detailsJson.contains("plain red log"), "ANSI control sequences should be stripped from buffered logs.");
        Assert.assertTrue(detailsJson.contains("open page"));
        Assert.assertTrue(detailsJson.contains("/api/attachment/att-1"));
        Assert.assertTrue(detailsJson.contains("hasAttachments\":true"));
    }

    @Test
    public void emptyNoTestSummaryAndSerializationHelpersReturnStableJson() throws Exception {
        forceServerRunning(true);
        setField("suiteName", "Empty Suite");
        Assert.assertEquals(invokeBuildFullStateJson(), "{\"suiteName\":\"Empty Suite\",\"tests\":[]}");
        Map<String, Object> mixedJsonValues = new java.util.LinkedHashMap<>();
        mixedJsonValues.put("number", 7);
        mixedJsonValues.put("nullValue", null);
        mixedJsonValues.put("text", "line\nquote\"tab\t");
        Assert.assertEquals(RealtimeReporter.toJson(mixedJsonValues),
                "{\"number\":7,\"nullValue\":null,\"text\":\"line\\nquote\\\"tab\\t\"}");
        Assert.assertEquals(RealtimeReporter.jsonString(null), "null");
        Assert.assertEquals(RealtimeReporter.classNameToFilePath(null), "");
        Assert.assertEquals(RealtimeReporter.classNameToFilePath("   "), "");

        RealtimeReporter.setCurrentTestId("current#test");
        Assert.assertEquals(RealtimeReporter.getCurrentTestId(), "current#test");
        RealtimeReporter.setCurrentTestId(" ");
        Assert.assertNull(RealtimeReporter.getCurrentTestId());
        RealtimeReporter.setCurrentTestId("current#test");
        RealtimeReporter.clearCurrentTestId();
        Assert.assertNull(RealtimeReporter.getCurrentTestId());
    }

    @Test
    public void httpHandlersGenerateExpectedOutputIntoTemporaryDirectory() throws Exception {
        forceServerRunning(true);
        setField("suiteName", "Output Suite");
        String testId = "com.example.OutputTest#writesFiles";
        RealtimeReporter.onTestsPlanned(List.of(new RealtimeReporter.TestCard(testId, "com.example.OutputTest", "writesFiles", "src/test/java/com/example/OutputTest.java")));
        RealtimeReporter.onTestStarted(testId);
        RealtimeReporter.appendConsoleLog(testId, "handler-log");
        RealtimeReporter.appendAttachment(testId, "text", "body.txt", "text/plain", "file payload".getBytes(StandardCharsets.UTF_8));
        RealtimeReporter.onTestFinished(testId, RealtimeReporter.TestStatus.PASSED, null);

        Path outputDirectory = Files.createTempDirectory("realtime-reporter-output");
        try {
            MockHttpExchange rootExchange = invokeHandler("handleRoot", "GET", "/");
            writeResponse(outputDirectory.resolve("dashboard.html"), rootExchange);
            Assert.assertEquals(rootExchange.responseCode, 200);
            Assert.assertTrue(Files.readString(outputDirectory.resolve("dashboard.html")).contains("SHAFT"));

            MockHttpExchange stateExchange = invokeHandler("handleState", "GET", "/api/state");
            writeResponse(outputDirectory.resolve("state.json"), stateExchange);
            Assert.assertEquals(stateExchange.responseCode, 200);
            Assert.assertTrue(Files.readString(outputDirectory.resolve("state.json")).contains("Output Suite"));
            Assert.assertEquals(stateExchange.getResponseHeaders().getFirst("Access-Control-Allow-Origin"), "*");

            String encodedTestId = URLEncoder.encode(testId, StandardCharsets.UTF_8);
            MockHttpExchange detailsExchange = invokeHandler("handleTestDetails", "GET", "/api/test/" + encodedTestId);
            writeResponse(outputDirectory.resolve("details.json"), detailsExchange);
            String details = Files.readString(outputDirectory.resolve("details.json"));
            Assert.assertEquals(detailsExchange.responseCode, 200);
            Assert.assertTrue(details.contains("handler-log"));
            Assert.assertTrue(details.contains("body.txt"));

            String attachmentId = getAttachments().get(testId).get(0).id;
            MockHttpExchange attachmentExchange = invokeHandler("handleAttachment", "GET", "/api/attachment/" + attachmentId);
            writeResponse(outputDirectory.resolve("attachment.txt"), attachmentExchange);
            Assert.assertEquals(attachmentExchange.responseCode, 200);
            Assert.assertEquals(attachmentExchange.getResponseHeaders().getFirst("Content-Type"), "text/plain");
            Assert.assertEquals(Files.readString(outputDirectory.resolve("attachment.txt")), "file payload");

            Assert.assertEquals(invokeHandler("handleRoot", "POST", "/").responseCode, 405);
            Assert.assertEquals(invokeHandler("handleState", "POST", "/api/state").responseCode, 405);
            Assert.assertEquals(invokeHandler("handleSse", "POST", "/api/events").responseCode, 405);
            Assert.assertEquals(invokeHandler("handleTestDetails", "POST", "/api/test/" + encodedTestId).responseCode, 405);
            Assert.assertEquals(invokeHandler("handleAttachment", "POST", "/api/attachment/" + attachmentId).responseCode, 405);
            Assert.assertEquals(invokeHandler("handleTestDetails", "GET", "/api/test/unknown%23id").responseCode, 404);
            Assert.assertEquals(invokeHandler("handleAttachment", "GET", "/api/attachment/missing").responseCode, 404);

            MockHttpExchange sseExchange = invokeHandler("handleSse", "GET", "/api/events");
            Assert.assertEquals(sseExchange.responseCode, 200);
            Assert.assertTrue(sseExchange.responseBodyAsString().contains(":ok"));
            Assert.assertEquals(sseExchange.getResponseHeaders().getFirst("Content-Type"), "text/event-stream");
        } finally {
            deleteRecursively(outputDirectory);
        }
    }

    @Test
    public void oversizedAttachmentsDeadSseClientsConstructorAndServerFailureBranchesAreCovered() throws Exception {
        forceServerRunning(true);
        String testId = "com.example.EdgeTest#coversEdges";
        RealtimeReporter.onTestsPlanned(List.of(new RealtimeReporter.TestCard(testId, "com.example.EdgeTest", "coversEdges", "")));
        RealtimeReporter.onTestStarted(testId);

        List<OutputStream> sseClients = getSseClients();
        sseClients.add(new OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new IOException("dead client");
            }
        });
        RealtimeReporter.appendConsoleLog(testId, "remove dead client");
        Assert.assertTrue(sseClients.isEmpty(), "Broadcasting should evict clients that throw IOException.");

        RealtimeReporter.appendAttachment(testId, "too-large", "too-large.bin", "application/octet-stream", new byte[26 * 1024 * 1024]);
        Assert.assertNull(getAttachments().get(testId), "Oversized attachments should be skipped before attachment metadata is created.");

        ((AtomicLong) getField("attachmentBytesInMemory")).set(250L * 1024 * 1024);
        RealtimeReporter.appendAttachment(testId, "budget", "budget.bin", "application/octet-stream", "x".getBytes(StandardCharsets.UTF_8));
        Assert.assertNull(getAttachments().get(testId), "Attachments should be skipped when the aggregate memory budget is exhausted.");

        Constructor<RealtimeReporter> constructor = RealtimeReporter.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        Assert.assertTrue(thrown.getCause() instanceof IllegalStateException);

        Method closeFailedServerResources = RealtimeReporter.class.getDeclaredMethod("closeFailedServerResources");
        closeFailedServerResources.setAccessible(true);
        closeFailedServerResources.invoke(null);
        Assert.assertFalse(RealtimeReporter.isRunning());
    }

    @Test
    public void realHttpServerEndpointsAreReachableAndStopIsIdempotent() throws Exception {
        startServerReflectively();
        if (!RealtimeReporter.isRunning()) {
            throw new SkipException("RealtimeReporter HTTP server could not start in this environment.");
        }
        String testId = "com.example.RealServerTest#roundTrip";
        RealtimeReporter.onTestsPlanned(List.of(new RealtimeReporter.TestCard(testId, "com.example.RealServerTest", "roundTrip", "")));
        RealtimeReporter.onTestStarted(testId);
        RealtimeReporter.appendAttachment(testId, null, null, null, "real server payload".getBytes(StandardCharsets.UTF_8));
        RealtimeReporter.onTestFinished(testId, RealtimeReporter.TestStatus.FAILED, new RuntimeException("server failure"));

        HttpResponse state = request("GET", "/api/state");
        Assert.assertEquals(state.code(), 200);
        Assert.assertTrue(state.body().contains(testId));

        HttpResponse details = request("GET", "/api/test/" + URLEncoder.encode(testId, StandardCharsets.UTF_8));
        Assert.assertEquals(details.code(), 200);
        Assert.assertTrue(details.body().contains("server failure"));
        Assert.assertTrue(details.body().contains("att-1"));

        HttpResponse rootPost = request("POST", "/");
        Assert.assertEquals(rootPost.code(), 405);

        RealtimeReporter.stopServer();
        RealtimeReporter.stopServer();
        Assert.assertFalse(RealtimeReporter.isRunning());
    }

    private static void startServerReflectively() throws Exception {
        Method startServer = RealtimeReporter.class.getDeclaredMethod("startServer");
        startServer.setAccessible(true);
        startServer.invoke(null);
    }

    private static MockHttpExchange invokeHandler(String methodName, String httpMethod, String path) throws Exception {
        Method handler = RealtimeReporter.class.getDeclaredMethod(methodName, HttpExchange.class);
        handler.setAccessible(true);
        MockHttpExchange exchange = new MockHttpExchange(httpMethod, path);
        handler.invoke(null, exchange);
        return exchange;
    }

    private static String invokeBuildFullStateJson() throws Exception {
        Method method = RealtimeReporter.class.getDeclaredMethod("buildFullStateJson");
        method.setAccessible(true);
        return (String) method.invoke(null);
    }

    private static String invokeBuildTestDetailsJson(RealtimeReporter.TestCard card, List<String> logs,
                                                     List<RealtimeReporter.AttachmentInfo> attachments) throws Exception {
        Method method = RealtimeReporter.class.getDeclaredMethod("buildTestDetailsJson", RealtimeReporter.TestCard.class, List.class, List.class);
        method.setAccessible(true);
        return (String) method.invoke(null, card, logs, attachments);
    }

    @SuppressWarnings("unchecked")
    private static ConcurrentMap<String, RealtimeReporter.TestCard> getTestCards() throws Exception {
        return (ConcurrentMap<String, RealtimeReporter.TestCard>) getField("testCards");
    }

    @SuppressWarnings("unchecked")
    private static ConcurrentMap<String, List<String>> getConsoleLogs() throws Exception {
        return (ConcurrentMap<String, List<String>>) getField("consoleLogs");
    }

    @SuppressWarnings("unchecked")
    private static ConcurrentMap<String, List<RealtimeReporter.AttachmentInfo>> getAttachments() throws Exception {
        return (ConcurrentMap<String, List<RealtimeReporter.AttachmentInfo>>) getField("attachments");
    }

    @SuppressWarnings("unchecked")
    private static List<OutputStream> getSseClients() throws Exception {
        return (List<OutputStream>) getField("sseClients");
    }

    private static Object getField(String name) throws Exception {
        Field field = RealtimeReporter.class.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(null);
    }

    private static void setField(String name, Object value) throws Exception {
        Field field = RealtimeReporter.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(null, value);
    }

    private static void forceServerRunning(boolean running) throws Exception {
        ((AtomicBoolean) getField("serverRunning")).set(running);
    }

    private static void resetRealtimeReporterState() throws Exception {
        RealtimeReporter.stopServer();
        forceServerRunning(false);
        RealtimeReporter.clearCurrentTestId();
        getTestCards().clear();
        getConsoleLogs().clear();
        getAttachments().clear();
        ((ConcurrentMap<?, ?>) getField("attachmentData")).clear();
        ((ConcurrentMap<?, ?>) getField("attachmentContentTypes")).clear();
        getSseClients().clear();
        ((AtomicLong) getField("attachmentBytesInMemory")).set(0);
        ((AtomicLong) getField("attachmentIdCounter")).set(0);
        setField("suiteName", "Test Execution");
        setField("DASHBOARD_URL", "http://localhost:1111");
        ExecutorService executor = (ExecutorService) getField("httpExecutor");
        if (executor != null) {
            executor.shutdownNow();
            setField("httpExecutor", null);
        }
        setField("httpServer", null);
    }

    private static HttpResponse request(String method, String path) throws Exception {
        String dashboardUrl = (String) getField("DASHBOARD_URL");
        HttpURLConnection connection = (HttpURLConnection) new URL(dashboardUrl + path).openConnection();
        connection.setRequestMethod(method);
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);
        int code = connection.getResponseCode();
        InputStream stream = code >= 400 ? connection.getErrorStream() : connection.getInputStream();
        String body = "";
        if (stream != null) {
            try (InputStream responseStream = stream; ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                responseStream.transferTo(baos);
                body = baos.toString(StandardCharsets.UTF_8);
            }
        }
        return new HttpResponse(code, body);
    }

    private static void writeResponse(Path outputPath, MockHttpExchange exchange) throws IOException {
        Files.write(outputPath, exchange.responseBodyBytes());
    }

    private static void deleteRecursively(Path path) throws IOException {
        if (path == null || !Files.exists(path)) {
            return;
        }
        try (var paths = Files.walk(path)) {
            paths.sorted((left, right) -> right.compareTo(left)).forEach(candidate -> {
                try {
                    Files.deleteIfExists(candidate);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
        }
    }

    private record HttpResponse(int code, String body) {
    }

    private static final class MockHttpExchange extends HttpExchange {
        private final Headers requestHeaders = new Headers();
        private final Headers responseHeaders = new Headers();
        private final ByteArrayOutputStream responseBody = new ByteArrayOutputStream();
        private final String method;
        private final URI uri;
        private int responseCode;

        private MockHttpExchange(String method, String path) {
            this.method = method;
            this.uri = URI.create(path);
        }

        @Override
        public Headers getRequestHeaders() {
            return requestHeaders;
        }

        @Override
        public Headers getResponseHeaders() {
            return responseHeaders;
        }

        @Override
        public URI getRequestURI() {
            return uri;
        }

        @Override
        public String getRequestMethod() {
            return method;
        }

        @Override
        public HttpContext getHttpContext() {
            return null;
        }

        @Override
        public void close() {
            try {
                responseBody.close();
            } catch (IOException ignored) {
                // ByteArrayOutputStream#close does not throw.
            }
        }

        @Override
        public InputStream getRequestBody() {
            return new ByteArrayInputStream(new byte[0]);
        }

        @Override
        public OutputStream getResponseBody() {
            return responseBody;
        }

        @Override
        public void sendResponseHeaders(int responseCode, long responseLength) {
            this.responseCode = responseCode;
        }

        @Override
        public InetSocketAddress getRemoteAddress() {
            return new InetSocketAddress("127.0.0.1", 12345);
        }

        @Override
        public int getResponseCode() {
            return responseCode;
        }

        @Override
        public InetSocketAddress getLocalAddress() {
            return new InetSocketAddress("127.0.0.1", 1111);
        }

        @Override
        public String getProtocol() {
            return "HTTP/1.1";
        }

        @Override
        public Object getAttribute(String name) {
            return null;
        }

        @Override
        public void setAttribute(String name, Object value) {
            // Not needed by the handlers under test.
        }

        @Override
        public void setStreams(InputStream inputStream, OutputStream outputStream) {
            // Not needed by the handlers under test.
        }

        @Override
        public HttpPrincipal getPrincipal() {
            return null;
        }

        private byte[] responseBodyBytes() {
            return responseBody.toByteArray();
        }

        private String responseBodyAsString() {
            return responseBody.toString(StandardCharsets.UTF_8);
        }
    }
}
