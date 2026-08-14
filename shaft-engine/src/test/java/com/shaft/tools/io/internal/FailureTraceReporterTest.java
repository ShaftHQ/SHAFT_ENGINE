package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.trace.TraceArtifactReference;
import io.appium.java_client.android.AndroidDriver;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Platform;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chromium.HasCdp;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;

@Test(singleThreaded = true)
public class FailureTraceReporterTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test(description = "Configured artifact MiB values should convert without integer overflow")
    public void configuredArtifactBudgetShouldUseLongArithmetic() {
        int original = SHAFT.Properties.reporting.traceMaxArtifactMb();
        try {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(2048);
            Assert.assertEquals(FailureTraceReporter.configuredMaxArtifactBytes(), 2_147_483_648L);
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(Integer.MAX_VALUE);
            Assert.assertEquals(FailureTraceReporter.configuredMaxArtifactBytes(),
                    Math.multiplyExact((long) Integer.MAX_VALUE, 1024L * 1024L));
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(-1);
            Assert.assertEquals(FailureTraceReporter.configuredMaxArtifactBytes(), 1024L * 1024L);
        } finally {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(original);
        }
    }

    @Test(description = "Failure mode should attach trace artifacts only for failed tests")
    public void failureModeShouldAttachTraceArtifactsOnlyForFailures() throws Exception {
        TestExecutionInfo failingInfo = info("failingScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            int beforePassing = attachments().size();

            FailureTraceReporter.attachOnFailure(info("passingScenario", null), "passing log", List.of());

            Assert.assertEquals(attachments().size(), beforePassing,
                    "Passing tests must not attach trace artifacts in failure mode.");

            FailureTraceReporter.attachOnFailure(failingInfo, "token=raw-secret", List.of());

            List<Attachment> added = attachments().subList(beforePassing, attachments().size());
            // In-report trace launcher (#3534 P2): the one-click self-contained viewer HTML plus the
            // full-fidelity zip download. Both carry the test id (and attempt suffix once retried).
            Assert.assertEquals(added.size(), 2, "The trace viewer HTML and the trace archive should both be attached.");
            Assert.assertTrue(added.stream().anyMatch(attachment -> "text/html".equals(attachment.getType())
                            && "SHAFT Trace Viewer - id-failingScenario".equals(attachment.getName())),
                    "Expected a one-click, in-report trace viewer HTML attachment.");
            Assert.assertTrue(added.stream().anyMatch(attachment -> "application/zip".equals(attachment.getType())
                            && "SHAFT Trace Report - id-failingScenario".equals(attachment.getName())),
                    "Expected the full-fidelity trace archive attachment for offline download.");
            // The JSON is embedded in the viewer HTML and the zip, never dangled as its own attachment.
            Assert.assertFalse(added.stream().anyMatch(attachment -> "application/json".equals(attachment.getType())));
            Assert.assertFalse(Files.exists(traceDirectory.resolve("SHAFT Trace Report.html")));
            Assert.assertFalse(Files.exists(traceDirectory.resolve("shaft-trace.json")));
            Assert.assertFalse(Files.isDirectory(traceDirectory.resolve("screenshots")));
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace.zip")));
            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                Assert.assertNotNull(zip.getEntry("SHAFT Trace Report.html"));
                Assert.assertNotNull(zip.getEntry("shaft-trace.json"));
                Assert.assertNotNull(zip.getEntry("shaft-network.har"));
                String html = readZipEntry(zip, "SHAFT Trace Report.html");
                Assert.assertTrue(html.contains("--shaft-primary"), html);
                Assert.assertTrue(html.contains("trace-summary"), html);
                Assert.assertTrue(html.contains("copyJson()"), html);
                Assert.assertTrue(html.contains("data-tab=\"domSnapshot\""), html);
                Assert.assertTrue(html.contains("dom-snapshot-frame"), html);
                Assert.assertTrue(html.contains("data-tab=\"screenshot\""), html);
                Assert.assertTrue(html.contains("screenshot-image"), html);
                Assert.assertTrue(html.contains("data-tab=\"timeline\""), html);
                Assert.assertTrue(html.contains("timeline-panel"), html);
                Assert.assertTrue(html.contains("id=\"trace-filmstrip\""), html);
                Assert.assertTrue(html.contains("role=\"listbox\""), html);
                Assert.assertTrue(html.contains("id=\"range-start\""), html);
                Assert.assertTrue(html.contains("id=\"range-end\""), html);
                Assert.assertTrue(html.contains("data-tab=\"comparison\""), html);
                Assert.assertTrue(html.contains("id=\"comparison-panel\""), html);
                Assert.assertTrue(html.contains("Before action"), html);
                Assert.assertTrue(html.contains("Action state"), html);
                Assert.assertTrue(html.contains("After action"), html);
                Assert.assertTrue(html.contains("Content-Security-Policy"), html);
                Assert.assertTrue(html.contains("default-src 'none'"), html);
                Assert.assertTrue(html.contains("function snapshotDocument(html)"), html);
                Assert.assertTrue(html.contains("intervalOverlaps(networkStartMs(entry), entry.durationMs, range)"), html);
                Assert.assertTrue(html.contains("window.addEventListener('popstate', restoreLocationState)"), html);
                Assert.assertTrue(html.contains("data-tab=\"environment\""), html);
                Assert.assertTrue(html.contains("network-panel"), html);
                Assert.assertTrue(html.contains("console-panel"), html);
                Assert.assertTrue(html.contains("id=\"network-result-count\""), html);
                Assert.assertTrue(html.contains("data-network-sort=\"size\""), html);
                Assert.assertTrue(html.contains("id=\"network-method-filter\""), html);
                Assert.assertTrue(html.contains("id=\"console-result-count\""), html);
                Assert.assertTrue(html.contains("data-console-sort=\"message\""), html);
                Assert.assertTrue(html.contains("id=\"console-level-filter\""), html);
                Assert.assertTrue(html.contains("data-tab=\"log\""), html);
                Assert.assertTrue(html.contains("const evidence = trace && trace.evidence"), html);
                String tracedJson = readZipEntry(zip, "shaft-trace.json");
                Assert.assertTrue(tracedJson.contains("\"environment\""), tracedJson);
                Assert.assertTrue(tracedJson.contains("\"os\""), tracedJson);
            }
            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"archive\": \"target/shaft-traces/id-failingScenario/shaft-trace.zip\""), index);
            Assert.assertTrue(index.contains("\"html\": \"SHAFT Trace Report.html\""), index);
            Assert.assertTrue(index.contains("\"json\": \"shaft-trace.json\""), index);
            Assert.assertTrue(index.contains("\"network\": \"shaft-network.har\""), index);
            Assert.assertFalse(index.contains("\"screenshots\""), "No screenshots entry when nothing was buffered: " + index);
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Rendered trace JSON should redact sensitive values and keep source fallback frame")
    public void traceJsonShouldRedactSecretsAndKeepFallbackFrame() throws Exception {
        RuntimeException throwable = failure();
        String json = FailureTraceReporter.renderTraceJson(
                info("failingScenario", throwable),
                "Authorization: Bearer raw-token\ncookie: session=raw-cookie\npassword=raw-password",
                List.of("C:\\secret\\video.mp4"));

        Assert.assertTrue(json.contains("customer.LoginTest.failingScenario(LoginTest.java:27)"));
        Assert.assertTrue(json.contains("********"));
        Assert.assertFalse(json.contains("raw-token"));
        Assert.assertFalse(json.contains("raw-cookie"));
        Assert.assertFalse(json.contains("raw-password"));
    }

    @Test(description = "Trace JSON should include structured action events and clear the recorder")
    public void traceJsonShouldIncludeActionsAndClearRecorder() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.finish(event, "failed", "Click failed token=raw-token",
                    new RuntimeException("boom password=raw-password"),
                    Map.of("apiToken", "raw-token", "visible", "checkout"),
                    List.of("Screenshot token=raw-token"));

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"actions\": ["), json);
            Assert.assertTrue(json.contains("\"category\": \"element\""), json);
            Assert.assertTrue(json.contains("\"name\": \"CLICK\""), json);
            Assert.assertTrue(json.contains("\"status\": \"failed\""), json);
            Assert.assertTrue(json.contains("\"locator\": \"By.id: pay\""), json);
            Assert.assertTrue(json.contains("\"caller\""), json);
            Assert.assertTrue(json.contains("\"durationMs\""), json);
            Assert.assertTrue(json.contains("\"apiToken\": \"********\""), json);
            Assert.assertFalse(json.contains("raw-token"));
            Assert.assertFalse(json.contains("raw-password"));
            Assert.assertTrue(TraceEventRecorder.snapshot().isEmpty(), "renderTraceJson should drain the action recorder.");
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should include actionability diagnostics as a redacted action object")
    public void traceJsonShouldIncludeActionabilityDiagnostics() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            Map<String, Object> actionability = new LinkedHashMap<>();
            actionability.put("locator", "By.id: pay");
            actionability.put("matchCount", 1);
            actionability.put("displayed", true);
            actionability.put("enabled", false);
            actionability.put("textPreview", "password=raw-password");
            actionability.put("css", Map.of("pointerEvents", "auto"));
            actionability.put("obscuringElement", Map.of("selector", ".modal-backdrop"));

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("click intercepted"), Map.of(), List.of(), actionability);

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"actionability\": {"), json);
            Assert.assertTrue(json.contains("\"matchCount\": 1"), json);
            Assert.assertTrue(json.contains("\"displayed\": true"), json);
            Assert.assertTrue(json.contains("\"pointerEvents\": \"auto\""), json);
            Assert.assertTrue(json.contains("\"selector\": \".modal-backdrop\""), json);
            Assert.assertFalse(json.contains("raw-password"), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should include before/after DOM snapshots keyed per action when enabled")
    public void traceJsonShouldIncludeDomSnapshotsWhenEnabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeDomSnapshots(true);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "<html><body>before</body></html>", "<html><body>after</body></html>");

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"domSnapshotBefore\": \"<html><body>before</body></html>\""), json);
            Assert.assertTrue(json.contains("\"domSnapshotAfter\": \"<html><body>after</body></html>\""), json);
            JsonNode session = JSON.readTree(json).path("session");
            JsonNode before = findArtifact(session, "snapshot-action-1-before");
            JsonNode after = findArtifact(session, "snapshot-action-1-after");
            Assert.assertEquals(before.path("kind").asText(), "dom-snapshot", before.toPrettyString());
            Assert.assertEquals(after.path("kind").asText(), "dom-snapshot", after.toPrettyString());
            Assert.assertTrue(before.path("path").asText().matches("resources/[0-9a-f]{64}\\.html"),
                    before.toPrettyString());
            Assert.assertTrue(after.path("path").asText().matches("resources/[0-9a-f]{64}\\.html"),
                    after.toPrettyString());
            Assert.assertEquals(before.path("metadata").path("actionId").asText(), "action-1");
            Assert.assertEquals(before.path("metadata").path("phase").asText(), "before");
            Assert.assertEquals(before.path("metadata").path("provider").asText(), "webdriver");
            Assert.assertEquals(before.path("metadata").path("fidelity").asText(), "structural");
            Assert.assertEquals(before.path("metadata").path("status").asText(), "available");
            Assert.assertEquals(after.path("metadata").path("actionId").asText(), "action-1");
            Assert.assertEquals(after.path("metadata").path("phase").asText(), "after");
            Assert.assertEquals(session.path("events").get(0).path("artifactIds"), JSON.readTree(
                    "[\"snapshot-action-1-before\",\"snapshot-action-1-after\"]"));
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should omit DOM snapshot fields when the property is disabled")
    public void traceJsonShouldOmitDomSnapshotsWhenDisabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeDomSnapshots(false);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "<html><body>before</body></html>", "<html><body>after</body></html>");

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertFalse(json.contains("domSnapshotBefore"), json);
            Assert.assertFalse(json.contains("domSnapshotAfter"), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should embed a screenshot keyed by the action's traceActionId when enabled")
    public void traceJsonShouldEmbedScreenshotKeyedByActionIdWhenEnabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeScreenshots(true);
            byte[] png = "fake-png-bytes".getBytes(StandardCharsets.UTF_8);

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.recordScreenshot(event, png);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"id\": \"action-1\""), json);
            Assert.assertTrue(json.contains("\"screenshot\": \""
                    + Base64.getEncoder().encodeToString(png) + "\""), json);
            JsonNode session = JSON.readTree(json).path("session");
            JsonNode screenshot = findArtifact(session, "screenshot-action-1");
            Assert.assertTrue(screenshot.path("path").asText().matches("resources/[0-9a-f]{64}\\.png"),
                    screenshot.toPrettyString());
            Assert.assertEquals(screenshot.path("metadata").path("sizeBytes").asText(), String.valueOf(png.length));
            Assert.assertFalse(screenshot.path("omitted").asBoolean());
            Assert.assertEquals(session.path("events").get(0).path("artifactIds").get(0).asText(),
                    "screenshot-action-1");
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should omit the screenshot field when the property is disabled")
    public void traceJsonShouldOmitScreenshotWhenDisabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeScreenshots(false);
            byte[] png = "fake-png-bytes".getBytes(StandardCharsets.UTF_8);

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.recordScreenshot(event, png);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertFalse(json.contains("\"screenshot\":"), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Buffered screenshots should persist as PNG files in the trace zip and directory, keyed by action id")
    public void failureModeShouldPersistScreenshotsWhenBuffered() throws Exception {
        TestExecutionInfo failingInfo = info("screenshotScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeScreenshots(true);
            byte[] png = "fake-png-bytes".getBytes(StandardCharsets.UTF_8);

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.recordScreenshot(event, png);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            Path screenshotFile = traceDirectory.resolve("screenshots").resolve("action-1.png");
            Assert.assertTrue(Files.exists(screenshotFile));
            Assert.assertEquals(Files.readAllBytes(screenshotFile), png);

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                JsonNode session = JSON.readTree(zip.getInputStream(zip.getEntry("shaft-trace.json")))
                        .path("session");
                String path = findArtifact(session, "screenshot-action-1").path("path").asText();
                Assert.assertNotNull(zip.getEntry(path));
                Assert.assertEquals(zip.getInputStream(zip.getEntry(path)).readAllBytes(), png);
            }

            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"screenshots\": \"screenshots\""), index);
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    /**
     * Minimal WebDriver + JavascriptExecutor fake returning a different outerHTML snapshot on
     * each successive {@code executeScript} call, simulating DOM state changing between the
     * before (start) and after (finish) capture points of one traced action.
     */
    private static final class RecordingJavascriptExecutorDriver implements WebDriver, org.openqa.selenium.JavascriptExecutor {
        private final List<String> snapshots;
        private int callIndex;

        RecordingJavascriptExecutorDriver(String... snapshots) {
            this.snapshots = List.of(snapshots);
        }

        @Override
        public Object executeScript(String script, Object... args) {
            String snapshot = snapshots.get(Math.min(callIndex, snapshots.size() - 1));
            callIndex++;
            return snapshot;
        }

        @Override
        public Object executeAsyncScript(String script, Object... args) {
            return null;
        }

        @Override
        public void get(String url) {
        }

        @Override
        public String getCurrentUrl() {
            return "https://example.test";
        }

        @Override
        public String getTitle() {
            return "";
        }

        @Override
        public List<WebElement> findElements(By by) {
            return List.of();
        }

        @Override
        public WebElement findElement(By by) {
            return null;
        }

        @Override
        public String getPageSource() {
            return "";
        }

        @Override
        public void close() {
        }

        @Override
        public void quit() {
        }

        @Override
        public java.util.Set<String> getWindowHandles() {
            return java.util.Set.of();
        }

        @Override
        public String getWindowHandle() {
            return "";
        }

        @Override
        public TargetLocator switchTo() {
            return null;
        }

        @Override
        public Navigation navigate() {
            return null;
        }

        @Override
        public Options manage() {
            return null;
        }
    }

    @Test(description = "Failed Appium touch actions should include mobile metadata and redacted native source")
    public void touchTraceShouldIncludeMobileFailureMetadata() throws Exception {
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeNativePageSource(true);
            SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
            SHAFT.Properties.mobile.set()
                    .automationName("UiAutomator2")
                    .appPackage("com.example.checkout")
                    .appActivity(".CheckoutActivity")
                    .bundleId("");
            AndroidDriver driver = mockedAndroidDriver();
            Mockito.doThrow(new WebDriverException("rotation failed"))
                    .when(driver).rotate(ScreenOrientation.LANDSCAPE);

            try {
                new TouchActions(driver).rotate(ScreenOrientation.LANDSCAPE);
                Assert.fail("Expected rotate to report a failed action.");
            } catch (RuntimeException expected) {
                // issue #4341: ElementActionsHelper#failAction throws RuntimeException, not
                // AssertionError, for this broken-action failure.
            }

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"category\": \"touch\""), json);
            Assert.assertTrue(json.contains("\"name\": \"rotate\""), json);
            Assert.assertTrue(json.contains("\"gestureParameters\": \"orientation=LANDSCAPE\""), json);
            Assert.assertTrue(json.contains("\"platformName\": \"Android\""), json);
            Assert.assertTrue(json.contains("\"automationName\": \"UiAutomator2\""), json);
            Assert.assertTrue(json.contains("\"appPackage\": \"com.example.checkout\""), json);
            Assert.assertTrue(json.contains("\"appActivity\": \".CheckoutActivity\""), json);
            Assert.assertTrue(json.contains("\"context\": \"NATIVE_APP\""), json);
            Assert.assertTrue(json.contains("\"orientation\": \"PORTRAIT\""), json);
            Assert.assertTrue(json.contains("\"windowSize\": \"1080x1920\""), json);
            Assert.assertTrue(json.contains("\"nativePageSourceExcerpt\""), json);
            Assert.assertFalse(json.contains("raw-password"), json);
        } finally {
            TraceEventRecorder.clear();
            new DriverFactoryHelper().setDriver(null);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Mobile context switches should be recorded as dedicated trace events")
    public void mobileContextSwitchShouldRecordTraceEvent() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
            AndroidDriver driver = mockedAndroidDriver();
            Mockito.when(driver.getContext()).thenReturn("NATIVE_APP", "WEBVIEW_checkout");

            new BrowserActions(driver).setContext("WEBVIEW_checkout");

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"category\": \"mobile-context\""), json);
            Assert.assertTrue(json.contains("\"name\": \"SET_CONTEXT\""), json);
            Assert.assertTrue(json.contains("\"contextBefore\": \"NATIVE_APP\""), json);
            Assert.assertTrue(json.contains("\"contextAfter\": \"WEBVIEW_checkout\""), json);
        } finally {
            TraceEventRecorder.clear();
            new DriverFactoryHelper().setDriver(null);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should include network, console, and unsupported observability metadata")
    public void traceJsonShouldIncludeBrowserObservabilitySections() throws Exception {
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeNetwork(true)
                    .traceIncludeConsole(true);

            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "POST",
                    "https://example.com/payments?token=raw-token",
                    500,
                    Map.of("Authorization", "Bearer raw-token", "X-Trace", "visible"),
                    Map.of("Set-Cookie", "session=raw-cookie", "Content-Type", "application/json"),
                    42,
                    18,
                    29,
                    "net::ERR_FAILED password=raw-password",
                    "{\"password\":\"raw-password\"}"));
            BrowserObservabilityRecorder.recordConsole("browser", "SEVERE",
                    "Uncaught token=raw-token", 123L);
            BrowserObservabilityRecorder.recordWarning("network", "Network capture is not supported by this driver.");
            BrowserObservabilityRecorder.recordWebSocket(BrowserObservabilityRecorder.captureSession(),
                    new BrowserObservabilityRecorder.WebSocketObservation("socket-1", "wss://example.test/socket",
                            "received", "frame", 1, "hello", "", 5, "available", ""));
            BrowserObservabilityRecorder.recordWebSocket(BrowserObservabilityRecorder.captureSession(),
                    new BrowserObservabilityRecorder.WebSocketObservation("socket-1", "wss://example.test/socket",
                            "sent", "frame", 2, "", "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81",
                            3, "malformed", "CDP WebSocket binary frame was malformed."));

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());
            var root = new tools.jackson.databind.ObjectMapper().readTree(json);

            Assert.assertTrue(json.contains("\"network\": ["), json);
            Assert.assertTrue(json.contains("\"method\": \"POST\""), json);
            Assert.assertTrue(json.contains("\"status\": 500"), json);
            Assert.assertTrue(root.path("evidence").path("network").get(0).path("timestamp").asLong(0L) > 0L,
                    "Network events must carry an epoch timestamp for timeline correlation: " + json);
            Assert.assertTrue(json.contains("\"console\": ["), json);
            Assert.assertTrue(json.contains("\"level\": \"SEVERE\""), json);
            Assert.assertTrue(json.contains("\"browserObservability\""), json);
            Assert.assertTrue(json.contains("Network capture is not supported by this driver."), json);
            Assert.assertEquals(root.path("evidence").path("browserObservability").path("webSockets")
                    .get(0).path("provider").asText(), "cdp", json);
            Assert.assertEquals(root.path("evidence").path("browserObservability").path("webSockets")
                    .get(0).path("text").asText(), "hello", json);
            var binary = root.path("evidence").path("browserObservability").path("webSockets").get(1);
            Assert.assertEquals(binary.path("requestId").asText(), "socket-1", json);
            Assert.assertEquals(binary.path("url").asText(), "wss://example.test/socket", json);
            Assert.assertEquals(binary.path("direction").asText(), "sent", json);
            Assert.assertEquals(binary.path("type").asText(), "frame", json);
            Assert.assertEquals(binary.path("opcode").asInt(), 2, json);
            Assert.assertEquals(binary.path("sha256").asText(),
                    "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81", json);
            Assert.assertEquals(binary.path("sizeBytes").asLong(), 3L, json);
            Assert.assertEquals(binary.path("status").asText(), "malformed", json);
            Assert.assertEquals(binary.path("reason").asText(), "CDP WebSocket binary frame was malformed.", json);
            Assert.assertTrue(binary.path("timestamp").asLong() > 0L, json);
            BrowserObservabilityRecorder.recordWebSocket(BrowserObservabilityRecorder.captureSession(),
                    new BrowserObservabilityRecorder.WebSocketObservation("socket", "", "", "frame", 2,
                            "", "private-value".repeat(1_000), 0, "available", ""));
            Assert.assertTrue(BrowserObservabilityRecorder.snapshotWebSockets(
                    BrowserObservabilityRecorder.captureSession()).getFirst().sha256().isEmpty());
            Assert.assertFalse(json.contains("raw-token"), json);
            Assert.assertFalse(json.contains("raw-cookie"), json);
            Assert.assertFalse(json.contains("raw-password"), json);
        } finally {
            BrowserObservabilityRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Browser observability should expose a HAR-like JSON envelope for Capture")
    public void browserObservabilityShouldDrainHarEnvelope() {
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceIncludeNetwork(true);

            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "GET",
                    "https://example.com/api",
                    200,
                    Map.of(),
                    Map.of("Content-Type", "application/json"),
                    12,
                    0,
                    2,
                    "",
                    "{}"));

            String har = BrowserObservabilityRecorder.drainNetworkHarJson();

            Assert.assertTrue(har.contains("\"version\": \"1.2\""), har);
            Assert.assertTrue(har.contains("\"entries\": ["), har);
            Assert.assertTrue(har.contains("\"method\": \"GET\""), har);
            String drainedAgain = BrowserObservabilityRecorder.drainNetworkHarJson();
            Assert.assertFalse(drainedAgain.contains("\"method\": \"GET\""), drainedAgain);
        } finally {
            BrowserObservabilityRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Selenium network interception should feed trace network events")
    public void networkInterceptorShouldFeedTraceNetworkEvents() throws Exception {
        AtomicReference<Filter> filterReference = new AtomicReference<>();
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterReference.set((Filter) context.arguments().get(1)))) {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeNetwork(true)
                    .traceIncludeConsole(false);
            BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver);
            Assert.assertTrue(interceptor.startObserving());

            HttpRequest request = new HttpRequest(HttpMethod.GET, "https://example.com/api?token=raw-token");
            request.addHeader("Authorization", "Bearer raw-token");
            HttpResponse response = new HttpResponse()
                    .setStatus(503)
                    .addHeader("Set-Cookie", "session=raw-cookie");
            response.setContent(Contents.utf8String("{\"token\":\"raw-token\"}"));

            filterReference.get().apply(ignoredRequest -> response).execute(request);

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());

            Assert.assertTrue(json.contains("\"network\": ["), json);
            Assert.assertTrue(json.contains("\"method\": \"GET\""), json);
            Assert.assertTrue(json.contains("\"status\": 503"), json);
            Assert.assertFalse(json.contains("raw-token"), json);
            Assert.assertFalse(json.contains("raw-cookie"), json);
        } finally {
            BrowserObservabilityRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should include an environment section for advanced debugging context")
    public void traceJsonShouldIncludeEnvironmentSection() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());
            var root = new tools.jackson.databind.ObjectMapper().readTree(json);

            Assert.assertTrue(root.path("environment").isObject(), json);
            Assert.assertEquals(root.path("environment").path("os").asText(), System.getProperty("os.name", ""), json);
            Assert.assertEquals(root.path("environment").path("javaVersion").asText(), System.getProperty("java.version", ""), json);
            Assert.assertFalse(root.path("environment").path("thread").asText().isBlank(), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should embed the full failing source file when it can be resolved")
    public void traceJsonShouldEmbedFullSourceFileWhenResolvable() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceIncludeCodeContext(true);
            RuntimeException throwable = new RuntimeException("boom");
            throwable.setStackTrace(new StackTraceElement[]{
                    new StackTraceElement("testPackage.unitTests.AttachmentReporterUnitTest",
                            "someScenario", "AttachmentReporterUnitTest.java", 20)
            });

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", throwable), "failed", List.of());
            var root = new tools.jackson.databind.ObjectMapper().readTree(json);

            Assert.assertTrue(root.path("source").path("file").asText().endsWith("AttachmentReporterUnitTest.java"), json);
            Assert.assertFalse(root.path("source").path("snippet").asText().isBlank(), json);
            Assert.assertTrue(root.path("source").path("fileContent").asText().contains("class AttachmentReporterUnitTest"),
                    "The full source file should be embedded for self-contained root-cause analysis.");
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace recorder should ignore actions when trace reporting is disabled")
    public void traceRecorderShouldIgnoreActionsWhenTraceIsDisabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(false);
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.finish(event, "passed", "Click passed", null, Map.of(), List.of());

            String json = FailureTraceReporter.renderTraceJson(info("passingScenario", null), "passed", List.of());

            Assert.assertTrue(TraceEventRecorder.snapshot().isEmpty());
            Assert.assertTrue(json.contains("\"actions\": []"), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace recorder should isolate action timelines by thread")
    public void traceRecorderShouldIsolateActionsByThread() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true);
            TraceEventRecorder.record("element", "MAIN_ACTION", "passed", "main-locator",
                    null, "main action", null, Map.of(), List.of());

            AtomicReference<String> otherThreadJson = new AtomicReference<>();
            AtomicReference<Throwable> otherThreadFailure = new AtomicReference<>();
            Thread otherThread = new Thread(() -> {
                try {
                    SHAFT.Properties.reporting.set().traceEnabled(true);
                    TraceEventRecorder.record("browser", "OTHER_ACTION", "passed", "other-locator",
                            null, "other action", null, Map.of(), List.of());
                    otherThreadJson.set(FailureTraceReporter.renderTraceJson(infoUnchecked("otherScenario", failure()), "other", List.of()));
                } catch (Throwable throwable) {
                    otherThreadFailure.set(throwable);
                } finally {
                    TraceEventRecorder.clear();
                    Properties.clearForCurrentThread();
                }
            });
            otherThread.start();
            otherThread.join();
            if (otherThreadFailure.get() != null) {
                throw new AssertionError("Other thread trace rendering failed.", otherThreadFailure.get());
            }

            String mainJson = FailureTraceReporter.renderTraceJson(info("mainScenario", failure()), "main", List.of());

            Assert.assertTrue(mainJson.contains("MAIN_ACTION"), mainJson);
            Assert.assertFalse(mainJson.contains("OTHER_ACTION"), mainJson);
            Assert.assertTrue(otherThreadJson.get().contains("OTHER_ACTION"), otherThreadJson.get());
            Assert.assertFalse(otherThreadJson.get().contains("MAIN_ACTION"), otherThreadJson.get());
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace JSON should include locator health data when locator health is enabled")
    public void traceJsonShouldIncludeLocatorHealthWhenEnabled() throws Exception {
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .locatorHealthEnabled(true)
                    .slowLocatorThresholdMillis(100);
            LocatorHealthReporter.reset();
            LocatorHealthReporter.recordLookup(By.xpath("/html/body/main/button[1]"), 150, 2, 0, true, 0);

            String json = FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed to click button", List.of());

            Assert.assertTrue(json.contains("\"locatorHealth\""));
            Assert.assertTrue(json.contains("\"healthScore\""));
            Assert.assertTrue(json.contains("absolute XPath"));
            Assert.assertTrue(json.contains("data-testid"));
        } finally {
            LocatorHealthReporter.reset();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Trace reporting properties should have failure-safe defaults and setters")
    public void traceReportingPropertiesShouldHaveDefaultsAndSetters() {
        try {
            Assert.assertTrue(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "auto");
            Assert.assertTrue(SHAFT.Properties.reporting.traceRetainFailedAttempts());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeScreenshots());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 50);

            SHAFT.Properties.reporting.set()
                    .traceEnabled(false)
                    .traceMode("always")
                    .traceRetainFailedAttempts(false)
                    .traceIncludeCodeContext(false)
                    .traceIncludeFullPageSnapshots(false)
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false)
                    .traceIncludeNetwork(false)
                    .traceIncludeConsole(false)
                    .traceMaxArtifactMb(7);

            Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "always");
            Assert.assertFalse(SHAFT.Properties.reporting.traceRetainFailedAttempts());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeScreenshots());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 7);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Auto trace mode without configured retries should resolve to failure semantics")
    public void autoModeWithoutRetriesShouldBehaveLikeFailureMode() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("auto");
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);

            Assert.assertEquals(FailureTraceReporter.effectiveTraceMode(), "failure");
            Assert.assertFalse(FailureTraceReporter.shouldAttachTrace(info("autoModeNoRetryPassingScenario", null)),
                    "A passing, non-retried test must not attach a trace under resolved failure semantics.");
            Assert.assertTrue(FailureTraceReporter.shouldAttachTrace(info("autoModeNoRetryFailingScenario", failure())),
                    "A failing test must still attach a trace under resolved failure semantics.");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Auto trace mode should promote to retry semantics when retries are configured")
    public void autoModeWithRetriesShouldPromoteToRetrySemantics() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("auto");
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

            Assert.assertEquals(FailureTraceReporter.effectiveTraceMode(), "retry");
            TestExecutionInfo retriedPassingInfo = info("autoModeWithRetryPassingScenario", null, true);
            Assert.assertTrue(FailureTraceReporter.shouldAttachTrace(retriedPassingInfo),
                    "A retried passing test must attach a trace once auto mode resolves to retry semantics.");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Explicit failure trace mode should not auto-promote even when retries are configured")
    public void explicitFailureModeShouldNotAutoPromoteWithRetries() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

            Assert.assertEquals(FailureTraceReporter.effectiveTraceMode(), "failure",
                    "An explicit trace mode value must be honored unchanged, never auto-promoted.");
            TestExecutionInfo retriedPassingInfo = info("explicitFailureModeRetryPassingScenario", null, true);
            Assert.assertFalse(FailureTraceReporter.shouldAttachTrace(retriedPassingInfo),
                    "Explicit failure mode must not attach traces for retried passing tests.");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Retained failed attempts should coexist with the latest attempt archive and be recorded in attempts history")
    public void retriesShouldRetainFailedAttemptArchivesWhenEnabled() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        TestExecutionInfo failingAttempt = info("attemptRetentionScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingAttempt);
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("retry").traceRetainFailedAttempts(true);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

            FailureTraceReporter.attachOnFailure(failingAttempt, "attempt one failed", List.of());
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace-attempt-1.zip")),
                    "The failed first attempt bundle should be retained under its attempt-indexed name.");
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace.zip")));

            TestExecutionInfo passingRetry = info("attemptRetentionScenario", null, true);
            FailureTraceReporter.attachOnFailure(passingRetry, "attempt two passed", List.of());

            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace-attempt-1.zip")),
                    "The retained failed attempt bundle must survive a later passing retry.");
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace.zip")),
                    "The root archive should always reflect the latest attempt.");
            try (ZipFile failed = new ZipFile(traceDirectory.resolve("shaft-trace-attempt-1.zip").toFile());
                 ZipFile latest = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                String failedJson = readZipEntry(failed, "shaft-trace.json");
                String latestJson = readZipEntry(latest, "shaft-trace.json");
                Assert.assertTrue(failedJson.contains("attempt one failed"), failedJson);
                Assert.assertFalse(failedJson.contains("attempt two passed"), failedJson);
                Assert.assertTrue(latestJson.contains("attempt two passed"), latestJson);
                Assert.assertFalse(latestJson.contains("attempt one failed"), latestJson);
            }

            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"attempt\": \"2\""), index);
            Assert.assertTrue(index.contains("\"retried\": \"true\""), index);
            Assert.assertTrue(index.contains("\"attempts\": ["), index);
            Assert.assertTrue(index.contains("\"attempt\": 1"), index);
            Assert.assertTrue(index.contains("\"attempt\": 2"), index);
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Persisted trace JSON should use private v3 evidence without changing the public v2 session")
    public void persistedTraceShouldUsePrivateV3WithoutChangingPublicV2Contract() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        TraceEventRecorder.record("element", "click", "passed", "id=pay", null, "clicked", null,
                Map.of("context", "web"), List.of());

        String json = FailureTraceReporter.renderTraceJson(info("v2SchemaScenario", failure()), "log", List.of());

        JsonNode root = JSON.readTree(json);
        Assert.assertEquals(root.path("schemaVersion").asText(), "3.0");
        JsonNode session = root.path("session");
        Assert.assertEquals(session.path("schemaVersion").asText(), "2.0");
        Assert.assertEquals(session.path("backend").asText(), "UNKNOWN");
        Assert.assertEquals(session.path("attempt").asInt(), 1);
        JsonNode event = session.path("events").get(0);
        Assert.assertTrue(event.path("id").asText().endsWith("/action-1"), event.toPrettyString());
        Assert.assertEquals(event.path("backend").asText(), "UNKNOWN");
        Assert.assertEquals(event.path("category").asText(), "element");
        Assert.assertEquals(event.path("name").asText(), "click");
        Assert.assertEquals(event.path("status").asText(), "PASSED");
        Assert.assertFalse(event.path("startedAt").asText().isBlank());
        Assert.assertTrue(event.path("durationMs").asLong() >= 0);
        Assert.assertTrue(event.has("source"));
        Assert.assertEquals(event.path("target").asText(), "id=pay");
        Assert.assertEquals(event.path("message").asText(), "clicked");
        Assert.assertEquals(event.path("metadata").path("context").asText(), "web");
        Assert.assertEquals(session.path("artifacts").get(0).path("id").asText(), "network");

        JsonNode evidence = root.path("evidence");
        Assert.assertTrue(evidence.path("actions").isArray(), root.toPrettyString());
        Assert.assertTrue(evidence.path("network").isArray(), root.toPrettyString());
        Assert.assertTrue(evidence.path("console").isArray(), root.toPrettyString());
        Assert.assertEquals(evidence.path("playwright").path("status").asText(), "unavailable",
                root.toPrettyString());
        Assert.assertTrue(evidence.path("playwright").path("actions").isEmpty(), root.toPrettyString());
        Assert.assertTrue(evidence.path("playwright").path("correlations").isEmpty(), root.toPrettyString());
        Assert.assertEquals(evidence.path("actions").get(0).path("id").asText(), "action-1");
        Assert.assertFalse(root.has("actions"), "v3 must have one canonical action location: " + root);
        Assert.assertFalse(root.has("network"), "v3 must have one canonical network location: " + root);
        Assert.assertFalse(root.has("console"), "v3 must have one canonical console location: " + root);
        Assert.assertFalse(root.has("browserObservability"),
                "v3 must have one canonical observability location: " + root);
        Assert.assertTrue(evidence.path("browserObservability").path("warnings").isArray(), root.toPrettyString());
    }

    @Test(description = "Automatic terminal snapshots should avoid unbounded CDP MHTML and persist structural evidence")
    public void terminalSnapshotShouldOmitUnboundedCdpMhtmlCapability() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalNativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("auto")
                    .traceIncludeFullPageSnapshots(true)
                    .traceIncludeNativePageSource(true);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            Mockito.when(((HasCdp) augmentedRemote)
                            .executeCdpCommand(Mockito.eq("Page.captureSnapshot"), Mockito.anyMap()))
                    .thenReturn(Map.of("data", "From: <Saved by Blink>\nresource-complete"));
            Mockito.when(augmentedRemote.getPageSource()).thenReturn("<html>structural fallback</html>");
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("provider").asText(), "webdriver", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("fidelity").asText(), "structural", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("status").asText(), "available", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("content").asText(),
                    "<html>structural fallback</html>", snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("reason").asText().contains("no enforceable response-size bound"),
                    snapshot.toPrettyString());
            Mockito.verify(augmentedRemote).getPageSource();
            Mockito.verify((HasCdp) augmentedRemote, Mockito.never())
                    .executeCdpCommand(Mockito.anyString(), Mockito.anyMap());
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            SHAFT.Properties.reporting.set()
                    .traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeNativePageSource(originalNativeSource);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Action DOM snapshot resources should be published with their exact phase bytes")
    public void failureModeShouldPersistActionDomSnapshotResources() throws Exception {
        TestExecutionInfo failingInfo = info("domResourceScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "<html><body>before resource</body></html>",
                    "<html><body>after resource</body></html>");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                JsonNode session = JSON.readTree(zip.getInputStream(zip.getEntry("shaft-trace.json")))
                        .path("session");
                String beforePath = findArtifact(session, "snapshot-action-1-before").path("path").asText();
                String afterPath = findArtifact(session, "snapshot-action-1-after").path("path").asText();
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry(beforePath)).readAllBytes(),
                        StandardCharsets.UTF_8), "<html><body>before resource</body></html>");
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry(afterPath)).readAllBytes(),
                        StandardCharsets.UTF_8), "<html><body>after resource</body></html>");
            }
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Action DOM snapshots should redact before their bounded cutoff")
    public void actionDomSnapshotsShouldRedactBeforeTruncation() throws Exception {
        String secret = "ACTION_BOUNDARY_SECRET";
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true);
            FailureTraceReporter.registerSensitiveSourceValue(secret);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "x".repeat(199_990) + secret + "tail", "<html>after</html>");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());

            JsonNode action = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of()))
                    .path("evidence").path("actions").get(0);

            Assert.assertFalse(action.path("domSnapshotBefore").asText().contains(secret), action.toPrettyString());
            Assert.assertFalse(action.path("domSnapshotBefore").asText().endsWith("ACTION_BOU"),
                    action.toPrettyString());
        } finally {
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Report-time sensitive values should resanitize earlier action DOM snapshots")
    public void actionDomSnapshotsShouldResanitizeAtReportTime() throws Exception {
        TestExecutionInfo failingInfo = info("lateSensitiveDomScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        String secret = "LATE_REGISTERED_ACTION_SECRET";
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "<html>" + secret + " before</html>", "<html>" + secret + " after</html>");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            FailureTraceReporter.registerSensitiveSourceValue(secret);
            TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                String json = readZipEntry(zip, "shaft-trace.json");
                Assert.assertFalse(json.contains(secret), json);
                JsonNode root = JSON.readTree(json);
                Assert.assertTrue(root.path("evidence").path("actions").get(0)
                        .path("domSnapshotBefore").asText().contains("********"), json);
                JsonNode session = root.path("session");
                int availableDomArtifacts = 0;
                for (JsonNode artifact : session.path("artifacts")) {
                    if (!"dom-snapshot".equals(artifact.path("kind").asText()) || artifact.path("omitted").asBoolean()) {
                        continue;
                    }
                    availableDomArtifacts++;
                    Assert.assertTrue(artifact.path("id").asText().equals("snapshot-action-1-before")
                            || artifact.path("id").asText().equals("snapshot-action-1-after"), artifact.toPrettyString());
                    String content = readZipEntry(zip, artifact.path("path").asText());
                    Assert.assertFalse(content.contains(secret), content);
                    Assert.assertTrue(content.contains("********"), content);
                }
                Assert.assertEquals(availableDomArtifacts, 2);
            }
        } finally {
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Minimum-budget archive should compact action evidence consistently")
    public void actionOverflowShouldStillPublishWithinMinimumBudget() throws Exception {
        Path archive = Files.createTempFile("shaft-action-budget-", ".zip");
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "3.0");
        ObjectNode session = root.putObject("session");
        session.put("schemaVersion", "2.0");
        session.put("id", "session-budget");
        var events = session.putArray("events");
        var artifacts = session.putArray("artifacts");
        ObjectNode evidence = root.putObject("evidence");
        var actions = evidence.putArray("actions");
        evidence.putArray("network");
        evidence.putArray("console");
        evidence.putObject("browserObservability");
        evidence.putObject("playwright");
        root.putObject("snapshot").put("content", "");
        int totalActions = 12_000;
        for (int index = 0; index < totalActions; index++) {
            ObjectNode action = actions.addObject();
            action.put("id", "action-" + index);
            action.put("name", index == totalActions - 1 ? "omitted-actions" : "click-" + index);
            action.put("message", "x".repeat(128));
            ObjectNode event = events.addObject();
            event.put("id", "session/action-" + index);
            event.put("name", index == totalActions - 1 ? "omitted-actions" : "click-" + index);
            event.put("message", "x".repeat(128));
            event.putArray("artifactIds").add("snapshot-action-" + index + "-before");
            ObjectNode artifact = artifacts.addObject();
            artifact.put("id", "snapshot-action-" + index + "-before");
            artifact.put("kind", "dom-snapshot");
            artifact.put("path", "resources/" + String.format("%064x", index) + ".html");
            artifact.put("mimeType", "text/html");
            artifact.put("omitted", true);
            artifact.putObject("metadata").put("actionId", "action-" + index)
                    .put("reason", "x".repeat(64));
        }
        try {
            List<String> plannedOmissions = new ArrayList<>();
            for (int index = 0; index < totalActions; index++) {
                plannedOmissions.add("resources/" + String.format("%064x", index) + ".html");
            }
            String prunedOmission = plannedOmissions.getLast();
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(
                    archive, JSON.writeValueAsString(root), "[]", Map.of(), null,
                    1024L * 1024L, 4L * 1024L * 1024L, "omitted", plannedOmissions);
            JsonNode compacted = JSON.readTree(bundle.json());
            JsonNode compactActions = compacted.path("evidence").path("actions");
            JsonNode compactEvents = compacted.path("session").path("events");
            JsonNode compactArtifacts = compacted.path("session").path("artifacts");
            Assert.assertTrue(compactActions.size() < totalActions, String.valueOf(compactActions.size()));
            Assert.assertEquals(compactActions.get(compactActions.size() - 1).path("name").asText(),
                    "omitted-actions");
            int retainedRealActions = compactActions.size() - 1;
            Assert.assertEquals(compactActions.get(compactActions.size() - 1)
                    .path("metadata").path("omittedCount").asInt(), totalActions - retainedRealActions);
            Assert.assertEquals(compactEvents.get(compactEvents.size() - 1).path("name").asText(),
                    "omitted-actions");
            JsonNode eventMarker = compactEvents.get(compactEvents.size() - 1);
            Assert.assertEquals(eventMarker.path("id").asText(), "session-budget/action-budget");
            Assert.assertFalse(eventMarker.path("startedAt").asText().isBlank(), eventMarker.toPrettyString());
            Instant.parse(eventMarker.path("startedAt").asText());
            Assert.assertEquals(eventMarker.path("durationMs").asLong(-1), 0L);
            Assert.assertTrue(eventMarker.has("source"), eventMarker.toPrettyString());
            Assert.assertTrue(eventMarker.has("target"), eventMarker.toPrettyString());
            Assert.assertTrue(compactArtifacts.size() <= compactEvents.size() - 1,
                    compactArtifacts.toPrettyString());
            Assert.assertFalse(bundle.omitted().contains(prunedOmission), bundle.omitted().toString());
            try (ZipFile zip = new ZipFile(archive.toFile())) {
                Assert.assertTrue(zip.getEntry("shaft-trace.json").getSize() <= 1024L * 1024L);
                Assert.assertTrue(zip.getEntry("SHAFT Trace Report.html").getSize() <= 1024L * 1024L);
            }
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test(description = "Report-time resanitization should preserve truncated action snapshot metadata")
    public void actionDomResanitizationShouldPreserveTruncatedMetadata() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "x".repeat(201_100), "<html>after</html>");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());

            JsonNode artifacts = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("truncatedActionDom", failure()), "failed", List.of()))
                    .path("session").path("artifacts");
            JsonNode before = findArtifact(JSON.createObjectNode().set("artifacts", artifacts),
                    "snapshot-action-1-before");
            Assert.assertEquals(before.path("metadata").path("status").asText(), "truncated",
                    before.toPrettyString());
            Assert.assertTrue(before.path("metadata").path("truncated").asBoolean(), before.toPrettyString());
            Assert.assertFalse(before.path("metadata").path("reason").asText().isBlank(), before.toPrettyString());
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Action DOM capture should enforce one cumulative per-report byte budget")
    public void actionDomSnapshotsShouldRespectCumulativeBudget() throws Exception {
        int originalMax = SHAFT.Properties.reporting.traceMaxArtifactMb();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true).traceMaxArtifactMb(1);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "1" + "x".repeat(199_999), "2" + "x".repeat(199_999),
                    "3" + "x".repeat(199_999), "4" + "x".repeat(199_999),
                    "5" + "x".repeat(199_999), "6" + "x".repeat(199_999));
            for (int index = 0; index < 3; index++) {
                TraceEventRecorder.Event event = TraceEventRecorder.start(
                        "element", "CLICK", By.id("pay-" + index), driver);
                TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());
            }

            JsonNode artifacts = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of()))
                    .path("session").path("artifacts");
            long retainedBytes = 0;
            int omitted = 0;
            for (JsonNode artifact : artifacts) {
                if (!"dom-snapshot".equals(artifact.path("kind").asText())) {
                    continue;
                }
                if ("omitted-budget".equals(artifact.path("metadata").path("status").asText())) {
                    omitted++;
                } else {
                    retainedBytes += artifact.path("metadata").path("sizeBytes").asLong();
                }
            }
            Assert.assertTrue(omitted > 0, artifacts.toPrettyString());
            Assert.assertTrue(retainedBytes <= 1024L * 1024L, artifacts.toPrettyString());
        } finally {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(originalMax);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Action recording should stay bounded and report omitted actions")
    public void actionRecorderShouldBoundAndReportOverflow() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(false);
            for (int index = 0; index <= 10_000; index++) {
                TraceEventRecorder.Event event = TraceEventRecorder.startForBackend(
                        "element", "click-" + index, "button", AutomationBackend.MICROSOFT_PLAYWRIGHT);
                TraceEventRecorder.finish(event, "passed", "clicked", null, Map.of(), List.of());
            }

            String json = FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of());
            JsonNode actions = JSON.readTree(json).path("evidence").path("actions");

            Assert.assertEquals(actions.size(), 10_000, actions.size());
            Assert.assertEquals(actions.get(0).path("name").asText(), "click-0");
            Assert.assertEquals(actions.get(actions.size() - 2).path("name").asText(), "click-9998");
            Assert.assertEquals(actions.get(actions.size() - 1).path("name").asText(), "omitted-actions");
            Assert.assertTrue(actions.get(actions.size() - 1).path("message").asText().contains("limit"));
            Assert.assertEquals(actions.get(actions.size() - 1).path("metadata").path("omitted").asText(),
                    "newest-tail");
            Assert.assertEquals(actions.get(actions.size() - 1).path("metadata").path("omittedCount").asInt(), 2);

            Path archive = Files.createTempFile("shaft-action-limit-budget-", ".zip");
            try {
                FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(
                        archive, json, "[]", Map.of(), null, 1024L * 1024L, 4L * 1024L * 1024L,
                        "omitted", List.of());
                JsonNode compacted = JSON.readTree(bundle.json());
                JsonNode compactActions = compacted.path("evidence").path("actions");
                JsonNode marker = compactActions.get(compactActions.size() - 1);
                int retainedRealActions = compactActions.size() - 1;
                Assert.assertEquals(marker.path("name").asText(), "omitted-actions");
                Assert.assertEquals(marker.path("metadata").path("omittedCount").asInt(),
                        10_001 - retainedRealActions, marker.toPrettyString());
            } finally {
                Files.deleteIfExists(archive);
            }
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Required entry convergence should move large inline DOM evidence to resources")
    public void expandedInlineActionDomShouldConvergeToResourceOnly() throws Exception {
        Path archive = Files.createTempFile("shaft-inline-dom-budget-", ".zip");
        String json = """
                {"schemaVersion":"3.0","session":{"schemaVersion":"2.0","events":[],"artifacts":[]},
                "snapshot":{"content":""},"evidence":{"actions":[{"id":"action-1",
                "domSnapshotBefore":"%s","domSnapshotAfter":"<html>after</html>"}],
                "network":[],"console":[],"browserObservability":{},"playwright":{}}}
                """.formatted("&".repeat(220_000));
        String html = "&".repeat(220_000);
        byte[] bytes = html.getBytes(StandardCharsets.UTF_8);
        SeleniumTraceCapture.Result result = new SeleniumTraceCapture.Result(
                "webdriver", "structural", "available", "", "action-dom-snapshot", html, false);
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), List.of(
                new TraceArtifactManifest.SnapshotResource(
                        "snapshot-action-1-before", "action-1", "before", result, bytes)),
                null, 1024L * 1024L, "omitted")) {
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(
                    archive, json, "[]", Map.of(), manifest,
                    1024L * 1024L, 4L * 1024L * 1024L, "omitted", List.of());
            JsonNode action = JSON.readTree(bundle.json()).path("evidence").path("actions").get(0);
            Assert.assertTrue(action.path("domSnapshotBefore").asText().isEmpty(), action.toPrettyString());
            Assert.assertTrue(action.path("domSnapshotAfter").asText().isEmpty(), action.toPrettyString());
            Assert.assertEquals(action.path("domSnapshotInlineStatus").asText(), "resource-only");
            try (ZipFile zip = new ZipFile(archive.toFile())) {
                Assert.assertTrue(zip.getEntry("shaft-trace.json").getSize() <= 1024L * 1024L);
                Assert.assertTrue(zip.getEntry("SHAFT Trace Report.html").getSize() <= 1024L * 1024L);
            }
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test(description = "Shared omitted DOM paths should retain each reference's action and phase ownership")
    public void sharedDomOmissionShouldPreserveReferenceOwnership() {
        String path = "resources/" + "a".repeat(64) + ".html";
        List<TraceArtifactReference> references = List.of(
                new TraceArtifactReference("snapshot-action-1-before", "dom-snapshot", path, "text/html", true,
                        Map.of("actionId", "action-1", "phase", "before", "omissionReason", "aggregate")),
                new TraceArtifactReference("snapshot-action-1-after", "dom-snapshot", path, "text/html", true,
                        Map.of("actionId", "action-1", "phase", "after", "omissionReason", "aggregate")));
        String json = """
                {"session":{"artifacts":[
                {"id":"snapshot-action-1-before","path":"%s","omitted":false,"metadata":{}},
                {"id":"snapshot-action-1-after","path":"%s","omitted":false,"metadata":{}}]}}
                """.formatted(path, path);

        JsonNode artifacts = JSON.readTree(FailureTraceReporter.reconcileArtifactOmissions(json, references))
                .path("session").path("artifacts");

        Assert.assertEquals(artifacts.get(0).path("metadata").path("phase").asText(), "before");
        Assert.assertEquals(artifacts.get(1).path("metadata").path("phase").asText(), "after");
        Assert.assertEquals(artifacts.get(0).path("metadata").path("actionId").asText(), "action-1");
        Assert.assertTrue(artifacts.get(0).path("omitted").asBoolean());
        Assert.assertTrue(artifacts.get(1).path("omitted").asBoolean());
    }

    @Test(description = "Aggregate DOM omission should replace a stale resource-only inline status")
    public void aggregateDomOmissionShouldRefreshInlineStatus() {
        String path = "resources/" + "b".repeat(64) + ".html";
        List<TraceArtifactReference> references = List.of(new TraceArtifactReference(
                "snapshot-action-1-before", "dom-snapshot", path, "text/html", true,
                Map.of("actionId", "action-1", "phase", "before", "omissionReason", "aggregate")));
        String json = """
                {"session":{"artifacts":[{"id":"snapshot-action-1-before","path":"%s",
                "omitted":false,"metadata":{}}]},"evidence":{"actions":[{"id":"action-1",
                "domSnapshotBefore":"","domSnapshotAfter":"","domSnapshotInlineStatus":"resource-only"}]}}
                """.formatted(path);

        JsonNode action = JSON.readTree(FailureTraceReporter.reconcileArtifactOmissions(json, references))
                .path("evidence").path("actions").get(0);

        Assert.assertEquals(action.path("domSnapshotInlineStatus").asText(), "omitted-budget",
                action.toPrettyString());
        Assert.assertTrue(action.path("domSnapshotInlineReason").asText().contains("resource"),
                action.toPrettyString());
    }

    @Test(description = "Sensitive suppression should discard captured action DOM sidecar resources")
    public void sensitiveTraceShouldSuppressActionDomResources() throws Exception {
        TestExecutionInfo failingInfo = info("sensitiveDomResourceScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        String secret = "PRIVATE_ACTION_DOM_SECRET";
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true);
            RecordingJavascriptExecutorDriver driver = new RecordingJavascriptExecutorDriver(
                    "<html>" + secret + " before</html>", "<html>" + secret + " after</html>");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), driver);
            TraceEventRecorder.finish(event, "failed", "Click failed", failure(), Map.of(), List.of());
            FailureTraceReporter.suppressSensitiveBrowserArtifacts();

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                String json = readZipEntry(zip, "shaft-trace.json");
                JsonNode artifacts = JSON.readTree(json).path("session").path("artifacts");
                Assert.assertFalse(json.contains(secret), json);
                for (JsonNode artifact : artifacts) {
                    Assert.assertNotEquals(artifact.path("kind").asText(), "dom-snapshot",
                            artifacts.toPrettyString());
                }
                Assert.assertEquals(zip.stream().filter(entry -> entry.getName().startsWith("resources/")
                        && entry.getName().endsWith(".html")).count(), 0L);
            }
        } finally {
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Terminal Selenium snapshots should report structural fallback after bounded CDP failure")
    public void terminalSnapshotShouldReportBoundedStructuralFallback() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalNativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeFullPageSnapshots(true)
                    .traceIncludeNativePageSource(true);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            Mockito.when(augmentedRemote.getPageSource()).thenReturn("<html>structural fallback</html>");
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("provider").asText(), "webdriver", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("fidelity").asText(), "structural", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("status").asText(), "available", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("content").asText(),
                    "<html>structural fallback</html>", snapshot.toPrettyString());
            Mockito.verify((HasCdp) augmentedRemote, Mockito.never())
                    .executeCdpCommand(Mockito.anyString(), Mockito.anyMap());
            Mockito.verify(augmentedRemote).getPageSource();
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set()
                    .traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeNativePageSource(originalNativeSource);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Disabling full-page snapshots should prevent CDP MHTML while retaining structural source")
    public void terminalSnapshotShouldHonorIndependentFullPageGate() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalNativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(false).traceIncludeNativePageSource(true);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            Mockito.when(augmentedRemote.getPageSource()).thenReturn("<html>structural only</html>");
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("provider").asText(), "webdriver", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("type").asText(), "webdriver-page-source", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("content").asText(), "<html>structural only</html>");
            Mockito.verify((HasCdp) augmentedRemote, Mockito.never())
                    .executeCdpCommand(Mockito.eq("Page.captureSnapshot"), Mockito.anyMap());
            Mockito.verify(augmentedRemote).getPageSource();
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeNativePageSource(originalNativeSource);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Resource-complete capture should degrade before CDP when the browser estimate exceeds the cap")
    public void terminalSnapshotShouldPreflightResourceCompleteProviderWork() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            Mockito.when(augmentedRemote.getPageSource()).thenReturn("<html>bounded structural fallback</html>");
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("provider").asText(), "webdriver", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("fidelity").asText(), "structural", snapshot.toPrettyString());
            Mockito.verify((HasCdp) augmentedRemote, Mockito.never())
                    .executeCdpCommand(Mockito.anyString(), Mockito.anyMap());
            Mockito.verify(augmentedRemote).getPageSource();
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(originalFullPage);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Bounded terminal snapshots should redact before truncation and report partial fidelity")
    public void terminalSnapshotShouldRedactBeforeHonestTruncation() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalNativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        String secret = "BOUNDARY_SECRET_VALUE";
        try {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeFullPageSnapshots(true)
                    .traceIncludeNativePageSource(true);
            FailureTraceReporter.registerSensitiveSourceValue(secret);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            String mhtml = "x".repeat(199_990) + secret + "tail";
            Mockito.when(augmentedRemote.getPageSource()).thenReturn(mhtml);
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("provider").asText(), "webdriver", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("fidelity").asText(), "partial", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("status").asText(), "truncated", snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("truncated").asBoolean(), snapshot.toPrettyString());
            Assert.assertFalse(snapshot.path("content").asText().contains(secret), snapshot.toPrettyString());
            Assert.assertFalse(snapshot.path("content").asText().endsWith("BOUNDARY_S"), snapshot.toPrettyString());
            Assert.assertFalse(snapshot.path("content").asText().contains("ECRET_VALUE"), snapshot.toPrettyString());
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set()
                    .traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeNativePageSource(originalNativeSource);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "A truncated snapshot should fail closed for an unterminated sensitive field")
    public void terminalSnapshotShouldOmitUnterminatedSensitiveBoundary() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver augmentedRemote = Mockito.mock(WebDriver.class,
                    Mockito.withSettings().extraInterfaces(HasCdp.class));
            String mhtml = "x".repeat(199_990) + "\"password\":\"" + "s".repeat(5_000) + "\"";
            Mockito.when(augmentedRemote.getPageSource()).thenReturn(mhtml);
            helper.setDriver(augmentedRemote);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "omitted-sensitive-boundary",
                    snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), snapshot.toPrettyString());
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(originalFullPage);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Escaped quotes must not terminate a sensitive field at the snapshot boundary")
    public void terminalSnapshotShouldOmitEscapedSensitiveBoundary() throws Exception {
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver driver = Mockito.mock(WebDriver.class);
            String source = "x".repeat(199_000) + "\"password\":\"aaa\\\""
                    + "s".repeat(500) + "\"" + "tail".repeat(500);
            Mockito.when(driver.getPageSource()).thenReturn(source);
            helper.setDriver(driver);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "omitted-sensitive-boundary",
                    snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), snapshot.toPrettyString());
        } finally {
            helper.setDriver(null);
            SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(originalFullPage);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Normal-sized structural snapshots should fail closed for escaped sensitive delimiters")
    public void terminalSnapshotShouldOmitEscapedSensitiveContentWithoutTruncation() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver driver = Mockito.mock(WebDriver.class);
            Mockito.when(driver.getPageSource())
                    .thenReturn("<script>{\"password\":\"aaa\\\"SECRET_SUFFIX\"}</script>");
            helper.setDriver(driver);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "omitted-sensitive-boundary",
                    snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), snapshot.toPrettyString());
        } finally {
            helper.setDriver(null);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "A normal host port at the end of structural HTML should not be treated as credentials")
    public void terminalSnapshotShouldRetainNormalHostPort() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver driver = Mockito.mock(WebDriver.class);
            Mockito.when(driver.getPageSource()).thenReturn("service=http://localhost:8080");
            helper.setDriver(driver);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "available", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("content").asText(), "service=http://localhost:8080");
        } finally {
            helper.setDriver(null);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "A URL credential split by the structural snapshot bound should fail closed")
    public void terminalSnapshotShouldOmitTruncatedUrlCredentials() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeFullPageSnapshots(true);
            WebDriver driver = Mockito.mock(WebDriver.class);
            String source = "x".repeat(199_990) + " href=https://user:"
                    + "s".repeat(2_000) + "@example.test/resource";
            Mockito.when(driver.getPageSource()).thenReturn(source);
            helper.setDriver(driver);

            JsonNode snapshot = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("failingScenario", failure()), "failed", List.of())).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "omitted-sensitive-boundary",
                    snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), snapshot.toPrettyString());
        } finally {
            helper.setDriver(null);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Snapshot HTML expansion should omit payload instead of rejecting required trace entries")
    public void expandedTerminalSnapshotShouldConvergeToBudgetOmission() throws Exception {
        Path archive = Files.createTempFile("shaft-snapshot-budget-", ".zip");
        String json = """
                {"schemaVersion":"3.0","session":{"schemaVersion":"2.0","events":[],"artifacts":[]},
                "snapshot":{"provider":"webdriver","fidelity":"structural","status":"available","reason":"",
                "type":"webdriver-page-source","content":"%s","byteCount":"199000","truncated":"false"},
                "evidence":{"actions":[],"network":[],"console":[],"browserObservability":{},"playwright":{}}}
                """.formatted("&".repeat(199_000));
        try {
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(
                    archive, json, "[]", Map.of(), null,
                    1024L * 1024L, 4L * 1024L * 1024L, "omitted", List.of());
            JsonNode snapshot = JSON.readTree(bundle.json()).path("snapshot");

            Assert.assertEquals(snapshot.path("status").asText(), "omitted-budget", snapshot.toPrettyString());
            Assert.assertEquals(snapshot.path("fidelity").asText(), "omitted", snapshot.toPrettyString());
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), snapshot.toPrettyString());
            try (ZipFile zip = new ZipFile(archive.toFile())) {
                Assert.assertTrue(zip.getEntry("shaft-trace.json").getSize() <= 1024L * 1024L);
                Assert.assertTrue(zip.getEntry("SHAFT Trace Report.html").getSize() <= 1024L * 1024L);
            }
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test(description = "Persisted private v3 evidence should import and correlate the active Playwright trace")
    public void persistedTraceShouldImportAndCorrelatePlaywrightEvidence() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        var event = TraceEventRecorder.startForBackend("element", "click", "getByRole(button)",
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        TraceEventRecorder.finish(event, "passed", "clicked", null, Map.of(), List.of());
        long actionStart = Instant.parse(TraceEventRecorder.snapshot().getFirst().startTime()).toEpochMilli();
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":" + actionStart + ",\"monotonicTime\":100}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":100,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"title\":\"Click Save\","
                        + "\"params\":{},\"stepId\":\"step@1\",\"beforeSnapshot\":\"before@call@1\"}\n"
                        + "{\"type\":\"log\",\"callId\":\"call@1\",\"message\":\"attempting click\"}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@1\",\"endTime\":110,"
                        + "\"afterSnapshot\":\"after@call@1\"}\n");
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class);
             MockedStatic<PlaywrightTraceImporter> importer = Mockito.mockStatic(PlaywrightTraceImporter.class,
                     Mockito.CALLS_REAL_METHODS)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(archive);
            importer.when(() -> PlaywrightTraceImporter.importTrace(Mockito.any(Path.class), Mockito.anyList()))
                    .thenAnswer(invocation -> {
                        Files.deleteIfExists(archive);
                        return invocation.callRealMethod();
                    });

            JsonNode root = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("playwrightImportScenario", failure()), "log", List.of()));

            JsonNode playwright = root.path("evidence").path("playwright");
            Assert.assertEquals(playwright.path("status").asText(), "available", root.toPrettyString());
            Assert.assertEquals(playwright.path("actions").size(), 1, root.toPrettyString());
            Assert.assertEquals(playwright.path("actions").get(0).path("callId").asText(), "call@1");
            Assert.assertEquals(playwright.path("actions").get(0).path("logs").get(0).asText(),
                    "attempting click");
            Assert.assertEquals(playwright.path("correlations").size(), 1, root.toPrettyString());
            Assert.assertEquals(playwright.path("correlations").get(0).path("shaftActionId").asText(), "action-1");
            Assert.assertEquals(playwright.path("correlations").get(0).path("playwrightCallId").asText(), "call@1");
            Assert.assertEquals(root.path("evidence").path("actions").get(0).path("metadata")
                    .path("playwrightCallId").asText(), "call@1");
            Assert.assertEquals(root.path("session").path("schemaVersion").asText(), "2.0");
            Assert.assertEquals(root.path("session").path("events").get(0).path("metadata")
                    .path("playwrightCallId").asText(), "call@1");
            traceManager.verify(PlaywrightTraceManager::getLastTracePath, Mockito.times(1));
            Assert.assertFalse(Files.exists(archive),
                    "The original generation must be removable after manifest staging and before import.");
        } finally {
            Files.deleteIfExists(archive);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Unsupported Playwright traces should fail soft without losing SHAFT actions")
    public void unsupportedPlaywrightTraceShouldRemainExplicitAndKeepShaftActions() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        var event = TraceEventRecorder.startForBackend("element", "click", "getByRole(button)",
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        TraceEventRecorder.finish(event, "passed", "clicked", null, Map.of(), List.of());
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":7,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n");
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(archive);

            JsonNode root = JSON.readTree(FailureTraceReporter.renderTraceJson(
                    info("malformedPlaywrightScenario", failure()), "log", List.of()));

            JsonNode playwright = root.path("evidence").path("playwright");
            Assert.assertEquals(playwright.path("status").asText(), "unsupported", root.toPrettyString());
            Assert.assertEquals(playwright.path("reason").asText(),
                    "Playwright native trace version is unsupported.");
            Assert.assertTrue(playwright.path("actions").isEmpty(), root.toPrettyString());
            Assert.assertTrue(playwright.path("correlations").isEmpty(), root.toPrettyString());
            Assert.assertEquals(root.path("evidence").path("actions").size(), 1, root.toPrettyString());
            Assert.assertFalse(root.path("evidence").path("actions").get(0).path("metadata")
                    .has("playwrightCallId"));
        } finally {
            Files.deleteIfExists(archive);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Malformed Playwright traces should remain explicit without leaking parser details")
    public void malformedPlaywrightTraceShouldRemainExplicit() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n");
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(archive);

            String json = FailureTraceReporter.renderTraceJson(
                    info("malformedPlaywrightScenario", failure()), "log", List.of());
            JsonNode playwright = JSON.readTree(json).path("evidence").path("playwright");

            Assert.assertEquals(playwright.path("status").asText(), "malformed", json);
            Assert.assertEquals(playwright.path("reason").asText(),
                    "Playwright native trace is malformed.");
            Assert.assertTrue(playwright.path("actions").isEmpty(), json);
            Assert.assertFalse(json.contains("finite wallTime"), json);
        } finally {
            Files.deleteIfExists(archive);
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Sensitive browser evidence should skip native Playwright import entirely")
    public void sensitiveTraceShouldSuppressPlaywrightNativeEvidence() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true);
        FailureTraceReporter.suppressSensitiveBrowserArtifacts();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver hostileDriver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(HasCdp.class));
        helper.setDriver(hostileDriver);
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@secret\",\"startTime\":100,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"title\":\"PRIVATE_NATIVE_SECRET\","
                        + "\"params\":{}}\n");
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(archive);

            String json = FailureTraceReporter.renderTraceJson(
                    info("sensitivePlaywrightScenario", failure()), "log", List.of());
            JsonNode snapshot = JSON.readTree(json).path("snapshot");
            JsonNode playwright = JSON.readTree(json).path("evidence").path("playwright");

            Assert.assertEquals(snapshot.path("provider").asText(), "none", json);
            Assert.assertEquals(snapshot.path("fidelity").asText(), "omitted", json);
            Assert.assertEquals(snapshot.path("status").asText(), "omitted-sensitive", json);
            Assert.assertEquals(snapshot.path("type").asText(), "omitted-sensitive", json);
            Assert.assertTrue(snapshot.path("content").asText().isEmpty(), json);
            Assert.assertEquals(playwright.path("status").asText(), "suppressed-sensitive", json);
            Assert.assertTrue(playwright.path("actions").isEmpty(), json);
            Assert.assertTrue(playwright.path("correlations").isEmpty(), json);
            Assert.assertFalse(json.contains("PRIVATE_NATIVE_SECRET"), json);
            traceManager.verify(PlaywrightTraceManager::getLastTracePath, Mockito.times(0));
            Mockito.verifyNoInteractions((HasCdp) hostileDriver);
            Mockito.verify(hostileDriver, Mockito.never()).getPageSource();
        } finally {
            helper.setDriver(null);
            Files.deleteIfExists(archive);
            TraceEventRecorder.clear();
            FailureTraceReporter.clearSensitiveValues();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Oversized imported Playwright evidence should not prevent trace publication")
    public void oversizedPlaywrightEvidenceShouldPublishAnExplicitBudgetOmission() throws Exception {
        TestExecutionInfo failingInfo = info("oversizedPlaywrightEvidenceScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        StringBuilder trace = new StringBuilder(
                "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n");
        String payload = "x".repeat(4_096);
        for (int action = 0; action < 3; action++) {
            trace.append("{\"type\":\"before\",\"callId\":\"call@").append(action)
                    .append("\",\"startTime\":").append(100 + action)
                    .append(",\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n");
            for (int log = 0; log < 100; log++) {
                trace.append("{\"type\":\"log\",\"callId\":\"call@").append(action)
                        .append("\",\"message\":\"").append(payload).append("\"}\n");
            }
        }
        Path archive = PlaywrightTraceTestFixtures.writeTrace(trace.toString());
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceMaxArtifactMb(1);
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(archive);

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            Path bundle = traceDirectory.resolve("shaft-trace.zip");
            Assert.assertTrue(Files.isRegularFile(bundle), "Required trace bundle must survive optional import overflow.");
            try (ZipFile zip = new ZipFile(bundle.toFile())) {
                JsonNode root = JSON.readTree(readZipEntry(zip, "shaft-trace.json"));
                JsonNode playwright = root.path("evidence").path("playwright");
                Assert.assertEquals(playwright.path("status").asText(), "omitted-budget", root.toPrettyString());
                Assert.assertEquals(playwright.path("reason").asText(),
                        "Playwright action evidence exceeded its bounded report budget.");
                Assert.assertTrue(playwright.path("actions").isEmpty(), root.toPrettyString());
                Assert.assertTrue(playwright.path("correlations").isEmpty(), root.toPrettyString());
            }
        } finally {
            Files.deleteIfExists(archive);
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Near-cap core JSON and an individually fitting Playwright import should still publish")
    public void cumulativeRequiredBudgetShouldOmitPlaywrightEvidenceBeforePublication() throws Exception {
        String padding = "p".repeat(700_000);
        String nativeEvidence = "n".repeat(300_000);
        String baseJson = """
                {"schemaVersion":"3.0","session":{"schemaVersion":"2.0","events":[{"metadata":{
                "playwrightCallId":"call@1","playwrightStepId":"step@1","playwrightCorrelation":"exact-operation-time"}}]},
                "evidence":{"playwright":{"status":"available","reason":"","actions":[{"callId":"call@1","title":"%s"}],
                "correlations":[{"shaftActionId":"action-1","playwrightCallId":"call@1","basis":"exact-operation-time"}]},
                "actions":[{"metadata":{"playwrightCallId":"call@1","playwrightStepId":"step@1",
                "playwrightCorrelation":"exact-operation-time"}}]},"padding":"%s"}
                """.formatted(nativeEvidence, padding);
        Path directory = Files.createTempDirectory("shaft-trace-cumulative-budget-");
        Path target = directory.resolve("shaft-trace.zip");
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), null,
                1024 * 1024, "omitted")) {
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(target,
                    baseJson, "[]", Map.of(), manifest, 1024 * 1024, 4L * 1024 * 1024,
                    "omitted", List.of());

            JsonNode root = JSON.readTree(bundle.json());
            Assert.assertEquals(root.path("evidence").path("playwright").path("status").asText(),
                    "omitted-budget", root.toPrettyString());
            Assert.assertTrue(root.path("evidence").path("playwright").path("actions").isEmpty());
            Assert.assertFalse(root.path("evidence").path("actions").get(0).path("metadata")
                    .has("playwrightCallId"));
            Assert.assertFalse(root.path("session").path("events").get(0).path("metadata")
                    .has("playwrightCallId"));
            Assert.assertTrue(Files.isRegularFile(target));
            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertTrue(zip.getEntry("shaft-trace.json").getSize() <= 1024 * 1024L);
                Assert.assertTrue(zip.getEntry("SHAFT Trace Report.html").getSize() <= 1024 * 1024L);
            }
        } finally {
            deleteDirectory(directory);
        }
    }

    @Test(description = "Artifact reconciliation growth should reapply the Playwright evidence fallback")
    public void aggregateReconciliationGrowthShouldReapplyPlaywrightBudgetFallback() throws Exception {
        String nativeEvidence = "n".repeat(50_000);
        String network = "[{\"url\":\"" + "x".repeat(50_000) + "\"}]";
        String baseJson = """
                {"schemaVersion":"3.0","session":{"schemaVersion":"2.0","events":[{"metadata":{
                "playwrightCallId":"call@1"}}],"artifacts":[{"id":"network","kind":"network",
                "path":"shaft-network.har","mimeType":"application/json","omitted":false,"metadata":{}}]},
                "evidence":{"playwright":{"status":"available","reason":"","actions":[{"callId":"call@1",
                "title":"%s"}],"correlations":[]},"actions":[{"metadata":{"playwrightCallId":"call@1"}}]}}
                """.formatted(nativeEvidence);
        String reason = "aggregate budget";
        Path directory = Files.createTempDirectory("shaft-trace-reconciliation-growth-");
        Path target = directory.resolve("shaft-trace.zip");
        TraceArtifactManifest manifest = TraceArtifactManifest.create(network, Map.of(), null,
                1024 * 1024, "per-entry budget");
        try {
            FailureTraceReporter.TraceArchiveBundle baseline = FailureTraceReporter.convergeTraceArchive(target,
                    baseJson, "[]", Map.of(), null, 1024 * 1024, 4L * 1024 * 1024,
                    reason, List.of());
            long initialJsonBytes = baseJson.getBytes(StandardCharsets.UTF_8).length;
            long initialHtmlBytes = baseline.html().getBytes(StandardCharsets.UTF_8).length;
            long maxEntryBytes = Math.max(initialJsonBytes, initialHtmlBytes) + 1;
            long maxTotalBytes = initialJsonBytes + initialHtmlBytes
                    + reason.getBytes(StandardCharsets.UTF_8).length;

            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(target,
                    baseJson, network, Map.of(), manifest, maxEntryBytes, maxTotalBytes,
                    reason, List.of());

            JsonNode root = JSON.readTree(bundle.json());
            Assert.assertEquals(root.path("evidence").path("playwright").path("status").asText(),
                    "omitted-budget", root.toPrettyString());
            Assert.assertTrue(findArtifact(root.path("session"), "network").path("omitted").asBoolean());
            Assert.assertTrue(bundle.json().getBytes(StandardCharsets.UTF_8).length <= maxEntryBytes);
            Assert.assertTrue(bundle.html().getBytes(StandardCharsets.UTF_8).length <= maxEntryBytes);
            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-network.har")).readAllBytes(),
                        StandardCharsets.UTF_8), reason);
            }
        } finally {
            manifest.close();
            deleteDirectory(directory);
        }
    }

    @Test(description = "Persisted trace archives should enforce the configured aggregate decompressed budget")
    public void persistedTraceArchiveShouldEnforceAggregateBudget() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-total-budget-");
        Path target = directory.resolve("shaft-trace.zip");
        try {
            List<String> omitted = FailureTraceReporter.renderTraceZip(target, "12345678901234567890",
                    "abcdefghijklmnopqrst", "[]", Map.of(), (Path) null, 64, "x");

            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertNotNull(zip.getEntry("shaft-trace.json"));
                Assert.assertNotNull(zip.getEntry("shaft-network.har"));
                Assert.assertNotNull(zip.getEntry("SHAFT Trace Report.html"));
                long total = zip.stream().mapToLong(ZipEntry::getSize).sum();
                Assert.assertTrue(total <= 64, "Total decompressed bytes must stay within the session cap: " + total);
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-trace.json")).readAllBytes(),
                        StandardCharsets.UTF_8), "12345678901234567890");
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("SHAFT Trace Report.html")).readAllBytes(),
                        StandardCharsets.UTF_8), "abcdefghijklmnopqrst");
                Assert.assertTrue(omitted.contains("shaft-network.har"), omitted.toString());
                Assert.assertTrue(zip.stream().anyMatch(entry -> {
                    try {
                        return "x".equals(new String(zip.getInputStream(entry).readAllBytes(), StandardCharsets.UTF_8));
                    } catch (IOException exception) {
                        throw new java.io.UncheckedIOException(exception);
                    }
                }), "At least one lower-priority entry should carry an explicit omission marker.");
            }
        } finally {
            deleteDirectory(directory);
        }
    }

    @Test(description = "Aggregate omissions should remain consistent across the JSON, viewer, and index")
    public void aggregateOmissionsShouldReconcileEveryPersistedReader() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-reconciled-");
        Path target = directory.resolve("shaft-trace.zip");
        String network = "[{\"url\":\"" + "n".repeat(500_000) + "\"}]";
        TraceArtifactManifest manifest = TraceArtifactManifest.create(network, Map.of(), null,
                1024 * 1024, "per-entry");
        String json = """
                {"schemaVersion":"3.0","session":{"artifacts":[{"id":"network","kind":"network",
                "path":"shaft-network.har","mimeType":"application/json","omitted":false,"metadata":{}}]},
                "evidence":{"actions":[],"network":[],"console":[],"browserObservability":{"warnings":[]}}}
                """;
        String reason = "aggregate budget";
        try {
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(target,
                    json, network, Map.of(), manifest, 1024 * 1024, 300_000, reason, List.of());
            JsonNode archiveJson;
            String archiveHtml;
            try (ZipFile zip = new ZipFile(target.toFile())) {
                archiveJson = JSON.readTree(zip.getInputStream(zip.getEntry("shaft-trace.json")));
                archiveHtml = new String(zip.getInputStream(zip.getEntry("SHAFT Trace Report.html")).readAllBytes(),
                        StandardCharsets.UTF_8);
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-network.har")).readAllBytes(),
                        StandardCharsets.UTF_8), reason);
                Assert.assertTrue(zip.stream().mapToLong(ZipEntry::getSize).sum() <= 300_000);
            }
            JsonNode artifact = findArtifact(archiveJson.path("session"), "network");
            Assert.assertTrue(artifact.path("omitted").asBoolean(), archiveJson.toPrettyString());
            Assert.assertEquals(artifact.path("metadata").path("omissionReason").asText(), reason);
            Assert.assertTrue(archiveHtml.contains("shaft-network.har"), archiveHtml);
            Assert.assertTrue(archiveHtml.contains("aggregate budget"), archiveHtml);

            String index = FailureTraceReporter.renderTraceIndexJson(info("aggregateReconciliation", failure()),
                    target, false, 1, bundle.omitted(), bundle.artifacts());
            JsonNode indexJson = JSON.readTree(index);
            Assert.assertEquals(indexJson.path("omittedEntries").get(0).asText(), "shaft-network.har");
            JsonNode indexedArtifact = findArtifact(indexJson, "network");
            Assert.assertTrue(indexedArtifact.path("omitted").asBoolean(), index);
            Assert.assertEquals(indexedArtifact.path("metadata").path("omissionReason").asText(), reason);
        } finally {
            manifest.close();
            deleteDirectory(directory);
        }
    }

    @Test(description = "Pre-omitted artifact markers should preserve their manifest reason during convergence")
    public void perEntryOmissionReasonShouldMatchZipAfterAggregateConvergence() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-pre-omitted-");
        Path target = directory.resolve("shaft-trace.zip");
        String network = "[{\"url\":\"" + "n".repeat(512) + "\"}]";
        String reason = "per-entry budget";
        TraceArtifactManifest manifest = TraceArtifactManifest.create(network, Map.of(), null, 64, reason);
        String json = """
                {"session":{"artifacts":[{"id":"network","kind":"network","path":"shaft-network.har",
                "mimeType":"application/json","omitted":true,
                "metadata":{"omissionReason":"per-entry budget"}}]}}
                """;
        try {
            FailureTraceReporter.TraceArchiveBundle bundle = FailureTraceReporter.convergeTraceArchive(target,
                    json, network, Map.of(), manifest, 1024 * 1024, 3L * 1024 * 1024, "aggregate budget",
                    List.of("shaft-network.har"));
            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-network.har")).readAllBytes(),
                        StandardCharsets.UTF_8), reason);
            }
            JsonNode artifact = findArtifact(JSON.readTree(bundle.json()).path("session"), "network");
            Assert.assertEquals(artifact.path("metadata").path("omissionReason").asText(), reason);
            Assert.assertTrue(bundle.html().contains(reason), bundle.html());
        } finally {
            manifest.close();
            deleteDirectory(directory);
        }
    }

    @Test(description = "Parallel same-id publication should keep the highest completed attempt and unique invocation paths")
    public void parallelSameIdPublicationShouldNotCrossWireArchives() throws Exception {
        TestExecutionInfo info = info("parallelPublicationScenario", failure());
        Path directory = FailureTraceReporter.traceDirectory(info);
        Path first = FailureTraceReporter.completedArchivePath(info, 1);
        Path second = FailureTraceReporter.completedArchivePath(info, 2);
        Path root = directory.resolve("shaft-trace.zip");
        Assert.assertNotEquals(first, second);
        Files.createDirectories(directory);
        Files.writeString(first, "first invocation", StandardCharsets.UTF_8);
        Files.writeString(second, "second invocation", StandardCharsets.UTF_8);
        CountDownLatch ready = new CountDownLatch(2);
        CountDownLatch start = new CountDownLatch(1);
        try (var executor = Executors.newFixedThreadPool(2)) {
            var publishFirst = executor.submit(() -> {
                ready.countDown();
                start.await();
                return FailureTraceReporter.publishLatest("parallel-publication-proof", 1, first, root);
            });
            var publishSecond = executor.submit(() -> {
                ready.countDown();
                start.await();
                return FailureTraceReporter.publishLatest("parallel-publication-proof", 2, second, root);
            });
            ready.await();
            start.countDown();
            publishFirst.get();
            Assert.assertTrue(publishSecond.get());
        } finally {
            Assert.assertEquals(Files.readString(root, StandardCharsets.UTF_8), "second invocation");
            deleteDirectory(directory);
        }
    }

    @Test(description = "A late lower attempt should extend history without replacing latest root metadata")
    public void reverseCompletionShouldKeepLatestRootAndCompleteOrderedHistory() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        TestExecutionInfo info = info("reverseCompletionScenario", failure());
        Path directory = FailureTraceReporter.traceDirectory(info);
        Path first = directory.resolve("first-completed.zip");
        Path second = directory.resolve("second-completed.zip");
        try {
            deleteDirectory(directory);
            Files.createDirectories(directory);
            Files.writeString(first, "first invocation", StandardCharsets.UTF_8);
            Files.writeString(second, "second invocation", StandardCharsets.UTF_8);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("retry").traceRetainFailedAttempts(true);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

            FailureTraceReporter.persistTraceArtifacts(info, second, Map.of(), 2, List.of());
            FailureTraceReporter.persistTraceArtifacts(info, first, Map.of(), 1, List.of());

            Assert.assertEquals(Files.readString(directory.resolve("shaft-trace.zip"), StandardCharsets.UTF_8),
                    "second invocation");
            String index = Files.readString(directory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"attempt\": \"2\""), index);
            Assert.assertTrue(index.contains("\"attempt\": 1"), index);
            Assert.assertTrue(index.contains("\"attempt\": 2"), index);
            Assert.assertTrue(index.indexOf("\"attempt\": 1") < index.indexOf("\"attempt\": 2"), index);
        } finally {
            deleteDirectory(directory);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "The canonical trace index should use recoverable atomic publication")
    public void traceIndexShouldUseRecoverableAtomicPublication() throws Exception {
        TestExecutionInfo info = info("atomicIndexPublicationScenario", failure());
        Path directory = FailureTraceReporter.traceDirectory(info);
        Path completedArchive = directory.resolve("completed.zip");
        try {
            deleteDirectory(directory);
            Files.createDirectories(directory);
            Files.writeString(completedArchive, "completed invocation", StandardCharsets.UTF_8);
            try (MockedStatic<TraceArchiveWriter> archiveWriter = Mockito.mockStatic(
                    TraceArchiveWriter.class, Mockito.CALLS_REAL_METHODS)) {
                FailureTraceReporter.persistTraceArtifacts(info, completedArchive, Map.of(), 1, List.of());

                Path index = directory.resolve("index.json");
                archiveWriter.verify(() -> TraceArchiveWriter.writeBytes(
                        Mockito.eq(index), Mockito.any(byte[].class)));
                Assert.assertTrue(JSON.readTree(Files.readString(index, StandardCharsets.UTF_8)).isObject());
            }
        } finally {
            deleteDirectory(directory);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Lossy path sanitization should not collapse distinct stable test ids")
    public void sanitizedAndTruncatedTestIdsShouldRemainCollisionResistant() throws Exception {
        Method method = FailureTraceReporterTest.class.getDeclaredMethod("failingScenario");
        TestExecutionInfo slash = new TestExecutionInfo("customer/test", "customer.LoginTest", "one", "one",
                "trace test", method, failure(), false);
        TestExecutionInfo colon = new TestExecutionInfo("customer:test", "customer.LoginTest", "two", "two",
                "trace test", method, failure(), false);
        String longPrefix = "x".repeat(130);
        TestExecutionInfo longOne = new TestExecutionInfo(longPrefix + "one", "customer.LoginTest", "one", "one",
                "trace test", method, failure(), false);
        TestExecutionInfo longTwo = new TestExecutionInfo(longPrefix + "two", "customer.LoginTest", "two", "two",
                "trace test", method, failure(), false);

        Assert.assertNotEquals(FailureTraceReporter.safeTestId(slash), FailureTraceReporter.safeTestId(colon));
        Assert.assertNotEquals(FailureTraceReporter.safeTestId(longOne), FailureTraceReporter.safeTestId(longTwo));
        Assert.assertTrue(FailureTraceReporter.safeTestId(longOne).length() <= 120);
        TestExecutionInfo dot = new TestExecutionInfo("..", "customer.LoginTest", "dot", "dot",
                "trace test", method, failure(), false);
        TestExecutionInfo reserved = new TestExecutionInfo("CON", "customer.LoginTest", "reserved", "reserved",
                "trace test", method, failure(), false);
        Assert.assertNotEquals(FailureTraceReporter.safeTestId(dot), "..");
        Assert.assertNotEquals(FailureTraceReporter.safeTestId(reserved), "CON");
        Path base = Path.of("target", "shaft-traces").toAbsolutePath().normalize();
        Assert.assertTrue(FailureTraceReporter.traceDirectory(dot).toAbsolutePath().normalize().startsWith(base));
    }

    @Test(description = "Disabling attempt retention should skip persisting attempt-indexed archives for failed retries")
    public void retriesShouldNotRetainFailedAttemptArchivesWhenDisabled() throws Exception {
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        TestExecutionInfo failingAttempt = info("attemptRetentionDisabledScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingAttempt);
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("retry").traceRetainFailedAttempts(false);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

            FailureTraceReporter.attachOnFailure(failingAttempt, "attempt one failed", List.of());

            Assert.assertFalse(Files.exists(traceDirectory.resolve("shaft-trace-attempt-1.zip")),
                    "No attempt-indexed archive should be written when retention is disabled.");
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace.zip")));
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "renderTraceJson four-arg overload should embed the attempt number and effective trace mode")
    public void renderTraceJsonFourArgShouldIncludeAttemptAndTraceMode() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            String json = FailureTraceReporter.renderTraceJson(
                    info("attemptFieldScenario", failure()), "failed", List.of(), 3);

            Assert.assertTrue(json.contains("\"attempt\": \"3\""), json);
            Assert.assertTrue(json.contains("\"traceMode\": \"failure\""), json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Oversized bundle entries should be listed in index.json omittedEntries instead of failing silently")
    public void oversizedScreenshotShouldBeListedInOmittedEntries() throws Exception {
        TestExecutionInfo failingInfo = info("truncationScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        try {
            deleteDirectory(traceDirectory);
            // Buffer the oversized screenshot while the artifact cap is generous: TraceEventRecorder.recordScreenshot
            // enforces shaft.trace.maxArtifactMb as a live *buffering* budget and would silently drop (never buffer)
            // a screenshot larger than the cap in effect at record time, which would short-circuit this test before
            // it ever reached the persist-time omission-marker logic under exercise.
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeScreenshots(true).traceMaxArtifactMb(50);
            byte[] oversizedPng = new byte[2 * 1024 * 1024];

            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.recordScreenshot(event, oversizedPng);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());

            // Now shrink the cap so the already-buffered 2MB screenshot exceeds it at persist time.
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(1);
            String v2Json = FailureTraceReporter.renderTraceJson(failingInfo, "failed", List.of());
            JsonNode screenshotArtifact = findArtifact(JSON.readTree(v2Json).path("session"),
                    "screenshot-action-1");
            Assert.assertTrue(screenshotArtifact.path("omitted").asBoolean());
            Assert.assertEquals(screenshotArtifact.path("metadata").path("omissionReason").asText(),
                    "Omitted because artifact exceeded shaft.trace.maxArtifactMb=1");

            // Recreate the action because rendering drains the recorder, then exercise persisted index/ZIP behavior.
            TraceEventRecorder.clear();
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(50);
            event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.recordScreenshot(event, oversizedPng);
            TraceEventRecorder.finish(event, "failed", "Click failed",
                    new RuntimeException("boom"), Map.of(), List.of());
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(1);
            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"omittedEntries\": ["), index);
            Assert.assertTrue(index.contains(screenshotArtifact.path("path").asText()), index);
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) throws Exception {
        return info(methodName, throwable, false);
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable, boolean retried) throws Exception {
        Method method = FailureTraceReporterTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-" + methodName, "customer.LoginTest", methodName, methodName,
                "trace test", method, throwable, retried);
    }

    private static TestExecutionInfo infoUnchecked(String methodName, Throwable throwable) {
        try {
            return info(methodName, throwable);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    private static RuntimeException failure() {
        RuntimeException throwable = new RuntimeException("boom password=raw-password");
        throwable.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("customer.LoginTest", "failingScenario", "LoginTest.java", 27),
                new StackTraceElement("com.shaft.listeners.internal.ExecutionLifecycleHelper", "attach", "ExecutionLifecycleHelper.java", 1)
        });
        return throwable;
    }

    @SuppressWarnings("unused")
    private static void failingScenario() {
        throw new UnsupportedOperationException("test marker");
    }

    private static AndroidDriver mockedAndroidDriver() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        WebDriver.Window window = Mockito.mock(WebDriver.Window.class);
        DesiredCapabilities capabilities = new DesiredCapabilities();
        capabilities.setPlatform(Platform.ANDROID);
        capabilities.setCapability("appium:automationName", "UiAutomator2");
        capabilities.setCapability("appium:appPackage", "com.example.checkout");
        capabilities.setCapability("appium:appActivity", ".CheckoutActivity");

        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.window()).thenReturn(window);
        Mockito.when(window.getSize()).thenReturn(new Dimension(1080, 1920));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP");
        Mockito.when(driver.getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        Mockito.when(driver.getPageSource()).thenReturn("<hierarchy text=\"Pay now\" password=\"raw-password\"/>");
        return driver;
    }

    private static List<Attachment> attachments() {
        List<Attachment> attachments = new ArrayList<>();
        Allure.getLifecycle().updateTestCase(result -> attachments.addAll(result.getAttachments()));
        return attachments;
    }

    private static void deleteDirectory(Path directory) throws Exception {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (Exception e) {
                            throw new IllegalStateException(e);
                        }
                    });
        }
    }

    private static String readZipEntry(ZipFile zip, String entryName) throws Exception {
        try (var input = zip.getInputStream(zip.getEntry(entryName))) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    @Test(description = "An advertised but missing native Playwright trace should remain explicit everywhere")
    public void missingNativeTraceShouldAgreeAcrossSchemaViewerZipAndIndex() throws Exception {
        TestExecutionInfo failingInfo = info("missingNativeTraceScenario", failure());
        Path traceDirectory = FailureTraceReporter.traceDirectory(failingInfo);
        Path missingTrace = Path.of("target", "missing-native-trace.zip");
        JsonNode schemaArtifacts = null;
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            deleteDirectory(traceDirectory);
            Files.deleteIfExists(missingTrace);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure").traceMaxArtifactMb(1);
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(missingTrace);

            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                String json = readZipEntry(zip, "shaft-trace.json");
                JsonNode root = JSON.readTree(json);
                schemaArtifacts = root.path("session").path("artifacts");
                JsonNode nativeArtifact = findArtifact(root.path("session"), "native-trace");
                Assert.assertTrue(nativeArtifact.path("omitted").asBoolean(), json);
                Assert.assertTrue(nativeArtifact.path("metadata").path("omissionReason").asText()
                        .contains("unavailable"), json);
                Assert.assertEquals(root.path("evidence").path("playwright").path("status").asText(),
                        "unavailable", json);
                Assert.assertEquals(root.path("evidence").path("playwright").path("reason").asText(),
                        "Playwright native trace was unavailable for import.", json);
                Assert.assertTrue(root.path("evidence").path("playwright").path("actions").isEmpty(), json);
                Assert.assertTrue(readZipEntry(zip, "missing-native-trace.zip").contains("unavailable"));
                String html = readZipEntry(zip, "SHAFT Trace Report.html");
                String truncationPayload = html.substring(
                        html.indexOf("<pre hidden id=\"trace-truncation\">")
                                + "<pre hidden id=\"trace-truncation\">".length(),
                        html.indexOf("</pre>", html.indexOf("<pre hidden id=\"trace-truncation\">")));
                Assert.assertTrue(truncationPayload.contains("missing-native-trace.zip"), truncationPayload);
            }
            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("missing-native-trace.zip"), index);
            Assert.assertEquals(JSON.readTree(index).path("artifacts"), schemaArtifacts,
                    "The persisted index must copy the finalized schema artifact graph exactly.");
        } finally {
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
            Files.deleteIfExists(missingTrace);
            Properties.clearForCurrentThread();
        }
    }

    private static JsonNode findArtifact(JsonNode session, String id) {
        for (JsonNode artifact : session.path("artifacts")) {
            if (id.equals(artifact.path("id").asText())) {
                return artifact;
            }
        }
        throw new AssertionError("Missing trace artifact reference: " + id + " in " + session.toPrettyString());
    }
}
