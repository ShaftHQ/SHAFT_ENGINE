package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Platform;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
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

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.zip.ZipFile;

public class FailureTraceReporterTest {

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
            Assert.assertEquals(added.size(), 1, "Only the self-contained trace archive should be attached.");
            Assert.assertEquals(added.getFirst().getName(), "SHAFT Trace Report - id-failingScenario",
                    "Attachment label carries the test id (and attempt suffix once retried) for retry-aware trace attribution.");
            Assert.assertEquals(added.getFirst().getType(), "application/zip");
            Assert.assertFalse(added.stream().anyMatch(attachment -> "text/html".equals(attachment.getType())),
                    "No dangling HTML attachment outside the archive.");
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
                Assert.assertTrue(html.contains("data-tab=\"environment\""), html);
                Assert.assertTrue(html.contains("network-panel"), html);
                Assert.assertTrue(html.contains("console-panel"), html);
                Assert.assertTrue(html.contains("data-tab=\"log\""), html);
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
                Assert.assertNotNull(zip.getEntry("screenshots/action-1.png"));
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
            } catch (AssertionError expected) {
                // expected
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

            String json = FailureTraceReporter.renderTraceJson(info("failingScenario", failure()), "failed", List.of());
            var root = new tools.jackson.databind.ObjectMapper().readTree(json);

            Assert.assertTrue(json.contains("\"network\": ["), json);
            Assert.assertTrue(json.contains("\"method\": \"POST\""), json);
            Assert.assertTrue(json.contains("\"status\": 500"), json);
            Assert.assertTrue(root.path("network").get(0).path("timestamp").asLong(0L) > 0L,
                    "Network events must carry an epoch timestamp for timeline correlation: " + json);
            Assert.assertTrue(json.contains("\"console\": ["), json);
            Assert.assertTrue(json.contains("\"level\": \"SEVERE\""), json);
            Assert.assertTrue(json.contains("\"browserObservability\""), json);
            Assert.assertTrue(json.contains("Network capture is not supported by this driver."), json);
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
            FailureTraceReporter.attachOnFailure(failingInfo, "failed", List.of());

            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"omittedEntries\": ["), index);
            Assert.assertTrue(index.contains("screenshots/action-1.png"), index);
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
}
