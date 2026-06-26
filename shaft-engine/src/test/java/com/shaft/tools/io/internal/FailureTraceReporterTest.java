package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.internal.locator.LocatorHealthReporter;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
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
            Assert.assertEquals(added.size(), 1, "Only the trace archive should be attached.");
            Assert.assertEquals(added.getFirst().getName(), "shaft-trace.zip");
            Assert.assertEquals(added.getFirst().getType(), "application/zip");
            Assert.assertFalse(added.stream().anyMatch(attachment -> "text/html".equals(attachment.getType())));
            Assert.assertFalse(added.stream().anyMatch(attachment -> "application/json".equals(attachment.getType())));
            Assert.assertFalse(Files.exists(traceDirectory.resolve("SHAFT Trace Report.html")));
            Assert.assertFalse(Files.exists(traceDirectory.resolve("shaft-trace.json")));
            Assert.assertTrue(Files.exists(traceDirectory.resolve("shaft-trace.zip")));
            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                Assert.assertNotNull(zip.getEntry("SHAFT Trace Report.html"));
                Assert.assertNotNull(zip.getEntry("shaft-trace.json"));
                Assert.assertNotNull(zip.getEntry("shaft-network.har"));
            }
            String index = Files.readString(traceDirectory.resolve("index.json"), StandardCharsets.UTF_8);
            Assert.assertTrue(index.contains("\"archive\": \"target/shaft-traces/id-failingScenario/shaft-trace.zip\""), index);
            Assert.assertTrue(index.contains("\"html\": \"SHAFT Trace Report.html\""), index);
            Assert.assertTrue(index.contains("\"json\": \"shaft-trace.json\""), index);
            Assert.assertTrue(index.contains("\"network\": \"shaft-network.har\""), index);
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
            new com.fasterxml.jackson.databind.ObjectMapper().readTree(json);

            Assert.assertTrue(json.contains("\"network\": ["), json);
            Assert.assertTrue(json.contains("\"method\": \"POST\""), json);
            Assert.assertTrue(json.contains("\"status\": 500"), json);
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
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "failure");
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 50);

            SHAFT.Properties.reporting.set()
                    .traceEnabled(false)
                    .traceMode("always")
                    .traceIncludeCodeContext(false)
                    .traceIncludeFullPageSnapshots(false)
                    .traceIncludeNativePageSource(false)
                    .traceIncludeNetwork(false)
                    .traceIncludeConsole(false)
                    .traceMaxArtifactMb(7);

            Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "always");
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeCodeContext());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeFullPageSnapshots());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNativePageSource());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNetwork());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeConsole());
            Assert.assertEquals(SHAFT.Properties.reporting.traceMaxArtifactMb(), 7);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) throws Exception {
        Method method = FailureTraceReporterTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-" + methodName, "customer.LoginTest", methodName, methodName,
                "trace test", method, throwable, false);
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
}
