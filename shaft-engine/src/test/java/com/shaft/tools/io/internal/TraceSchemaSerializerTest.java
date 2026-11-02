package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.tools.io.trace.TraceArtifactReference;
import com.shaft.tools.io.trace.TraceSession;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.time.Instant;
import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;
import java.util.Arrays;

public class TraceSchemaSerializerTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @AfterMethod
    public void clearRecorder() {
        TraceEventRecorder.clear();
    }

    @Test
    public void shouldPreservePerEventBackendsAfterDriverTeardownAndFailClosedForMixedSessions() {
        TraceEventRecorder.ActionEvent selenium = action("action-1", AutomationBackend.SELENIUM_WEBDRIVER,
                "Authorization: Bearer raw-token", Map.of("apiToken", "raw-token"));
        TraceEventRecorder.ActionEvent appium = action("action-2", AutomationBackend.APPIUM,
                "{\"password\":\"raw-password\"}", Map.of("cookie", "raw-cookie"));

        TraceSession session = TraceSchemaSerializer.create("mixed", 1, List.of(selenium, appium),
                List.of(new TraceArtifactReference("network", "network", "shaft-network.har",
                        "application/json", false, Map.of("token", "raw-artifact-token"))));
        JsonNode json = JSON.readTree(TraceSchemaSerializer.toJson(session));

        Assert.assertEquals(json.path("schemaVersion").asText(), "2.0");
        Assert.assertEquals(json.path("backend").asText(), "UNKNOWN");
        Assert.assertEquals(json.path("events").get(0).path("backend").asText(), "SELENIUM_WEBDRIVER");
        Assert.assertEquals(json.path("events").get(1).path("backend").asText(), "APPIUM");
        Assert.assertFalse(json.toString().contains("raw-token"), json.toPrettyString());
        Assert.assertFalse(json.toString().contains("raw-password"), json.toPrettyString());
        Assert.assertFalse(json.toString().contains("raw-cookie"), json.toPrettyString());
        Assert.assertFalse(json.toString().contains("raw-artifact-token"), json.toPrettyString());
    }

    @Test
    public void shouldSerializeExactStableEventSemanticsAndSingleBackendProfiles() throws Exception {
        for (AutomationBackend backend : List.of(AutomationBackend.SELENIUM_WEBDRIVER, AutomationBackend.APPIUM,
                AutomationBackend.MICROSOFT_PLAYWRIGHT)) {
            TraceEventRecorder.ActionEvent action = action("action-1", backend, "failed", Map.of());
            TraceSession session = TraceSchemaSerializer.create("checkout", 2, List.of(action), List.of());
            JsonNode json = JSON.readTree(TraceSchemaSerializer.toJson(session));
            JsonNode event = json.path("events").get(0);

            Assert.assertEquals(json.path("backend").asText(), backend.name());
            Assert.assertEquals(event.path("id").asText(), json.path("id").asText() + "/action-1");
            Assert.assertEquals(event.path("startedAt").asText(), "2026-08-11T00:00:00Z");
            Assert.assertEquals(event.path("durationMs").asLong(), 5L);
            Assert.assertEquals(event.path("source").asText(), "CheckoutTest.java:20");
            Assert.assertEquals(event.path("metadata").path("url").asText(), "https://example.test");
            Assert.assertEquals(event.path("metadata").path("exceptionType").asText(),
                    "java.lang.RuntimeException");
            Assert.assertEquals(event.path("metadata").path("exceptionMessage").asText(), "failed");
        }
    }

    @Test
    public void shouldRetainFormerPublicEventConstructorDescriptor() throws Exception {
        Constructor<TraceEventRecorder.Event> constructor = TraceEventRecorder.Event.class.getDeclaredConstructor(
                boolean.class, String.class, String.class, String.class, String.class, long.class,
                String.class, String.class, String.class, String.class, WebDriver.class);

        TraceEventRecorder.Event event = constructor.newInstance(true, "action-1", "element", "click",
                "2026-08-11T00:00:00Z", 1L, "id=pay", "", "CheckoutTest.java:20", "", null);

        Assert.assertEquals(Arrays.stream(TraceEventRecorder.Event.class.getRecordComponents())
                        .map(java.lang.reflect.RecordComponent::getName).toList(),
                List.of("enabled", "id", "category", "name", "startTime", "startNanos", "locator", "url",
                        "caller", "domSnapshotBefore", "driver"));
        Assert.assertEquals(event.category(), "element");
    }

    @Test
    public void recorderShouldCaptureSeleniumAndAppiumBackendAtActionTime() {
        WebDriver selenium = Mockito.mock(WebDriver.class);
        AndroidDriver appium = Mockito.mock(AndroidDriver.class);

        TraceEventRecorder.Event webEvent = TraceEventRecorder.start("browser", "navigate", "", selenium);
        TraceEventRecorder.finish(webEvent, "passed", "done", null, Map.of(), List.of());
        TraceEventRecorder.Event mobileEvent = TraceEventRecorder.start("element", "tap", "id=pay", appium);
        TraceEventRecorder.finish(mobileEvent, "passed", "done", null, Map.of(), List.of());

        Assert.assertEquals(TraceEventRecorder.snapshot().get(0).backend(), AutomationBackend.SELENIUM_WEBDRIVER);
        Assert.assertEquals(TraceEventRecorder.snapshot().get(1).backend(), AutomationBackend.APPIUM);
    }

    @Test
    public void eventShouldNeverLinkAnUnpublishedScreenshotArtifact() {
        TraceEventRecorder.ActionEvent invalidScreenshot = new TraceEventRecorder.ActionEvent("action-1",
                AutomationBackend.SELENIUM_WEBDRIVER, "element", "click", "failed", Instant.now().toString(), 1,
                "id=pay", "", "CheckoutTest.java:20", "failed", "", "", List.of(), Map.of(), Map.of(), "", "",
                "not-valid-base64");

        TraceSession session = TraceSchemaSerializer.create("invalid-screenshot", 1, List.of(invalidScreenshot),
                List.of(new TraceArtifactReference("network", "network", "shaft-network.har",
                        "application/json", false, Map.of())));

        Assert.assertTrue(session.events().getFirst().artifactIds().isEmpty());
        Assert.assertTrue(session.artifacts().stream().noneMatch(artifact -> artifact.kind().equals("screenshot")));
    }

    @Test
    public void attachmentSummariesShouldBeRedactedMetadataWithoutInventedArtifactReferences() {
        TraceEventRecorder.ActionEvent action = new TraceEventRecorder.ActionEvent("action-1",
                AutomationBackend.SELENIUM_WEBDRIVER, "element", "click", "failed", Instant.now().toString(), 1,
                "id=pay", "", "CheckoutTest.java:20", "failed", "", "",
                List.of("Screenshot token=raw-token"), Map.of(), Map.of(), "", "", "");

        TraceSession session = TraceSchemaSerializer.create("attachment-summary", 1, List.of(action), List.of());
        JsonNode json = JSON.readTree(TraceSchemaSerializer.toJson(session));

        Assert.assertEquals(json.path("events").get(0).path("metadata").path("attachmentSummaries").asText(),
                "Screenshot token=********");
        Assert.assertTrue(json.path("events").get(0).path("artifactIds").isEmpty());
        Assert.assertTrue(json.path("artifacts").isEmpty());
    }

    @Test
    public void recorderShouldCaptureLivePlaywrightAndFailClosedWhenDisconnected() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Page page = Mockito.mock(Page.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Browser browser = Mockito.mock(Browser.class);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(browser.isConnected()).thenReturn(true);
        try (MockedStatic<PlaywrightSessionManager> manager = Mockito.mockStatic(PlaywrightSessionManager.class)) {
            manager.when(PlaywrightSessionManager::currentSession).thenReturn(session);
            TraceEventRecorder.Event live = TraceEventRecorder.start("browser", "navigate", "", null);
            TraceEventRecorder.finish(live, "passed", "done", null, Map.of(), List.of());

            Mockito.when(browser.isConnected()).thenReturn(false);
            TraceEventRecorder.Event disconnected = TraceEventRecorder.start("browser", "navigate", "", null);
            TraceEventRecorder.finish(disconnected, "failed", "closed", null, Map.of(), List.of());
        }

        Assert.assertEquals(TraceEventRecorder.snapshot().get(0).backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertEquals(TraceEventRecorder.snapshot().get(1).backend(), AutomationBackend.UNKNOWN);
    }

    private static TraceEventRecorder.ActionEvent action(String id, AutomationBackend backend, String message,
                                                         Map<String, String> metadata) {
        return new TraceEventRecorder.ActionEvent(id, backend, "element", "click", "failed",
                Instant.parse("2026-08-11T00:00:00Z").toString(), 5, "id=pay", "https://example.test",
                "CheckoutTest.java:20", message, "java.lang.RuntimeException", message, List.of(), metadata,
                Map.of(), "", "", "");
    }
}
