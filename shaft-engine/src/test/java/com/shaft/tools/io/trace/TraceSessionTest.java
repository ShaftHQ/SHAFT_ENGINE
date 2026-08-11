package com.shaft.tools.io.trace;

import com.shaft.gui.capabilities.AutomationBackend;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TraceSessionTest {

    @Test
    public void shouldExposeAnImmutableBackendNeutralSchema() {
        Map<String, String> eventMetadata = new HashMap<>(Map.of("context", "NATIVE_APP"));
        List<String> eventArtifacts = new ArrayList<>(List.of("screenshot-1"));
        TraceEvent event = new TraceEvent("session-1/action-1", AutomationBackend.APPIUM, "element", "tap",
                TraceEventStatus.PASSED, Instant.parse("2026-08-11T00:00:00Z"), 42, "CheckoutTest.java:20",
                "id=pay", "tapped", eventMetadata, eventArtifacts);
        Map<String, String> artifactMetadata = new HashMap<>(Map.of("pixelRatio", "3"));
        TraceArtifactReference artifact = new TraceArtifactReference("screenshot-1", "screenshot",
                "screenshots/action-1.png", "image/png", false, artifactMetadata);
        List<TraceEvent> events = new ArrayList<>(List.of(event));
        List<TraceArtifactReference> artifacts = new ArrayList<>(List.of(artifact));
        Map<String, String> sessionMetadata = new HashMap<>(Map.of("platform", "android"));
        TraceSession session = new TraceSession("session-1", AutomationBackend.APPIUM,
                Instant.parse("2026-08-11T00:00:00Z"), "checkout", 1, events, artifacts, sessionMetadata);

        eventMetadata.put("context", "WEBVIEW");
        eventArtifacts.clear();
        artifactMetadata.put("pixelRatio", "1");
        events.clear();
        artifacts.clear();
        sessionMetadata.put("platform", "ios");

        Assert.assertEquals(TraceSession.SCHEMA_VERSION, "2.0");
        Assert.assertEquals(session.events(), List.of(event));
        Assert.assertEquals(session.artifacts(), List.of(artifact));
        Assert.assertEquals(event.metadata().get("context"), "NATIVE_APP");
        Assert.assertEquals(event.artifactIds(), List.of("screenshot-1"));
        Assert.assertEquals(artifact.metadata().get("pixelRatio"), "3");
        Assert.assertEquals(session.metadata().get("platform"), "android");
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> session.metadata().put("platform", "ios"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> session.events().add(event));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> session.artifacts().add(artifact));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> event.metadata().put("context", "WEBVIEW"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> event.artifactIds().add("other"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> artifact.metadata().put("pixelRatio", "1"));
    }

    @Test
    public void shouldRejectInvalidEventAndArtifactReferences() {
        Map<String, String> metadataWithNull = new HashMap<>();
        metadataWithNull.put("key", null);
        List<String> artifactIdsWithNull = new ArrayList<>();
        artifactIdsWithNull.add(null);
        Assert.expectThrows(NullPointerException.class, () -> new TraceArtifactReference("id", "network",
                "shaft-network.har", "application/json", false, metadataWithNull));
        Assert.expectThrows(NullPointerException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "navigate", TraceEventStatus.PASSED, Instant.now(), 0, "", "", "",
                metadataWithNull, List.of()));
        Assert.expectThrows(NullPointerException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "navigate", TraceEventStatus.PASSED, Instant.now(), 0, "", "", "", Map.of(),
                artifactIdsWithNull));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceEvent("", AutomationBackend.UNKNOWN,
                "browser", "navigate", TraceEventStatus.PASSED, Instant.now(), 0, "", "", "", Map.of(),
                List.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceArtifactReference("", "network",
                "shaft-network.har", "application/json", false, Map.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceArtifactReference("id", "",
                "shaft-network.har", "application/json", false, Map.of()));
        for (String path : List.of("", "/absolute", "C:\\absolute", "../escape", "a/../escape", "a\\b",
                "a//b", "./a", "C:/absolute", "scheme:value", "screenshots/")) {
            Assert.expectThrows(IllegalArgumentException.class, () -> new TraceArtifactReference("id", "network",
                    path, "application/json", false, Map.of()));
        }
        Instant now = Instant.now();
        Assert.expectThrows(NullPointerException.class, () -> new TraceEvent("event", null, "browser", "navigate",
                TraceEventStatus.PASSED, now, 0, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "", "navigate", TraceEventStatus.PASSED, now, 0, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "", TraceEventStatus.PASSED, now, 0, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(NullPointerException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "navigate", null, now, 0, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(NullPointerException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "navigate", TraceEventStatus.PASSED, null, 0, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceEvent("event", AutomationBackend.UNKNOWN,
                "browser", "navigate", TraceEventStatus.PASSED, now, -1, "", "", "", Map.of(), List.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceSession("", AutomationBackend.UNKNOWN,
                Instant.now(), "test", 1, List.of(), List.of(), Map.of()));
        Assert.expectThrows(NullPointerException.class, () -> new TraceSession("id", null, Instant.now(), "test", 1,
                List.of(), List.of(), Map.of()));
        Assert.expectThrows(NullPointerException.class, () -> new TraceSession("id", AutomationBackend.UNKNOWN, null,
                "test", 1, List.of(), List.of(), Map.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceSession("id", AutomationBackend.UNKNOWN,
                Instant.now(), "test", 0, List.of(), List.of(), Map.of()));

        TraceEvent dangling = new TraceEvent("id/action-1", AutomationBackend.UNKNOWN, "element", "click",
                TraceEventStatus.FAILED, now, 1, "CheckoutTest.java:20", "id=pay", "failed", Map.of(),
                List.of("screenshot-missing"));
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceSession("id", AutomationBackend.UNKNOWN,
                now, "test", 1, List.of(dangling), List.of(), Map.of()));
        TraceArtifactReference first = new TraceArtifactReference("duplicate", "screenshot", "screenshots/one.png",
                "image/png", false, Map.of());
        TraceArtifactReference second = new TraceArtifactReference("duplicate", "screenshot", "screenshots/two.png",
                "image/png", false, Map.of());
        Assert.expectThrows(IllegalArgumentException.class, () -> new TraceSession("id", AutomationBackend.UNKNOWN,
                now, "test", 1, List.of(), List.of(first, second), Map.of()));
    }
}
