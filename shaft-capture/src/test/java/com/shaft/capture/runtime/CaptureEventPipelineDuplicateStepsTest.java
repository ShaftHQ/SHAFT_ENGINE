package com.shaft.capture.runtime;

import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for issue #3409's duplicated-step reports: every recorder payload is
 * delivered through up to three racing channels (in-page queue drain, loopback sink POST, BiDi
 * script channel), so the pipeline must treat {@code clientActionId} as the authoritative
 * one-user-action identity — across timestamp drift and out-of-order arrival — instead of relying
 * only on byte-identical fingerprints.
 */
class CaptureEventPipelineDuplicateStepsTest {
    private static final Instant BASE = Instant.parse("2026-01-02T03:04:05Z");

    @TempDir
    Path temp;

    @Test
    void duplicateClickDeliveriesWithTimestampDriftProduceOneStep() throws Exception {
        try (Harness harness = new Harness(temp)) {
            harness.pipeline.accept(click("action-1", BASE, 1));
            // A keyboard event flushes the pending click into the store.
            harness.pipeline.accept(keyboard("action-2", BASE.plusMillis(500)));
            // The same user click arrives again from another delivery channel with drifted
            // arrival metadata; it must not become a second step.
            harness.pipeline.accept(click("action-1", BASE.plusMillis(30), 1));
            harness.pipeline.close();

            List<CaptureEvent> events = harness.store.read().events();
            assertEquals(1, events.stream().filter(CaptureEvent.ClickEvent.class::isInstance).count(),
                    "one user click must stay one step: " + events);
            assertEquals(1, events.stream().filter(CaptureEvent.KeyboardEvent.class::isInstance).count());
        }
    }

    @Test
    void stepDeleteArrivingBeforeItsClickLeavesTheStepDeleted() throws Exception {
        // Double-click coalescing revokes the first single click (step_delete) and replaces it
        // with one clickCount=2 click. Async channels can reorder those payloads so the deletion
        // overtakes the click it deletes; the revocation must still win.
        try (Harness harness = new Harness(temp)) {
            harness.pipeline.accept(click("action-2", BASE.plusMillis(80), 2));
            harness.pipeline.accept(stepDelete("action-1", BASE.plusMillis(90)));
            harness.pipeline.accept(click("action-1", BASE, 1));
            harness.pipeline.close();

            List<CaptureEvent> events = harness.store.read().events();
            List<CaptureEvent.ClickEvent> clicks = events.stream()
                    .filter(CaptureEvent.ClickEvent.class::isInstance)
                    .map(CaptureEvent.ClickEvent.class::cast)
                    .toList();
            assertEquals(1, clicks.size(), "the revoked single click must not resurrect: " + events);
            assertEquals(2, clicks.getFirst().clickCount());
        }
    }

    @Test
    void mergedInputKeystrokesSharingOneClientActionIdStillProduceOneTypedStep() throws Exception {
        // Input signals deliberately reuse one clientActionId across keystrokes (the UI merges
        // them into one row); the one-shot dedup must not swallow the merged flush.
        try (Harness harness = new Harness(temp)) {
            harness.pipeline.accept(input("action-1", BASE, "M", false));
            harness.pipeline.accept(input("action-1", BASE.plusMillis(50), "Mo", false));
            harness.pipeline.accept(input("action-1", BASE.plusMillis(100), "Mohab", true));
            harness.pipeline.close();

            List<CaptureEvent> events = harness.store.read().events();
            assertEquals(1, events.stream().filter(CaptureEvent.TypeEvent.class::isInstance).count(),
                    "merged keystrokes must flush exactly one TypeEvent: " + events);
        }
    }

    @Test
    void deletedInputStepDoesNotResurrectFromALateCommittedDelivery() throws Exception {
        try (Harness harness = new Harness(temp)) {
            harness.pipeline.accept(input("action-1", BASE, "Mohab", true));
            harness.pipeline.accept(stepDelete("action-1", BASE.plusMillis(50)));
            // The committed input arrives again from another channel after its deletion.
            harness.pipeline.accept(input("action-1", BASE.plusMillis(10), "Mohab", true));
            harness.pipeline.close();

            List<CaptureEvent> events = harness.store.read().events();
            assertTrue(events.stream().noneMatch(CaptureEvent.TypeEvent.class::isInstance),
                    "a deleted input step must stay deleted: " + events);
        }
    }

    private static BrowserSignal click(String clientActionId, Instant timestamp, int clickCount) {
        return new BrowserSignal(
                "click",
                timestamp,
                "window-1",
                page(),
                target("greet"),
                Map.of("button", 0, "clickCount", clickCount,
                        "clientActionId", clientActionId, "stepDescription", "Click Greet"));
    }

    private static BrowserSignal keyboard(String clientActionId, Instant timestamp) {
        return new BrowserSignal(
                "keyboard",
                timestamp,
                "window-1",
                page(),
                target("greet"),
                Map.of("keys", List.of("ENTER"),
                        "clientActionId", clientActionId, "stepDescription", "Press ENTER on Greet"));
    }

    private static BrowserSignal input(String clientActionId, Instant timestamp, String value, boolean committed) {
        return new BrowserSignal(
                "input",
                timestamp,
                "window-1",
                page(),
                target("name"),
                Map.of("value", value, "committed", committed,
                        "clientActionId", clientActionId, "stepDescription", "Type into Name"));
    }

    private static BrowserSignal stepDelete(String clientActionId, Instant timestamp) {
        return new BrowserSignal(
                "step_delete",
                timestamp,
                "window-1",
                page(),
                Map.of(),
                Map.of("clientActionId", clientActionId));
    }

    private static Map<String, Object> page() {
        return Map.of("url", "https://example.test", "title", "Example");
    }

    private static Map<String, Object> target(String id) {
        return Map.of(
                "logicalElementId", id,
                "tagName", "button",
                "role", "button",
                "accessibleName", id,
                "attributes", Map.of("id", id),
                "locators", List.of(Map.of(
                        "strategy", "ID",
                        "expression", id,
                        "uniquenessCount", 1,
                        "visible", true,
                        "stable", true,
                        "signals", List.of("STABLE_ATTRIBUTE"))),
                "visible", true,
                "enabled", true,
                "selected", false);
    }

    private static final class Harness implements AutoCloseable {
        private final CaptureSessionStore store;
        private final CaptureEventPipeline pipeline;

        Harness(Path temp) {
            Path output = temp.resolve("capture-" + System.nanoTime() + ".json");
            store = new CaptureSessionStore(output);
            store.start(CaptureSession.start(
                    "duplicate-steps-session",
                    BASE,
                    new BrowserMetadata("chrome", "149", "test", "browser", Map.of())));
            pipeline = new CaptureEventPipeline(
                    store,
                    output,
                    CapturePrivacyPolicy.defaults(),
                    ignored -> { },
                    ignored -> { });
        }

        @Override
        public void close() {
            pipeline.close();
        }
    }
}
