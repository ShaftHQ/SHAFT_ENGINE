package com.shaft.intellij.java;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code fileOpened}/{@code fileClosed} themselves drive {@code FileEditorManager}/{@code Project},
 * which need a live IntelliJ platform and are not exercised by this lightweight Gradle unit test JVM
 * (same documented gap as {@code FailedRunDoctorNotifierTest}). This class instead covers every pure
 * piece those callbacks delegate to: the notification-content decision ({@link
 * CaptureReadinessNotifier#plan(JsonObject)}) and the fire-once/refire-on-reopen dedup ({@link
 * CaptureReadinessNotifier.NotificationGate}), both plain Java with no platform types involved.
 */
class CaptureReadinessNotifierTest {
    private static JsonObject json(String text) {
        return JsonParser.parseString(text).getAsJsonObject();
    }

    @Test
    void planProducesExactlyOneNotificationWithTheFullUntruncatedFindingsText() {
        JsonObject report = json("""
                {
                  "readiness": "READY",
                  "readinessWarnings": ["Step 8 needs a follow-up assertion after navigation."],
                  "warnings": ["Recording used a positional locator."],
                  "flakySteps": ["step-3"],
                  "unsupportedEvents": ["drag-and-drop on canvas"],
                  "requiredUserInputs": ["data.searchbox-input-4"],
                  "fallbackLocators": ["#searchbox_input"]
                }
                """);

        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(report);

        assertTrue(plan.isPresent());
        String content = plan.get().content();
        assertTrue(content.contains("Step 8 needs a follow-up assertion after navigation."));
        assertTrue(content.contains("Recording used a positional locator."));
        assertTrue(content.contains("potentially flaky — step-3"));
        assertTrue(content.contains("not converted to code — drag-and-drop on canvas"));
        assertTrue(content.contains("input required before running — data.searchbox-input-4"));
        assertTrue(content.contains("fallback locator — #searchbox_input"));
        assertFalse(content.contains("..."), "the full findings text must never be clipped with an ellipsis");
        assertEquals("SHAFT Capture readiness: 6 findings", plan.get().title());
    }

    @Test
    void planIsEmptyWhenTheReportHasNoFindingsAtAll() {
        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(json("{}"));

        assertTrue(plan.isEmpty());
    }

    @Test
    void planIsAWarningWhenReadinessIsBlocked() {
        JsonObject report = json("""
                {
                  "readiness": "BLOCKED",
                  "unsupportedEvents": ["drag-and-drop on canvas"]
                }
                """);

        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(report);

        assertTrue(plan.isPresent());
        assertTrue(plan.get().warning());
    }

    @Test
    void planIsAWarningWhenReadinessWarningsArePresentEvenIfNotBlocked() {
        JsonObject report = json("""
                {
                  "readiness": "RISKY",
                  "readinessWarnings": ["Step 8 needs a follow-up assertion after navigation."]
                }
                """);

        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(report);

        assertTrue(plan.isPresent());
        assertTrue(plan.get().warning());
    }

    @Test
    void planIsAWarningWhenWarningsArePresentEvenIfReady() {
        JsonObject report = json("""
                {
                  "readiness": "READY",
                  "warnings": ["Recording used a positional locator."]
                }
                """);

        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(report);

        assertTrue(plan.isPresent());
        assertTrue(plan.get().warning());
    }

    @Test
    void planIsInfoWhenFindingsExistButNoneAreBlockingOrWarnings() {
        JsonObject report = json("""
                {
                  "readiness": "READY",
                  "flakySteps": ["step-3"]
                }
                """);

        Optional<CaptureReadinessNotifier.NotificationPlan> plan = CaptureReadinessNotifier.plan(report);

        assertTrue(plan.isPresent());
        assertFalse(plan.get().warning());
    }

    @Test
    void gateFiresOnceThenSuppressesTheSameUnchangedReport() {
        CaptureReadinessNotifier.NotificationGate gate = new CaptureReadinessNotifier.NotificationGate();
        JsonObject report = json("{\"warnings\": [\"Recording used a positional locator.\"]}");

        assertTrue(gate.evaluate("path/A.java", report).isPresent());
        assertTrue(gate.evaluate("path/A.java", report).isEmpty());
    }

    @Test
    void gateFiresAgainWhenTheReportChanges() {
        CaptureReadinessNotifier.NotificationGate gate = new CaptureReadinessNotifier.NotificationGate();
        JsonObject original = json("{\"warnings\": [\"Recording used a positional locator.\"]}");
        JsonObject changed = json("{\"warnings\": [\"Recording used a positional locator.\", \"a new one\"]}");

        assertTrue(gate.evaluate("path/A.java", original).isPresent());
        assertTrue(gate.evaluate("path/A.java", changed).isPresent());
    }

    @Test
    void gateFiresAgainAfterForgetEvenWithTheSameUnchangedReport() {
        CaptureReadinessNotifier.NotificationGate gate = new CaptureReadinessNotifier.NotificationGate();
        JsonObject report = json("{\"warnings\": [\"Recording used a positional locator.\"]}");

        assertTrue(gate.evaluate("path/A.java", report).isPresent());
        gate.forget("path/A.java");
        assertTrue(gate.evaluate("path/A.java", report).isPresent(),
                "re-opening the file must notify again even when the report has not changed");
    }

    @Test
    void gateNeverRecordsStateForAReportWithNoFindings() {
        CaptureReadinessNotifier.NotificationGate gate = new CaptureReadinessNotifier.NotificationGate();

        assertTrue(gate.evaluate("path/A.java", json("{}")).isEmpty());
        assertTrue(gate.evaluate("path/A.java", json("{}")).isEmpty());
    }
}
