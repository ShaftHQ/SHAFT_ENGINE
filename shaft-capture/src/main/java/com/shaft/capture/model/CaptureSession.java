package com.shaft.capture.model;

import com.fasterxml.jackson.databind.JsonNode;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Immutable versioned SHAFT Capture session.
 *
 * @param schemaVersion recording schema version
 * @param sessionId stable capture identifier
 * @param status session status
 * @param startedAt capture start time
 * @param endedAt capture end time when stopped
 * @param browser browser metadata
 * @param events ordered capture events
 * @param checkpoints ordered human-review checkpoints
 * @param dataReferences external data references used by events
 * @param redactionSummary safe privacy summary
 * @param extensions forward-compatible generator or platform data
 */
public record CaptureSession(
        String schemaVersion,
        String sessionId,
        SessionStatus status,
        Instant startedAt,
        Instant endedAt,
        BrowserMetadata browser,
        List<CaptureEvent> events,
        List<Checkpoint> checkpoints,
        List<ExternalTestDataReference> dataReferences,
        RedactionSummary redactionSummary,
        Map<String, JsonNode> extensions) {
    /**
     * Current persisted schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Session lifecycle states.
     */
    public enum SessionStatus {
        INCOMPLETE,
        COMPLETED
    }

    /**
     * Creates an immutable validated session.
     */
    public CaptureSession {
        schemaVersion = ModelSupport.requireText(schemaVersion, "Schema version");
        sessionId = ModelSupport.requireText(sessionId, "Session ID");
        status = status == null ? SessionStatus.INCOMPLETE : status;
        if (startedAt == null) {
            throw new IllegalArgumentException("Session start time is required.");
        }
        if (endedAt != null && endedAt.isBefore(startedAt)) {
            throw new IllegalArgumentException("Session end time cannot precede its start time.");
        }
        if (status == SessionStatus.COMPLETED && endedAt == null) {
            throw new IllegalArgumentException("Completed sessions require an end time.");
        }
        if (browser == null) {
            throw new IllegalArgumentException("Browser metadata is required.");
        }
        events = sortedEvents(events);
        checkpoints = sortedCheckpoints(checkpoints);
        dataReferences = sortedReferences(dataReferences);
        redactionSummary = redactionSummary == null ? RedactionSummary.empty() : redactionSummary;
        extensions = ModelSupport.extensions(extensions);
    }

    /**
     * Starts an incomplete session that remains readable after interruption.
     *
     * @param sessionId stable capture identifier
     * @param startedAt capture start
     * @param browser browser metadata
     * @return new incomplete session
     */
    public static CaptureSession start(String sessionId, Instant startedAt, BrowserMetadata browser) {
        return new CaptureSession(CURRENT_SCHEMA_VERSION, sessionId, SessionStatus.INCOMPLETE,
                startedAt, null, browser, List.of(), List.of(), List.of(), RedactionSummary.empty(), Map.of());
    }

    /**
     * Returns a copy containing an additional event.
     *
     * @param event event to append
     * @return updated session
     */
    public CaptureSession append(CaptureEvent event) {
        ensureIncomplete();
        List<CaptureEvent> updated = new ArrayList<>(events);
        updated.add(event);
        return new CaptureSession(schemaVersion, sessionId, status, startedAt, endedAt, browser,
                updated, checkpoints, dataReferences, redactionSummary, extensions);
    }

    /**
     * Returns a copy containing an additional checkpoint.
     *
     * @param checkpoint checkpoint to append
     * @return updated session
     */
    public CaptureSession checkpoint(Checkpoint checkpoint) {
        ensureIncomplete();
        List<Checkpoint> updated = new ArrayList<>(checkpoints);
        updated.add(checkpoint);
        return new CaptureSession(schemaVersion, sessionId, status, startedAt, endedAt, browser,
                events, updated, dataReferences, redactionSummary, extensions);
    }

    /**
     * Returns a copy containing external data references and their safe summary.
     *
     * @param references references to add
     * @param summary privacy summary to merge
     * @return updated session
     */
    public CaptureSession withDataReferences(
            List<ExternalTestDataReference> references,
            RedactionSummary summary) {
        ensureIncomplete();
        List<ExternalTestDataReference> updated = new ArrayList<>(dataReferences);
        if (references != null) {
            updated.addAll(references);
        }
        return new CaptureSession(schemaVersion, sessionId, status, startedAt, endedAt, browser,
                events, checkpoints, updated, redactionSummary.merge(summary), extensions);
    }

    /**
     * Returns a completed copy.
     *
     * @param stoppedAt stop time
     * @return completed session
     */
    public CaptureSession complete(Instant stoppedAt) {
        ensureIncomplete();
        return new CaptureSession(schemaVersion, sessionId, SessionStatus.COMPLETED, startedAt,
                stoppedAt, browser, events, checkpoints, dataReferences, redactionSummary, extensions);
    }

    /**
     * Returns an explicitly interrupted incomplete copy.
     *
     * @param interruptedAt interruption time
     * @return incomplete session with an end time
     */
    public CaptureSession interrupt(Instant interruptedAt) {
        ensureIncomplete();
        return new CaptureSession(schemaVersion, sessionId, SessionStatus.INCOMPLETE, startedAt,
                interruptedAt, browser, events, checkpoints, dataReferences, redactionSummary, extensions);
    }

    private void ensureIncomplete() {
        if (status != SessionStatus.INCOMPLETE) {
            throw new IllegalStateException("Completed capture sessions cannot be modified.");
        }
    }

    private static List<CaptureEvent> sortedEvents(List<CaptureEvent> values) {
        List<CaptureEvent> sorted = values == null
                ? new ArrayList<>()
                : new ArrayList<>(values);
        sorted.sort(Comparator.comparingLong(event -> event.context().sequence()));
        Set<Long> sequences = new HashSet<>();
        for (CaptureEvent event : sorted) {
            if (event == null || !sequences.add(event.context().sequence())) {
                throw new IllegalArgumentException("Capture event sequences must be unique.");
            }
        }
        return List.copyOf(sorted);
    }

    private static List<Checkpoint> sortedCheckpoints(List<Checkpoint> values) {
        List<Checkpoint> sorted = values == null
                ? new ArrayList<>()
                : new ArrayList<>(values);
        sorted.sort(Comparator.comparingLong(Checkpoint::sequence).thenComparing(Checkpoint::id));
        return List.copyOf(sorted);
    }

    private static List<ExternalTestDataReference> sortedReferences(List<ExternalTestDataReference> values) {
        List<ExternalTestDataReference> sorted = values == null
                ? new ArrayList<>()
                : new ArrayList<>(values);
        sorted.sort(Comparator.comparing(ExternalTestDataReference::id));
        Set<String> ids = new HashSet<>();
        for (ExternalTestDataReference reference : sorted) {
            if (reference == null || !ids.add(reference.id())) {
                throw new IllegalArgumentException("External test-data reference IDs must be unique.");
            }
        }
        return List.copyOf(sorted);
    }
}
