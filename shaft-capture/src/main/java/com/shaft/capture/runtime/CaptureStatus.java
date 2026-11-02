package com.shaft.capture.runtime;

import com.shaft.capture.model.CaptureReadiness;

import java.time.Instant;
import java.util.List;

/**
 * Safe recorder status returned by CLI and MCP controls.
 *
 * @param state recorder lifecycle state
 * @param sessionId logical capture session identifier
 * @param browser browser family
 * @param currentUrl sanitized current URL
 * @param eventCount persisted semantic event count
 * @param readiness deterministic readiness state
 * @param warnings safe recorder warnings
 * @param outputPath capture JSON output path
 * @param aiEnabled always false for deterministic recording
 * @param processId owning process ID
 * @param startedAt session start time
 * @param networkTransactionCount recorded network transaction count, {@code 0} when API capture is disabled
 * @param lastEndpoints most-recent-first, bounded list of recently observed endpoints ({@code METHOD url})
 * @param pendingSignalCount debounced browser signals (uncommitted typed input, pending clicks) not yet persisted as events
 * @param checkpoints first-class assertion/checkpoint steps for the inspector (issue #5960); empty when none
 */
public record CaptureStatus(
        State state,
        String sessionId,
        String browser,
        String currentUrl,
        int eventCount,
        CaptureReadiness.State readiness,
        List<String> warnings,
        String outputPath,
        boolean aiEnabled,
        long processId,
        Instant startedAt,
        int networkTransactionCount,
        List<String> lastEndpoints,
        int pendingSignalCount,
        List<CheckpointStep> checkpoints) {
    /**
     * Recorder lifecycle states.
     */
    public enum State {
        STARTING,
        ACTIVE,
        STOPPING,
        COMPLETED,
        INCOMPLETE,
        DISCARDED,
        FAILED,
        NOT_RUNNING
    }

    /**
     * Creates immutable safe status.
     */
    public CaptureStatus {
        state = state == null ? State.NOT_RUNNING : state;
        sessionId = text(sessionId);
        browser = text(browser);
        currentUrl = text(currentUrl);
        readiness = readiness == null ? CaptureReadiness.State.READY : readiness;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        outputPath = text(outputPath);
        if (eventCount < 0) {
            throw new IllegalArgumentException("Capture event count cannot be negative.");
        }
        if (networkTransactionCount < 0) {
            throw new IllegalArgumentException("Capture network transaction count cannot be negative.");
        }
        lastEndpoints = lastEndpoints == null ? List.of() : List.copyOf(lastEndpoints);
        if (pendingSignalCount < 0) {
            throw new IllegalArgumentException("Capture pending signal count cannot be negative.");
        }
        checkpoints = checkpoints == null ? List.of() : List.copyOf(checkpoints);
    }

    /**
     * One checkpoint/assertion step surfaced in status JSON for the Automation inspector.
     *
     * @param id stable checkpoint id
     * @param sequence related event sequence
     * @param kind checkpoint kind name ({@code ASSERTION}, {@code USER_MARKER}, …)
     * @param description reviewer-facing description (Ready-pack Then oracle text when suggested)
     */
    public record CheckpointStep(String id, long sequence, String kind, String description) {
        /**
         * Creates an immutable checkpoint step summary.
         */
        public CheckpointStep {
            id = text(id);
            kind = text(kind);
            description = text(description);
        }
    }

    /**
     * Compatibility constructor for callers compiled before checkpoint steps were added to status
     * (issue #5960).
     */
    public CaptureStatus(
            State state,
            String sessionId,
            String browser,
            String currentUrl,
            int eventCount,
            CaptureReadiness.State readiness,
            List<String> warnings,
            String outputPath,
            boolean aiEnabled,
            long processId,
            Instant startedAt,
            int networkTransactionCount,
            List<String> lastEndpoints,
            int pendingSignalCount) {
        this(state, sessionId, browser, currentUrl, eventCount, readiness, warnings, outputPath, aiEnabled,
                processId, startedAt, networkTransactionCount, lastEndpoints, pendingSignalCount, List.of());
    }

    /**
     * Compatibility constructor for callers compiled before the pending-signal counter was added.
     *
     * @param state recorder lifecycle state
     * @param sessionId logical capture session identifier
     * @param browser browser family
     * @param currentUrl sanitized current URL
     * @param eventCount persisted semantic event count
     * @param readiness deterministic readiness state
     * @param warnings safe recorder warnings
     * @param outputPath capture JSON output path
     * @param aiEnabled always false for deterministic recording
     * @param processId owning process ID
     * @param startedAt session start time
     * @param networkTransactionCount recorded network transaction count
     * @param lastEndpoints most-recent-first, bounded list of recently observed endpoints
     */
    public CaptureStatus(
            State state,
            String sessionId,
            String browser,
            String currentUrl,
            int eventCount,
            CaptureReadiness.State readiness,
            List<String> warnings,
            String outputPath,
            boolean aiEnabled,
            long processId,
            Instant startedAt,
            int networkTransactionCount,
            List<String> lastEndpoints) {
        this(state, sessionId, browser, currentUrl, eventCount, readiness, warnings, outputPath, aiEnabled,
                processId, startedAt, networkTransactionCount, lastEndpoints, 0);
    }

    /**
     * Compatibility constructor for callers compiled before network capture counters were added.
     *
     * @param state recorder lifecycle state
     * @param sessionId logical capture session identifier
     * @param browser browser family
     * @param currentUrl sanitized current URL
     * @param eventCount persisted semantic event count
     * @param readiness deterministic readiness state
     * @param warnings safe recorder warnings
     * @param outputPath capture JSON output path
     * @param aiEnabled always false for deterministic recording
     * @param processId owning process ID
     * @param startedAt session start time
     */
    public CaptureStatus(
            State state,
            String sessionId,
            String browser,
            String currentUrl,
            int eventCount,
            CaptureReadiness.State readiness,
            List<String> warnings,
            String outputPath,
            boolean aiEnabled,
            long processId,
            Instant startedAt) {
        this(state, sessionId, browser, currentUrl, eventCount, readiness, warnings, outputPath, aiEnabled,
                processId, startedAt, 0, List.of());
    }

    /**
     * Compatibility constructor for callers compiled before readiness was added.
     *
     * @param state recorder lifecycle state
     * @param sessionId logical capture session identifier
     * @param browser browser family
     * @param currentUrl sanitized current URL
     * @param eventCount persisted semantic event count
     * @param warnings safe recorder warnings
     * @param outputPath capture JSON output path
     * @param aiEnabled always false for deterministic recording
     * @param processId owning process ID
     * @param startedAt session start time
     */
    public CaptureStatus(
            State state,
            String sessionId,
            String browser,
            String currentUrl,
            int eventCount,
            List<String> warnings,
            String outputPath,
            boolean aiEnabled,
            long processId,
            Instant startedAt) {
        this(state, sessionId, browser, currentUrl, eventCount, CaptureReadiness.State.READY, warnings,
                outputPath, aiEnabled, processId, startedAt, 0, List.of());
    }

    /**
     * Returns a safe status for an idle runtime.
     *
     * @return not-running status
     */
    public static CaptureStatus notRunning() {
        return new CaptureStatus(State.NOT_RUNNING, "", "", "", 0, CaptureReadiness.State.READY, List.of(),
                "", false, ProcessHandle.current().pid(), null, 0, List.of());
    }

    private static String text(String value) {
        return value == null ? "" : value;
    }
}
