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
        List<String> lastEndpoints) {
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
