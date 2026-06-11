package com.shaft.capture.runtime;

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
 * @param warnings safe recorder warnings
 * @param outputPath capture JSON output path
 * @param aiEnabled always false for deterministic recording
 * @param processId owning process ID
 * @param startedAt session start time
 */
public record CaptureStatus(
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
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        outputPath = text(outputPath);
        if (eventCount < 0) {
            throw new IllegalArgumentException("Capture event count cannot be negative.");
        }
    }

    /**
     * Returns a safe status for an idle runtime.
     *
     * @return not-running status
     */
    public static CaptureStatus notRunning() {
        return new CaptureStatus(State.NOT_RUNNING, "", "", "", 0, List.of(),
                "", false, ProcessHandle.current().pid(), null);
    }

    private static String text(String value) {
        return value == null ? "" : value;
    }
}
