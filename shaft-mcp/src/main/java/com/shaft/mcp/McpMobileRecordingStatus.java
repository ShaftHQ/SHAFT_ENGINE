package com.shaft.mcp;

import com.shaft.capture.model.CaptureReadiness;

import java.nio.file.Path;
import java.util.List;

/**
 * Status for MCP mobile action recording.
 *
 * <p>Shares the web {@link com.shaft.capture.runtime.CaptureStatus} vocabulary so the mobile
 * recorder speaks the same language as the web Capture recorder (issue #3497): recorded units are
 * "steps" (surfaced via {@code actionCount}), and {@code readiness} reuses the frozen web
 * {@link CaptureReadiness.State} (Ready/Risky/Blocked) rather than forking a parallel enum.
 *
 * @param readiness Ready/Risky/Blocked verdict rolled up from step warnings; never {@code null}
 * @param steps per-step summaries (stable stepId, sequence, action, risk) an agent can target with
 *         {@code mobile_step_delete}/{@code mobile_step_reorder}; empty when inactive
 */
public record McpMobileRecordingStatus(
        boolean active,
        Path outputPath,
        String mode,
        int actionCount,
        boolean includeSensitiveValues,
        List<String> warnings,
        CaptureReadiness.State readiness,
        List<McpMobileStepSummary> steps) {
    /**
     * Creates an immutable mobile recording status.
     */
    public McpMobileRecordingStatus {
        mode = mode == null ? "" : mode.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        readiness = readiness == null ? CaptureReadiness.State.READY : readiness;
        steps = steps == null ? List.of() : List.copyOf(steps);
    }

    /**
     * Back-compatible constructor for callers that predate the step summaries (#3526); defaults
     * {@code steps} to an empty list.
     */
    public McpMobileRecordingStatus(
            boolean active,
            Path outputPath,
            String mode,
            int actionCount,
            boolean includeSensitiveValues,
            List<String> warnings,
            CaptureReadiness.State readiness) {
        this(active, outputPath, mode, actionCount, includeSensitiveValues, warnings, readiness, List.of());
    }

    /**
     * Back-compatible constructor for callers that predate the shared readiness contract; defaults
     * readiness to {@link CaptureReadiness.State#READY} and {@code steps} to an empty list.
     */
    public McpMobileRecordingStatus(
            boolean active,
            Path outputPath,
            String mode,
            int actionCount,
            boolean includeSensitiveValues,
            List<String> warnings) {
        this(active, outputPath, mode, actionCount, includeSensitiveValues, warnings, CaptureReadiness.State.READY,
                List.of());
    }
}
