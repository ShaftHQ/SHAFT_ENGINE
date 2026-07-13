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
 */
public record McpMobileRecordingStatus(
        boolean active,
        Path outputPath,
        String mode,
        int actionCount,
        boolean includeSensitiveValues,
        List<String> warnings,
        CaptureReadiness.State readiness) {
    /**
     * Creates an immutable mobile recording status.
     */
    public McpMobileRecordingStatus {
        mode = mode == null ? "" : mode.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        readiness = readiness == null ? CaptureReadiness.State.READY : readiness;
    }

    /**
     * Back-compatible constructor for callers that predate the shared readiness contract; defaults
     * readiness to {@link CaptureReadiness.State#READY}.
     */
    public McpMobileRecordingStatus(
            boolean active,
            Path outputPath,
            String mode,
            int actionCount,
            boolean includeSensitiveValues,
            List<String> warnings) {
        this(active, outputPath, mode, actionCount, includeSensitiveValues, warnings, CaptureReadiness.State.READY);
    }
}
