package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Status for MCP mobile action recording.
 */
public record McpMobileRecordingStatus(
        boolean active,
        Path outputPath,
        String mode,
        int actionCount,
        boolean includeSensitiveValues,
        List<String> warnings) {
    /**
     * Creates an immutable mobile recording status.
     */
    public McpMobileRecordingStatus {
        mode = mode == null ? "" : mode.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
