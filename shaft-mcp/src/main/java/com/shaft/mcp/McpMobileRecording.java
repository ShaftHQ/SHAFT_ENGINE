package com.shaft.mcp;

import java.util.List;

/**
 * Persisted MCP mobile action recording.
 */
public record McpMobileRecording(
        String schemaVersion,
        String mode,
        String startedAt,
        String stoppedAt,
        boolean includeSensitiveValues,
        List<McpMobileRecordedAction> actions,
        List<String> warnings) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable recording.
     */
    public McpMobileRecording {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        mode = mode == null ? "" : mode.trim();
        startedAt = startedAt == null ? "" : startedAt.trim();
        stoppedAt = stoppedAt == null ? "" : stoppedAt.trim();
        actions = actions == null ? List.of() : List.copyOf(actions);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
