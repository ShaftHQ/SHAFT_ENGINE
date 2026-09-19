package com.shaft.mcp;

import java.util.List;

/**
 * Fluent-API gap map over accepted Gherkin steps (issue #5954). Never writes files.
 *
 * @param schemaVersion map schema
 * @param status        {@code ok} or {@code error}
 * @param message       human-readable status
 * @param steps         per-step classifications
 * @param wroteFiles    always {@code false}
 */
public record McpDesignGapMap(
        String schemaVersion,
        String status,
        String message,
        List<McpDesignGapMapStep> steps,
        boolean wroteFiles) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";
    public static final String STATUS_OK = "ok";
    public static final String STATUS_ERROR = "error";

    public McpDesignGapMap {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        steps = steps == null ? List.of() : List.copyOf(steps);
    }
}
