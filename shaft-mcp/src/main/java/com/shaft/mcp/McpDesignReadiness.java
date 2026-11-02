package com.shaft.mcp;

import java.util.List;

/**
 * Design pack readiness / example-mapping gate (issue #5955). Never writes files.
 *
 * @param schemaVersion   readiness schema
 * @param status          {@code draft}, {@code needs_questions}, or {@code ready}
 * @param message         human-readable status
 * @param unmetConditions ordered blockers preventing Ready / handoff
 * @param handoffAllowed  true only when status is {@code ready}
 * @param wroteFiles      always {@code false}
 */
public record McpDesignReadiness(
        String schemaVersion,
        String status,
        String message,
        List<String> unmetConditions,
        boolean handoffAllowed,
        boolean wroteFiles) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";
    public static final String STATUS_DRAFT = "draft";
    public static final String STATUS_NEEDS_QUESTIONS = "needs_questions";
    public static final String STATUS_READY = "ready";

    public McpDesignReadiness {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_DRAFT : status;
        message = message == null ? "" : message;
        unmetConditions = unmetConditions == null ? List.of() : List.copyOf(unmetConditions);
    }
}
