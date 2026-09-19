package com.shaft.mcp;

import java.util.List;

/**
 * Deterministic Gherkin quality lint (issue #5953). Never writes files.
 *
 * @param schemaVersion lint schema
 * @param status        {@code ok}, {@code blocked}, or {@code error}
 * @param message       human-readable status
 * @param findings      ordered findings
 * @param acceptBlocked true while unwaived error-level findings remain
 * @param wroteFiles    always {@code false}
 */
public record McpDesignLint(
        String schemaVersion,
        String status,
        String message,
        List<McpDesignLintFinding> findings,
        boolean acceptBlocked,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
    static final String STATUS_OK = "ok";
    static final String STATUS_BLOCKED = "blocked";
    static final String STATUS_ERROR = "error";

    public McpDesignLint {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        findings = findings == null ? List.of() : List.copyOf(findings);
    }
}
