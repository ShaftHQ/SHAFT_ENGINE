package com.shaft.mcp;

/**
 * Declarative Gherkin review artifact. Never writes repository files (issue #5949).
 *
 * @param schemaVersion analysis/draft schema
 * @param status        {@code error} or {@code drafted}
 * @param message       human-readable status
 * @param feature       Gherkin Feature text, or empty when blocked
 * @param wroteFiles    always {@code false}
 */
public record McpDesignGherkinDraft(
        String schemaVersion,
        String status,
        String message,
        String feature,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
    static final String STATUS_ERROR = "error";
    static final String STATUS_DRAFTED = "drafted";

    public McpDesignGherkinDraft {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        feature = feature == null ? "" : feature;
    }
}
