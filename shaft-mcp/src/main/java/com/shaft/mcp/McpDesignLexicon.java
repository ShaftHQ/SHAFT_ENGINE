package com.shaft.mcp;

import java.util.List;

/**
 * Project-local ubiquitous-language suggestions (issue #5951).
 */
public record McpDesignLexicon(
        String schemaVersion,
        String status,
        String message,
        List<String> suggestions,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";

    public McpDesignLexicon {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? "error" : status;
        message = message == null ? "" : message;
        suggestions = suggestions == null ? List.of() : List.copyOf(suggestions);
    }
}
