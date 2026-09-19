package com.shaft.mcp;

import java.util.List;

/**
 * Scenario Outline plus editable Examples for an analyzed pack (issue #5950).
 */
public record McpDesignExamples(
        String schemaVersion,
        String status,
        String message,
        List<String> headers,
        List<McpDesignExampleRow> rows,
        String outline,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
    static final String STATUS_ERROR = "error";
    static final String STATUS_DRAFTED = "drafted";
    static final String STATUS_SCENARIO = "scenario";

    public McpDesignExamples {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        headers = headers == null ? List.of() : List.copyOf(headers);
        rows = rows == null ? List.of() : List.copyOf(rows);
        outline = outline == null ? "" : outline;
    }
}
