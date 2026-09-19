package com.shaft.mcp;

import java.util.List;

/**
 * AC-to-scenario coverage for a Design pack (issue #5952). Never writes files.
 *
 * @param schemaVersion      coverage schema
 * @param status             {@code ok}, {@code blocked}, or {@code error}
 * @param message            human-readable status
 * @param covered            AC IDs referenced by at least one tagged scenario
 * @param uncovered          AC IDs with no scenario and no waive
 * @param waived             residual-risk AC with a reason string
 * @param untaggedScenarios  scenario names missing an {@code @AC-*} tag
 * @param readyBlocked       true when uncovered AC or untagged scenarios remain
 * @param wroteFiles         always {@code false}
 */
public record McpDesignCoverage(
        String schemaVersion,
        String status,
        String message,
        List<String> covered,
        List<String> uncovered,
        List<McpDesignWaivedAc> waived,
        List<String> untaggedScenarios,
        boolean readyBlocked,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
    static final String STATUS_OK = "ok";
    static final String STATUS_BLOCKED = "blocked";
    static final String STATUS_ERROR = "error";

    public McpDesignCoverage {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        covered = covered == null ? List.of() : List.copyOf(covered);
        uncovered = uncovered == null ? List.of() : List.copyOf(uncovered);
        waived = waived == null ? List.of() : List.copyOf(waived);
        untaggedScenarios = untaggedScenarios == null ? List.of() : List.copyOf(untaggedScenarios);
    }
}
