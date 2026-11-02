package com.shaft.mcp;

import java.util.List;

/**
 * Structured story pack produced by {@code design_ingest}. Never writes repository files.
 *
 * @param schemaVersion        pack schema
 * @param status               {@code ok} or {@code error}
 * @param message              human-readable status
 * @param actor                parsed actor, if any
 * @param outcome              parsed outcome, if any
 * @param sourceKind           {@code paste}, {@code file}, or {@code url}
 * @param acceptanceCriteria   numbered criteria
 * @param warnings             non-fatal notes
 * @param wroteFiles           always {@code false} for ingest
 */
public record McpDesignPack(
        String schemaVersion,
        String status,
        String message,
        String actor,
        String outcome,
        String sourceKind,
        List<McpDesignAcceptanceCriterion> acceptanceCriteria,
        List<String> warnings,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
}
