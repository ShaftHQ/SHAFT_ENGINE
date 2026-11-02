package com.shaft.mcp;

import java.util.List;

/**
 * Preview-only unified diff that inserts reviewed SHAFT code into an existing Java target.
 *
 * <p>The diff is never written to disk by the MCP server. IntelliJ (or another client) shows the
 * preview and applies it only under explicit user approval.</p>
 *
 * @param schemaVersion response schema version
 * @param targetSourcePath repository-relative Java target path
 * @param insertionAnchor method or textual anchor the insertion was placed after
 * @param targetExists whether the target file already exists
 * @param insertedLineCount number of lines the diff would add
 * @param unifiedDiff standard unified-diff text the client can render or apply
 * @param warnings preview-only and guardrail reminders
 */
public record McpCodingPartnerDiff(
        String schemaVersion,
        String targetSourcePath,
        String insertionAnchor,
        boolean targetExists,
        int insertedLineCount,
        String unifiedDiff,
        List<String> warnings) {
    /**
     * Creates an immutable coding partner diff.
     */
    public McpCodingPartnerDiff {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
        targetSourcePath = targetSourcePath == null ? "" : targetSourcePath.trim();
        insertionAnchor = insertionAnchor == null ? "" : insertionAnchor.trim();
        unifiedDiff = unifiedDiff == null ? "" : unifiedDiff;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
