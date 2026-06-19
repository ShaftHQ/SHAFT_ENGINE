package com.shaft.mcp;

import java.util.List;

/**
 * Official SHAFT user-guide search result for MCP agents.
 *
 * @param schemaVersion result schema version
 * @param query searched query
 * @param sourceIndexUrl official guide search index URL
 * @param matches ranked official guide matches
 * @param guidanceRules rules agents should apply when writing SHAFT code
 * @param warnings safe warnings about missing or partial guide evidence
 */
public record McpGuideSearchResult(
        String schemaVersion,
        String query,
        String sourceIndexUrl,
        List<McpGuideMatch> matches,
        List<String> guidanceRules,
        List<String> warnings) {
    /**
     * Current result schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable guide search result.
     */
    public McpGuideSearchResult {
        schemaVersion = text(schemaVersion).isBlank() ? CURRENT_SCHEMA_VERSION : text(schemaVersion);
        query = text(query);
        sourceIndexUrl = text(sourceIndexUrl);
        matches = matches == null ? List.of() : List.copyOf(matches);
        guidanceRules = guidanceRules == null ? List.of() : List.copyOf(guidanceRules);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
