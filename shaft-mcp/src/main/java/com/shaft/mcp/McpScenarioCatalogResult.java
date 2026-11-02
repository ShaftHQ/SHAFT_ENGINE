package com.shaft.mcp;

import java.util.List;

/**
 * Scenario catalog response returned to MCP clients.
 *
 * @param schemaVersion response schema version
 * @param area requested catalog area
 * @param intent requested user intent
 * @param scenarios matching scenarios
 * @param guidanceRules global rules for all scenarios
 * @param warnings safe warnings for the calling agent
 */
public record McpScenarioCatalogResult(
        String schemaVersion,
        String area,
        String intent,
        List<McpTestAutomationScenario> scenarios,
        List<String> guidanceRules,
        List<String> warnings) {
    /**
     * Current response schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable catalog result.
     */
    public McpScenarioCatalogResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        area = text(area);
        intent = text(intent);
        scenarios = scenarios == null ? List.of() : List.copyOf(scenarios);
        guidanceRules = guidanceRules == null ? List.of() : List.copyOf(guidanceRules);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
