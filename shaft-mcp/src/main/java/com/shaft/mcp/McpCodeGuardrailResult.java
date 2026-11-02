package com.shaft.mcp;

import java.util.List;

/**
 * Result of checking generated or agent-authored code against SHAFT MCP guardrails.
 *
 * @param schemaVersion response schema version
 * @param language checked language
 * @param passed whether no blocking findings were detected
 * @param violations blocking and advisory findings
 * @param warnings safe warnings for the calling agent
 */
public record McpCodeGuardrailResult(
        String schemaVersion,
        String language,
        boolean passed,
        List<McpCodeGuardrailViolation> violations,
        List<String> warnings) {
    /**
     * Current response schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable guardrail result.
     */
    public McpCodeGuardrailResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        language = language == null || language.isBlank() ? "java" : language.trim();
        violations = violations == null ? List.of() : List.copyOf(violations);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
