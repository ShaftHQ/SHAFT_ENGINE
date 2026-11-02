package com.shaft.mcp;

import java.util.List;

/**
 * MCP healer result for guarded Selenium/SHAFT test reruns and review-only fixes.
 *
 * @param schemaVersion result schema version
 * @param status overall healer status
 * @param attempts guarded rerun attempts
 * @param analysis latest Doctor analysis, when a failing Allure attempt was available
 * @param actions review-only remediation actions
 * @param codeBlocks review-only snippets and agent handoff instructions
 * @param warnings safe warnings
 */
public record McpHealerRunResult(
        String schemaVersion,
        Status status,
        List<McpHealerAttemptResult> attempts,
        McpAnalysisReport analysis,
        List<McpActionRecord> actions,
        List<McpCodeBlock> codeBlocks,
        List<String> warnings) {
    /**
     * Current result schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Overall healer status.
     */
    public enum Status {
        PASSED,
        FAILED_WITH_SUGGESTIONS,
        PRODUCT_BUG_SUSPECTED,
        GUARDRAIL_STOPPED
    }

    /**
     * Creates an immutable healer result.
     */
    public McpHealerRunResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        status = status == null ? Status.GUARDRAIL_STOPPED : status;
        attempts = attempts == null ? List.of() : List.copyOf(attempts);
        actions = actions == null ? List.of() : List.copyOf(actions);
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
