package com.shaft.mcp;

import java.util.List;

/**
 * Actionable remediation item returned by MCP analysis.
 *
 * @param id stable action identifier
 * @param title short title
 * @param category diagnosis or workflow category
 * @param action human-readable action
 * @param evidenceIds cited evidence identifiers
 * @param codeBlockIds related code block identifiers
 * @param status action status
 */
public record McpActionRecord(
        String id,
        String title,
        String category,
        String action,
        List<String> evidenceIds,
        List<String> codeBlockIds,
        Status status) {
    /**
     * Action status.
     */
    public enum Status {
        SUGGESTED,
        NEEDS_EVIDENCE,
        PROVIDER_ADVISORY
    }

    /**
     * Creates an immutable action record.
     */
    public McpActionRecord {
        id = text(id);
        title = text(title);
        category = text(category);
        action = text(action);
        evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
        codeBlockIds = codeBlockIds == null ? List.of() : List.copyOf(codeBlockIds);
        status = status == null ? Status.SUGGESTED : status;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
