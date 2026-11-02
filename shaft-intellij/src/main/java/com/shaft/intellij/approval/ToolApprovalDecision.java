package com.shaft.intellij.approval;

/**
 * Enum representing the possible approval decisions for tool invocations.
 */
public enum ToolApprovalDecision {
    /**
     * Approve this specific tool invocation once; the grant is consumed after a single check.
     */
    APPROVE_ONCE("Approve once"),

    /**
     * Approve all future invocations of this tool permanently.
     */
    APPROVE_TOOL_ALWAYS("Approve tool always"),

    /**
     * Approve all tools for all future invocations.
     */
    APPROVE_ALL_TOOLS("Approve all tools"),

    /**
     * Deny the tool invocation.
     */
    DENY("Deny");

    private final String displayLabel;

    ToolApprovalDecision(String displayLabel) {
        this.displayLabel = displayLabel;
    }

    /**
     * Returns the human-readable display label for this decision.
     *
     * @return display label
     */
    public String getDisplayLabel() {
        return displayLabel;
    }
}
