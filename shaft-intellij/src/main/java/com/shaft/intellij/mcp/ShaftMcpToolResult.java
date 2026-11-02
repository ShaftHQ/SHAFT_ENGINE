package com.shaft.intellij.mcp;

import org.jetbrains.annotations.Nullable;

/**
 * Result returned to the IntelliJ tool window after an MCP call.
 *
 * @param success whether the request completed successfully
 * @param output formatted output or diagnostic text
 * @param errorCategory the error category if failed, or null if successful
 * @param recoveryAction the suggested recovery action if failed, or null if successful
 */
public record ShaftMcpToolResult(boolean success, String output, @Nullable McpInvocationError errorCategory, @Nullable String recoveryAction) {
    /**
     * Creates a successful result.
     *
     * @param output output
     * @return result
     */
    public static ShaftMcpToolResult success(String output) {
        return new ShaftMcpToolResult(true, output, null, null);
    }

    /**
     * Creates a failed result with error categorization.
     *
     * @param output diagnostic output
     * @param errorCategory the error category
     * @param recoveryAction the suggested recovery action
     * @return result
     */
    public static ShaftMcpToolResult failure(String output, McpInvocationError errorCategory, String recoveryAction) {
        return new ShaftMcpToolResult(false, output, errorCategory, recoveryAction);
    }

    /**
     * Creates a failed result without categorization (legacy).
     *
     * @param output diagnostic output
     * @return result
     */
    public static ShaftMcpToolResult failure(String output) {
        return new ShaftMcpToolResult(false, output, null, null);
    }
}
