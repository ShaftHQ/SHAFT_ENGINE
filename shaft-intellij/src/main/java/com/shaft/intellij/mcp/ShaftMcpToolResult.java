package com.shaft.intellij.mcp;

/**
 * Result returned to the IntelliJ tool window after an MCP call.
 *
 * @param success whether the request completed successfully
 * @param output formatted output or diagnostic text
 */
public record ShaftMcpToolResult(boolean success, String output) {
    /**
     * Creates a successful result.
     *
     * @param output output
     * @return result
     */
    public static ShaftMcpToolResult success(String output) {
        return new ShaftMcpToolResult(true, output);
    }

    /**
     * Creates a failed result.
     *
     * @param output diagnostic output
     * @return result
     */
    public static ShaftMcpToolResult failure(String output) {
        return new ShaftMcpToolResult(false, output);
    }
}
