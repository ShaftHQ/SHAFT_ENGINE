package com.shaft.intellij.mcp;

/**
 * Classification of MCP invocation failures with recovery guidance.
 */
public enum McpInvocationError {
    /**
     * Request exceeded the timeout deadline.
     */
    TIMEOUT("Request timed out waiting for MCP response.", "Retry"),
    /**
     * MCP server process exited or was killed.
     */
    PROCESS_EXITED("MCP server process exited.", "Restart MCP server"),
    /**
     * MCP server returned malformed or unparseable JSON.
     */
    MALFORMED_RESPONSE("MCP server returned malformed response.", "Retry"),
    /**
     * MCP server returned an error response (tools/call error branch).
     */
    TOOL_ERROR("MCP tool error response.", "View logs"),
    /**
     * Communication lost (broken pipe, IOException during send).
     */
    CONNECTION_LOST("Connection to MCP server lost.", "Restart MCP server");

    private final String message;
    private final String recoveryAction;

    McpInvocationError(String message, String recoveryAction) {
        this.message = message;
        this.recoveryAction = recoveryAction;
    }

    /**
     * Returns the human-readable message for this error category.
     */
    public String message() {
        return message;
    }

    /**
     * Returns the suggested recovery action label.
     */
    public String recoveryAction() {
        return recoveryAction;
    }

    /**
     * Categorizes an exception based on its type and context.
     *
     * @param exception the exception to categorize
     * @return the error category
     */
    public static McpInvocationError categorize(Throwable exception) {
        if (exception == null) {
            return TOOL_ERROR;
        }
        String message = exception.getMessage() == null ? "" : exception.getMessage();

        // Check for process exit first, since the composed message from requestTimedOut()
        // contains both "Timed out waiting for SHAFT MCP response" and "process exit code"
        // when the process dies mid-call. We must detect the exit before falling through to TIMEOUT.
        if (message.contains("process exit code")) {
            return PROCESS_EXITED;
        }
        if (message.contains("Timed out waiting for SHAFT MCP response")) {
            return TIMEOUT;
        }
        if (message.contains("SHAFT MCP returned an error response")) {
            return TOOL_ERROR;
        }
        if (exception instanceof java.io.UncheckedIOException) {
            return CONNECTION_LOST;
        }
        if (message.contains("Failed to read SHAFT MCP response") ||
            message.contains("Interrupted while waiting")) {
            return CONNECTION_LOST;
        }
        return TOOL_ERROR;
    }
}
