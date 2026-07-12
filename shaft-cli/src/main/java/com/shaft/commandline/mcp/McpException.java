package com.shaft.commandline.mcp;

/**
 * Raised when an MCP interaction fails: a JSON-RPC protocol error, a transport failure,
 * or a tool-level error result.
 */
public class McpException extends RuntimeException {

    /**
     * @param message the failure description
     */
    public McpException(String message) {
        super(message);
    }

    /**
     * @param message the failure description
     * @param cause   the underlying cause
     */
    public McpException(String message, Throwable cause) {
        super(message, cause);
    }
}
