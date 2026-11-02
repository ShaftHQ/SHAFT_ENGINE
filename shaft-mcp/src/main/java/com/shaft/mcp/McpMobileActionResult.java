package com.shaft.mcp;

import java.util.List;

/**
 * Mobile action result returned by MCP mobile tools.
 */
public record McpMobileActionResult(
        String action,
        boolean recorded,
        McpCodeBlock codeBlock,
        List<String> warnings) {
    /**
     * Creates an immutable mobile action result.
     */
    public McpMobileActionResult {
        action = action == null ? "" : action.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
