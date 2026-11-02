package com.shaft.mcp;

import java.util.List;

/**
 * Mobile accessibility tree or current Appium page source returned for MCP inspection.
 */
public record McpMobileAccessibilityTree(
        String currentContext,
        String source,
        int characterCount,
        boolean truncated,
        List<String> warnings) {
    /**
     * Creates an immutable mobile accessibility tree snapshot.
     */
    public McpMobileAccessibilityTree {
        currentContext = currentContext == null ? "" : currentContext.trim();
        source = source == null ? "" : source;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
