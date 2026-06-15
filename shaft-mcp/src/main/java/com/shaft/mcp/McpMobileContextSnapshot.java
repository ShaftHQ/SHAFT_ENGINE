package com.shaft.mcp;

import java.util.List;

/**
 * Current Appium context state for native, hybrid, or mobile web sessions.
 */
public record McpMobileContextSnapshot(
        String currentContext,
        List<String> contexts,
        String pageSource,
        int sourceCharacterCount,
        boolean truncated,
        List<String> warnings) {
    /**
     * Creates an immutable context snapshot.
     */
    public McpMobileContextSnapshot {
        currentContext = currentContext == null ? "" : currentContext.trim();
        contexts = contexts == null ? List.of() : List.copyOf(contexts);
        pageSource = pageSource == null ? "" : pageSource;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
