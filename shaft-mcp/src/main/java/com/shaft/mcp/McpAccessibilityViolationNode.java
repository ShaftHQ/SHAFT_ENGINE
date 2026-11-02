package com.shaft.mcp;

/**
 * A single DOM node implicated in an accessibility violation, returned by {@code browser_accessibility_audit}.
 */
public record McpAccessibilityViolationNode(
        String target,
        String html) {
}
