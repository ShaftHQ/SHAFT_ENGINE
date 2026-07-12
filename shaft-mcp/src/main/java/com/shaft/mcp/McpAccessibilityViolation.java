package com.shaft.mcp;

import java.util.List;

/**
 * A single axe-core rule violation, returned by {@code browser_accessibility_audit}.
 */
public record McpAccessibilityViolation(
        String id,
        String impact,
        String description,
        String help,
        String helpUrl,
        List<String> tags,
        int nodeCount,
        List<McpAccessibilityViolationNode> nodes) {
}
