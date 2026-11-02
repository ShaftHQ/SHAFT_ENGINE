package com.shaft.mcp;

import java.util.List;

/**
 * Non-asserting axe-core accessibility audit result returned by {@code browser_accessibility_audit}.
 * The audit is reported as data; callers decide whether the violations found are acceptable.
 */
public record McpAccessibilityAuditResult(
        double accessibilityScore,
        int violationsCount,
        int passesCount,
        List<String> wcagTags,
        List<McpAccessibilityViolation> violations,
        List<String> warnings) {
}
