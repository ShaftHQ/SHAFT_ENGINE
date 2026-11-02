package com.shaft.mcp;

/**
 * One acceptance criterion in a Design-stage story pack (issue #5947).
 *
 * @param id   stable identifier such as {@code AC-01}
 * @param text redacted criterion text
 */
public record McpDesignAcceptanceCriterion(String id, String text) {
}
