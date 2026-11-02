package com.shaft.mcp;

/**
 * Code guardrail finding returned to MCP clients.
 *
 * @param kind stable violation kind
 * @param severity ERROR for blocking findings, WARNING for advisory findings
 * @param message safe human-readable message
 * @param line one-based line number, or 0 when unavailable
 * @param snippet short source excerpt
 */
public record McpCodeGuardrailViolation(
        String kind,
        String severity,
        String message,
        int line,
        String snippet) {
    /**
     * Creates an immutable guardrail violation.
     */
    public McpCodeGuardrailViolation {
        kind = text(kind);
        severity = severity == null || severity.isBlank() ? "ERROR" : severity.trim();
        message = text(message);
        line = Math.max(0, line);
        snippet = text(snippet);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
