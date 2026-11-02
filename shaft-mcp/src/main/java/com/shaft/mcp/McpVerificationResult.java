package com.shaft.mcp;

import java.util.List;

/**
 * Result of a guarded, focused Maven verification command.
 *
 * @param schemaVersion response schema version
 * @param status PASSED, FAILED, or TIMED_OUT
 * @param exitCode process exit code (-1 when timed out)
 * @param timedOut whether the command exceeded the verification timeout
 * @param command the exact tokenized command that ran after guardrails
 * @param outputSummary redacted, length-bounded process output
 * @param warnings safe guardrail reminders
 */
public record McpVerificationResult(
        String schemaVersion,
        String status,
        int exitCode,
        boolean timedOut,
        List<String> command,
        String outputSummary,
        List<String> warnings) {
    /**
     * Creates an immutable verification result.
     */
    public McpVerificationResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
        status = status == null || status.isBlank() ? "FAILED" : status.trim();
        command = command == null ? List.of() : List.copyOf(command);
        outputSummary = outputSummary == null ? "" : outputSummary;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
