package com.shaft.mcp;

import java.util.List;

/**
 * One guarded rerun attempt made by the MCP healer.
 *
 * @param attemptNumber one-based attempt number
 * @param command exact command argument vector
 * @param exitCode process exit code, or -1 when timed out
 * @param timedOut whether the process exceeded the timeout
 * @param passed whether the process and expected Allure evidence passed
 * @param allureResultCount populated Allure result files observed for this attempt
 * @param failedAllureResultCount non-passing Allure result files observed for this attempt
 * @param diagnostics bounded redacted process diagnostics
 * @param allureResultPaths changed Allure result files used for analysis
 */
public record McpHealerAttemptResult(
        int attemptNumber,
        List<String> command,
        int exitCode,
        boolean timedOut,
        boolean passed,
        int allureResultCount,
        int failedAllureResultCount,
        String diagnostics,
        List<String> allureResultPaths) {
    /**
     * Creates an immutable attempt result.
     */
    public McpHealerAttemptResult {
        attemptNumber = Math.max(1, attemptNumber);
        command = command == null ? List.of() : List.copyOf(command);
        allureResultCount = Math.max(0, allureResultCount);
        failedAllureResultCount = Math.max(0, failedAllureResultCount);
        diagnostics = diagnostics == null ? "" : diagnostics.trim();
        allureResultPaths = allureResultPaths == null ? List.of() : List.copyOf(allureResultPaths);
    }
}
