package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Result returned after invoking the SHAFT modular project upgrader through MCP.
 *
 * @param schemaVersion result schema version
 * @param projectRoot upgraded project root
 * @param reportPath optional upgrader report path
 * @param dryRun whether the upgrader was run in dry-run mode
 * @param exitCode upgrader process exit code
 * @param stdout captured standard output
 * @param stderr captured standard error
 * @param timedOut whether the process exceeded the timeout
 * @param command executed command
 */
public record McpShaftProjectUpgradeResult(
        String schemaVersion,
        Path projectRoot,
        Path reportPath,
        boolean dryRun,
        int exitCode,
        String stdout,
        String stderr,
        boolean timedOut,
        List<String> command) {
    /**
     * Current result schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable project upgrade result.
     */
    public McpShaftProjectUpgradeResult {
        schemaVersion = text(schemaVersion).isBlank() ? CURRENT_SCHEMA_VERSION : text(schemaVersion);
        stdout = text(stdout);
        stderr = text(stderr);
        command = command == null ? List.of() : List.copyOf(command);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
