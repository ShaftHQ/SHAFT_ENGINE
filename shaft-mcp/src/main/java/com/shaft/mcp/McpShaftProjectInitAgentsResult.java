package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Result returned after scaffolding SHAFT agent/skill definitions into a user's test repo.
 *
 * @param schemaVersion result schema version
 * @param targetDirectory repo directory the agent definitions were scaffolded into
 * @param loop coding-agent loop the definitions were generated for
 * @param generatedFiles files written or overwritten by this call
 * @param warnings non-fatal warnings, including files skipped because they already existed
 */
public record McpShaftProjectInitAgentsResult(
        String schemaVersion,
        Path targetDirectory,
        String loop,
        List<Path> generatedFiles,
        List<String> warnings) {
    /**
     * Current result schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable agent-scaffolding result.
     */
    public McpShaftProjectInitAgentsResult {
        schemaVersion = text(schemaVersion).isBlank() ? CURRENT_SCHEMA_VERSION : text(schemaVersion);
        loop = text(loop);
        generatedFiles = generatedFiles == null ? List.of() : List.copyOf(generatedFiles);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
