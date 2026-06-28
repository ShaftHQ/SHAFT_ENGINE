package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Result returned after generating a SHAFT project through MCP.
 *
 * @param schemaVersion result schema version
 * @param projectDirectory generated project directory
 * @param pomPath generated Maven POM path
 * @param runner selected test runner
 * @param platform selected starting platform
 * @param templateProject reused SHAFT example project
 * @param addedModules SHAFT modules added to the generated POM
 * @param warnings non-fatal generation warnings
 */
public record McpShaftProjectGenerationResult(
        String schemaVersion,
        Path projectDirectory,
        Path pomPath,
        String runner,
        String platform,
        String templateProject,
        List<String> addedModules,
        List<String> warnings) {
    /**
     * Current result schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable project generation result.
     */
    public McpShaftProjectGenerationResult {
        schemaVersion = text(schemaVersion).isBlank() ? CURRENT_SCHEMA_VERSION : text(schemaVersion);
        runner = text(runner);
        platform = text(platform);
        templateProject = text(templateProject);
        addedModules = addedModules == null ? List.of() : List.copyOf(addedModules);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
