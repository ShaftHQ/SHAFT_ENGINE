package com.shaft.mcp;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Manifest of local evidence files reviewers should inspect for Capture codegen changes.
 *
 * @param schemaVersion manifest schema version
 * @param artifactPaths local artifact paths
 * @param validationCommands suggested validation commands
 * @param warnings safe warnings
 */
public record McpEvidencePack(
        String schemaVersion,
        List<String> artifactPaths,
        List<String> validationCommands,
        List<String> warnings) {
    /**
     * Creates an immutable evidence pack.
     */
    public McpEvidencePack {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion;
        artifactPaths = artifactPaths == null ? List.of() : List.copyOf(artifactPaths);
        validationCommands = validationCommands == null ? List.of() : List.copyOf(validationCommands);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    static McpEvidencePack of(Path sourcePath, Path reportPath, Path reviewPath, List<Path> screenshots) {
        List<String> artifacts = new ArrayList<>();
        add(artifacts, sourcePath);
        add(artifacts, reportPath);
        add(artifacts, reviewPath);
        if (screenshots != null) {
            screenshots.forEach(path -> add(artifacts, path));
        }
        return new McpEvidencePack(
                "1.0",
                artifacts,
                List.of(
                        "mvn -pl shaft-mcp -am -Dtest=CaptureServiceTest test -DheadlessExecution=true -Dgpg.skip",
                        "py -3 scripts/ci/validate_shaft_mcp_configuration.py"),
                List.of("Manifest only; files remain local."));
    }

    private static void add(List<String> values, Path path) {
        if (path != null) {
            values.add(path.toString());
        }
    }
}
