package com.shaft.mcp;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.Map;

/**
 * Resolves the writable local root used by the MCP server for generated engine artifacts.
 */
final class McpRuntimePaths {
    static final String WORKSPACE_ENVIRONMENT_VARIABLE = "SHAFT_MCP_WORKSPACE_ROOT";
    static final String WORKSPACE_SYSTEM_PROPERTY = "shaft.mcp.workspaceRoot";

    private McpRuntimePaths() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves and creates the effective MCP runtime root for the current process.
     *
     * @return writable runtime root
     */
    static Path currentRoot() {
        return resolveRoot(
                System.getProperty(WORKSPACE_SYSTEM_PROPERTY),
                System.getenv(),
                Path.of("").toAbsolutePath().normalize(),
                System.getProperty("os.name", ""),
                Path.of(System.getProperty("user.home")).toAbsolutePath().normalize());
    }

    static Path applicationDataRoot() {
        return applicationDataRoot(
                System.getenv(),
                System.getProperty("os.name", ""),
                Path.of(System.getProperty("user.home")).toAbsolutePath().normalize());
    }

    static Path resolveRoot(
            Map<String, String> environment,
            Path currentDirectory,
            String operatingSystemName,
            Path userHome) {
        return resolveRoot(null, environment, currentDirectory, operatingSystemName, userHome);
    }

    static Path resolveRoot(
            String configuredWorkspace,
            Map<String, String> environment,
            Path currentDirectory,
            String operatingSystemName,
            Path userHome) {
        configuredWorkspace = firstNonBlank(configuredWorkspace, environment.get(WORKSPACE_ENVIRONMENT_VARIABLE));
        if (configuredWorkspace != null && !configuredWorkspace.isBlank()) {
            return ensureWritableDirectory(Path.of(configuredWorkspace));
        }

        Path normalizedCurrentDirectory = currentDirectory.toAbsolutePath().normalize();
        if (!isProtectedWindowsDirectory(normalizedCurrentDirectory, environment, operatingSystemName)
                && isWritableDirectory(normalizedCurrentDirectory)) {
            return normalizedCurrentDirectory;
        }

        return ensureWritableDirectory(applicationDataRoot(environment, operatingSystemName, userHome)
                .resolve("work"));
    }

    private static Path applicationDataRoot(
            Map<String, String> environment,
            String operatingSystemName,
            Path userHome) {
        String normalizedOperatingSystem = operatingSystemName.toLowerCase(Locale.ROOT);
        if (normalizedOperatingSystem.contains("win")) {
            String localAppData = environment.get("LOCALAPPDATA");
            Path base = localAppData == null || localAppData.isBlank()
                    ? userHome.resolve("AppData").resolve("Local")
                    : Path.of(localAppData);
            return base.resolve("ShaftHQ").resolve("shaft-mcp");
        }
        if (normalizedOperatingSystem.contains("mac") || normalizedOperatingSystem.contains("darwin")) {
            return userHome.resolve("Library").resolve("Application Support")
                    .resolve("ShaftHQ").resolve("shaft-mcp");
        }
        String xdgDataHome = environment.get("XDG_DATA_HOME");
        Path base = xdgDataHome == null || xdgDataHome.isBlank()
                ? userHome.resolve(".local").resolve("share")
                : Path.of(xdgDataHome);
        return base.resolve("shafthq").resolve("shaft-mcp");
    }

    private static boolean isProtectedWindowsDirectory(
            Path directory,
            Map<String, String> environment,
            String operatingSystemName) {
        if (!operatingSystemName.toLowerCase(Locale.ROOT).contains("win")) {
            return false;
        }
        String windowsRoot = firstNonBlank(environment.get("SystemRoot"), environment.get("WINDIR"));
        if (windowsRoot == null) {
            Path root = directory.getRoot();
            windowsRoot = root == null ? "C:\\Windows" : root.resolve("Windows").toString();
        }
        return directory.startsWith(Path.of(windowsRoot).toAbsolutePath().normalize());
    }

    private static String firstNonBlank(String first, String second) {
        if (first != null && !first.isBlank()) {
            return first;
        }
        if (second != null && !second.isBlank()) {
            return second;
        }
        return null;
    }

    private static boolean isWritableDirectory(Path directory) {
        if (!Files.isDirectory(directory)) {
            return false;
        }
        try {
            Path marker = Files.createTempFile(directory, ".shaft-mcp-write-test", ".tmp");
            Files.deleteIfExists(marker);
            return true;
        } catch (IOException | RuntimeException ignored) {
            return false;
        }
    }

    private static Path ensureWritableDirectory(Path directory) {
        Path normalized = directory.toAbsolutePath().normalize();
        try {
            Files.createDirectories(normalized);
        } catch (IOException exception) {
            throw new IllegalStateException("MCP runtime root cannot be created: " + normalized, exception);
        }
        if (!isWritableDirectory(normalized)) {
            throw new IllegalStateException("MCP runtime root is not writable: " + normalized);
        }
        return normalized;
    }
}
