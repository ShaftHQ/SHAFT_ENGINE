package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpRuntimePathsTest {
    @TempDir
    Path temp;

    @Test
    void configuredWorkspaceRootShouldWinAndBeCreated() {
        Path configuredWorkspace = temp.resolve("configured-workspace");
        Map<String, String> environment = Map.of(
                McpRuntimePaths.WORKSPACE_ENVIRONMENT_VARIABLE, configuredWorkspace.toString());

        Path resolvedRoot = McpRuntimePaths.resolveRoot(
                environment,
                temp.resolve("current-directory"),
                "Windows 11",
                temp.resolve("home"));

        assertEquals(configuredWorkspace.toAbsolutePath().normalize(), resolvedRoot);
        assertTrue(Files.isDirectory(resolvedRoot));
    }

    @Test
    void writableNonProtectedCurrentDirectoryShouldBeUsed() throws Exception {
        Path currentDirectory = Files.createDirectories(temp.resolve("current-directory"));

        Path resolvedRoot = McpRuntimePaths.resolveRoot(
                Map.of(),
                currentDirectory,
                "Linux",
                temp.resolve("home"));

        assertEquals(currentDirectory.toAbsolutePath().normalize(), resolvedRoot);
    }

    @Test
    void protectedWindowsDirectoryShouldFallBackToLocalAppData() throws Exception {
        Path windowsRoot = Files.createDirectories(temp.resolve("Windows"));
        Path currentDirectory = Files.createDirectories(windowsRoot.resolve("System32"));
        Path localAppData = temp.resolve("local-app-data");
        Map<String, String> environment = new HashMap<>();
        environment.put("SystemRoot", windowsRoot.toString());
        environment.put("LOCALAPPDATA", localAppData.toString());

        Path resolvedRoot = McpRuntimePaths.resolveRoot(
                environment,
                currentDirectory,
                "Windows 11",
                temp.resolve("home"));

        Path expectedRoot = localAppData
                .resolve("ShaftHQ")
                .resolve("shaft-mcp")
                .resolve("work")
                .toAbsolutePath()
                .normalize();
        assertEquals(expectedRoot, resolvedRoot);
        assertTrue(Files.isDirectory(resolvedRoot));
    }
}
