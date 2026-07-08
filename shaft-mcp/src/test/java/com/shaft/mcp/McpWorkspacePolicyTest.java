package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpWorkspacePolicyTest {

    @Test
    void existingReportsNotFoundInsteadOfSandboxWordingForAMissingPath(@TempDir Path workspace) {
        McpWorkspacePolicy policy = McpWorkspacePolicy.of(workspace);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> policy.existing("allure-results", "Allure result path"));

        assertTrue(failure.getMessage().contains("Allure result path was not found in this project"));
        assertTrue(failure.getMessage().contains("SHAFT reporting enabled"));
        assertFalse(failure.getMessage().contains("outside the MCP workspace"));
    }

    @Test
    void existingStillFailsClosedForAGenuineWorkspaceEscape(@TempDir Path workspace) throws Exception {
        Path outside = Files.createTempDirectory("mcp-workspace-policy-escape");
        McpWorkspacePolicy policy = McpWorkspacePolicy.of(workspace);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> policy.existing(outside.toString(), "Repository root"));

        assertTrue(failure.getMessage().contains("Repository root is outside the MCP workspace"));
        assertFalse(failure.getMessage().contains("was not found in this project"));
    }

    @Test
    void existingResolvesAPathThatActuallyExists(@TempDir Path workspace) throws Exception {
        Path allureResults = Files.createDirectories(workspace.resolve("allure-results"));
        McpWorkspacePolicy policy = McpWorkspacePolicy.of(workspace);

        Path resolved = policy.existing("allure-results", "Allure result path");

        assertTrue(Files.isSameFile(resolved, allureResults));
    }
}
