package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link McpLauncherLocator}'s discovery order, driven entirely against temp
 * directories via its package-private overridable constructor. No real process is launched.
 */
class McpLauncherLocatorTest {

    @Test
    void envOverrideWithSiblingArgsFileUsesArgfileLaunch(@TempDir Path tempDir) throws IOException {
        Path jar = tempDir.resolve("shaft-mcp.jar");
        Files.createFile(jar);
        Path argsFile = tempDir.resolve("shaft-mcp.args");
        Files.createFile(argsFile);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), tempDir.resolve("no-versions"), tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(jar.toString(), spec.jarPath());
        assertEquals("@" + argsFile, spec.baseCommand().get(spec.baseCommand().size() - 1));
    }

    @Test
    void envOverridePointingDirectlyAtArgsFileUsesItAsArgfile(@TempDir Path tempDir) throws IOException {
        Path argsFile = tempDir.resolve("custom.args");
        Files.createFile(argsFile);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", argsFile.toString()),
                tempDir.resolve("no-versions"),
                tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(List.of(spec.baseCommand().get(0), "@" + argsFile), spec.baseCommand());
    }

    @Test
    void envOverridePointingAtPlainJarUsesClasspathLaunch(@TempDir Path tempDir) throws IOException {
        Path jar = tempDir.resolve("standalone-shaft-mcp.jar");
        Files.createFile(jar);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), tempDir.resolve("no-versions"), tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(List.of(spec.baseCommand().get(0), "-cp", jar.toString(), "com.shaft.mcp.ShaftMcpApplication"),
                spec.baseCommand());
    }

    @Test
    void envOverridePointingAtMissingFileThrows(@TempDir Path tempDir) {
        Path missing = tempDir.resolve("does-not-exist.jar");

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", missing.toString()),
                tempDir.resolve("no-versions"),
                tempDir.resolve("no-dev"));

        assertThrows(McpException.class, locator::locate);
    }

    @Test
    void versionsDirectorySelectsNewestSubdirWithArgsFile(@TempDir Path tempDir) throws IOException {
        Path versionsRoot = tempDir.resolve("versions");
        Path older = versionsRoot.resolve("1.0.0");
        Files.createDirectories(older);
        Files.createFile(older.resolve("shaft-mcp.args"));

        Path newer = versionsRoot.resolve("2.0.0");
        Files.createDirectories(newer);
        Files.createFile(newer.resolve("shaft-mcp.args"));

        // Lexicographically largest but has no shaft-mcp.args, so it must be skipped.
        Path incomplete = versionsRoot.resolve("3.0.0");
        Files.createDirectories(incomplete);

        McpLauncherLocator locator = new McpLauncherLocator(Map.of(), versionsRoot, tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        Path expectedArgs = newer.resolve("shaft-mcp.args");
        assertEquals(List.of(spec.baseCommand().get(0), "@" + expectedArgs), spec.baseCommand());
        assertEquals(newer.resolve("shaft-mcp.jar").toString(), spec.jarPath());
    }

    @Test
    void nothingFoundThrowsActionableException(@TempDir Path tempDir) throws IOException {
        Path emptyVersions = tempDir.resolve("versions");
        Files.createDirectories(emptyVersions);
        Path emptyDev = tempDir.resolve("dev");
        Files.createDirectories(emptyDev);

        McpLauncherLocator locator = new McpLauncherLocator(Map.of(), emptyVersions, emptyDev);

        McpException exception = assertThrows(McpException.class, locator::locate);
        assertTrue(exception.getMessage().contains("SHAFT_MCP_JAR"));
    }
}
