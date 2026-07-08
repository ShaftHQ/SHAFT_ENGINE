package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpProjectScopeTest {
    @TempDir
    Path temporary;

    @Test
    void javaArgFileWorkspacePropertiesUseCurrentProject() throws IOException {
        Path argsFile = temporary.resolve("shaft-mcp.args");
        Files.writeString(argsFile, """
                "-Duser.dir=C:/Users/me/AppData/Local/ShaftHQ/shaft-mcp/work"
                "-Dshaft.mcp.workspaceRoot=C:/Users/me/AppData/Local/ShaftHQ/shaft-mcp/work"
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> command = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);

        Path scopedArgs = Path.of(command.get(1).substring(1));
        String content = Files.readString(scopedArgs);
        String expectedProject = project.toString().replace('\\', '/');
        assertTrue(content.contains("\"-Duser.dir=" + expectedProject + "\""));
        assertTrue(content.contains("\"-Dshaft.mcp.workspaceRoot=" + expectedProject + "\""));
        assertFalse(content.contains("ShaftHQ/shaft-mcp/work"));
        assertEquals("java", command.get(0));
    }

    @Test
    void javaArgFileMissingWorkspacePropertiesGetsProjectScopePrefix() throws IOException {
        Path argsFile = temporary.resolve("plain.args");
        Files.writeString(argsFile, """
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> command = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);

        List<String> lines = Files.readAllLines(Path.of(command.get(1).substring(1)));
        String expectedProject = project.toString().replace('\\', '/');
        assertEquals("\"-Duser.dir=" + expectedProject + "\"", lines.get(0));
        assertEquals("\"-Dshaft.mcp.workspaceRoot=" + expectedProject + "\"", lines.get(1));
        assertEquals("-cp", lines.get(2));
    }

    @Test
    void plainJavaCommandGetsProjectWorkspaceProperties() throws IOException {
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> command = ShaftMcpProjectScope.commandForProject(List.of(
                "java",
                "-Duser.dir=C:/Users/me/AppData/Local/ShaftHQ/shaft-mcp/work",
                "-jar",
                "shaft-mcp.jar"), project);

        String expectedProject = project.toString().replace('\\', '/');
        assertEquals("java", command.get(0));
        assertEquals("-Duser.dir=" + expectedProject, command.get(1));
        assertEquals("-Dshaft.mcp.workspaceRoot=" + expectedProject, command.get(2));
        assertFalse(command.contains("-Duser.dir=C:/Users/me/AppData/Local/ShaftHQ/shaft-mcp/work"));
        assertEquals("-jar", command.get(3));
    }

    @Test
    void environmentIncludesCurrentProjectWorkspace() {
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                Map.of("OPENAI_API_KEY", "secret"), project);

        assertEquals(project.toString(), environment.get(ShaftMcpProjectScope.WORKSPACE_ENVIRONMENT_VARIABLE));
        assertEquals("secret", environment.get("OPENAI_API_KEY"));
    }

    @Test
    void argFileCommandIsStableAcrossRepeatedCallsForUnchangedSource() throws IOException {
        Path argsFile = temporary.resolve("shaft-mcp.args");
        Files.writeString(argsFile, """
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> first = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);
        List<String> second = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);

        assertEquals(first, second,
                "Repeated calls with an unchanged source argfile must return the same scoped command so "
                        + "ShaftMcpInvocationService.acquireClient can reuse its shared MCP process.");
    }

    @Test
    void changedArgFileContentProducesNewScopedFileWithUpdatedContent() throws IOException {
        Path argsFile = temporary.resolve("shaft-mcp.args");
        Files.writeString(argsFile, """
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> first = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);
        Path firstScopedFile = Path.of(first.get(1).substring(1));

        Files.writeString(argsFile, """
                -cp
                "shaft-mcp-updated.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        List<String> second = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);
        Path secondScopedFile = Path.of(second.get(1).substring(1));

        assertFalse(first.equals(second), "Changed source content must yield a different scoped command.");
        assertTrue(Files.readString(secondScopedFile).contains("shaft-mcp-updated.jar"));
        assertFalse(Files.exists(firstScopedFile), "The superseded scoped argfile should be cleaned up.");
    }

    @Test
    void deletedScopedFileIsRegenerated() throws IOException {
        Path argsFile = temporary.resolve("shaft-mcp.args");
        Files.writeString(argsFile, """
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        List<String> first = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);
        Path firstScopedFile = Path.of(first.get(1).substring(1));
        Files.delete(firstScopedFile);

        List<String> second = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), project);
        Path secondScopedFile = Path.of(second.get(1).substring(1));

        assertTrue(Files.exists(secondScopedFile), "A missing scoped argfile must be regenerated, not left dangling.");
        assertTrue(Files.readString(secondScopedFile).contains("shaft-mcp.jar"));
    }

    @Test
    void differentProjectRootsProduceDistinctStableArgFiles() throws IOException {
        Path argsFile = temporary.resolve("shaft-mcp.args");
        Files.writeString(argsFile, """
                -cp
                "shaft-mcp.jar"
                com.shaft.mcp.ShaftMcpApplication
                """);
        Path projectA = temporary.resolve("projectA").toAbsolutePath().normalize();
        Path projectB = temporary.resolve("projectB").toAbsolutePath().normalize();

        List<String> forA = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), projectA);
        List<String> forB = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), projectB);
        List<String> forARepeated = ShaftMcpProjectScope.commandForProject(List.of("java", "@" + argsFile), projectA);

        assertFalse(forA.equals(forB), "Distinct project roots must not share a scoped argfile.");
        assertEquals(forA, forARepeated,
                "Serving project B must not evict or perturb project A's cached scoped argfile.");
    }
}
