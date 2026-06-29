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
    void environmentIncludesCurrentProjectWorkspace() {
        Path project = temporary.resolve("project").toAbsolutePath().normalize();

        Map<String, String> environment = ShaftMcpProjectScope.environmentForProject(
                Map.of("OPENAI_API_KEY", "secret"), project);

        assertEquals(project.toString(), environment.get(ShaftMcpProjectScope.WORKSPACE_ENVIRONMENT_VARIABLE));
        assertEquals("secret", environment.get("OPENAI_API_KEY"));
    }
}
