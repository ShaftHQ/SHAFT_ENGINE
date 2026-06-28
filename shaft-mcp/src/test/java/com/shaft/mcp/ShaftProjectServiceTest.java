package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftProjectServiceTest {
    @TempDir
    Path temp;

    @Test
    void createProjectCopiesExampleAndAppliesGeneratorRules() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectGenerationResult result = service.createProject(
                "generated",
                "TestNG",
                "api",
                "com.example",
                "shaft-api-testng",
                "1.2.3",
                List.of("shaft-heal"),
                true,
                true,
                false);

        Path generated = temp.resolve("generated");
        String pom = Files.readString(generated.resolve("pom.xml"));
        assertEquals(generated, result.projectDirectory());
        assertEquals("shaft-testng-api", result.templateProject());
        assertTrue(pom.contains("<groupId>com.example</groupId>"));
        assertTrue(pom.contains("<artifactId>shaft-api-testng</artifactId>"));
        assertTrue(pom.contains("<version>1.2.3</version>"));
        assertTrue(pom.contains("<artifactId>shaft-heal</artifactId>"));
        assertTrue(Files.exists(generated.resolve(".github/workflows/api.yml")));
        assertTrue(Files.exists(generated.resolve(".github/dependabot.yml")));
    }

    @Test
    void resourceDestinationRejectsTraversalOutsideProjectDirectory() {
        Path project = temp.resolve("project");

        assertEquals(project.resolve("src/test/java/SampleTest.java").toAbsolutePath().normalize(),
                ShaftProjectService.safeResourceDestination(project, "src/test/java/SampleTest.java"));
        assertThrows(IllegalArgumentException.class,
                () -> ShaftProjectService.safeResourceDestination(project, "../pom.xml"));
        assertThrows(IllegalArgumentException.class,
                () -> ShaftProjectService.safeResourceDestination(project, "src/../../pom.xml"));
        assertThrows(IllegalArgumentException.class,
                () -> ShaftProjectService.safeResourceDestination(project, temp.resolve("pom.xml").toString()));
    }

    @Test
    void upgradeProjectRunsExistingScriptAndRequiresApprovalForMutation() throws Exception {
        Path project = Files.createDirectories(temp.resolve("current"));
        Files.writeString(project.resolve("pom.xml"), "<project/>");
        Path script = temp.resolve("upgrade_to_modular_shaft.py");
        Files.writeString(script, "# script");
        FakeRunner runner = new FakeRunner();
        runner.result = new McpProcessRunner.ProcessResult(0, "ok", "", false);
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                runner,
                script,
                List.of("python"));

        McpShaftProjectUpgradeResult result = service.upgradeProject(
                "current",
                "basic",
                true,
                false,
                "",
                "",
                0,
                false,
                false);

        assertEquals(0, result.exitCode());
        assertEquals(project, runner.workingDirectory);
        assertTrue(runner.commands.getFirst().contains(script.toString()));
        assertTrue(runner.commands.getFirst().contains("--dry-run"));
        assertTrue(runner.commands.getFirst().contains("--no-ai"));
        assertThrows(IllegalArgumentException.class, () -> service.upgradeProject(
                "current",
                "basic",
                false,
                false,
                "",
                "",
                0,
                false,
                false));
    }

    private static final class FakeRunner implements McpProcessRunner {
        private final List<List<String>> commands = new ArrayList<>();
        private Path workingDirectory;
        private ProcessResult result = new ProcessResult(0, "", "", false);

        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            commands.add(command);
            this.workingDirectory = workingDirectory;
            return result;
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new UnsupportedOperationException("No process starts in unit tests.");
        }
    }
}
