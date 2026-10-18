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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftProjectServiceCoverageGapsTest {
    @TempDir
    Path temp;

    @Test
    void createProjectRejectsExistingDirectoryUnknownRunnerAndUnsupportedPlatform() throws Exception {
        Path existing = Files.createDirectories(temp.resolve("taken"));
        Files.writeString(existing.resolve("keep.txt"), "keep");
        ShaftProjectService service = service();

        IllegalArgumentException exists = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "taken", "TestNG", "api", "com.example", "demo", "1.0.0", "9.9.9",
                List.of(), false, false, false));
        IllegalArgumentException runner = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "spock-out", "Spock", "api", "com.example", "demo", "1.0.0", "9.9.9",
                List.of(), false, false, true));
        IllegalArgumentException platform = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "cucumber-mobile", "Cucumber", "mobile", "com.example", "demo", "1.0.0",
                List.of(), false, false, true));

        assertTrue(exists.getMessage().contains("already exists"));
        assertEquals("keep", Files.readString(existing.resolve("keep.txt")));
        assertTrue(runner.getMessage().contains("Unsupported SHAFT project runner"));
        assertTrue(platform.getMessage().contains("Cucumber"));
        assertTrue(platform.getMessage().contains("mobile"));
        assertFalse(Files.exists(temp.resolve("spock-out")));
        assertFalse(Files.exists(temp.resolve("cucumber-mobile")));
    }

    @Test
    void blankArtifactIdGeneratesAndUnknownModulesAreIgnored() throws Exception {
        McpShaftProjectGenerationResult result = service().createProject(
                "blank-artifact",
                "JUnit",
                "api",
                "com.example",
                "   ",
                "1.2.3",
                "9.9.9",
                List.of("not-a-module", "shaft-heal", " "),
                true,
                false,
                true);

        String pom = Files.readString(result.projectDirectory().resolve("pom.xml"));
        assertEquals("blank-artifact", result.projectDirectory().getFileName().toString());
        assertTrue(pom.contains("<artifactId>shaft-api-junit</artifactId>"), pom);
        assertTrue(pom.contains("<artifactId>shaft-heal</artifactId>"), pom);
        assertFalse(pom.contains("not-a-module"), pom);
        assertTrue(result.addedModules().contains("shaft-heal"));
        assertFalse(result.addedModules().contains("not-a-module"));
        assertTrue(Files.exists(result.projectDirectory().resolve(".github/workflows/api.yml")));
    }

    @Test
    void mobileProjectDoesNotCopyTheWebWorkflow() throws Exception {
        McpShaftProjectGenerationResult result = service().createProject(
                "mobile-app",
                "TestNG",
                "mobile",
                "com.example",
                "mobile-demo",
                "1.0.0",
                List.of("shaft-heal"),
                true,
                false,
                true);

        Path project = result.projectDirectory();
        assertEquals("mobile", result.platform());
        assertTrue(Files.exists(project.resolve("pom.xml")));
        assertTrue(Files.readString(project.resolve("pom.xml")).contains("<artifactId>shaft-heal</artifactId>"));
        assertFalse(Files.exists(project.resolve(".github/workflows/web.yml")));
        assertFalse(Files.exists(project.resolve(".github/workflows/api.yml")));
    }

    @Test
    void upgradeFlagsArePassedToTheFakeRunnerWithoutExecutingPython() throws Exception {
        Path project = Files.createDirectories(temp.resolve("current"));
        Files.writeString(project.resolve("pom.xml"), "<project/>");
        FakeRunner runner = new FakeRunner();
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp), runner, temp.resolve("upgrade_to_modular_shaft.py"), List.of("python"));

        IllegalArgumentException type = assertThrows(IllegalArgumentException.class, () -> service.upgradeProject(
                "current", "nope", true, false, "", "", 1, false, false));
        assertTrue(runner.commands.isEmpty());

        McpShaftProjectUpgradeResult result = service.upgradeProject(
                "current", "session", false, true, "9.9.9", "mvn -q test-compile", 0, true, true);
        List<String> command = runner.commands.getFirst();
        assertEquals(0, result.exitCode());
        assertFalse(result.dryRun());
        assertTrue(command.contains("--yes"));
        assertTrue(command.contains("--report"));
        assertTrue(command.contains("--shaft-version"));
        assertTrue(command.contains("9.9.9"));
        assertTrue(command.contains("--compile-command"));
        assertTrue(command.contains("mvn -q test-compile"));
        assertTrue(command.contains("--compile-timeout"));
        assertTrue(command.contains("900"));
        assertTrue(command.contains("--skip-baseline-compile"));
        assertFalse(command.contains("--dry-run"));
        assertFalse(command.contains("--no-ai"));
        assertTrue(type.getMessage().contains("Unsupported SHAFT project upgrade type"));
    }

    private ShaftProjectService service() {
        return new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"),
                () -> "9.9.9");
    }

    private static final class FakeRunner implements McpProcessRunner {
        private final List<List<String>> commands = new ArrayList<>();

        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            commands.add(command);
            return new ProcessResult(0, "ok", "", false);
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new UnsupportedOperationException("No process starts in unit tests.");
        }
    }
}
