package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftProjectGuardTest {
    @TempDir
    Path temp;

    @Test
    void createProjectRejectsUnsafeChoicesAndSkipsMobileWorkflow() throws Exception {
        ShaftProjectService service = service();

        IllegalArgumentException runner = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "out", "NUnit", "web", "com.example", "demo", "1.0.0", "9.9.9",
                List.of(), false, false, true));
        assertTrue(runner.getMessage().contains("Unsupported SHAFT project runner"));

        IllegalArgumentException platform = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "out", "Cucumber", "mobile", "com.example", "demo", "1.0.0", "9.9.9",
                List.of(), false, false, true));
        assertTrue(platform.getMessage().contains("Unsupported SHAFT project platform"));

        McpShaftProjectGenerationResult first = service.createProject(
                "generated", "TestNG", "web", "com.example&co", "", "1.0.0", "9.9.9",
                List.of("shaft-heal", "not-a-module", " "), true, false, false);
        String pom = Files.readString(first.projectDirectory().resolve("pom.xml"));
        assertTrue(pom.contains("<groupId>com.example&amp;co</groupId>"), pom);
        assertTrue(pom.contains("<artifactId>shaft-web-testng</artifactId>"), pom);
        assertTrue(pom.contains("<artifactId>shaft-heal</artifactId>"), pom);
        assertTrue(pom.contains("<artifactId>shaft-visual</artifactId>"), pom);
        assertFalse(pom.contains("not-a-module"), pom);
        assertTrue(Files.exists(first.projectDirectory().resolve(".github/workflows/web.yml")));

        IllegalArgumentException exists = assertThrows(IllegalArgumentException.class, () -> service.createProject(
                "generated", "TestNG", "web", "com.example", "demo", "1.0.0", "9.9.9",
                List.of(), false, false, false));
        assertTrue(exists.getMessage().contains("already exists"));

        McpShaftProjectGenerationResult mobile = service.createProject(
                "mobile-app", "TestNG", "mobile", "com.example", "mobile-demo", "1.0.0", "9.9.9",
                List.of("shaft-heal"), true, true, true);
        assertFalse(Files.exists(mobile.projectDirectory().resolve(".github/workflows/web.yml")));
        assertFalse(Files.exists(mobile.projectDirectory().resolve(".github/workflows/api.yml")));
        assertTrue(Files.exists(mobile.projectDirectory().resolve(".github/dependabot.yml")));
        assertTrue(mobile.addedModules().contains("shaft-heal"));
        assertFalse(mobile.addedModules().contains("shaft-visual"));
    }

    private ShaftProjectService service() {
        return new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new IdleRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"),
                () -> {
                    throw new AssertionError("shaft version resolver must not be called");
                });
    }

    private static final class IdleRunner implements McpProcessRunner {
        @Override
        public ProcessResult run(List<String> command, Path workingDirectory, Map<String, String> environment,
                Duration timeout) {
            throw new AssertionError("project generation must not run a process");
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new AssertionError("project generation must not start a process");
        }
    }
}
