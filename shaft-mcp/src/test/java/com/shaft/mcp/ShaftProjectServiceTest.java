package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

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
                "9.9.9",
                List.of("shaft-heal", "shaft-sikulix"),
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
        assertTrue(pom.contains("<shaft.version>9.9.9</shaft.version>"));
        assertTrue(pom.contains("<artifactId>shaft-heal</artifactId>"));
        assertTrue(pom.contains("<artifactId>shaft-sikulix</artifactId>"));
        assertTrue(Files.exists(generated.resolve(".github/workflows/api.yml")));
        assertTrue(Files.exists(generated.resolve(".github/dependabot.yml")));
    }

    @Test
    void createProjectUsesDefaultShaftVersionFromResolver() throws Exception {
        AtomicBoolean resolverCalled = new AtomicBoolean();
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"),
                () -> {
                    resolverCalled.set(true);
                    return "10.10.20260703";
                });

        service.createProject(
                "generated-default",
                "TestNG",
                "web",
                "com.example",
                "",
                "1.0.0",
                "",
                List.of(),
                false,
                false,
                true);

        Path generated = temp.resolve("generated-default");
        String pom = Files.readString(generated.resolve("pom.xml"));
        assertTrue(resolverCalled.get());
        assertTrue(pom.contains("<shaft.version>10.10.20260703</shaft.version>"));
    }

    @Test
    void generatedWebSamplesSearchFirstResultAndValidateResultPage() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"),
                () -> "11.11.20260702");

        McpShaftProjectGenerationResult testngResult = service.createProject(
                "generated-web-ng",
                "TestNG",
                "web",
                "com.example",
                "",
                "1.0.0",
                null,
                List.of(),
                false,
                false,
                true);
        String testngSample = Files.readString(
                testngResult.projectDirectory().resolve("src/test/java/testPackage/TestClass.java"));
        assertTrue(testngSample.contains("click(firstSearchResult)"));
        assertTrue(testngSample.contains("expectedResultTitle"));
        assertTrue(testngSample.contains("expectedResultText"));

        McpShaftProjectGenerationResult cucumberResult = service.createProject(
                "generated-web-cucumber",
                "Cucumber",
                "web",
                "com.example",
                "",
                "1.0.0",
                null,
                List.of(),
                false,
                false,
                true);
        String cucumberFlow = Files.readString(cucumberResult.projectDirectory()
                .resolve("src/test/resources/features/SampleFeatureFile.feature"));
        assertTrue(cucumberFlow.contains(
                "I Click the element found by \"xpath\": \"(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"title\" attribute of the browser, contains \"SHAFT\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"text\" attribute of the browser, contains \"SHAFT\""));
    }

    @Test
    void initAgentsGeneratesClaudeSkillBridgesAndGuidanceFile() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("claude", "repo", false);

        Path repo = temp.resolve("repo");
        assertEquals(repo, result.targetDirectory());
        assertEquals("claude", result.loop());
        assertTrue(result.warnings().isEmpty());
        Path bridge = repo.resolve(".claude/skills/recording-shaft-tests-with-mcp/SKILL.md");
        assertTrue(Files.exists(bridge));
        assertTrue(result.generatedFiles().contains(bridge));
        String bridgeContent = Files.readString(bridge);
        assertTrue(bridgeContent.startsWith("---\nname: recording-shaft-tests-with-mcp\n"));
        assertTrue(bridgeContent.contains("shaft-mcp:shaft_guide_search"));
        assertTrue(Files.exists(repo.resolve("SHAFT-AGENTS.md")));
        String guidance = Files.readString(repo.resolve("SHAFT-AGENTS.md"));
        assertTrue(guidance.contains("MERGE the relevant parts"));
        assertTrue(guidance.contains("recording-shaft-tests-with-mcp"));
    }

    @Test
    void initAgentsGeneratesCodexSkillBridgesUnderCodexDirectory() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("codex", "codex-repo", false);

        assertEquals("codex", result.loop());
        assertTrue(Files.exists(
                temp.resolve("codex-repo/.codex/skills/writing-shaft-tests/SKILL.md")));
        assertTrue(Files.exists(temp.resolve("codex-repo/SHAFT-AGENTS.md")));
    }

    @Test
    void initAgentsRejectsUnsupportedLoop() {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        assertThrows(IllegalArgumentException.class, () -> service.initAgents("vscode", "repo", false));
    }

    @Test
    void initAgentsSkipsExistingFileWithWarningWhenOverwriteFalse() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("existing-repo"));
        Path guidance = repo.resolve("SHAFT-AGENTS.md");
        Files.writeString(guidance, "custom content");

        McpShaftProjectInitAgentsResult result = service.initAgents("claude", "existing-repo", false);

        assertEquals("custom content", Files.readString(guidance));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("SHAFT-AGENTS.md")));
        assertTrue(result.generatedFiles().stream().noneMatch(guidance::equals));
    }

    @Test
    void initAgentsOverwritesExistingFileWhenOverwriteTrue() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("overwrite-repo"));
        Path guidance = repo.resolve("SHAFT-AGENTS.md");
        Files.writeString(guidance, "stale content");

        McpShaftProjectInitAgentsResult result = service.initAgents("claude", "overwrite-repo", true);

        assertTrue(result.warnings().isEmpty());
        assertTrue(result.generatedFiles().contains(guidance));
        assertTrue(Files.readString(guidance).contains("SHAFT Agent Guidance"));
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
