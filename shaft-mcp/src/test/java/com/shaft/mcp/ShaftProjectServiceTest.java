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
import static org.junit.jupiter.api.Assertions.assertFalse;
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
                "I Click the element found by \"xpath\": \"(//div[contains(@class,'mw-search-result-heading')])[1]//a\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"title\" attribute of the browser, contains \"Unit testing\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"text\" attribute of the browser, contains \"Software testing\""));
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
    void initAgentsGeneratesFullSkillContentForMethodologySkillButThinBridgeForToolSkills() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        service.initAgents("claude", "repo", false);

        Path repo = temp.resolve("repo");
        Path methodologySkill = repo.resolve(".claude/skills/act-as-mohab/SKILL.md");
        assertTrue(Files.exists(methodologySkill));
        String methodologyContent = Files.readString(methodologySkill);
        assertTrue(methodologyContent.contains("Evidence Over Inference"));
        assertFalse(methodologyContent.contains("Generated by `shaft_project_init_agents`"));

        Path toolSkill = repo.resolve(".claude/skills/writing-shaft-tests/SKILL.md");
        assertTrue(Files.exists(toolSkill));
        String toolContent = Files.readString(toolSkill);
        assertTrue(toolContent.contains("Generated by `shaft_project_init_agents`"));
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

        assertThrows(IllegalArgumentException.class, () -> service.initAgents("cursor", "repo", false));
    }

    @Test
    void initAgentsGeneratesOpencodeSkillBridgesUnderOpencodeDirectory() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("opencode", "opencode-repo", false);

        assertEquals("opencode", result.loop());
        Path bridge = temp.resolve("opencode-repo/.opencode/skills/writing-shaft-tests/SKILL.md");
        assertTrue(Files.exists(bridge));
        String bridgeContent = Files.readString(bridge);
        assertTrue(bridgeContent.startsWith("---\nname: writing-shaft-tests\n"));
        assertTrue(Files.exists(temp.resolve("opencode-repo/SHAFT-AGENTS.md")));
    }

    @Test
    void initAgentsGeneratesVscodeInstructionsUnderGithubInstructionsDirectory() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("vscode", "vscode-repo", false);

        assertEquals("vscode", result.loop());
        Path instructions = temp.resolve(
                "vscode-repo/.github/instructions/recording-shaft-tests-with-mcp.instructions.md");
        assertTrue(Files.exists(instructions));
        assertTrue(result.generatedFiles().contains(instructions));
        String instructionsContent = Files.readString(instructions);
        assertTrue(instructionsContent.contains("applyTo: \"**\""));
        assertTrue(instructionsContent.contains("name: recording-shaft-tests-with-mcp"));
        assertTrue(instructionsContent.contains("description:"));
        assertTrue(Files.exists(temp.resolve("vscode-repo/SHAFT-AGENTS.md")));
    }

    @Test
    void initAgentsGeneratesFullBodyWithApplyToFrontMatterForVscodeMethodologySkill() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        service.initAgents("vscode", "vscode-repo", false);

        Path instructions = temp.resolve("vscode-repo/.github/instructions/act-as-mohab.instructions.md");
        assertTrue(Files.exists(instructions));
        String content = Files.readString(instructions);
        assertTrue(content.contains("applyTo: \"**\""));
        assertTrue(content.contains("name: act-as-mohab"));
        assertTrue(content.contains("Evidence Over Inference"));
        assertFalse(content.contains("Generated by `shaft_project_init_agents`"));
    }

    @Test
    void initAgentsSkipsExistingVscodeInstructionsFileWithWarningWhenOverwriteFalse() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("existing-vscode-repo"));
        Path instructions = Files.createDirectories(repo.resolve(".github/instructions"))
                .resolve("recording-shaft-tests-with-mcp.instructions.md");
        Files.writeString(instructions, "custom content");

        McpShaftProjectInitAgentsResult result = service.initAgents("vscode", "existing-vscode-repo", false);

        assertEquals("custom content", Files.readString(instructions));
        assertTrue(result.warnings().stream()
                .anyMatch(warning -> warning.contains("recording-shaft-tests-with-mcp.instructions.md")));
        assertTrue(result.generatedFiles().stream().noneMatch(instructions::equals));
    }

    @Test
    void initAgentsOverwritesExistingVscodeInstructionsFileWhenOverwriteTrue() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("overwrite-vscode-repo"));
        Path instructions = Files.createDirectories(repo.resolve(".github/instructions"))
                .resolve("recording-shaft-tests-with-mcp.instructions.md");
        Files.writeString(instructions, "stale content");

        McpShaftProjectInitAgentsResult result = service.initAgents("vscode", "overwrite-vscode-repo", true);

        assertTrue(result.warnings().isEmpty());
        assertTrue(result.generatedFiles().contains(instructions));
        assertTrue(Files.readString(instructions).contains("applyTo: \"**\""));
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
