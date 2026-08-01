package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

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

    // Issue #3788 Defect C: the sample used to search a phrase and click Wikipedia's top-ranked
    // full-text result, asserting exact equality against that live, ranked title -- unfixable by
    // data updates alone (drifted three times in one day during grid verification: "Unit testing"
    // -> "Test automation" -> "Robot Framework - Wikipedia"). The redesigned flow types the *exact*
    // title of an existing article and submits (Enter); MediaWiki redirects an exact title match
    // straight to that article (verified server-side via `curl -L`), so the assertions below no
    // longer depend on search ranking. This test is the spec for that template shape -- it must
    // mirror shaft-engine's example templates (TestNG/JUnit/Cucumber web samples) exactly.
    @Test
    void generatedWebSamplesSearchExactTitleAndValidateArticlePage() throws Exception {
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
        Path testngProject = testngResult.projectDirectory();
        String testngSample = Files.readString(
                testngProject.resolve("src/test/java/testPackage/TestClass.java"));
        // No more "click the ranked first search result" step or locator.
        assertFalse(testngSample.contains("firstSearchResult"));
        assertFalse(testngSample.contains("click(firstSearchResult)"));
        // Deterministic title/text assertions still read from test data, same field names.
        assertTrue(testngSample.contains("expectedResultTitle"));
        assertTrue(testngSample.contains("expectedResultText"));
        // Visual check repointed at the larger, always-static logo lockup, not the small icon alone.
        assertTrue(testngSample.contains("//a[@class='mw-logo']"));
        assertFalse(testngSample.contains("mw-logo-icon"));

        String testngData = Files.readString(
                testngProject.resolve("src/test/resources/testDataFiles/simpleJSON.json"));
        assertTrue(testngData.contains("\"searchQuery\": \"Software testing\""));
        assertTrue(testngData.contains("\"expectedResultTitle\": \"Software testing\""));
        assertFalse(testngData.contains("Software testing framework"));

        String testngProperties = Files.readString(
                testngProject.resolve("src/main/resources/properties/custom.properties"));
        assertTrue(testngProperties.contains("defaultElementIdentificationTimeout=20"));

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
        assertFalse(cucumberFlow.contains(
                "I Click the element found by \"xpath\": \"(//div[contains(@class,'mw-search-result-heading')])[1]//a\""));
        assertTrue(cucumberFlow.contains("I Type \"Software testing\" into the element found by \"id\": \"searchInput\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"title\" attribute of the browser, contains \"Software testing\""));
        assertTrue(cucumberFlow.contains("I Assert that the \"text\" attribute of the browser, contains \"Software testing\""));
        assertTrue(cucumberFlow.contains("\"xpath\": \"//a[@class='mw-logo']\""));
        assertFalse(cucumberFlow.contains("mw-logo-icon"));

        String cucumberProperties = Files.readString(
                cucumberResult.projectDirectory().resolve("src/main/resources/properties/custom.properties"));
        assertTrue(cucumberProperties.contains("defaultElementIdentificationTimeout=20"));
    }

    @Test
    void initAgentsScaffoldsAllSupportedHostsInAnEmptyRepository() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("all", "repo", false);

        Path repo = temp.resolve("repo");
        assertEquals(repo, result.targetDirectory());
        assertEquals("all", result.loop());
        assertTrue(result.warnings().isEmpty());
        assertHostAdapter(repo.resolve("AGENTS.md"), ".agents/skills/shaft-developer/SKILL.md");
        assertHostAdapter(repo.resolve("CLAUDE.md"), ".claude/skills/shaft-developer/SKILL.md");
        assertHostAdapter(repo.resolve(".github/copilot-instructions.md"),
                "instructions/shaft-developer/SKILL.md");
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-developer/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-developer/references/routing.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-developer/references/sources.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-developer/agents/openai.yaml")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-automated-test-authoring/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-automated-test-authoring/references/playbook.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/references/shaft-mcp-tools.md")));
        assertTrue(Files.exists(repo.resolve(".agents/skills/evaluation-prompts.md")));
        assertTrue(Files.exists(repo.resolve(".claude/skills/shaft-developer/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".opencode/skills/shaft-developer/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".github/instructions/shaft-developer/SKILL.md")));
        assertTrue(Files.readString(repo.resolve(".agents/skills/shaft-developer/SKILL.md"))
                .contains("name: shaft-developer"));
        assertFalse(Files.readString(repo.resolve(".agents/skills/shaft-automated-test-authoring/SKILL.md"))
                .contains("SHAFT bridge"));
        assertFalse(Files.exists(repo.resolve(".agents/skills/act-as-shaft-dev")));
        assertFalse(Files.exists(repo.resolve(".claude/skills/act-as-shaft-dev")));
        assertFalse(Files.exists(repo.resolve(".github/instructions/act-as-shaft-dev.instructions.md")));
        assertFalse(Files.exists(repo.resolve(".codex")));
        assertFalse(Files.exists(repo.resolve("SHAFT-AGENTS.md")));
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
    void initAgentsRetainsTheOpenCodePublicLoopWithoutCreatingCodexFiles() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        McpShaftProjectInitAgentsResult result = service.initAgents("opencode", "opencode-repo", false);

        assertEquals("opencode", result.loop());
        Path repo = temp.resolve("opencode-repo");
        assertTrue(Files.exists(repo.resolve(".opencode/skills/shaft-developer/SKILL.md")));
        assertHostAdapter(repo.resolve("AGENTS.md"), ".opencode/skills/shaft-developer/SKILL.md");
        assertFalse(Files.exists(repo.resolve(".agents")));
        assertFalse(Files.exists(repo.resolve("SHAFT-AGENTS.md")));
    }

    @Test
    void initAgentsPreservesPreexistingClaudeTextAndUsesItsNestedAdapter() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("claude-repo"));
        Path claudeDirectory = Files.createDirectories(repo.resolve(".claude"));
        Path adapter = claudeDirectory.resolve("CLAUDE.md");
        String userText = "# Team instructions\r\n\r\nKeep this exact text.\r\n";
        Files.writeString(adapter, userText);

        service.initAgents("claude", "claude-repo", false);

        String updated = Files.readString(adapter);
        assertTrue(updated.startsWith(userText));
        assertTrue(updated.contains("skills/shaft-developer/SKILL.md"));
        assertFalse(Files.exists(repo.resolve("CLAUDE.md")));
        assertTrue(Files.exists(repo.resolve(".claude/skills/shaft-developer/SKILL.md")));
    }

    @Test
    void initAgentsUpdatesEveryRecognizedExistingAdapterOnAMixedHostRepository() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("mixed-repo"));
        Path agents = repo.resolve("AGENTS.md");
        Path claude = repo.resolve("CLAUDE.md");
        Path copilot = Files.createDirectories(repo.resolve(".github")).resolve("copilot-instructions.md");
        Files.writeString(agents, "codex user text");
        Files.writeString(claude, "claude user text");
        Files.writeString(copilot, "copilot user text");

        service.initAgents("claude", "mixed-repo", false);

        assertTrue(Files.readString(agents).startsWith("codex user text"));
        assertTrue(Files.readString(claude).startsWith("claude user text"));
        assertTrue(Files.readString(copilot).startsWith("copilot user text"));
        assertHostAdapter(agents, ".agents/skills/shaft-developer/SKILL.md");
        assertHostAdapter(claude, ".claude/skills/shaft-developer/SKILL.md");
        assertHostAdapter(copilot, "instructions/shaft-developer/SKILL.md");
        assertTrue(Files.exists(repo.resolve(".agents/skills/shaft-developer/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".claude/skills/shaft-developer/SKILL.md")));
        assertTrue(Files.exists(repo.resolve(".github/instructions/shaft-developer/SKILL.md")));
        assertFalse(Files.exists(repo.resolve(".opencode")));
    }

    @Test
    void initAgentsIsByteIdempotentWhenRefreshed() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = temp.resolve("idempotent-repo");

        service.initAgents("all", "idempotent-repo", true);
        Map<Path, byte[]> first = fileBytes(repo);
        service.initAgents("all", "idempotent-repo", true);
        Map<Path, byte[]> second = fileBytes(repo);

        assertEquals(first.keySet(), second.keySet());
        first.forEach((path, bytes) -> assertTrue(java.util.Arrays.equals(bytes, second.get(path)), path.toString()));
        String agents = Files.readString(repo.resolve("AGENTS.md"));
        String codexMarker = "SHAFT-MANAGED:BEGIN shaft-developer:codex";
        String openCodeMarker = "SHAFT-MANAGED:BEGIN shaft-developer:opencode";
        assertEquals(agents.indexOf(codexMarker), agents.lastIndexOf(codexMarker));
        assertEquals(agents.indexOf(openCodeMarker), agents.lastIndexOf(openCodeMarker));
    }

    @Test
    void initAgentsDoesNotClobberAMalformedManagedBlock() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("malformed-repo"));
        Path agents = repo.resolve("AGENTS.md");
        String malformed = "user text\n<!-- SHAFT-MANAGED:BEGIN shaft-developer:codex -->\nunfinished";
        Files.writeString(agents, malformed);

        McpShaftProjectInitAgentsResult result = service.initAgents("codex", "malformed-repo", true);

        assertEquals(malformed, Files.readString(agents));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("malformed managed block")));
    }

    @Test
    void initAgentsNeverOverwritesAnUnownedSkillFile() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("existing-repo"));
        Path adapter = repo.resolve("CLAUDE.md");
        Files.writeString(adapter, "custom instructions");
        Path skill = Files.createDirectories(repo.resolve(".claude/skills/shaft-automated-test-authoring"))
                .resolve("SKILL.md");
        Files.writeString(skill, "custom skill");

        McpShaftProjectInitAgentsResult result = service.initAgents("claude", "existing-repo", false);

        assertEquals("custom skill", Files.readString(skill));
        assertTrue(Files.readString(adapter).startsWith("custom instructions"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("shaft-automated-test-authoring")));

        McpShaftProjectInitAgentsResult refreshed = service.initAgents("claude", "existing-repo", true);

        assertEquals("custom skill", Files.readString(skill));
        assertTrue(refreshed.warnings().stream().anyMatch(warning -> warning.contains("unowned")));
        assertTrue(Files.readString(adapter).startsWith("custom instructions"));
    }

    @Test
    void initAgentsRefreshesAProvenanceTrackedSkillFile() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        service.initAgents("claude", "managed-repo", true);
        Path skill = temp.resolve("managed-repo/.claude/skills/shaft-automated-test-authoring/SKILL.md");
        Files.writeString(skill, "stale generated skill");

        service.initAgents("claude", "managed-repo", true);

        assertTrue(Files.readString(skill).contains("# SHAFT Automated Test Authoring"));
    }

    @Test
    void initAgentsRejectsHostSkillDirectorySymlinkOutsideTheProject() throws Exception {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));
        Path repo = Files.createDirectories(temp.resolve("symlink-repo"));
        Path outside = Files.createDirectories(temp.resolve("outside-skills"));
        Path link = repo.resolve(".agents");
        try {
            Files.createSymbolicLink(link, outside);

            assertThrows(IllegalArgumentException.class, () -> service.initAgents("codex", "symlink-repo", true));
            assertFalse(Files.exists(outside.resolve("skills/shaft-developer/SKILL.md")));
        } catch (UnsupportedOperationException | SecurityException exception) {
            assumeTrue(false, "Symlink creation is unavailable: " + exception.getMessage());
        } catch (java.io.IOException exception) {
            assumeTrue(false, "Symlink creation failed: " + exception.getMessage());
        } finally {
            Files.deleteIfExists(link);
        }
    }

    @Test
    void initAgentsRejectsTargetTraversalWithoutWritingOutsideTheWorkspace() {
        ShaftProjectService service = new ShaftProjectService(
                McpWorkspacePolicy.of(temp),
                new FakeRunner(),
                temp.resolve("upgrade_to_modular_shaft.py"),
                List.of("python"));

        assertThrows(IllegalArgumentException.class, () -> service.initAgents("codex", "../outside", true));
        assertFalse(Files.exists(temp.getParent().resolve("outside/AGENTS.md")));
    }

    private static void assertHostAdapter(Path adapter, String routerPath) throws Exception {
        assertTrue(Files.exists(adapter));
        String content = Files.readString(adapter);
        assertTrue(content.contains("shaft-developer"));
        assertTrue(content.contains(routerPath));
        assertFalse(content.contains("act-as-shaft-dev"));
    }

    private static Map<Path, byte[]> fileBytes(Path root) throws Exception {
        Map<Path, byte[]> files = new LinkedHashMap<>();
        try (var paths = Files.walk(root)) {
            for (Path path : paths.filter(Files::isRegularFile).sorted().toList()) {
                files.put(root.relativize(path), Files.readAllBytes(path));
            }
        }
        return files;
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
