package com.shaft.mcp;

import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HealerServiceTest {
    @TempDir
    Path temp;

    @Test
    void rejectsUnsafeCommands() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("cmd", "/c", "mvn test"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-Dtest=A;B"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void passingRerunReturnsPassedAndAddsHeadlessOffline(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);

        McpHealerRunResult result = service(tempDir, 0, "passed", "ok").runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=PassingTest"),
                tempDir.resolve("target/healer").toString(),
                1,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", null);

        assertEquals(McpHealerRunResult.Status.PASSED, result.status());
        assertTrue(result.attempts().getFirst().passed());
        assertTrue(result.attempts().getFirst().command().contains("-DheadlessExecution=true"));
        assertTrue(result.attempts().getFirst().command().contains("--offline"));
    }

    @Test
    void failedRerunReturnsDoctorSuggestionsAndAgentHandoff(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);

        McpHealerRunResult result = service(
                tempDir, 1, "failed", "NoSuchElementException: missing login").runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=LoginTest"),
                tempDir.resolve("target/healer").toString(),
                1,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", null);

        assertEquals(McpHealerRunResult.Status.FAILED_WITH_SUGGESTIONS, result.status());
        assertEquals(CauseCategory.LOCATOR, result.analysis().primaryCause());
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.id().equals("agent-healer-handoff")
                        && block.code().contains("No SHAFT provider API key is required")));
    }

    @Test
    void failedPlaywrightRerunIncludesReplayEvidenceChecklistFallback(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);

        McpHealerRunResult result = service(
                tempDir, 1, "failed", "NoSuchElementException: missing login").runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=LoginTest"),
                tempDir.resolve("target/healer").toString(),
                1,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", "playwright");

        String checklist = result.codeBlocks().stream()
                .filter(block -> block.id().equals("playwright-replay-evidence-checklist"))
                .findFirst()
                .map(McpCodeBlock::code)
                .orElse("");
        assertTrue(checklist.contains("Failed locator: unavailable in retained Doctor evidence"), checklist);
        assertTrue(checklist.contains("browser_get_page_dom"), checklist);
        assertTrue(checklist.contains("browser_take_screenshot"), checklist);
        assertTrue(checklist.contains("element_is_displayed"), checklist);
        assertTrue(checklist.contains("element_is_enabled"), checklist);
        assertTrue(checklist.contains("capture_generate_replay"), checklist);
    }

    @Test
    void noAllureChangesStopsAtGuardrail(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);

        McpHealerRunResult result = service(tempDir, 1, "", "no allure").runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=NoAllureTest"),
                tempDir.resolve("target/healer").toString(),
                1,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", null);

        assertEquals(McpHealerRunResult.Status.GUARDRAIL_STOPPED, result.status());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("No populated Allure")));
    }

    @Test
    void nonShaftProjectStopsBeforeAnyMavenRun(@TempDir Path tempDir) throws Exception {
        Path repository = Files.createDirectories(tempDir.resolve("repo"));
        Files.writeString(repository.resolve("pom.xml"), "<project/>", StandardCharsets.UTF_8);
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> {
                    throw new AssertionError("Maven must not run for a non-SHAFT repository");
                });

        McpHealerRunResult result = service.runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=NoAllureTest"),
                tempDir.resolve("target/healer").toString(),
                1,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", null);

        assertEquals(McpHealerRunResult.Status.GUARDRAIL_STOPPED, result.status());
        assertTrue(result.attempts().isEmpty());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("does not look like a SHAFT project")));
    }

    @Test
    void maxAttemptsIsClampedAndRecorded(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);

        McpHealerRunResult result = service(
                tempDir, 1, "broken", "TimeoutException: still waiting").runFailedTest(
                repository.toString(),
                List.of("mvn", "test", "-Dtest=SlowTest"),
                tempDir.resolve("target/healer").toString(),
                9,
                false,
                false,
                List.of(),
                false,
                false,
                false,
                false,
                "driver", null);

        assertEquals(5, result.attempts().size());
        assertEquals(McpHealerRunResult.Status.FAILED_WITH_SUGGESTIONS, result.status());
    }

    @Test
    void verifyFocusedPassesAndForcesHeadlessOffline() throws Exception {
        Path repository = fakeRepository(temp);
        HealerService service = new HealerService(McpWorkspacePolicy.of(temp),
                (command, directory, timeout) -> new HealerService.ProcessResult(0, false, "BUILD SUCCESS"));

        McpVerificationResult result = service.verifyFocused(
                repository.toString(), List.of("mvn", "-q", "test-compile"), false);

        assertEquals("PASSED", result.status());
        assertEquals(0, result.exitCode());
        assertTrue(result.command().contains("-DheadlessExecution=true"));
        assertTrue(result.command().contains("--offline"));
        assertTrue(result.outputSummary().contains("BUILD SUCCESS"));
    }

    @Test
    void verifyFocusedReportsFailureExitCode() throws Exception {
        Path repository = fakeRepository(temp);
        HealerService service = new HealerService(McpWorkspacePolicy.of(temp),
                (command, directory, timeout) -> new HealerService.ProcessResult(1, false, "BUILD FAILURE"));

        McpVerificationResult result = service.verifyFocused(
                repository.toString(), List.of("mvn", "test-compile"), false);

        assertEquals("FAILED", result.status());
        assertEquals(1, result.exitCode());
    }

    @Test
    void verifyFocusedRejectsReleaseGoals() throws Exception {
        Path repository = fakeRepository(temp);
        HealerService service = new HealerService(McpWorkspacePolicy.of(temp),
                (command, directory, timeout) -> new HealerService.ProcessResult(0, false, "unused"));

        assertThrows(IllegalArgumentException.class,
                () -> service.verifyFocused(repository.toString(), List.of("mvn", "deploy"), false));
    }

    @Test
    void verifyFocusedReportsTimeout() throws Exception {
        Path repository = fakeRepository(temp);
        HealerService service = new HealerService(McpWorkspacePolicy.of(temp),
                (command, directory, timeout) -> new HealerService.ProcessResult(-1, true, "stalled"));

        McpVerificationResult result = service.verifyFocused(
                repository.toString(), List.of("mvn", "test"), true);

        assertEquals("TIMED_OUT", result.status());
        assertTrue(result.timedOut());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("timed out")));
    }

    @Test
    void rejectsRepositoryRootAsFile(@TempDir Path tempDir) throws Exception {
        Path file = Files.createFile(tempDir.resolve("fake-repo"));
        HealerService service = service();

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(file.toString(), List.of("mvn", "test"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void usesDefaultOutputDirectoryWhenBlank(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        HealerService service = service(tempDir, 1, "failed", "TimeoutException: still waiting");

        McpHealerRunResult result = service.runFailedTest(repository.toString(), List.of("mvn", "test"),
                "", 1, false, false, List.of(), false, false, false, false, "driver", null);

        Path expectedReport = tempDir.toRealPath().resolve("target/shaft-healer/attempt-1/doctor-report.json");
        assertEquals(McpHealerRunResult.Status.FAILED_WITH_SUGGESTIONS, result.status());
        assertEquals(expectedReport.toString(), result.analysis().jsonReportPath());
    }

    @Test
    void usesDefaultOutputDirectoryWhenNull(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        HealerService service = service(tempDir, 1, "failed", "TimeoutException: still waiting");

        McpHealerRunResult result = service.runFailedTest(repository.toString(), List.of("mvn", "test"),
                null, 1, false, false, List.of(), false, false, false, false, "driver", null);

        Path expectedReport = tempDir.toRealPath().resolve("target/shaft-healer/attempt-1/doctor-report.json");
        assertEquals(McpHealerRunResult.Status.FAILED_WITH_SUGGESTIONS, result.status());
        assertEquals(expectedReport.toString(), result.analysis().jsonReportPath());
    }

    @Test
    void verifyFocusedUsesWorkspaceRootWhenBlank(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> new HealerService.ProcessResult(0, false, "ok"));

        McpVerificationResult result = service.verifyFocused("", List.of("mvn", "test-compile"), false);

        assertEquals("PASSED", result.status());
    }

    @Test
    void verifyFocusedUsesWorkspaceRootWhenNull(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> new HealerService.ProcessResult(0, false, "ok"));

        McpVerificationResult result = service.verifyFocused(null, List.of("mvn", "test-compile"), false);

        assertEquals("PASSED", result.status());
    }

    @Test
    void rejectsEmptyCommand() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of(),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsNullCommand() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), null,
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsNonMavenExecutable() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("gradle", "test"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsAbsolutePathMavenExecutable() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        // Absolute Maven paths (not wrapper) are rejected
        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("/usr/bin/mvn", "test"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsLongArguments() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));
        String longArg = "x".repeat(2001);

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", longArg),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsNullArgumentInCommand() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        // Null in command list causes NPE in validationCommand when accessing string methods
        assertThrows(Exception.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", null),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsBlankArgument() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "   "),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsMavenFileSettingsOption() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-s", "settings.xml"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsMavenGlobalSettingsOption() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-gs", "settings.xml"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsMavenToolchainsOption() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-t", "toolchains.xml"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsMavenExtensionClassPathOption() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-Dmaven.ext.class.path=/ext"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsMavenMultiModuleProjectDirectoryOption() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-Dmaven.multiModuleProjectDirectory=/root"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsReleaseProfile() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-Prelease"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsDeployGoal() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "deploy"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsReleaseGoal() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "release:perform"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsScmGoal() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "scm:commit"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsVersionsGoal() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "versions:set"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsHeadlessExecutionNotTrue() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-DheadlessExecution=false"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsUnknownMavenGoal() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "unknown:goal"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void rejectsParentTraversalInProjectSelection() throws Exception {
        HealerService service = service();
        Path repository = Files.createDirectories(temp.resolve("repo"));

        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-pl", ".."),
                        "", 1, false, false, List.of(), false, false, false, false, "driver", null));
    }

    @Test
    void acceptsProfileArgumentValue(@TempDir Path tempDir) throws Exception {
        // "-p" takes a profile-name value (previousOptionTakesValue); "myprofile" must NOT be
        // rejected as an unknown/unallowlisted Maven goal just because it doesn't start with "-".
        // Uses a real SHAFT-project fixture (not a bare directory) so command/path validation is
        // the only thing under test -- a bare directory would short-circuit at the earlier
        // non-SHAFT-project guardrail (a normal GUARDRAIL_STOPPED result, not a validation failure).
        Path repository = fakeRepository(tempDir);
        java.util.concurrent.atomic.AtomicReference<List<String>> capturedCommand = new java.util.concurrent.atomic.AtomicReference<>();
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> {
                    capturedCommand.set(command);
                    return new HealerService.ProcessResult(0, false, "no allure evidence");
                });

        McpHealerRunResult result = service.runFailedTest(
                repository.toString(), List.of("mvn", "test", "-p", "myprofile"),
                tempDir.resolve("target/healer").toString(),
                1, false, false, List.of(), false, false, false, false, "driver", null);

        assertEquals(McpHealerRunResult.Status.GUARDRAIL_STOPPED, result.status());
        assertTrue(capturedCommand.get().contains("-p"), capturedCommand.get().toString());
        assertTrue(capturedCommand.get().contains("myprofile"), capturedCommand.get().toString());
    }

    @Test
    void addsMissingHeadlessExecutionForTestGoals(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        var capturedCommand = new Object() { List<String> cmd; };
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> {
                    capturedCommand.cmd = command;
                    writeAllure(directory, "passed", "");
                    return new HealerService.ProcessResult(0, false, "");
                });

        service.runFailedTest(repository.toString(), List.of("mvn", "test"),
                tempDir.resolve("target/healer").toString(), 1, false, false, List.of(),
                false, false, false, false, "driver", null);

        assertTrue(capturedCommand.cmd.contains("-DheadlessExecution=true"),
                "Headless flag should be added for test goal");
    }

    @Test
    void addsOfflineModeWhenNetworkNotApproved(@TempDir Path tempDir) throws Exception {
        Path repository = fakeRepository(tempDir);
        var capturedCommand = new Object() { List<String> cmd; };
        HealerService service = new HealerService(McpWorkspacePolicy.of(tempDir),
                (command, directory, timeout) -> {
                    capturedCommand.cmd = command;
                    writeAllure(directory, "passed", "");
                    return new HealerService.ProcessResult(0, false, "");
                });

        service.runFailedTest(repository.toString(), List.of("mvn", "test"),
                tempDir.resolve("target/healer").toString(), 1, false, false, List.of(),
                false, false, false, false, "driver", null);

        assertTrue(capturedCommand.cmd.contains("--offline"),
                "Offline flag should be added when network not approved");
    }

    private HealerService service() {
        return new HealerService(McpWorkspacePolicy.of(temp));
    }

    private static HealerService service(Path root, int exitCode, String status, String message) {
        return new HealerService(McpWorkspacePolicy.of(root),
                (command, directory, timeout) -> {
                    if (!status.isBlank()) {
                        writeAllure(directory, status, message);
                    }
                    return new HealerService.ProcessResult(exitCode, false, message);
                });
    }

    private static Path fakeRepository(Path root) throws Exception {
        Path repository = Files.createDirectories(root.resolve("repo"));
        Files.writeString(repository.resolve("pom.xml"),
                "<project><dependencies><dependency><groupId>io.github.shafthq</groupId>"
                        + "<artifactId>shaft-engine</artifactId></dependency></dependencies></project>",
                StandardCharsets.UTF_8);
        return repository;
    }

    private static void writeAllure(Path repository, String status, String message) {
        try {
            Path output = Files.createDirectories(repository.resolve("allure-results"));
            String id = "healer-" + System.nanoTime();
            Files.writeString(output.resolve(id + "-result.json"), """
                    {
                      "uuid": "%s",
                      "historyId": "%s",
                      "name": "HealerTest",
                      "fullName": "example.HealerTest",
                      "status": "%s",
                      "start": 1,
                      "stop": 2,
                      "statusDetails": {
                        "message": "%s",
                        "trace": "trace"
                      }
                    }
                    """.formatted(id, id, status, message), StandardCharsets.UTF_8);
        } catch (Exception exception) {
            throw new IllegalStateException(exception);
        }
    }
}
