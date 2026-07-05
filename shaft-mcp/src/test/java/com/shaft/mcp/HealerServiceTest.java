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
                        "", 1, false, false, List.of(), false, false, false, false, "driver"));
        assertThrows(IllegalArgumentException.class,
                () -> service.runFailedTest(repository.toString(), List.of("mvn", "test", "-Dtest=A;B"),
                        "", 1, false, false, List.of(), false, false, false, false, "driver"));
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
                "driver");

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
                "driver");

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
                tempDir, 1, "failed", "NoSuchElementException: missing login").runFailedPlaywrightTest(
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
                "driver");

        String checklist = result.codeBlocks().stream()
                .filter(block -> block.id().equals("playwright-replay-evidence-checklist"))
                .findFirst()
                .map(McpCodeBlock::code)
                .orElse("");
        assertTrue(checklist.contains("Failed locator: unavailable in retained Doctor evidence"), checklist);
        assertTrue(checklist.contains("playwright_browser_get_page_dom"), checklist);
        assertTrue(checklist.contains("playwright_browser_take_screenshot"), checklist);
        assertTrue(checklist.contains("playwright_element_is_displayed"), checklist);
        assertTrue(checklist.contains("playwright_element_is_enabled"), checklist);
        assertTrue(checklist.contains("playwright_replay_recording"), checklist);
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
                "driver");

        assertEquals(McpHealerRunResult.Status.GUARDRAIL_STOPPED, result.status());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("No populated Allure")));
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
                "driver");

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
        Files.writeString(repository.resolve("pom.xml"), "<project/>", StandardCharsets.UTF_8);
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
