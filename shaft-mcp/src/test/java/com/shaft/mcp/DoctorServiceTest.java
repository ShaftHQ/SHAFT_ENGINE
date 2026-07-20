package com.shaft.mcp;

import tools.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiProviderRegistry;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ProcessingLocation;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorServiceTest {
    private final AiProviderRegistry registry = new AiProviderRegistry();

    @AfterEach
    void cleanup() {
        registry.clearForCurrentThread();
        SHAFT.Properties.clearForCurrentThread();
    }

    @Test
    void toolAnalyzesAllowlistedEvidenceOffline(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("allure-results"));
        Path result = input.resolve("mcp-result.json");
        Files.writeString(result, new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "mcp",
                "historyId", "mcp",
                "name", "mcp",
                "fullName", "example.Mcp.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.diagnosis().primaryCause());
        assertEquals(McpAnalysisReport.Status.DETERMINISTIC, analysis.status());
        assertTrue(analysis.codeBlocks().stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.WAIT));
        assertTrue(Files.isRegularFile(Path.of(analysis.markdownReportPath())));
    }

    @Test
    void emptyAllurePathsAutomaticallyAnalyzeTheMostRecentResultsInTheWorkspace(@TempDir Path temp) throws Exception {
        Path stale = Files.createDirectories(temp.resolve("old-module/allure-results"));
        Path staleResult = stale.resolve("stale-result.json");
        Files.writeString(staleResult, new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "stale",
                "historyId", "stale",
                "name", "stale",
                "fullName", "example.Stale.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element",
                        "trace", "trace"))), StandardCharsets.UTF_8);
        Files.setLastModifiedTime(staleResult, java.nio.file.attribute.FileTime.fromMillis(1_000_000L));
        Path fresh = Files.createDirectories(temp.resolve("target/allure-results"));
        Files.writeString(fresh.resolve("fresh-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "fresh",
                "historyId", "fresh",
                "name", "fresh",
                "fullName", "example.Fresh.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(),
                List.of(),
                temp.resolve("doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.diagnosis().primaryCause());
        assertEquals(McpAnalysisReport.Status.DETERMINISTIC, analysis.status());
    }

    @Test
    void emptyAllurePathsAutomaticallyAnalyzeASingleFileAllureHtmlReportWhenNoResultsDirectoryExists(
            @TempDir Path temp) throws Exception {
        Path reportDir = Files.createDirectories(temp.resolve("shaft-visual/allure-report"));
        Files.writeString(reportDir.resolve("2026-01-01_00-00-00-000_AllureReport.html"),
                syntheticAllureHtmlReport(), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(),
                List.of(),
                temp.resolve("doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.diagnosis().primaryCause());
        assertEquals(McpAnalysisReport.Status.DETERMINISTIC, analysis.status());
    }

    @Test
    void emptyAllurePathsWithNoResultsAnywhereExplainHowToProduceThem(@TempDir Path temp) {
        IllegalArgumentException failure = org.junit.jupiter.api.Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> service(temp).analyzeFailedAllure(
                        List.of(),
                        List.of(),
                        temp.resolve("doctor-output").toString(),
                        false,
                        false,
                        1,
                        "",
                        List.of(),
                        false,
                        false,
                        false,
                        "driver", null));

        assertTrue(failure.getMessage().contains("No Allure results were found in this workspace"));
        assertTrue(failure.getMessage().contains("SHAFT reporting enabled"));
    }

    @Test
    void configuredProviderAddsAdvisoryThroughDoctorAnalyze(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("provider-allure-results"));
        Files.writeString(input.resolve("provider-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "provider",
                "historyId", "provider",
                "name", "provider",
                "fullName", "example.Provider.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element",
                        "trace", "trace"))), StandardCharsets.UTF_8);
        registry.registerForCurrentThread(new DoctorSnippetProvider());
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("doctor-test")
                .remoteConsent(true)
                .allowedEvidenceCategories("TEXT,LOG");

        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("provider-doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                true,
                false,
                true,
                "driver", null);

        assertEquals(CauseCategory.LOCATOR, analysis.diagnosis().primaryCause());
        assertEquals(AiResponseStatus.SUCCESS, analysis.providerFallback().status());
        assertTrue(analysis.providerFallback().used());
        assertTrue(analysis.codeBlocks().stream().anyMatch(block -> block.id().startsWith("ai-")));
    }

    @Test
    void agentDoctorAdvisoryIsReturnedWhenMcpUseAiHasNoConfiguredProvider(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("agent-allure-results"));
        Files.writeString(input.resolve("agent-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "agent",
                "historyId", "agent",
                "name", "agent",
                "fullName", "example.Agent.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("agent-doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                true,
                false,
                false,
                "driver", null);

        assertTrue(analysis.codeBlocks().stream()
                .anyMatch(block -> block.id().equals("agent-doctor-advisory")
                        && block.title().contains("Agent LLM repair handoff")), analysis.codeBlocks().toString());
    }

    @Test
    void playwrightDoctorIncludesReplayEvidenceChecklistWithLocatorContext(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("playwright-allure-results"));
        Files.writeString(input.resolve("playwright-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "playwright",
                "historyId", "playwright",
                "name", "playwright",
                "fullName", "example.Playwright.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element: By.cssSelector: #login-button",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("playwright-doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", "playwright");

        String checklist = blockCode(analysis, "playwright-replay-evidence-checklist");
        String evidenceId = analysis.diagnosis().findings().stream()
                .flatMap(finding -> finding.evidenceIds().stream())
                .findFirst()
                .orElseThrow();
        assertTrue(checklist.contains("By.cssSelector: #login-button"), checklist);
        assertTrue(checklist.contains(evidenceId), checklist);
        assertPlaywrightReplayTools(checklist);
    }

    @Test
    void suggestPlaywrightFixRebuildsReplayEvidenceChecklistFromAnExistingReport(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("playwright-suggest-results"));
        Files.writeString(input.resolve("playwright-suggest-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "playwright-suggest",
                "historyId", "playwright-suggest",
                "name", "playwright-suggest",
                "fullName", "example.PlaywrightSuggest.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element: By.cssSelector: #login-button",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var firstPass = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("playwright-suggest-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", "playwright");

        var rebuilt = service(temp).suggestFix(
                firstPass.jsonReportPath(),
                "",
                List.of(),
                false,
                false,
                false,
                "driver", "playwright");

        assertEquals(CauseCategory.LOCATOR, rebuilt.diagnosis().primaryCause());
        assertEquals(McpAnalysisReport.Status.DETERMINISTIC, rebuilt.status());
        String checklist = blockCode(rebuilt, "playwright-replay-evidence-checklist");
        assertTrue(checklist.contains("By.cssSelector: #login-button"), checklist);
        assertPlaywrightReplayTools(checklist);
    }

    @Test
    void suggestFixRebuildsDeterministicActionsFromAnExistingWebDriverReport(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("webdriver-suggest-results"));
        Files.writeString(input.resolve("webdriver-suggest-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "webdriver-suggest",
                "historyId", "webdriver-suggest",
                "name", "webdriver-suggest",
                "fullName", "example.WebDriverSuggest.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var firstPass = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("webdriver-suggest-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        var rebuilt = service(temp).suggestFix(
                firstPass.jsonReportPath(),
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, rebuilt.diagnosis().primaryCause());
        assertTrue(rebuilt.codeBlocks().stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.WAIT));
    }

    @Test
    void plainAnalyzeMethodReturnsAContentFreeSummaryOffline(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("allure-results"));
        Files.writeString(input.resolve("plain-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "plain",
                "historyId", "plain",
                "name", "plain",
                "fullName", "example.Plain.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var summary = new DoctorService().analyze(
                List.of(input.toString()),
                List.of(),
                List.of(temp.toString()),
                temp.resolve("plain-doctor-output").toString(),
                false,
                false,
                1);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, summary.diagnosis().primaryCause());
        assertTrue(summary.evidenceItemCount() >= 0);
        assertTrue(Files.isRegularFile(Path.of(summary.jsonReportPath())));
    }

    @Test
    void plainAnalyzeMethodUsesTheConfiguredProviderAdvisoryPathWhenPilotIsEnabled(@TempDir Path temp)
            throws Exception {
        Path input = Files.createDirectories(temp.resolve("allure-results"));
        Files.writeString(input.resolve("configured-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "configured",
                "historyId", "configured",
                "name", "configured",
                "fullName", "example.Configured.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element",
                        "trace", "trace"))), StandardCharsets.UTF_8);
        registry.registerForCurrentThread(new DoctorSnippetProvider());
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("doctor-test")
                .remoteConsent(true)
                .allowedEvidenceCategories("TEXT,LOG");

        var summary = new DoctorService().analyze(
                List.of(input.toString()),
                List.of(),
                List.of(temp.toString()),
                temp.resolve("configured-plain-output").toString(),
                false,
                false,
                1);

        assertEquals(CauseCategory.LOCATOR, summary.diagnosis().primaryCause());
        assertTrue(Files.isRegularFile(Path.of(summary.jsonReportPath())));
    }

    @Test
    void proposeFixReturnsAnUnsuccessfulProviderPatchWithoutAConfiguredProviderInsteadOfCallingRepairService(
            @TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("allure-results"));
        Files.writeString(input.resolve("propose-fix-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "propose-fix",
                "historyId", "propose-fix",
                "name", "propose-fix",
                "fullName", "example.ProposeFix.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))), StandardCharsets.UTF_8);
        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("propose-fix-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        var proposal = new DoctorService().proposeFix(
                temp.toString(),
                "deadbeef",
                analysis.jsonReportPath(),
                "",
                "ISSUE-1",
                List.of(),
                List.of(),
                List.of(),
                false,
                "",
                true);

        assertTrue(proposal.providerPatch() != null, "Expected a provider-fallback result without a configured AI provider");
        assertFalse(proposal.providerPatch().successful());
        assertTrue(proposal.proposal() == null,
                "Without a successful provider patch, proposeFix must not proceed to DoctorRepairService");
    }

    @Test
    void webdriverDoctorDoesNotIncludePlaywrightReplayGuidance(@TempDir Path temp) throws Exception {
        Path input = Files.createDirectories(temp.resolve("webdriver-allure-results"));
        Files.writeString(input.resolve("webdriver-result.json"), new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "webdriver",
                "historyId", "webdriver",
                "name", "webdriver",
                "fullName", "example.WebDriver.test",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "statusDetails", Map.of(
                        "message", "NoSuchElementException: unable to locate element: By.cssSelector: #login-button",
                        "trace", "trace"))), StandardCharsets.UTF_8);

        var analysis = service(temp).analyzeFailedAllure(
                List.of(input.toString()),
                List.of(),
                temp.resolve("webdriver-doctor-output").toString(),
                false,
                false,
                1,
                "",
                List.of(),
                false,
                false,
                false,
                "driver", null);

        String output = analysis.codeBlocks().stream()
                .map(block -> block.title() + "\n" + block.code() + "\n" + block.placement())
                .reduce("", (left, right) -> left + "\n" + right);
        assertTrue(!output.contains("playwright_browser_get_page_dom"), output);
        assertTrue(!output.contains("playwright_replay_recording"), output);
    }

    @Test
    void documentedExternalClientsInvokeDoctorAnalyzeWithoutCredentials() throws Exception {
        Path root = repositoryRoot();
        String json = Files.readString(root.resolve(
                "shaft-mcp/src/test/resources/fixtures/shaft-pilot/mcp/doctor-analyze-invocations.json"));
        var fixture = new ObjectMapper().readTree(json);

        assertEquals("doctor_analyze_failed_allure", fixture.path("tool").asText());
        Set<String> clients = new java.util.LinkedHashSet<>();
        fixture.path("clients").forEach(client -> clients.add(client.path("name").asText()));
        assertEquals(Set.of("ChatGPT", "Codex", "Claude", "Gemini", "GitHub Copilot"), clients);
        assertTrue(fixture.path("arguments").path("includeScreenshots").isBoolean());
        assertTrue(fixture.path("arguments").path("includePageSnapshots").isBoolean());
        assertTrue(fixture.path("arguments").path("useAi").isBoolean());
        assertTrue(!json.contains("API_KEY") && !json.contains("apiKey")
                && !json.toLowerCase(java.util.Locale.ROOT).contains("authorization"));
    }

    @Test
    void failedAllureAnalysisRejectsPathsOutsideWorkspace(@TempDir Path temp) throws Exception {
        Path outside = Files.createTempDirectory("shaft-mcp-outside");
        Files.writeString(outside.resolve("result.json"), "{}", StandardCharsets.UTF_8);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service(temp).analyzeFailedAllure(
                        List.of(outside.toString()),
                        List.of(),
                        temp.resolve("doctor-output").toString(),
                        false,
                        false,
                        1,
                        "",
                        List.of(),
                        false,
                        false,
                        false,
                        "driver", null));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void suggestFixRejectsMalformedWorkspaceReport(@TempDir Path temp) throws Exception {
        Path report = temp.resolve("doctor-report.json");
        Files.writeString(report, "not-json", StandardCharsets.UTF_8);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service(temp).suggestFix(
                        report.toString(),
                        "",
                        List.of(),
                        false,
                        false,
                        false,
                        "driver", null));

        assertTrue(failure.getMessage().contains("Doctor report could not be read"));
    }

    @Test
    void draftPublicationRequiresExplicitApprovalBeforeReadingManifest() {
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> new DoctorService().publishDraftPr(
                        "missing-proposal.json", false, "", false, "", ""));

        assertTrue(failure.getMessage().contains("approval"));
    }

    @Test
    void healedLocatorToolRejectsRepositoryOutsideWorkspace(@TempDir Path temp) throws Exception {
        SHAFT.Properties.healing.set().sourcePatchEnabled(true);
        Path workspace = Files.createDirectories(temp.resolve("workspace"));
        Path outside = Files.createDirectories(temp.resolve("outside"));
        Path source = outside.resolve("src/test/java/example/LoginTest.java");
        Files.createDirectories(source.getParent());
        Files.writeString(source, """
                package example;
                import org.openqa.selenium.By;
                class LoginTest {
                    private static final By LOGIN = By.id("old-login");
                }
                """, StandardCharsets.UTF_8);
        Path report = workspace.resolve("healing-report.json");
        Files.writeString(report, """
                {
                  "schemaVersion": "2.0",
                  "attemptId": "attempt-mcp",
                  "originalLocator": "By.id: old-login",
                  "candidates": [{
                    "candidateId": "candidate-1",
                    "proposedLocator": "By.id: new-login",
                    "evidence": ["test-id exact match"],
                    "unique": true,
                    "contextMatched": true
                  }],
                  "decision": {
                    "status": "RECOVERED",
                    "selectedCandidateId": "candidate-1",
                    "confidence": 0.94
                  },
                  "action": {
                    "outcome": "PASSED",
                    "postActionVerification": "ELEMENT_INTERACTABLE"
                  }
                }
                """, StandardCharsets.UTF_8);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service(workspace).proposeHealedLocator(
                        outside.toString(),
                        report.toString(),
                        "src/test/java/example/LoginTest.java",
                        true,
                        "target/proposals"));

        assertTrue(failure.getMessage().contains("Repository root is outside the MCP workspace"));
    }

    @Test
    void healedLocatorToolCreatesProposalWithoutChangingSource(@TempDir Path temp) throws Exception {
        SHAFT.Properties.healing.set().sourcePatchEnabled(true);
        Path source = temp.resolve("src/test/java/example/LoginTest.java");
        Files.createDirectories(source.getParent());
        String sourceContent = """
                package example;
                import org.openqa.selenium.By;
                class LoginTest {
                    private static final By LOGIN = By.id("old-login");
                }
                """;
        Files.writeString(source, sourceContent, StandardCharsets.UTF_8);
        Path report = temp.resolve("healing-report.json");
        Files.writeString(report, """
                {
                  "schemaVersion": "2.0",
                  "attemptId": "attempt-mcp",
                  "originalLocator": "By.id: old-login",
                  "candidates": [{
                    "candidateId": "candidate-1",
                    "proposedLocator": "By.id: new-login",
                    "evidence": ["test-id exact match"],
                    "unique": true,
                    "contextMatched": true
                  }],
                  "decision": {
                    "status": "RECOVERED",
                    "selectedCandidateId": "candidate-1",
                    "confidence": 0.94
                  },
                  "action": {
                    "outcome": "PASSED",
                    "postActionVerification": "ELEMENT_INTERACTABLE"
                  }
                }
                """, StandardCharsets.UTF_8);

        var proposal = service(temp).proposeHealedLocator(
                temp.toString(),
                report.toString(),
                "src/test/java/example/LoginTest.java",
                true,
                temp.resolve("target/proposals").toString());

        assertEquals(sourceContent, Files.readString(source, StandardCharsets.UTF_8));
        assertTrue(proposal.patch().content().contains("By.id(\"new-login\")"));
        assertTrue(Files.isRegularFile(Path.of(proposal.manifestPath())));
    }

    /**
     * Builds a minimal synthetic SHAFT single-file Allure HTML report: the embedded-data marker
     * plus one {@code data/test-results/*.json} entry, matching the empirically observed format
     * (base64-encoded JSON passed to repeated {@code d("name","base64")} calls).
     */
    private static String syntheticAllureHtmlReport() throws Exception {
        String encoded = java.util.Base64.getEncoder().encodeToString(new ObjectMapper().writeValueAsString(Map.of(
                "uuid", "html-report",
                "historyId", "html-report",
                "name", "reportTest",
                "fullName", "example.Report.reportTest",
                "status", "failed",
                "start", 1,
                "stop", 2,
                "error", Map.of(
                        "message", "TimeoutException: condition failed to be met",
                        "trace", "trace"))).getBytes(StandardCharsets.UTF_8));
        return "<html><body><script>"
                + "window.allureReportDataReady = false;"
                + "window.allureReportData = window.allureReportData || {};"
                + "function d(name, value){ return new Promise(function (resolve) {"
                + "window.allureReportData[name] = value; return resolve(true); }); }"
                + "</script><script defer>Promise.allSettled(["
                + "d(\"data/test-results/html-report.json\",\"" + encoded + "\")"
                + "]);</script></body></html>";
    }

    private static Path repositoryRoot() {
        Path current = Path.of("").toAbsolutePath().normalize();
        while (current != null) {
            if (Files.isRegularFile(current.resolve("pom.xml"))
                    && Files.isDirectory(current.resolve("shaft-mcp"))
                    && Files.isDirectory(current.resolve("shaft-engine"))) {
                return current;
            }
            current = current.getParent();
        }
        throw new IllegalStateException("Repository root could not be resolved.");
    }

    private static DoctorService service(Path root) {
        return new DoctorService(McpWorkspacePolicy.of(root), new McpDoctorRemediationService());
    }

    private static String blockCode(McpAnalysisReport analysis, String blockId) {
        return analysis.codeBlocks().stream()
                .filter(block -> block.id().equals(blockId))
                .findFirst()
                .map(McpCodeBlock::code)
                .orElse("");
    }

    private static void assertPlaywrightReplayTools(String text) {
        assertTrue(text.contains("playwright_browser_get_page_dom"), text);
        assertTrue(text.contains("playwright_browser_take_screenshot"), text);
        assertTrue(text.contains("playwright_element_is_displayed"), text);
        assertTrue(text.contains("playwright_element_is_enabled"), text);
        assertTrue(text.contains("playwright_replay_recording"), text);
    }

    private static final class DoctorSnippetProvider implements AiProvider {
        @Override
        public String id() {
            return "doctor-test";
        }

        @Override
        public AiCapabilities capabilities() {
            return new AiCapabilities(true, false, false, 16_000, ProcessingLocation.REMOTE);
        }

        @Override
        public AiProviderAvailability availability() {
            return AiProviderAvailability.ready();
        }

        @Override
        public AiResponse execute(AiRequest request) {
            String evidenceId = request.evidence().getFirst().id();
            var payload = new ObjectMapper().createObjectNode();
            payload.put("schemaVersion", "1.0");
            var block = payload.putArray("codeBlocks").addObject();
            block.put("title", "Provider wait advisory");
            block.put("kind", "WAIT");
            block.put("code", "driver.element().waitUntil(ExpectedConditions.elementToBeClickable(TARGET_ELEMENT), true);");
            block.put("placement", "Paste before the failing click.");
            block.put("copyPasteReady", false);
            block.putArray("imports").add("org.openqa.selenium.support.ui.ExpectedConditions");
            block.putArray("evidenceIds").add(evidenceId);
            block.putArray("warnings").add("Review before applying.");
            return AiResponse.success(id(), "doctor-test-model", payload,
                    Duration.ofMillis(1), AiUsage.empty(), request.deterministicFallback());
        }
    }
}
