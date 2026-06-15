package com.shaft.mcp;

import com.fasterxml.jackson.databind.ObjectMapper;
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
                "driver");

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.diagnosis().primaryCause());
        assertEquals(McpAnalysisReport.Status.DETERMINISTIC, analysis.status());
        assertTrue(analysis.codeBlocks().stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.WAIT));
        assertTrue(Files.isRegularFile(Path.of(analysis.markdownReportPath())));
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
                "driver");

        assertEquals(CauseCategory.LOCATOR, analysis.diagnosis().primaryCause());
        assertEquals(AiResponseStatus.SUCCESS, analysis.providerFallback().status());
        assertTrue(analysis.providerFallback().used());
        assertTrue(analysis.codeBlocks().stream().anyMatch(block -> block.id().startsWith("ai-")));
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
        assertEquals(Set.of("Codex", "Claude", "Gemini", "GitHub Copilot"), clients);
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
                        "driver"));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void draftPublicationRequiresExplicitApprovalBeforeReadingManifest() {
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> new DoctorService().publishDraftPr(
                        "missing-proposal.json", false, "", false, "", ""));

        assertTrue(failure.getMessage().contains("approval"));
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

        var proposal = new DoctorService().proposeHealedLocator(
                temp.toString(),
                report.toString(),
                "src/test/java/example/LoginTest.java",
                true,
                temp.resolve("target/proposals").toString());

        assertEquals(sourceContent, Files.readString(source, StandardCharsets.UTF_8));
        assertTrue(proposal.patch().content().contains("By.id(\"new-login\")"));
        assertTrue(Files.isRegularFile(Path.of(proposal.manifestPath())));
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
