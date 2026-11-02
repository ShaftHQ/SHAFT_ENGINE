package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.RankedCause;
import com.shaft.doctor.model.RedactionSummary;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies per-ranked-cause remediation blocks: trust-tagged titles, one copy/paste fix-prompt
 * block per cause, the top-5 cap, and the legacy single-primary-cause fallback when no ranked
 * causes are present (e.g. an older persisted diagnosis).
 */
class McpDoctorRemediationServiceTest {
    private final McpDoctorRemediationService service = new McpDoctorRemediationService();

    @Test
    void emitsATrustTaggedBlockSetAndFixPromptPerRankedCause() {
        Diagnosis diagnosis = diagnosis(List.of(
                rankedCause(CauseCategory.LOCATOR, 88, "Locator fix prompt body."),
                rankedCause(CauseCategory.TIMING_SYNCHRONIZATION, 60, "Timing fix prompt body.")));

        List<McpCodeBlock> blocks = service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);

        assertTrue(blocks.stream().anyMatch(block -> block.title().contains("LOCATOR (trust 88%)")),
                blocks.toString());
        assertTrue(blocks.stream().anyMatch(block -> block.title().contains("TIMING_SYNCHRONIZATION (trust 60%)")),
                blocks.toString());
        assertTrue(blocks.stream().anyMatch(block -> block.code().equals("Locator fix prompt body.")
                && block.copyPasteReady()),
                blocks.toString());
        assertTrue(blocks.stream().anyMatch(block -> block.code().equals("Timing fix prompt body.")
                && block.copyPasteReady()),
                blocks.toString());
        // Locator's category blocks (locator-review + explicit-wait) must retain their kinds.
        assertTrue(blocks.stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.LOCATOR));
        assertTrue(blocks.stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.WAIT));
    }

    @Test
    void capsRankedRemediationAtTopFiveCauses() {
        List<RankedCause> six = new ArrayList<>();
        CauseCategory[] categories = CauseCategory.values();
        for (int index = 0; index < 6; index++) {
            six.add(rankedCause(categories[index % categories.length], 90 - index, "Prompt " + index));
        }
        // Ranked causes must be unique per category for this test to exercise six distinct entries;
        // CauseCategory has 8 values so the first six are already distinct.
        Diagnosis diagnosis = diagnosis(six);

        List<McpCodeBlock> blocks = service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);

        long fixPromptBlocks = blocks.stream().filter(block -> block.id().startsWith("fix-prompt-")).count();
        assertEquals(5, fixPromptBlocks, blocks.toString());
    }

    @Test
    void fallsBackToSinglePrimaryCauseBlocksWhenRankedCausesAreEmpty() {
        Diagnosis diagnosis = diagnosis(List.of());

        List<McpCodeBlock> blocks = service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);

        assertTrue(blocks.stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.LOCATOR));
        assertTrue(blocks.stream().anyMatch(block -> block.kind() == McpCodeBlock.Kind.WAIT));
        assertTrue(blocks.stream().noneMatch(block -> block.title().contains("trust")), blocks.toString());
        assertTrue(blocks.stream().noneMatch(block -> block.id().startsWith("fix-prompt-")), blocks.toString());
    }

    @Test
    void unavailableProviderKeepsAdvisoryProseAndAddsNoExecutableAction() {
        String plantedLocator = org.openqa.selenium.By.cssSelector("#unique-doctor-locator-canary").toString();
        String cssJson = "{\"method\":\"css selector\",\"selector\":\"#unique-doctor-locator-canary\"}";
        String xpathJson = "{\"method\":\"xpath\",\"selector\":\"//*[@id='unique-doctor-xpath-canary']\"}";
        String plantedPlaywright = "getByRole(\"button\", { name: \"unique-playwright-role-canary\" })";
        String plantedShaftBuilder = "SHAFT.GUI.Locator.hasId(\"unique-shaft-builder-canary\")";
        String plantedSelenium3 = "{\"using\":\"css selector\",\"value\":\"#unique-selenium3-value-canary\"}";
        String plantedSecrets = "Authorization: Basic dXNlcjpwYXNz Authorization: planted-no-scheme-token-value"
                + " api_key=supersecretvalue123"
                + " {\"authorization\":\"Basic dXNlcjpwYXNz\"}"
                + " {\"api_key\":\"supersecretvalue123\"}"
                + " sk-proj-secret-canary-4864 Bearer planted-bearer-token-value";
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        McpDoctorRemediationService aiService = new McpDoctorRemediationService(request -> {
            captured.set(request);
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                    Duration.ZERO, request.deterministicFallback());
        });
        EvidenceItem exceptionEvidence = new EvidenceItem(
                "e-1",
                EvidenceCategory.EXCEPTION_CHAIN,
                "text/plain",
                "",
                "sha-1",
                plantedSecrets.length(),
                "NoSuchElementException for " + plantedLocator + " " + plantedPlaywright + " "
                        + plantedShaftBuilder + " " + plantedSelenium3 + " " + plantedSecrets,
                false,
                false,
                Map.of(),
                new EvidenceProvenance("fixture", "fixture/result.json", "sha-1"));
        EvidenceItem allureEvidence = new EvidenceItem(
                "e-2",
                EvidenceCategory.ALLURE_RESULT,
                "application/json",
                "",
                "sha-2",
                cssJson.length() + xpathJson.length() + plantedPlaywright.length()
                        + plantedShaftBuilder.length() + plantedSelenium3.length(),
                cssJson + " " + xpathJson + " " + plantedPlaywright + " " + plantedShaftBuilder
                        + " " + plantedSelenium3,
                false,
                false,
                Map.of(),
                new EvidenceProvenance("fixture", "fixture/allure-result.json", "sha-2"));
        String summary = "Locator " + cssJson + " and " + xpathJson + " and " + plantedPlaywright
                + " and " + plantedShaftBuilder + " and " + plantedSelenium3
                + " failed with " + plantedSecrets;
        DoctorAnalysisResult result = new DoctorAnalysisResult(
                new EvidenceBundle(EvidenceBundle.CURRENT_SCHEMA_VERSION, "bundle-1",
                        List.of(exceptionEvidence, allureEvidence),
                        new RedactionSummary(List.of(), List.of(), 0), Map.of()),
                diagnosis(summary, List.of(rankedCause(CauseCategory.LOCATOR, 88,
                        "Replace " + plantedLocator + " " + cssJson + " " + xpathJson + " "
                                + plantedPlaywright + " " + plantedShaftBuilder + " "
                                + plantedSelenium3 + " and keep " + plantedSecrets))),
                "", "", "");

        McpAnalysisReport report = aiService.build(
                result, null, List.of(), true, ApprovalPolicy.denyAll(), "driver");

        assertNotNull(captured.get(), "Unavailable fallback must still build a provider request.");
        String blob = captured.get().text() + "\n" + captured.get().evidence().stream()
                .map(EvidenceReference::content)
                .reduce("", (left, right) -> left + "\n" + right);
        assertFalse(blob.contains(plantedLocator), blob);
        assertFalse(blob.contains("By.cssSelector"), blob);
        assertFalse(blob.contains(cssJson), blob);
        assertFalse(blob.contains(xpathJson), blob);
        assertFalse(blob.contains("#unique-doctor-locator-canary"), blob);
        assertFalse(blob.contains("unique-doctor-xpath-canary"), blob);
        assertFalse(blob.contains("unique-playwright-role-canary"), blob);
        assertFalse(blob.contains("unique-shaft-builder-canary"), blob);
        assertFalse(blob.contains("unique-selenium3-value-canary"), blob);
        assertFalse(blob.contains("Basic dXNlcjpwYXNz"), blob);
        assertFalse(blob.contains("planted-no-scheme-token-value"), blob);
        assertFalse(blob.contains("supersecretvalue123"), blob);
        assertFalse(blob.contains("sk-proj-secret-canary-4864"), blob);
        assertFalse(blob.contains("planted-bearer-token-value"), blob);
        assertTrue(report.actions().stream()
                .filter(action -> action.id().startsWith("provider-"))
                .noneMatch(action -> action.status() == McpActionRecord.Status.SUGGESTED),
                report.actions().toString());
        assertTrue(report.actions().stream().noneMatch(action ->
                action.status() == McpActionRecord.Status.SUGGESTED
                        && "PROVIDER_ADVISORY".equals(action.category())), report.actions().toString());
        assertTrue(report.codeBlocks().stream().anyMatch(block ->
                block.kind() == McpCodeBlock.Kind.PROVIDER_ADVISORY), report.codeBlocks().toString());
    }

    @Test
    void twoArgDeterministicBlocksOverloadDefaultsToWebdriverBackend() {
        Diagnosis diagnosis = diagnosis(List.of(rankedCause(CauseCategory.LOCATOR, 88, "Locator fix prompt body.")));

        List<McpCodeBlock> viaTwoArgOverload = service.deterministicBlocks(diagnosis, "driver");
        List<McpCodeBlock> viaExplicitWebdriver =
                service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);

        assertEquals(viaExplicitWebdriver, viaTwoArgOverload);
    }

    @Test
    void nullBackendDefaultsToWebdriverInTheThreeArgOverload() {
        Diagnosis diagnosis = diagnosis(List.of(rankedCause(CauseCategory.LOCATOR, 88, "Locator fix prompt body.")));

        List<McpCodeBlock> viaNullBackend = service.deterministicBlocks(diagnosis, "driver", null);
        List<McpCodeBlock> viaExplicitWebdriver =
                service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);

        assertEquals(viaExplicitWebdriver, viaNullBackend);
    }

    @Test
    void categoryBlocksCoverDataEnvironmentAndInfrastructure() {
        Diagnosis diagnosis = diagnosis(List.of(
                rankedCause(CauseCategory.DATA, 80, "Read the missing data."),
                rankedCause(CauseCategory.ENVIRONMENT_CONFIGURATION, 70, "Check configuration."),
                rankedCause(CauseCategory.INFRASTRUCTURE, 40, "Inspect the environment."),
                rankedCause(CauseCategory.UNKNOWN, 20, "Collect more evidence.")));

        List<McpCodeBlock> blocks = service.deterministicBlocks(diagnosis, "driver", CodegenBackend.WEBDRIVER);
        String code = blocks.stream().map(McpCodeBlock::code).reduce("", (left, right) -> left + "\n" + right);

        assertTrue(code.contains("Missing required test data."), code);
        assertTrue(code.contains("Required test environment configuration is missing."), code);
        assertEquals(2, blocks.stream().filter(block -> block.title().contains("Investigation checklist")).count());
        assertTrue(blocks.stream().anyMatch(block -> block.id().equals("fix-prompt-data")));
        assertTrue(blocks.stream().anyMatch(block -> block.id().equals("fix-prompt-unknown")));
    }

    @Test
    void providerRequestKeepsReadableSourceAndDropsSecretOversizedAndMissingFiles(@TempDir Path repository)
            throws Exception {
        Files.writeString(repository.resolve("Sample.java"), "class Sample {}", StandardCharsets.UTF_8);
        Files.writeString(repository.resolve("Sample.json"), "{\"ok\":true}", StandardCharsets.UTF_8);
        Files.writeString(repository.resolve("Sample.xml"), "<ok/>", StandardCharsets.UTF_8);
        Files.writeString(repository.resolve("secret.java"),
                "String token = \"Authorization: Bearer planted-token-value\";", StandardCharsets.UTF_8);
        Files.writeString(repository.resolve("huge.java"), "x".repeat(21_000), StandardCharsets.UTF_8);
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        McpDoctorRemediationService aiService = new McpDoctorRemediationService(request -> {
            captured.set(request);
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                    Duration.ZERO, request.deterministicFallback());
        });
        DoctorAnalysisResult result = new DoctorAnalysisResult(
                new EvidenceBundle(
                        EvidenceBundle.CURRENT_SCHEMA_VERSION,
                        "bundle-source",
                        List.of(),
                        new RedactionSummary(List.of("rule"), List.of("password"), 2),
                        Map.of()),
                diagnosis("Source evidence check.", List.of()),
                "",
                "",
                "");

        McpAnalysisReport report = aiService.build(
                result,
                repository,
                List.of("Sample.java", "Sample.json", "Sample.xml", "secret.java", "missing.java", "huge.java"),
                true,
                ApprovalPolicy.denyAll(),
                "driver");

        String evidence = captured.get().evidence().stream()
                .map(item -> item.mediaType() + ":" + item.content())
                .reduce("", (left, right) -> left + "\n" + right);
        assertTrue(evidence.contains("text/x-java-source:class Sample {}"), evidence);
        assertTrue(evidence.contains("application/json:{\"ok\":true}"), evidence);
        assertTrue(evidence.contains("application/xml:<ok/>"), evidence);
        assertFalse(evidence.contains("planted-token-value"), evidence);
        assertFalse(evidence.contains("xxxxx"), evidence);
        assertTrue(report.warnings().stream().anyMatch(warning -> warning.contains("secret-like")));
        assertTrue(report.warnings().stream().anyMatch(warning -> warning.contains("could not be read")));
        assertTrue(report.warnings().stream().anyMatch(warning -> warning.contains("source evidence budget")));
        assertTrue(report.warnings().stream().anyMatch(warning -> warning.contains("omitted")));
        assertTrue(report.warnings().stream().anyMatch(warning -> warning.contains("redacted sensitive field")));
    }

    private static RankedCause rankedCause(CauseCategory category, int trust, String fixPrompt) {
        return new RankedCause(
                category,
                trust,
                Confidence.HIGH,
                "Deterministic rationale for " + category + ".",
                List.of("e-1"),
                fixPrompt);
    }

    private static Diagnosis diagnosis(List<RankedCause> rankedCauses) {
        return diagnosis("Locator did not resolve an element.", rankedCauses);
    }

    private static Diagnosis diagnosis(String summary, List<RankedCause> rankedCauses) {
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(),
                Confidence.HIGH,
                summary,
                "Deterministic rule precedence selected this primary cause.",
                List.of(),
                List.of(),
                List.of(),
                rankedCauses);
    }
}
