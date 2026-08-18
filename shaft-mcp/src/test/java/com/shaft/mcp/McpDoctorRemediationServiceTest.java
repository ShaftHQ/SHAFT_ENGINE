package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.RankedCause;
import com.shaft.doctor.model.RedactionSummary;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
        McpDoctorRemediationService aiService = new McpDoctorRemediationService(request ->
                AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "", "unavailable",
                        Duration.ZERO, request.deterministicFallback()));
        DoctorAnalysisResult result = new DoctorAnalysisResult(
                new EvidenceBundle(EvidenceBundle.CURRENT_SCHEMA_VERSION, "bundle-1", List.of(),
                        new RedactionSummary(List.of(), List.of(), 0), Map.of()),
                diagnosis(List.of(rankedCause(CauseCategory.LOCATOR, 88, "Locator fix prompt body."))),
                "", "", "");

        McpAnalysisReport report = aiService.build(
                result, null, List.of(), true, ApprovalPolicy.denyAll(), "driver");

        assertTrue(report.actions().stream().noneMatch(action ->
                action.status() != McpActionRecord.Status.PROVIDER_ADVISORY
                        && action.id().startsWith("provider-")), report.actions().toString());
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
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(),
                Confidence.HIGH,
                "Locator did not resolve an element.",
                "Deterministic rule precedence selected this primary cause.",
                List.of(),
                List.of(),
                List.of(),
                rankedCauses);
    }
}
