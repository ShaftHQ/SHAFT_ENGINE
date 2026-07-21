package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.RankedCause;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

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
