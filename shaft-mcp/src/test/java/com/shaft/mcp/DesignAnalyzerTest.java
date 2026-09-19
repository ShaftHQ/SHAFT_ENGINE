package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignAnalyzerTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void vagueStoryNeverAllowsGherkinEvenWhenEveryGapIdIsAccepted() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/vague-story.txt"));
        McpDesignAnalysis analysis = service.analyze(story, "", "", "");

        assertEquals(McpDesignAnalysis.STATUS_NEEDS_QUESTIONS, analysis.status());
        assertTrue(analysis.blockingCount() > 0);
        assertFalse(analysis.gherkinGenerationAllowed());
        assertFalse(DesignGherkinGate.allowed(analysis));
        assertFalse(analysis.wroteFiles());
        String joined = analysis.pack().acceptanceCriteria().stream()
                .map(McpDesignAcceptanceCriterion::text)
                .collect(Collectors.joining(" "))
                .toLowerCase();
        assertFalse(joined.contains("login"));
        assertFalse(joined.contains("checkout"));

        String allIds = analysis.gaps().stream().map(McpDesignGap::id).collect(Collectors.joining(","));
        McpDesignAnalysis accepted = service.analyze(story, "", "", allIds);
        assertFalse(accepted.gherkinGenerationAllowed());
        assertTrue(accepted.gaps().stream().anyMatch(gap -> !gap.waivable() && "blocking".equals(gap.severity())));
    }

    @Test
    void completeCheckoutHasZeroBlockingGapsAndAnOraclePerCriterion() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/complete-checkout.txt"));
        McpDesignAnalysis analysis = service.analyze(story, "", "", "");

        assertEquals(McpDesignAnalysis.STATUS_COMPLETE, analysis.status(), analysis.message() + analysis.gaps());
        assertEquals(0, analysis.blockingCount(), analysis.gaps().toString());
        assertTrue(analysis.gherkinGenerationAllowed());
        assertEquals(analysis.pack().acceptanceCriteria().size(), analysis.oracles().size());
        assertFalse(analysis.wroteFiles());
    }

    @Test
    void contradictoryCriteriaAreListedNotMerged() {
        String story = """
                As a shopper
                I want to check out
                so that I can buy items
                - Then the user must be logged in
                - Then the user must not be logged in
                """;
        McpDesignAnalysis analysis = service.analyze(story, "", "", "");
        List<McpDesignGap> contradictions = analysis.gaps().stream()
                .filter(gap -> "contradiction".equals(gap.kind()))
                .toList();
        assertEquals(2, analysis.pack().acceptanceCriteria().size());
        assertFalse(contradictions.isEmpty(), analysis.gaps().toString());
        assertTrue(contradictions.get(0).tracedAcIds().containsAll(List.of("AC-01", "AC-02")));
    }

    @Test
    void nfrHintWithoutANumberIsAWarningNotBlocking() {
        String story = """
                As a shopper
                I want to check out
                so that I can buy items
                - Then a valid Visa payment places the order and the cart is emptied
                - Then a declined card does not place the order and an error status is shown
                - Then a duplicate submit within 30 seconds creates one order
                - Then totals use banker's rounding to 2 decimals
                - The page should be accessible
                """;
        McpDesignAnalysis analysis = service.analyze(story, "", "", "");
        assertTrue(analysis.gaps().stream().anyMatch(gap -> "nfr_hint".equals(gap.kind())
                && "warning".equals(gap.severity())), analysis.gaps().toString());
        assertEquals(0, analysis.blockingCount(), analysis.gaps().toString());
    }

    @Test
    void acceptingWaivableGapsYieldsResidualRiskAccepted() {
        String story = """
                As a shopper
                I want to check out with a saved card
                so that I can buy items
                - Then a valid Visa payment places the order and the cart is emptied
                """;
        McpDesignAnalysis first = service.analyze(story, "", "", "");
        assertEquals(McpDesignAnalysis.STATUS_NEEDS_QUESTIONS, first.status(), first.gaps().toString());
        String accepted = first.gaps().stream()
                .filter(gap -> gap.waivable() && "blocking".equals(gap.severity()))
                .map(McpDesignGap::id)
                .collect(Collectors.joining(","));
        assertFalse(accepted.isBlank(), first.gaps().toString());
        McpDesignAnalysis second = service.analyze(story, "", "", accepted);
        assertEquals(McpDesignAnalysis.STATUS_RESIDUAL, second.status(), second.gaps().toString());
        assertTrue(second.gherkinGenerationAllowed());
        assertTrue(second.residualRiskAccepted());
    }

    @Test
    void ingestErrorFailsClosed() {
        McpDesignAnalysis analysis = service.analyze("  ", "", "", "");
        assertEquals(McpDesignAnalysis.STATUS_ERROR, analysis.status());
        assertFalse(analysis.gherkinGenerationAllowed());
        assertFalse(analysis.wroteFiles());
        assertTrue(analysis.gaps().isEmpty());
    }
}
