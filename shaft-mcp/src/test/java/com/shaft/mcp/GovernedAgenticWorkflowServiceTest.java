package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GovernedAgenticWorkflowServiceTest {

    @Test
    void fixtureToolReturnsReviewOnlyProposalWithProvenance() {
        McpAgenticWorkflowResult result = new GovernedAgenticWorkflowService().runFixture(
                "mcp-login",
                "Log in and open profile",
                "",
                0,
                null,
                null,
                null,
                null,
                null);

        assertAll(
                () -> assertEquals("PROPOSAL_READY", result.status()),
                () -> assertTrue(result.requiresHumanReview()),
                () -> assertFalse(result.humanVisibleDiff().isBlank()),
                () -> assertEquals(5, result.provenanceEntries().size()),
                () -> assertTrue(result.provenanceEntries().stream()
                        .anyMatch(entry -> entry.contains("commands=") && entry.contains("read_journey"))),
                () -> assertTrue(result.warnings().stream()
                        .anyMatch(text -> text.contains("Human/policy review required"))));
    }

    @Test
    void fixtureToolAuditsDeniedModelMutations() {
        McpAgenticWorkflowResult result = new GovernedAgenticWorkflowService().runFixture(
                "mcp-mutate",
                "Save settings",
                "",
                0,
                "src/test/java/fixtures/SettingsTest.java",
                true,
                true,
                true,
                null);

        assertEquals("PROPOSAL_READY", result.status());
        assertTrue(result.provenanceEntries().stream().anyMatch(entry -> entry.contains("denied=")));
        assertTrue(result.warnings().stream().anyMatch(text -> text.contains("never test authority")));
    }
}
