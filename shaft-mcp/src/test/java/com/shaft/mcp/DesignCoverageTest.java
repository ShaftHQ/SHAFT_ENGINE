package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignCoverageTest {
    private static final String PARTIAL = """
            Feature: Check out

              @AC-01
              Scenario: valid payment
                Given a shopper
                When check out
                Then a valid payment places the order

              @AC-02
              Scenario: declined card
                Given a shopper
                When check out
                Then a declined card does not place the order
            """;

    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void threeAcPackWithTwoTaggedScenariosLeavesAc03UncoveredAndBlocksReady() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        McpDesignCoverage coverage = service.coverage(story, "", "", "", PARTIAL, "");
        assertEquals(McpDesignCoverage.STATUS_BLOCKED, coverage.status(), coverage.message());
        assertTrue(coverage.covered().contains("AC-01"));
        assertTrue(coverage.covered().contains("AC-02"));
        assertTrue(coverage.uncovered().contains("AC-03"), coverage.uncovered().toString());
        assertTrue(coverage.readyBlocked());
        assertTrue(coverage.waived().isEmpty());
        assertFalse(coverage.wroteFiles());
    }

    @Test
    void waivingUncoveredAcWithReasonUnblocksReadyAndKeepsResidualRisk() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        McpDesignCoverage coverage = service.coverage(story, "", "", "", PARTIAL, "AC-03:out of v1 scope");
        assertEquals(McpDesignCoverage.STATUS_OK, coverage.status(), coverage.message());
        assertFalse(coverage.readyBlocked());
        assertTrue(coverage.uncovered().isEmpty(), coverage.uncovered().toString());
        assertEquals(1, coverage.waived().size());
        assertEquals("AC-03", coverage.waived().get(0).id());
        assertEquals("out of v1 scope", coverage.waived().get(0).reason());
        assertFalse(coverage.wroteFiles());
    }

    @Test
    void untaggedScenarioBlocksReady() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String gherkin = """
                Feature: Check out

                  Scenario: valid payment
                    Given a shopper
                    When check out
                    Then a valid payment places the order
                """;
        McpDesignCoverage coverage = service.coverage(story, "", "", "", gherkin, "");
        assertTrue(coverage.readyBlocked());
        assertTrue(coverage.untaggedScenarios().contains("valid payment"), coverage.untaggedScenarios().toString());
        assertFalse(coverage.wroteFiles());
    }

    @Test
    void autoDraftedCompleteCheckoutCoversEveryAc() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/complete-checkout.txt"));
        McpDesignCoverage coverage = service.coverage(story, "", "", "", "", "");
        assertEquals(McpDesignCoverage.STATUS_OK, coverage.status(), coverage.message());
        assertFalse(coverage.readyBlocked());
        assertTrue(coverage.uncovered().isEmpty(), coverage.uncovered().toString());
        assertFalse(coverage.wroteFiles());
    }
}
