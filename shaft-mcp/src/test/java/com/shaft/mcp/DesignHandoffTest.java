package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignHandoffTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void nonReadyPackCannotHandoff() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String partial = """
                Feature: Check out
                  @AC-01
                  Scenario: valid payment
                    Given a shopper
                    When check out
                    Then a valid payment places the order
                """;
        McpDesignHandoff handoff = service.handoff(
                story, "", "", "", partial, "", "", "false", "", "");
        assertEquals(McpDesignHandoff.STATUS_BLOCKED, handoff.status(), handoff.message());
        assertFalse(handoff.wroteFiles());
        assertTrue(handoff.automationPrefill().isEmpty());
        assertFalse(handoff.unmetConditions().isEmpty(), handoff.unmetConditions().toString());
    }

    @Test
    void readyPackPrefillsAutomationWithoutWritingFiles() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/readiness-ready.feature"));
        McpDesignHandoff handoff = service.handoff(
                story, "", "", "", gherkin, "", "", "true", gherkin, "https://shop.example/checkout");
        assertEquals(McpDesignHandoff.STATUS_READY, handoff.status(), handoff.message());
        assertFalse(handoff.wroteFiles());
        assertFalse(handoff.scenarios().isEmpty(), handoff.scenarios().toString());
        assertFalse(handoff.acceptanceCriteria().isEmpty(), handoff.acceptanceCriteria().toString());
        assertEquals("https://shop.example/checkout", handoff.optionalUrl());
        assertEquals("https://shop.example/checkout", handoff.automationPrefill().get("url"));
        assertEquals("true", handoff.automationPrefill().get("requiresCaptureOrLiveExecute"));
    }
}
