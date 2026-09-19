package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignReadinessTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void draftPackCannotHandoff() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String partial = """
                Feature: Check out
                  @AC-01
                  Scenario: valid payment
                    Given a shopper
                    When check out
                    Then a valid payment places the order
                """;
        McpDesignReadiness readiness = service.readiness(story, "", "", "", partial, "", "", "false", "");
        assertEquals(McpDesignReadiness.STATUS_DRAFT, readiness.status(), readiness.message());
        assertFalse(readiness.handoffAllowed());
        assertFalse(readiness.wroteFiles());
        assertTrue(readiness.unmetConditions().stream().anyMatch(item -> item.startsWith("coverage:")),
                readiness.unmetConditions().toString());
    }

    @Test
    void fullyAcceptedFixtureIsReady() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/readiness-ready.feature"));
        McpDesignReadiness readiness = service.readiness(
                story, "", "", "", gherkin, "", "", "true", gherkin);
        assertEquals(McpDesignReadiness.STATUS_READY, readiness.status(), readiness.message());
        assertTrue(readiness.handoffAllowed());
        assertTrue(readiness.unmetConditions().isEmpty(), readiness.unmetConditions().toString());
        assertFalse(readiness.wroteFiles());
    }

    @Test
    void editingGherkinAfterReadyReturnsDraft() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/three-ac.txt"));
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/readiness-ready.feature"));
        String edited = gherkin + "\n  # edited after accept\n";
        McpDesignReadiness readiness = service.readiness(
                story, "", "", "", edited, "", "", "true", gherkin);
        assertEquals(McpDesignReadiness.STATUS_DRAFT, readiness.status(), readiness.message());
        assertFalse(readiness.handoffAllowed());
        assertTrue(readiness.unmetConditions().stream().anyMatch(item -> item.contains("edited after Ready")),
                readiness.unmetConditions().toString());
    }
}
