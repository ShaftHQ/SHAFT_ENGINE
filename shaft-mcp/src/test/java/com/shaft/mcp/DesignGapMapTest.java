package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignGapMapTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void assertionMapsAndJourneyNeedsRecording() throws Exception {
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/gap-map-mixed.feature"));
        McpDesignGapMap map = service.gapMap("", "", "", "", gherkin);
        assertEquals(McpDesignGapMap.STATUS_OK, map.status(), map.message());
        assertFalse(map.wroteFiles());
        assertEquals(2, map.steps().size(), map.steps().toString());
        McpDesignGapMapStep journey = map.steps().get(0);
        McpDesignGapMapStep assertion = map.steps().get(1);
        assertEquals(McpDesignGapMapStep.NEEDS_RECORDING, journey.classification(), journey.toString());
        assertEquals(McpDesignGapMapStep.MAPPED, assertion.classification(), assertion.toString());
        assertEquals("Validations", assertion.shaftType());
        assertEquals("assertThat", assertion.shaftMethod());
        assertTrue(assertion.note().contains("Validations.assertThat"), assertion.note());
    }

    @Test
    void ambiguousMatchesAreListedNotPicked() {
        String gherkin = """
                Feature: Ambiguous
                  Scenario: Two catalog hits
                    Then the browser url contains /ok and should equal done
                """;
        McpDesignGapMap map = service.gapMap("", "", "", "", gherkin);
        assertEquals(McpDesignGapMap.STATUS_OK, map.status(), map.message());
        assertEquals(1, map.steps().size());
        McpDesignGapMapStep step = map.steps().get(0);
        assertEquals(McpDesignGapMapStep.AMBIGUOUS, step.classification(), step.toString());
        assertTrue(step.candidates().size() >= 2, step.candidates().toString());
        assertEquals("", step.shaftMethod());
        assertFalse(map.wroteFiles());
    }
}
