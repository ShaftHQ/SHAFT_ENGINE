package com.shaft.mcp;

import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.aria.AriaNode;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import org.junit.jupiter.api.Test;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #5818: MCP agent context uses SemanticLocatorResolver with evidence + confidence;
 * ambiguous targets never silently resolve.
 */
class SemanticLocatorAgentBridgeTest {

    @Test
    void resolvesUniqueRoleWithConfidenceAndEvidence() {
        ElementSnapshot snapshot = snapshot(
                "button", "button", "Sign in", "",
                List.of(candidate(LocatorCandidate.LocatorStrategy.ROLE, "button:Sign in", 1),
                        candidate(LocatorCandidate.LocatorStrategy.XPATH, "//button[1]", 1)));

        SemanticLocatorAgentBridge.ResolveOutcome outcome = SemanticLocatorAgentBridge.resolve(snapshot);

        assertTrue(outcome.resolution().isPresent());
        assertFalse(outcome.ambiguous());
        assertEquals(SemanticLocatorStrategy.ROLE, outcome.resolution().orElseThrow().strategy());
        assertEquals(1.0, outcome.resolution().orElseThrow().confidence());
        Map<String, Object> payload = SemanticLocatorAgentBridge.toPayload(outcome);
        assertEquals(true, payload.get("resolved"));
        assertEquals(false, payload.get("ambiguous"));
        assertEquals("ROLE", payload.get("strategy"));
        assertTrue(((Map<?, ?>) payload.get("evidence")).containsKey("inspectionNotes"));
    }

    @Test
    void ambiguousRoleNeverSilentlyPicks() {
        ElementSnapshot snapshot = snapshot(
                "button", "button", "OK", "",
                List.of(candidate(LocatorCandidate.LocatorStrategy.ROLE, "button:OK", 3),
                        candidate(LocatorCandidate.LocatorStrategy.CSS, ".ok", 1)));

        SemanticLocatorAgentBridge.ResolveOutcome outcome = SemanticLocatorAgentBridge.resolve(snapshot);

        assertTrue(outcome.ambiguous());
        assertTrue(outcome.resolution().isEmpty());
        Map<String, Object> payload = SemanticLocatorAgentBridge.toPayload(outcome);
        assertEquals(true, payload.get("ambiguous"));
        assertEquals(false, payload.get("resolved"));
        assertEquals(3, payload.get("matchCount"));
        assertEquals(0.0, payload.get("confidence"));
    }

    @Test
    void ariaForestCanSeedEvidenceForResolution() {
        List<AriaNode> forest = List.of(
                new AriaNode("button", "Continue", List.of()),
                new AriaNode("link", "Help", List.of()));

        var evidence = SemanticLocatorAgentBridge.evidenceFromAriaForest(forest, "button", "Continue");
        SemanticLocatorAgentBridge.ResolveOutcome outcome = SemanticLocatorAgentBridge.resolve(evidence);

        assertTrue(outcome.resolution().isPresent());
        assertEquals(SemanticLocatorStrategy.ROLE, outcome.resolution().orElseThrow().strategy());
        assertTrue(outcome.evidence().inspectionNotes().contains("aria-forest"));
    }

    @Test
    void prefersSemanticOverStructuralWhenBothUnique() {
        ElementSnapshot snapshot = snapshot(
                "button", "button", "Save", "",
                List.of(candidate(LocatorCandidate.LocatorStrategy.ID, "save", 1),
                        candidate(LocatorCandidate.LocatorStrategy.ROLE, "button:Save", 1)));

        SemanticLocatorAgentBridge.ResolveOutcome outcome = SemanticLocatorAgentBridge.resolve(snapshot);

        assertEquals(SemanticLocatorStrategy.ROLE, outcome.resolution().orElseThrow().strategy());
    }

    private static ElementSnapshot snapshot(
            String tag, String role, String accessibleName, String label, List<LocatorCandidate> locators) {
        return new ElementSnapshot("logical-1", tag, role, accessibleName, label,
                Map.of(), locators, true, true, false);
    }

    private static LocatorCandidate candidate(
            LocatorCandidate.LocatorStrategy strategy, String expression, int uniqueness) {
        return new LocatorCandidate(strategy, expression, uniqueness, true, true,
                EnumSet.of(LocatorCandidate.LocatorSignal.ACCESSIBLE));
    }
}
