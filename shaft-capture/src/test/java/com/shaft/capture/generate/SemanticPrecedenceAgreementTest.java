package com.shaft.capture.generate;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.locator.semantic.SemanticElementEvidence;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolution;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolver;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #5819: capture ranking weights and engine resolve share FR-1 precedence on fixtures
 * when UNIQUE_ID emission is not in play.
 */
class SemanticPrecedenceAgreementTest {

    @Test
    void captureStrategyPrioritiesMatchEngineRankPriority() {
        assertEquals(SemanticLocatorStrategy.ROLE.rankPriority(),
                SemanticCaptureMapping.rankPriority(LocatorCandidate.LocatorStrategy.ROLE));
        assertEquals(SemanticLocatorStrategy.LABEL.rankPriority(),
                SemanticCaptureMapping.rankPriority(LocatorCandidate.LocatorStrategy.LABEL));
        assertEquals(SemanticLocatorStrategy.TEST_ID.rankPriority(),
                SemanticCaptureMapping.rankPriority(LocatorCandidate.LocatorStrategy.TEST_ID));
        assertTrue(SemanticCaptureMapping.rankPriority(LocatorCandidate.LocatorStrategy.ROLE)
                > SemanticCaptureMapping.rankPriority(LocatorCandidate.LocatorStrategy.ID));
    }

    @Test
    void emissionTierMapOntoEngineStrategies() {
        assertEquals(List.of(SemanticLocatorStrategy.ID),
                SemanticCaptureMapping.engineStrategiesFor(LocatorPolicy.Tier.UNIQUE_ID));
        assertTrue(SemanticCaptureMapping.engineStrategiesFor(LocatorPolicy.Tier.VERIFIED_ROLE)
                .contains(SemanticLocatorStrategy.ROLE));
    }

    @Test
    void withoutUniqueIdTierRankerAndResolverAgreeOnRole() {
        LocatorCandidate role = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Sign in", 1, true, true,
                Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true);
        LocatorCandidate xpath = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.XPATH, "//button[1]", 1, true, false,
                Set.of(LocatorCandidate.LocatorSignal.GENERATED), "//button[1]", false);
        ElementSnapshot target = new ElementSnapshot(
                "logical-1", "button", "button", "Sign in", "",
                Map.of(), List.of(role, xpath), true, true, false);

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);
        assertEquals(LocatorCandidate.LocatorStrategy.ROLE, selection.selected().candidate().strategy());

        SemanticElementEvidence evidence = SemanticElementEvidence.builder()
                .role("button", "Sign in", 1)
                .xpath("//button[1]", 1)
                .inspectionNotes("fixture: capture↔engine agreement")
                .build();
        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);
        assertEquals(SemanticLocatorStrategy.ROLE, resolution.strategy());
        assertEquals(
                SemanticCaptureMapping.toEngine(selection.selected().candidate().strategy()).orElseThrow(),
                resolution.strategy());
    }

    @Test
    void uniqueIdEmissionStillPreferredForCodegenWhileEngineFr1PrefersRole() {
        LocatorCandidate role = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Save", 1, true, true,
                Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true);
        LocatorCandidate id = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ID, "save-btn", 1, true, true,
                Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE));
        ElementSnapshot target = new ElementSnapshot(
                "logical-2", "button", "button", "Save", "",
                Map.of("id", "save-btn"), List.of(role, id), true, true, false);

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);
        assertEquals(LocatorPolicy.Tier.UNIQUE_ID, selection.selected().tier().orElseThrow());

        SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(
                SemanticElementEvidence.builder()
                        .role("button", "Save", 1)
                        .id("save-btn", 1)
                        .build());
        assertEquals(SemanticLocatorStrategy.ROLE, resolution.strategy());
    }
}
