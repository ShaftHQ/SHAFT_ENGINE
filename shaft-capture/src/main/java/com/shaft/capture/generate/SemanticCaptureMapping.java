package com.shaft.capture.generate;

import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;

import java.util.List;
import java.util.Optional;

/**
 * Single-source bridge between capture {@link LocatorCandidate.LocatorStrategy} /
 * {@link LocatorPolicy.Tier} and engine {@link SemanticLocatorStrategy} (issue #5819 / #5457).
 *
 * <h2>Intentional difference: codegen emission vs runtime resolve</h2>
 *
 * <ul>
 *   <li><b>Runtime / agent / heal (FR-1)</b>: {@link SemanticLocatorStrategy} order —
 *       ROLE → ACCESSIBLE_NAME → LABEL → TEXT → TEST_ID → ID → NAME → CSS → XPATH.</li>
 *   <li><b>Capture codegen emission</b>: {@link LocatorPolicy.Tier} elevates a unique,
 *       stable, human-authored id ({@link LocatorPolicy.Tier#UNIQUE_ID}) above a verified
 *       ARIA role for <em>generated source stability</em>. That is not a second FR-1;
 *       within a tier, {@link LocatorRanker} still scores strategies via
 *       {@link SemanticLocatorStrategy#rankPriority()}.</li>
 * </ul>
 */
public final class SemanticCaptureMapping {
    private SemanticCaptureMapping() {
    }

    /**
     * Maps a capture evidence strategy onto the engine FR-1 enum.
     *
     * @param strategy capture strategy
     * @return matching engine strategy, or empty when unknown
     */
    public static Optional<SemanticLocatorStrategy> toEngine(LocatorCandidate.LocatorStrategy strategy) {
        if (strategy == null) {
            return Optional.empty();
        }
        return switch (strategy) {
            case ROLE -> Optional.of(SemanticLocatorStrategy.ROLE);
            case ACCESSIBLE_NAME -> Optional.of(SemanticLocatorStrategy.ACCESSIBLE_NAME);
            case LABEL -> Optional.of(SemanticLocatorStrategy.LABEL);
            case TEST_ID -> Optional.of(SemanticLocatorStrategy.TEST_ID);
            case ID -> Optional.of(SemanticLocatorStrategy.ID);
            case NAME -> Optional.of(SemanticLocatorStrategy.NAME);
            case CSS -> Optional.of(SemanticLocatorStrategy.CSS);
            case XPATH -> Optional.of(SemanticLocatorStrategy.XPATH);
        };
    }

    /**
     * FR-1-aligned ranking weight for a capture strategy (higher = preferred).
     *
     * @param strategy capture strategy
     * @return rank priority, or {@code 0} when unmapped
     */
    public static int rankPriority(LocatorCandidate.LocatorStrategy strategy) {
        return toEngine(strategy).map(SemanticLocatorStrategy::rankPriority).orElse(0);
    }

    /**
     * Documents which engine strategies typically back each capture emission tier.
     *
     * @param tier capture codegen tier
     * @return engine strategies associated with that emission path
     */
    public static List<SemanticLocatorStrategy> engineStrategiesFor(LocatorPolicy.Tier tier) {
        return switch (tier) {
            case UNIQUE_ID -> List.of(SemanticLocatorStrategy.ID);
            case VERIFIED_ROLE -> List.of(
                    SemanticLocatorStrategy.ROLE,
                    SemanticLocatorStrategy.ACCESSIBLE_NAME,
                    SemanticLocatorStrategy.LABEL);
            case VERIFIED_XPATH -> List.of(SemanticLocatorStrategy.XPATH, SemanticLocatorStrategy.CSS);
        };
    }
}
