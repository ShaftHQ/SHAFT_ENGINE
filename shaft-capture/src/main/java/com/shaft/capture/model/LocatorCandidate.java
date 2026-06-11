package com.shaft.capture.model;

import java.util.Comparator;
import java.util.Set;

/**
 * Locator evidence retained for deterministic ranking and review.
 *
 * @param strategy locator strategy
 * @param expression sanitized locator expression
 * @param uniquenessCount number of matching elements observed
 * @param visible whether the target was visible
 * @param stable whether the evidence appeared stable
 * @param signals additional deterministic scoring signals
 */
public record LocatorCandidate(
        LocatorStrategy strategy,
        String expression,
        int uniquenessCount,
        boolean visible,
        boolean stable,
        Set<LocatorSignal> signals) {
    /**
     * Supported locator evidence strategies.
     */
    public enum LocatorStrategy {
        ROLE(100),
        ACCESSIBLE_NAME(95),
        LABEL(90),
        TEST_ID(85),
        ID(80),
        NAME(70),
        CSS(50),
        XPATH(25);

        private final int baseScore;

        LocatorStrategy(int baseScore) {
            this.baseScore = baseScore;
        }
    }

    /**
     * Deterministic evidence signals.
     */
    public enum LocatorSignal {
        USER_PROVIDED(25),
        ACCESSIBLE(15),
        LABEL_ASSOCIATED(12),
        TEST_ATTRIBUTE(10),
        STABLE_ATTRIBUTE(8),
        GENERATED(-5),
        POSITIONAL(-15),
        DYNAMIC_VALUE(-25);

        private final int weight;

        LocatorSignal(int weight) {
            this.weight = weight;
        }
    }

    /**
     * Stable best-first ordering for candidate review and replay.
     */
    public static final Comparator<LocatorCandidate> BEST_FIRST = Comparator
            .comparingInt(LocatorCandidate::score).reversed()
            .thenComparing(candidate -> candidate.strategy().name())
            .thenComparing(LocatorCandidate::expression);

    /**
     * Creates immutable locator evidence.
     */
    public LocatorCandidate {
        strategy = strategy == null ? LocatorStrategy.CSS : strategy;
        expression = ModelSupport.requireText(expression, "Locator expression");
        if (uniquenessCount < 0) {
            throw new IllegalArgumentException("Locator uniqueness count cannot be negative.");
        }
        signals = signals == null || signals.isEmpty()
                ? Set.of()
                : java.util.Collections.unmodifiableSet(java.util.EnumSet.copyOf(signals));
    }

    /**
     * Computes a deterministic score without model inference.
     *
     * @return candidate score
     */
    public int score() {
        int score = strategy.baseScore;
        score += uniquenessCount == 1 ? 30 : uniquenessCount == 0 ? -20 : -10;
        score += visible ? 10 : -10;
        score += stable ? 10 : -10;
        score += signals.stream().mapToInt(signal -> signal.weight).sum();
        return score;
    }
}
