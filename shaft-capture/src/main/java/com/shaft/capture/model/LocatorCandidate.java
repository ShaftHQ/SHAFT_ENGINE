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
 * @param replayXpath literal XPath the in-page recorder computed and self-verified for this
 *                    candidate (issue #4026), or blank when none was computed. When present,
 *                    it is the exact string the engine resolves at replay -- record and replay
 *                    share one artifact instead of independently deriving the same locator twice.
 * @param roleXpathVerified whether the recorder self-verified that {@code SHAFT.GUI.Locator.hasRole(...)}'s
 *                    EXACT fixed per-role XPath union (issue #4239 P1.0a/ladder) resolves uniquely to
 *                    this element in the live DOM. Only meaningful for {@link LocatorStrategy#ROLE}
 *                    candidates: {@code uniquenessCount} on a ROLE candidate is derived by re-deriving
 *                    {@code inferredRole} across the page, which can disagree with what the fixed XPath
 *                    union {@code hasRole(...)} actually ships (e.g. a {@code <div role="button">}
 *                    truthfully has {@code uniquenessCount == 1} yet matches zero elements via
 *                    {@code hasRole(Role.BUTTON)}'s tag-shape union) -- this is the only signal that
 *                    tells the two apart, so codegen's rung-1 emission gate must check it explicitly.
 */
public record LocatorCandidate(
        LocatorStrategy strategy,
        String expression,
        int uniquenessCount,
        boolean visible,
        boolean stable,
        Set<LocatorSignal> signals,
        String replayXpath,
        boolean roleXpathVerified) {
    /**
     * Creates immutable locator evidence with no recorded {@code replayXpath} and
     * {@code roleXpathVerified} defaulted to {@code false} (additive backward-compatible overload;
     * existing 6-arg call sites keep compiling unchanged).
     *
     * @param strategy locator strategy
     * @param expression raw locator expression
     * @param uniquenessCount number of matching elements observed
     * @param visible whether the target was visible
     * @param stable whether the evidence appeared stable
     * @param signals additional deterministic scoring signals
     */
    public LocatorCandidate(
            LocatorStrategy strategy,
            String expression,
            int uniquenessCount,
            boolean visible,
            boolean stable,
            Set<LocatorSignal> signals) {
        this(strategy, expression, uniquenessCount, visible, stable, signals, "", false);
    }

    /**
     * Creates immutable locator evidence with {@code roleXpathVerified} defaulted to {@code false}
     * (additive backward-compatible overload; existing 7-arg call sites keep compiling unchanged).
     *
     * @param strategy locator strategy
     * @param expression raw locator expression
     * @param uniquenessCount number of matching elements observed
     * @param visible whether the target was visible
     * @param stable whether the evidence appeared stable
     * @param signals additional deterministic scoring signals
     * @param replayXpath self-verified replay XPath, or blank when none was computed
     */
    public LocatorCandidate(
            LocatorStrategy strategy,
            String expression,
            int uniquenessCount,
            boolean visible,
            boolean stable,
            Set<LocatorSignal> signals,
            String replayXpath) {
        this(strategy, expression, uniquenessCount, visible, stable, signals, replayXpath, false);
    }
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
        USER_PROVIDED(500),
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
        replayXpath = replayXpath == null ? "" : replayXpath;
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
