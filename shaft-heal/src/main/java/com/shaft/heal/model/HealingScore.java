package com.shaft.heal.model;

import java.util.Map;

/**
 * Separate deterministic, visual, provider, and final ranking scores.
 *
 * @param deterministicScore deterministic evidence score
 * @param visualScore optional visual score, or {@code null}
 * @param providerScore optional AI/VLM provider score, or {@code null}
 * @param finalScore effective ranking score
 * @param evidenceScores per-category deterministic scores
 */
public record HealingScore(
        double deterministicScore,
        Double visualScore,
        Double providerScore,
        double finalScore,
        Map<String, Double> evidenceScores) {
    /**
     * Creates a bounded immutable score.
     */
    public HealingScore {
        deterministicScore = bounded(deterministicScore);
        visualScore = visualScore == null ? null : bounded(visualScore);
        providerScore = providerScore == null ? null : bounded(providerScore);
        finalScore = bounded(finalScore);
        evidenceScores = evidenceScores == null ? Map.of() : Map.copyOf(evidenceScores);
    }

    private static double bounded(double value) {
        return Math.max(0, Math.min(1, value));
    }
}
