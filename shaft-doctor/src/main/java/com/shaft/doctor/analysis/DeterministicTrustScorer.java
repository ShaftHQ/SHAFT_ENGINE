package com.shaft.doctor.analysis;

import com.shaft.doctor.model.Confidence;

/**
 * Deterministic 5-95 trust scorer for ranked root causes.
 *
 * <p>This mirrors the additive factor-model pattern used by the AI-advisory
 * {@code DoctorConfidenceScorer} (base score plus a small set of independently
 * rationalized deltas, clamped to a bounded range) but is driven entirely by
 * deterministic rule-match metadata -- no provider calls, no randomness, and no
 * dependency on AI advisory types.</p>
 */
final class DeterministicTrustScorer {
    private static final int MIN_TRUST = 5;
    private static final int MAX_TRUST = 95;
    private static final int HIGH_BASE = 75;
    private static final int MEDIUM_BASE = 55;
    private static final int LOW_BASE = 35;
    private static final int MAX_CITATION_BONUS = 15;
    private static final int CITATION_POINTS_PER_ITEM = 5;
    private static final int PRECEDENCE_BONUS_BASE = 10;
    private static final int PRECEDENCE_DECAY_PER_POSITION = 2;
    private static final int CONTRADICTION_PENALTY_PER_MATCH = 10;

    private DeterministicTrustScorer() {
    }

    /**
     * Scores one rule match's trust percentage among its competing matches.
     *
     * @param confidence rule confidence band backing the match
     * @param citedEvidenceCount number of distinct evidence items the match cites
     * @param precedencePosition 0-based position of the match in stable rule precedence order
     * @param disjointHighContradictions count of other HIGH-confidence matches in a different
     *     cause category whose cited evidence is disjoint from this match's cited evidence
     * @return the clamped 5-95 trust percentage and a human-readable rationale
     */
    static TrustResult score(
            Confidence confidence,
            int citedEvidenceCount,
            int precedencePosition,
            int disjointHighContradictions) {
        StringBuilder rationale = new StringBuilder();
        int score = applyConfidenceBand(confidence, rationale);
        score += applyCitationFactor(citedEvidenceCount, rationale);
        score += applyPrecedenceFactor(precedencePosition, rationale);
        score -= applyContradictionPenalty(disjointHighContradictions, rationale);

        int clamped = Math.max(MIN_TRUST, Math.min(MAX_TRUST, score));
        if (!rationale.isEmpty() && rationale.charAt(rationale.length() - 1) == ' ') {
            rationale.setLength(rationale.length() - 1);
        }
        return new TrustResult(clamped, rationale.toString());
    }

    /**
     * Factor 1: base score from the rule's confidence band.
     *
     * @param confidence rule confidence band
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the base score contributed by this factor
     */
    private static int applyConfidenceBand(Confidence confidence, StringBuilder rationale) {
        int base = switch (confidence) {
            case HIGH -> HIGH_BASE;
            case MEDIUM -> MEDIUM_BASE;
            case LOW, UNKNOWN -> LOW_BASE;
        };
        rationale.append("Rule confidence ").append(confidence).append(" contributes a base of ")
                .append(base).append(". ");
        return base;
    }

    /**
     * Factor 2: rewards citing more distinct evidence items, capped so a single rule
     * cannot dominate the score.
     *
     * @param citedEvidenceCount number of distinct cited evidence items
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyCitationFactor(int citedEvidenceCount, StringBuilder rationale) {
        int bonus = Math.min(MAX_CITATION_BONUS, CITATION_POINTS_PER_ITEM * Math.max(0, citedEvidenceCount));
        rationale.append("Citing ").append(Math.max(0, citedEvidenceCount))
                .append(" evidence item(s) adds ").append(bonus).append(". ");
        return bonus;
    }

    /**
     * Factor 3: rewards earlier rule-precedence positions, decaying toward zero.
     *
     * @param precedencePosition 0-based rule precedence position
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyPrecedenceFactor(int precedencePosition, StringBuilder rationale) {
        int position = Math.max(0, precedencePosition);
        int bonus = Math.max(0, PRECEDENCE_BONUS_BASE - PRECEDENCE_DECAY_PER_POSITION * position);
        rationale.append("Rule precedence position ").append(position).append(" adds ")
                .append(bonus).append(". ");
        return bonus;
    }

    /**
     * Factor 4: penalizes each other HIGH-confidence match whose cited evidence is disjoint,
     * reflecting genuine competing root-cause candidates.
     *
     * @param disjointHighContradictions count of disjoint competing HIGH-confidence matches
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score penalty (non-negative) contributed by this factor
     */
    private static int applyContradictionPenalty(int disjointHighContradictions, StringBuilder rationale) {
        int count = Math.max(0, disjointHighContradictions);
        int penalty = CONTRADICTION_PENALTY_PER_MATCH * count;
        if (penalty > 0) {
            rationale.append(count)
                    .append(" competing disjoint high-confidence cause(s) subtract ")
                    .append(penalty).append(". ");
        }
        return penalty;
    }

    /**
     * A computed trust score and its human-readable rationale.
     *
     * @param trustPercentage clamped 5-95 trust percentage
     * @param rationale explanation enumerating each contributing factor
     */
    record TrustResult(int trustPercentage, String rationale) {
    }
}
