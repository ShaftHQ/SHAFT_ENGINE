package com.shaft.doctor.ai;

import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.DoctorAdvisory;

import java.util.List;

/**
 * Computes confidence scores (0-100) for Doctor AI advisories based on evidence quality,
 * citation completeness, and provider-assessed confidence levels.
 */
final class DoctorConfidenceScorer {
    private DoctorConfidenceScorer() {
    }

    /**
     * Computes a 0-100 confidence score for a successful advisory.
     *
     * @param analysis the parsed provider analysis containing hypotheses, observations, and actions
     * @return a record containing the confidence score and rationale
     */
    static ConfidenceResult score(DoctorAdvisory.ProviderAnalysis analysis) {
        if (analysis.hypotheses().isEmpty()) {
            // No hypotheses means no substantive advisory.
            return new ConfidenceResult(0, "No hypotheses provided.");
        }

        int baseScore = 50;
        StringBuilder rationale = new StringBuilder();

        // Factor 1: Provider confidence levels in hypotheses.
        int averageConfidence = scoreProviderConfidence(analysis.hypotheses());
        rationale.append("Provider confidence: ");
        if (averageConfidence >= 75) {
            baseScore += 20;
            rationale.append("high");
        } else if (averageConfidence >= 50) {
            rationale.append("medium");
        } else {
            baseScore -= 15;
            rationale.append("low");
        }
        rationale.append(" (").append(averageConfidence).append("). ");

        // Factor 2: Citation completeness of hypotheses and actions.
        int citedHypotheses = (int) analysis.hypotheses().stream()
                .filter(DoctorAdvisory.Hypothesis::cited).count();
        int citedActions = (int) analysis.recommendedActions().stream()
                .filter(DoctorAdvisory.RecommendedAction::cited).count();

        rationale.append("Citation: ").append(citedHypotheses).append("/").append(analysis.hypotheses().size())
                .append(" hypotheses, ").append(citedActions).append("/").append(analysis.recommendedActions().size())
                .append(" actions cited. ");

        boolean fullyHypothesisCited = citedHypotheses == analysis.hypotheses().size()
                && !analysis.hypotheses().isEmpty();
        boolean fullyActionCited = citedActions == analysis.recommendedActions().size()
                || analysis.recommendedActions().isEmpty();
        if (fullyHypothesisCited) {
            baseScore += 15;
        } else if (citedHypotheses > 0) {
            baseScore += 5;
        } else {
            baseScore -= 25; // Uncited proposed fix: low band.
        }
        if (fullyActionCited) {
            baseScore += 10;
        } else if (citedActions > 0) {
            baseScore += 5;
        }

        // Factor 3: Contradicts deterministic diagnosis.
        boolean contradicts = analysis.hypotheses().stream()
                .anyMatch(DoctorAdvisory.Hypothesis::contradictsDeterministic);
        if (contradicts) {
            baseScore -= 20;
            rationale.append("Contradicts deterministic diagnosis. ");
        }

        // Factor 4: Missing evidence gaps.
        if (!analysis.missingEvidence().isEmpty()) {
            baseScore -= 10;
            rationale.append("Provider identified ").append(analysis.missingEvidence().size())
                    .append(" missing evidence gaps. ");
        }

        // Factor 5: Single vs. multiple hypotheses.
        if (analysis.hypotheses().size() == 1) {
            baseScore += 10;
            rationale.append("Single focused hypothesis. ");
        } else if (analysis.hypotheses().size() > 2) {
            baseScore -= 5;
            rationale.append("Multiple hypotheses reduce confidence. ");
        }

        int finalScore = Math.max(0, Math.min(100, baseScore));
        if (!rationale.isEmpty() && rationale.charAt(rationale.length() - 1) == ' ') {
            rationale.setLength(rationale.length() - 1);
        }

        return new ConfidenceResult(finalScore, rationale.toString());
    }

    /**
     * Computes an average score from provider-reported per-hypothesis confidence levels.
     *
     * @param hypotheses the list of hypotheses with provider confidence
     * @return an integer 0-100 representing the average provider confidence
     */
    private static int scoreProviderConfidence(List<DoctorAdvisory.Hypothesis> hypotheses) {
        if (hypotheses.isEmpty()) {
            return 0;
        }
        int total = 0;
        for (DoctorAdvisory.Hypothesis hypothesis : hypotheses) {
            total += switch (hypothesis.confidence()) {
                case HIGH -> 80;
                case MEDIUM -> 50;
                case LOW -> 25;
                case UNKNOWN -> 0;
            };
        }
        return total / hypotheses.size();
    }

    /**
     * Encapsulates a computed confidence score and its rationale.
     *
     * @param score the computed confidence 0-100
     * @param rationale human-readable explanation of the score
     */
    record ConfidenceResult(int score, String rationale) {
    }
}
