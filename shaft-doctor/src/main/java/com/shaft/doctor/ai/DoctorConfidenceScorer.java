package com.shaft.doctor.ai;

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

        StringBuilder rationale = new StringBuilder();
        int baseScore = 50;
        baseScore += applyProviderConfidenceFactor(analysis, rationale);
        baseScore += applyCitationFactor(analysis, rationale);
        baseScore += applyContradictionFactor(analysis, rationale);
        baseScore += applyMissingEvidenceFactor(analysis, rationale);
        baseScore += applyHypothesisCountFactor(analysis, rationale);

        int finalScore = Math.max(0, Math.min(100, baseScore));
        if (!rationale.isEmpty() && rationale.charAt(rationale.length() - 1) == ' ') {
            rationale.setLength(rationale.length() - 1);
        }

        return new ConfidenceResult(finalScore, rationale.toString());
    }

    /**
     * Factor 1: scores provider-reported confidence levels across hypotheses.
     *
     * @param analysis  the parsed provider analysis
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyProviderConfidenceFactor(DoctorAdvisory.ProviderAnalysis analysis,
            StringBuilder rationale) {
        int averageConfidence = scoreProviderConfidence(analysis.hypotheses());
        int delta;
        rationale.append("Provider confidence: ");
        if (averageConfidence >= 75) {
            delta = 20;
            rationale.append("high");
        } else if (averageConfidence >= 50) {
            delta = 0;
            rationale.append("medium");
        } else {
            delta = -15;
            rationale.append("low");
        }
        rationale.append(" (").append(averageConfidence).append("). ");
        return delta;
    }

    /**
     * Factor 2: scores citation completeness of hypotheses and recommended actions.
     *
     * @param analysis  the parsed provider analysis
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyCitationFactor(DoctorAdvisory.ProviderAnalysis analysis, StringBuilder rationale) {
        int citedHypotheses = (int) analysis.hypotheses().stream()
                .filter(DoctorAdvisory.Hypothesis::cited).count();
        int citedActions = (int) analysis.recommendedActions().stream()
                .filter(DoctorAdvisory.RecommendedAction::cited).count();

        rationale.append("Citation: ").append(citedHypotheses).append("/").append(analysis.hypotheses().size())
                .append(" hypotheses, ").append(citedActions).append("/").append(analysis.recommendedActions().size())
                .append(" actions cited. ");

        int delta = 0;
        boolean fullyHypothesisCited = citedHypotheses == analysis.hypotheses().size()
                && !analysis.hypotheses().isEmpty();
        boolean fullyActionCited = citedActions == analysis.recommendedActions().size()
                || analysis.recommendedActions().isEmpty();
        if (fullyHypothesisCited) {
            delta += 15;
        } else if (citedHypotheses > 0) {
            delta += 5;
        } else {
            delta -= 25; // Uncited proposed fix: low band.
        }
        if (fullyActionCited) {
            delta += 10;
        } else if (citedActions > 0) {
            delta += 5;
        }
        return delta;
    }

    /**
     * Factor 3: penalizes hypotheses that contradict the deterministic diagnosis.
     *
     * @param analysis  the parsed provider analysis
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyContradictionFactor(DoctorAdvisory.ProviderAnalysis analysis, StringBuilder rationale) {
        boolean contradicts = analysis.hypotheses().stream()
                .anyMatch(DoctorAdvisory.Hypothesis::contradictsDeterministic);
        if (contradicts) {
            rationale.append("Contradicts deterministic diagnosis. ");
            return -20;
        }
        return 0;
    }

    /**
     * Factor 4: penalizes advisories where the provider flagged missing evidence gaps.
     *
     * @param analysis  the parsed provider analysis
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyMissingEvidenceFactor(DoctorAdvisory.ProviderAnalysis analysis, StringBuilder rationale) {
        if (!analysis.missingEvidence().isEmpty()) {
            rationale.append("Provider identified ").append(analysis.missingEvidence().size())
                    .append(" missing evidence gaps. ");
            return -10;
        }
        return 0;
    }

    /**
     * Factor 5: rewards a single focused hypothesis and penalizes conflicting multiple hypotheses.
     *
     * @param analysis  the parsed provider analysis
     * @param rationale the shared rationale builder to append explanatory text to
     * @return the score delta contributed by this factor
     */
    private static int applyHypothesisCountFactor(DoctorAdvisory.ProviderAnalysis analysis, StringBuilder rationale) {
        if (analysis.hypotheses().size() == 1) {
            rationale.append("Single focused hypothesis. ");
            return 10;
        } else if (analysis.hypotheses().size() > 2) {
            rationale.append("Multiple hypotheses reduce confidence. ");
            return -5;
        }
        return 0;
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
