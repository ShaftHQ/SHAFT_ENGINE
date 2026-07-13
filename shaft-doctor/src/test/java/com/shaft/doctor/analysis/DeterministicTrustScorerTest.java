package com.shaft.doctor.analysis;

import com.shaft.doctor.model.Confidence;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the additive factor model behind {@link DeterministicTrustScorer}: confidence-band
 * base, citation and precedence bonuses, the contradiction penalty, and 5-95 clamping.
 */
class DeterministicTrustScorerTest {

    @Test
    void firstPrecedenceHighConfidenceMatchScoresHighestUncontested() {
        DeterministicTrustScorer.TrustResult result = DeterministicTrustScorer.score(
                Confidence.HIGH, 1, 0, 0);

        assertEquals(90, result.trustPercentage());
        assertTrue(result.rationale().contains("Rule confidence HIGH contributes a base of 75"),
                result.rationale());
    }

    @Test
    void confidenceBandsProduceDistinctBaseScores() {
        int high = DeterministicTrustScorer.score(Confidence.HIGH, 0, 0, 0).trustPercentage();
        int medium = DeterministicTrustScorer.score(Confidence.MEDIUM, 0, 0, 0).trustPercentage();
        int low = DeterministicTrustScorer.score(Confidence.LOW, 0, 0, 0).trustPercentage();

        assertTrue(high > medium, "HIGH (" + high + ") should outscore MEDIUM (" + medium + ")");
        assertTrue(medium > low, "MEDIUM (" + medium + ") should outscore LOW (" + low + ")");
    }

    @Test
    void citationBonusIsCappedRegardlessOfEvidenceCount() {
        DeterministicTrustScorer.TrustResult fewCitations = DeterministicTrustScorer.score(
                Confidence.LOW, 1, 5, 0);
        DeterministicTrustScorer.TrustResult manyCitations = DeterministicTrustScorer.score(
                Confidence.LOW, 50, 5, 0);

        // 50 cited items would be 250 points uncapped; the cap keeps the delta at 15 - 5 = 10.
        assertEquals(10, manyCitations.trustPercentage() - fewCitations.trustPercentage());
    }

    @Test
    void trustNeverReachesTheExtremesEvenUnderExtremeInputs() {
        DeterministicTrustScorer.TrustResult saturatedHigh = DeterministicTrustScorer.score(
                Confidence.HIGH, 100, 0, 0);
        DeterministicTrustScorer.TrustResult saturatedLow = DeterministicTrustScorer.score(
                Confidence.LOW, 0, 50, 20);

        assertEquals(95, saturatedHigh.trustPercentage());
        assertEquals(5, saturatedLow.trustPercentage());
    }

    @Test
    void contradictionPenaltyReducesTrustPerCompetingDisjointHighMatch() {
        DeterministicTrustScorer.TrustResult uncontested = DeterministicTrustScorer.score(
                Confidence.HIGH, 1, 0, 0);
        DeterministicTrustScorer.TrustResult contested = DeterministicTrustScorer.score(
                Confidence.HIGH, 1, 0, 1);

        assertEquals(10, uncontested.trustPercentage() - contested.trustPercentage());
        assertTrue(contested.rationale().contains("competing disjoint high-confidence cause(s) subtract 10"),
                contested.rationale());
    }

    @Test
    void precedenceBonusDecaysWithLaterPositionsAndFloorsAtZero() {
        int first = DeterministicTrustScorer.score(Confidence.MEDIUM, 0, 0, 0).trustPercentage();
        int sixth = DeterministicTrustScorer.score(Confidence.MEDIUM, 0, 5, 0).trustPercentage();
        int wayLater = DeterministicTrustScorer.score(Confidence.MEDIUM, 0, 20, 0).trustPercentage();

        assertTrue(first > sixth, "position 0 (" + first + ") should outscore position 5 (" + sixth + ")");
        assertEquals(sixth, wayLater, "the precedence bonus should floor at zero rather than go negative");
    }
}
