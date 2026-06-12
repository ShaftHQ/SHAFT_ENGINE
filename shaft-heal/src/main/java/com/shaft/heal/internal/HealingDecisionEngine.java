package com.shaft.heal.internal;

import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingDecision;

import java.util.Comparator;
import java.util.List;

final class HealingDecisionEngine {
    private HealingDecisionEngine() {
        throw new IllegalStateException("Utility class");
    }

    static DecisionResult decide(
            List<RankedCandidate> candidates,
            HealingConfiguration configuration,
            boolean visibilityRequired) {
        if (candidates.isEmpty()) {
            return result(HealingDecision.Status.NO_CANDIDATES, null, 0,
                    "No candidates matched the retained deterministic evidence.");
        }
        List<RankedCandidate> ranked = candidates.stream()
                .sorted(Comparator
                        .comparingDouble((RankedCandidate candidate) -> candidate.report().score().finalScore())
                        .reversed()
                        .thenComparing(candidate -> candidate.report().candidateId()))
                .toList();
        List<RankedCandidate> eligible = ranked.stream()
                .filter(candidate -> candidate.report().unique())
                .filter(candidate -> candidate.report().contextMatched())
                .filter(candidate -> !visibilityRequired || candidate.report().interactable())
                .toList();
        if (eligible.isEmpty()) {
            return result(HealingDecision.Status.REJECTED_PRECONDITION, null, 0,
                    "Candidates failed uniqueness, context, visibility, or interactability checks.");
        }
        RankedCandidate top = eligible.getFirst();
        double confidence = top.report().score().finalScore();
        if (top.report().score().deterministicScore() < configuration.minimumConfidence()) {
            return result(HealingDecision.Status.BELOW_THRESHOLD, null, confidence,
                    "The best deterministic candidate did not meet the minimum confidence.");
        }
        if (eligible.size() > 1) {
            double margin = confidence - eligible.get(1).report().score().finalScore();
            if (margin < configuration.ambiguityMargin()) {
                return result(HealingDecision.Status.AMBIGUOUS, null, confidence,
                        "The leading candidate was not sufficiently separated from the next candidate.");
            }
        }
        HealingDecision decision = new HealingDecision(
                HealingDecision.Status.RECOVERED,
                top.report().candidateId(),
                confidence,
                "A unique candidate passed deterministic confidence and action preconditions.",
                true,
                false);
        return new DecisionResult(decision, top);
    }

    private static DecisionResult result(
            HealingDecision.Status status,
            RankedCandidate candidate,
            double confidence,
            String reason) {
        return new DecisionResult(
                new HealingDecision(status, "", confidence, reason, true, false),
                candidate);
    }

    record DecisionResult(HealingDecision decision, RankedCandidate selected) {
    }
}
