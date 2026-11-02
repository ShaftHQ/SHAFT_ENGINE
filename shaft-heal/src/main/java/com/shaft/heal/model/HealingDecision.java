package com.shaft.heal.model;

import java.util.Objects;

/**
 * Final safe recovery decision.
 *
 * @param status normalized decision status
 * @param selectedCandidateId selected candidate, or empty
 * @param confidence final confidence
 * @param reason safe explanation
 * @param actionOnly whether recovery applies to the current action only
 * @param sourcePatchProposed whether a reviewed Doctor proposal was emitted
 */
public record HealingDecision(
        Status status,
        String selectedCandidateId,
        double confidence,
        String reason,
        boolean actionOnly,
        boolean sourcePatchProposed) {
    /**
     * Decision status.
     */
    public enum Status {
        RECOVERED,
        NO_HISTORY,
        NO_CANDIDATES,
        BELOW_THRESHOLD,
        AMBIGUOUS,
        REJECTED_PRECONDITION,
        PROVIDER_FALLBACK
    }

    /**
     * Creates an immutable decision.
     */
    public HealingDecision {
        status = Objects.requireNonNull(status, "status");
        selectedCandidateId = Objects.requireNonNullElse(selectedCandidateId, "");
        confidence = Math.max(0, Math.min(1, confidence));
        reason = Objects.requireNonNullElse(reason, "");
    }
}
