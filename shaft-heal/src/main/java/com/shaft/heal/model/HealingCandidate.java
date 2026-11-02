package com.shaft.heal.model;

import java.util.List;
import java.util.Objects;

/**
 * Explainable candidate derived from deterministic page evidence.
 *
 * @param candidateId stable attempt-local candidate identifier
 * @param proposedLocator proposed replacement locator
 * @param fingerprint candidate fingerprint
 * @param score separate ranking scores
 * @param evidence matched evidence descriptions
 * @param unique whether the proposed locator is unique
 * @param visible whether the element is displayed
 * @param interactable whether the element is displayed and enabled
 * @param contextMatched whether frame and shadow context matched
 */
public record HealingCandidate(
        String candidateId,
        String proposedLocator,
        LocatorFingerprint fingerprint,
        HealingScore score,
        List<String> evidence,
        boolean unique,
        boolean visible,
        boolean interactable,
        boolean contextMatched) {
    /**
     * Creates an immutable candidate.
     */
    public HealingCandidate {
        candidateId = Objects.requireNonNullElse(candidateId, "");
        proposedLocator = Objects.requireNonNullElse(proposedLocator, "");
        fingerprint = Objects.requireNonNull(fingerprint, "fingerprint");
        score = Objects.requireNonNull(score, "score");
        evidence = evidence == null ? List.of() : List.copyOf(evidence);
    }
}
