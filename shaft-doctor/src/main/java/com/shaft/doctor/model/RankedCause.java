package com.shaft.doctor.model;

import java.util.List;

/**
 * One ranked candidate root cause emitted alongside the legacy single-primary-cause fields.
 *
 * <p>Unlike {@link Diagnosis#primaryCause()}, which names exactly one supported cause, a
 * diagnosis may carry several {@code RankedCause} entries -- including in the case where
 * Doctor deliberately declines to pick a single primary cause because independent evidence
 * supports more than one. Each entry is independently trusted, cited, and actionable.</p>
 *
 * @param category cause category this entry ranks
 * @param trustPercentage deterministic trust in the 5-95 range (never 0 or 100; see
 *     {@code com.shaft.doctor.analysis.DeterministicTrustScorer})
 * @param confidence rule confidence band backing this cause
 * @param rationale human-readable explanation of how the trust score was computed
 * @param evidenceIds cited evidence identifiers supporting this cause
 * @param fixPrompt self-contained, copy/paste-ready prompt describing the recommended fix
 */
public record RankedCause(
        CauseCategory category,
        int trustPercentage,
        Confidence confidence,
        String rationale,
        List<String> evidenceIds,
        String fixPrompt) {
    /**
     * Creates a validated immutable ranked cause.
     */
    public RankedCause {
        category = category == null ? CauseCategory.UNKNOWN : category;
        trustPercentage = Math.max(0, Math.min(100, trustPercentage));
        confidence = confidence == null ? Confidence.UNKNOWN : confidence;
        rationale = DoctorModelSupport.requireText(rationale, "Ranked cause rationale");
        evidenceIds = DoctorModelSupport.list(evidenceIds);
        fixPrompt = DoctorModelSupport.requireText(fixPrompt, "Ranked cause fix prompt");
    }
}
