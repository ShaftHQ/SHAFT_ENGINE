package com.shaft.doctor.model;

import java.util.List;

/**
 * Versioned deterministic diagnosis.
 *
 * @param schemaVersion diagnosis schema version
 * @param primaryCause highest-precedence supported cause
 * @param contributingCauses other supported causes
 * @param confidence confidence in the primary cause
 * @param summary concise diagnosis
 * @param rationale rule precedence and uncertainty explanation
 * @param findings cited observations and inferences
 * @param remediations deterministic next actions
 * @param missingEvidence evidence gaps that limit certainty
 * @param rankedCauses all candidate root causes ordered by descending trust percentage,
 *     each with a copy/paste-ready fix prompt; never null, may be empty
 */
public record Diagnosis(
        String schemaVersion,
        CauseCategory primaryCause,
        List<CauseCategory> contributingCauses,
        Confidence confidence,
        String summary,
        String rationale,
        List<Finding> findings,
        List<Remediation> remediations,
        List<String> missingEvidence,
        List<RankedCause> rankedCauses) {
    /**
     * Current diagnosis schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.1";

    /**
     * Creates a validated immutable diagnosis.
     */
    public Diagnosis {
        schemaVersion = DoctorModelSupport.requireText(schemaVersion, "Diagnosis schema version");
        primaryCause = primaryCause == null ? CauseCategory.UNKNOWN : primaryCause;
        contributingCauses = DoctorModelSupport.list(contributingCauses);
        confidence = confidence == null ? Confidence.UNKNOWN : confidence;
        summary = DoctorModelSupport.requireText(summary, "Diagnosis summary");
        rationale = DoctorModelSupport.requireText(rationale, "Diagnosis rationale");
        findings = DoctorModelSupport.list(findings);
        remediations = DoctorModelSupport.list(remediations);
        missingEvidence = DoctorModelSupport.list(missingEvidence);
        rankedCauses = DoctorModelSupport.list(rankedCauses);
    }

    /**
     * Backward-compatible constructor for callers built before ranked-cause support existed.
     * Delegates to the canonical constructor with an empty {@link #rankedCauses()}.
     *
     * @param schemaVersion diagnosis schema version
     * @param primaryCause highest-precedence supported cause
     * @param contributingCauses other supported causes
     * @param confidence confidence in the primary cause
     * @param summary concise diagnosis
     * @param rationale rule precedence and uncertainty explanation
     * @param findings cited observations and inferences
     * @param remediations deterministic next actions
     * @param missingEvidence evidence gaps that limit certainty
     */
    public Diagnosis(
            String schemaVersion,
            CauseCategory primaryCause,
            List<CauseCategory> contributingCauses,
            Confidence confidence,
            String summary,
            String rationale,
            List<Finding> findings,
            List<Remediation> remediations,
            List<String> missingEvidence) {
        this(schemaVersion, primaryCause, contributingCauses, confidence, summary, rationale,
                findings, remediations, missingEvidence, List.of());
    }
}
