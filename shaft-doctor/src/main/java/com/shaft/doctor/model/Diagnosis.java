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
        List<String> missingEvidence) {
    /**
     * Current diagnosis schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

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
    }
}
