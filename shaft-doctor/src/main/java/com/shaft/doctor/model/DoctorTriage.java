package com.shaft.doctor.model;

import java.util.List;

/**
 * Run-level deterministic failure triage.
 *
 * @param schemaVersion triage schema version
 * @param bundleId evidence bundle identifier
 * @param totalAllureResults valid Allure result count
 * @param failingAttempts failed or broken attempt count
 * @param hiddenRetryFailures retry groups with a prior non-passing attempt and a final pass
 * @param recurringFailures current failures with signatures seen in historical bundles
 * @param primarySignature first current failure signature
 * @param summary concise deterministic triage summary
 * @param evidenceIds cited evidence identifiers
 */
public record DoctorTriage(
        String schemaVersion,
        String bundleId,
        int totalAllureResults,
        int failingAttempts,
        int hiddenRetryFailures,
        int recurringFailures,
        String primarySignature,
        String summary,
        List<String> evidenceIds) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates immutable triage.
     */
    public DoctorTriage {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        bundleId = DoctorModelSupport.text(bundleId);
        totalAllureResults = Math.max(0, totalAllureResults);
        failingAttempts = Math.max(0, failingAttempts);
        hiddenRetryFailures = Math.max(0, hiddenRetryFailures);
        recurringFailures = Math.max(0, recurringFailures);
        primarySignature = DoctorModelSupport.text(primarySignature);
        summary = DoctorModelSupport.requireText(summary, "Triage summary");
        evidenceIds = DoctorModelSupport.list(evidenceIds);
    }
}
