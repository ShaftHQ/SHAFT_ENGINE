package com.shaft.doctor.model;

/**
 * Deterministic execution digest for reporting.
 *
 * @param schemaVersion digest schema version
 * @param bundleId evidence bundle identifier
 * @param totalAllureResults valid Allure result count
 * @param failingAttempts failed or broken attempt count
 * @param hiddenRetryFailures retry-hidden failure count
 * @param recurringFailures recurring failure count
 * @param primaryCause deterministic primary cause
 * @param confidence deterministic confidence
 * @param summary concise execution summary
 */
public record ExecutionIntelligence(
        String schemaVersion,
        String bundleId,
        int totalAllureResults,
        int failingAttempts,
        int hiddenRetryFailures,
        int recurringFailures,
        CauseCategory primaryCause,
        Confidence confidence,
        String summary) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates immutable execution intelligence.
     */
    public ExecutionIntelligence {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        bundleId = DoctorModelSupport.text(bundleId);
        totalAllureResults = Math.max(0, totalAllureResults);
        failingAttempts = Math.max(0, failingAttempts);
        hiddenRetryFailures = Math.max(0, hiddenRetryFailures);
        recurringFailures = Math.max(0, recurringFailures);
        primaryCause = primaryCause == null ? CauseCategory.UNKNOWN : primaryCause;
        confidence = confidence == null ? Confidence.UNKNOWN : confidence;
        summary = DoctorModelSupport.requireText(summary, "Execution intelligence summary");
    }
}
