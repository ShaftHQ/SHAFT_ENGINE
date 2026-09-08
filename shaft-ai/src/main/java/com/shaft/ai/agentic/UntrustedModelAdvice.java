package com.shaft.ai.agentic;

import java.util.List;
import java.util.Objects;

/**
 * Model output treated as advisory only (FR-4). Never authorizes test writes or cleanup.
 *
 * @param suggestedTestMutation optional path the model wants changed
 * @param suggestedCleanup whether the model asks for cleanup
 * @param suggestedDestructiveBrowser whether the model asks for a destructive browser action
 * @param suggestedCredentialRead whether the model asks to read credentials
 * @param alternateDiagnosis alternate diagnosis text (disagreement signal when present with primary)
 * @param notes free-form model notes (untrusted page/prompt content may appear here)
 */
public record UntrustedModelAdvice(
        String suggestedTestMutation,
        boolean suggestedCleanup,
        boolean suggestedDestructiveBrowser,
        boolean suggestedCredentialRead,
        String alternateDiagnosis,
        List<String> notes) {
    /**
     * Creates immutable untrusted advice.
     */
    public UntrustedModelAdvice {
        suggestedTestMutation = suggestedTestMutation == null ? "" : suggestedTestMutation.trim();
        alternateDiagnosis = alternateDiagnosis == null ? "" : alternateDiagnosis.trim();
        notes = notes == null ? List.of() : List.copyOf(notes);
    }

    /**
     * Empty advice.
     *
     * @return empty advice
     */
    public static UntrustedModelAdvice none() {
        return new UntrustedModelAdvice("", false, false, false, "", List.of());
    }

    /**
     * Returns whether the advice attempts any trust-boundary mutation.
     *
     * @return {@code true} when any mutation authority is requested
     */
    public boolean requestsTrustBoundaryMutation() {
        return !suggestedTestMutation.isBlank()
                || suggestedCleanup
                || suggestedDestructiveBrowser
                || suggestedCredentialRead;
    }

    /**
     * Returns whether alternate diagnosis disagrees with a primary diagnosis.
     *
     * @param primaryDiagnosis workflow diagnosis
     * @return {@code true} when both are present and differ
     */
    public boolean disagreesWith(String primaryDiagnosis) {
        String primary = Objects.requireNonNullElse(primaryDiagnosis, "").trim();
        return !alternateDiagnosis.isBlank() && !primary.isBlank() && !alternateDiagnosis.equalsIgnoreCase(primary);
    }
}
