package com.shaft.capture.privacy;

import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.RedactionSummary;

/**
 * Privacy-classified value ready for safe session and data-file handling.
 *
 * @param reference reference stored in the session
 * @param externalizedValue ordinary value written only to external test data, or null for secrets
 * @param summary safe redaction summary
 */
public record ClassifiedValue(
        ExternalTestDataReference reference,
        String externalizedValue,
        RedactionSummary summary) {
    /**
     * Creates a classified value.
     */
    public ClassifiedValue {
        if (reference == null) {
            throw new IllegalArgumentException("Classified value requires a data reference.");
        }
        summary = summary == null ? RedactionSummary.empty() : summary;
        if (reference.classification() == ExternalTestDataReference.DataClassification.SECRET
                && externalizedValue != null) {
            throw new IllegalArgumentException("Secret values cannot be externalized by SHAFT Capture.");
        }
    }

    /**
     * Returns whether an ordinary value should be written to external test data.
     *
     * @return true when a safe external value is present
     */
    public boolean persistable() {
        return externalizedValue != null;
    }
}
