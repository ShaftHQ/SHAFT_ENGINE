package com.shaft.doctor.model;

import java.util.List;

/**
 * Safe aggregate redaction metadata without original values.
 *
 * @param appliedRules redaction rule names
 * @param removedFieldNames sensitive field names removed
 * @param omittedItems evidence items omitted by policy or limits
 */
public record RedactionSummary(
        List<String> appliedRules,
        List<String> removedFieldNames,
        int omittedItems) {
    /**
     * Creates an immutable summary.
     */
    public RedactionSummary {
        appliedRules = DoctorModelSupport.list(appliedRules).stream().sorted().toList();
        removedFieldNames = DoctorModelSupport.list(removedFieldNames).stream().sorted().toList();
        if (omittedItems < 0) {
            throw new IllegalArgumentException("Omitted item count cannot be negative.");
        }
    }
}
