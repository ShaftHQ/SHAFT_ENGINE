package com.shaft.capture.model;

import java.util.Set;

/**
 * Safe aggregate of privacy decisions without removed values.
 *
 * @param redactedValueCount number of values replaced by references
 * @param removedAttributeCount number of sensitive attributes removed
 * @param redactedUrlParameterCount number of URL parameters redacted
 * @param appliedRules names of deterministic rules that were applied
 */
public record RedactionSummary(
        int redactedValueCount,
        int removedAttributeCount,
        int redactedUrlParameterCount,
        Set<String> appliedRules) {
    /**
     * Creates a safe immutable summary.
     */
    public RedactionSummary {
        if (redactedValueCount < 0 || removedAttributeCount < 0 || redactedUrlParameterCount < 0) {
            throw new IllegalArgumentException("Redaction counts cannot be negative.");
        }
        appliedRules = appliedRules == null
                ? Set.of()
                : java.util.Collections.unmodifiableSet(new java.util.TreeSet<>(appliedRules));
    }

    /**
     * Returns an empty summary.
     *
     * @return empty summary
     */
    public static RedactionSummary empty() {
        return new RedactionSummary(0, 0, 0, Set.of());
    }

    /**
     * Combines two safe summaries.
     *
     * @param other summary to add
     * @return combined summary
     */
    public RedactionSummary merge(RedactionSummary other) {
        if (other == null) {
            return this;
        }
        java.util.Set<String> rules = new java.util.TreeSet<>(appliedRules);
        rules.addAll(other.appliedRules);
        return new RedactionSummary(
                redactedValueCount + other.redactedValueCount,
                removedAttributeCount + other.removedAttributeCount,
                redactedUrlParameterCount + other.redactedUrlParameterCount,
                rules);
    }
}
