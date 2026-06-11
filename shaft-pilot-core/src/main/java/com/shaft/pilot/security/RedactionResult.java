package com.shaft.pilot.security;

import com.shaft.pilot.ai.EvidenceReference;

import java.util.List;
import java.util.Set;

/**
 * Redacted request content and a safe summary.
 *
 * @param redactedText sanitized primary text
 * @param redactedEvidence sanitized evidence
 * @param appliedRules names of rules that changed content
 * @param removedFields field or attribute names removed without original values
 * @param safeSummary safe summary suitable for audit logs
 */
public record RedactionResult(
        String redactedText,
        List<EvidenceReference> redactedEvidence,
        Set<String> appliedRules,
        Set<String> removedFields,
        String safeSummary) {
    /**
     * Creates an immutable redaction result.
     */
    public RedactionResult {
        redactedEvidence = redactedEvidence == null ? List.of() : List.copyOf(redactedEvidence);
        appliedRules = appliedRules == null ? Set.of() : Set.copyOf(appliedRules);
        removedFields = removedFields == null ? Set.of() : Set.copyOf(removedFields);
        safeSummary = safeSummary == null ? "" : safeSummary;
    }
}
