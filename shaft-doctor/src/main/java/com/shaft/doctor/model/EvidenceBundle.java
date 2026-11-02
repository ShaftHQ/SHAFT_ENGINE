package com.shaft.doctor.model;

import java.util.List;
import java.util.Map;

/**
 * Versioned portable evidence bundle.
 *
 * @param schemaVersion evidence schema version
 * @param bundleId stable content-derived identifier
 * @param evidence retained evidence in deterministic order
 * @param redaction safe redaction summary
 * @param metadata portable environment-independent metadata
 */
public record EvidenceBundle(
        String schemaVersion,
        String bundleId,
        List<EvidenceItem> evidence,
        RedactionSummary redaction,
        Map<String, String> metadata) {
    /**
     * Current evidence-bundle schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates a validated immutable bundle.
     */
    public EvidenceBundle {
        schemaVersion = DoctorModelSupport.requireText(schemaVersion, "Evidence schema version");
        bundleId = DoctorModelSupport.requireText(bundleId, "Bundle ID");
        evidence = DoctorModelSupport.list(evidence);
        if (evidence.stream().map(EvidenceItem::id).distinct().count() != evidence.size()) {
            throw new IllegalArgumentException("Evidence IDs must be unique within a bundle.");
        }
        if (redaction == null) {
            redaction = new RedactionSummary(List.of(), List.of(), 0);
        }
        metadata = DoctorModelSupport.strings(metadata);
    }
}
