package com.shaft.doctor.model;

/**
 * Portable provenance for one evidence item.
 *
 * @param adapter stable collector adapter name
 * @param sourceReference sanitized path or source label
 * @param originalSha256 checksum of approved source bytes after redaction and before bundle formatting
 */
public record EvidenceProvenance(
        String adapter,
        String sourceReference,
        String originalSha256) {
    /**
     * Creates validated provenance.
     */
    public EvidenceProvenance {
        adapter = DoctorModelSupport.requireText(adapter, "Provenance adapter");
        sourceReference = DoctorModelSupport.relativePath(sourceReference, "Source reference");
        originalSha256 = DoctorModelSupport.text(originalSha256);
    }
}
