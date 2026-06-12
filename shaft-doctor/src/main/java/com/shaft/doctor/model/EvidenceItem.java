package com.shaft.doctor.model;

import java.util.Map;

/**
 * One sanitized, bounded, portable evidence item.
 *
 * @param id stable content-derived identifier
 * @param category evidence category
 * @param mediaType normalized media type
 * @param relativePath optional path inside the evidence bundle
 * @param sha256 checksum of sanitized retained content
 * @param sizeBytes retained byte count
 * @param content optional UTF-8 textual content
 * @param redacted whether sensitive content was removed
 * @param truncated whether an artifact size limit was applied
 * @param attributes safe normalized metadata
 * @param provenance source provenance
 */
public record EvidenceItem(
        String id,
        EvidenceCategory category,
        String mediaType,
        String relativePath,
        String sha256,
        long sizeBytes,
        String content,
        boolean redacted,
        boolean truncated,
        Map<String, String> attributes,
        EvidenceProvenance provenance) {
    /**
     * Creates validated immutable evidence.
     */
    public EvidenceItem {
        id = DoctorModelSupport.requireText(id, "Evidence ID");
        category = category == null ? EvidenceCategory.OTHER : category;
        mediaType = DoctorModelSupport.requireText(mediaType, "Evidence media type");
        relativePath = DoctorModelSupport.relativePath(relativePath, "Evidence path");
        sha256 = DoctorModelSupport.requireText(sha256, "Evidence checksum");
        if (sizeBytes < 0) {
            throw new IllegalArgumentException("Evidence size cannot be negative.");
        }
        content = content == null ? null : content;
        attributes = DoctorModelSupport.strings(attributes);
        if (provenance == null) {
            throw new IllegalArgumentException("Evidence provenance is required.");
        }
    }
}
