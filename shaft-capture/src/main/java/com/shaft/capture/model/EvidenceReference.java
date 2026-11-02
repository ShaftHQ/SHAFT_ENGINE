package com.shaft.capture.model;

/**
 * Safe reference to optional capture evidence.
 *
 * @param id stable evidence identifier
 * @param type evidence category
 * @param relativePath sanitized relative artifact path
 * @param mediaType media type
 * @param sha256 optional content digest
 */
public record EvidenceReference(
        String id,
        EvidenceType type,
        String relativePath,
        String mediaType,
        String sha256) {
    /**
     * Supported evidence categories. Evidence collection remains opt-in.
     */
    public enum EvidenceType {
        SCREENSHOT,
        DOM_SNAPSHOT,
        ACCESSIBILITY_TREE,
        CONSOLE_LOG
    }

    /**
     * Creates an immutable evidence reference.
     */
    public EvidenceReference {
        id = ModelSupport.requireText(id, "Evidence ID");
        type = type == null ? EvidenceType.SCREENSHOT : type;
        relativePath = ModelSupport.relativePath(relativePath, "Evidence path");
        mediaType = ModelSupport.text(mediaType);
        sha256 = ModelSupport.text(sha256);
    }
}
