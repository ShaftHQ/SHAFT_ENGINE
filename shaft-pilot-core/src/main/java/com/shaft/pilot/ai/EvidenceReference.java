package com.shaft.pilot.ai;

import java.util.Objects;

/**
 * Textual evidence supplied to a provider after approval and redaction.
 *
 * @param id caller-owned stable identifier
 * @param category evidence category
 * @param mediaType evidence media type
 * @param content evidence content
 */
public record EvidenceReference(String id, EvidenceCategory category, String mediaType, String content) {
    /**
     * Creates an immutable evidence reference.
     */
    public EvidenceReference {
        id = Objects.requireNonNullElse(id, "");
        category = Objects.requireNonNull(category, "category");
        mediaType = Objects.requireNonNullElse(mediaType, "text/plain");
        content = Objects.requireNonNullElse(content, "");
    }
}
