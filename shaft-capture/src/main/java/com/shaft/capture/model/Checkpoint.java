package com.shaft.capture.model;

import java.time.Instant;

/**
 * Human-review checkpoint in a capture session.
 *
 * @param id stable checkpoint identifier
 * @param sequence related event sequence
 * @param timestamp checkpoint timestamp
 * @param kind checkpoint kind
 * @param description reviewer-facing description
 */
public record Checkpoint(
        String id,
        long sequence,
        Instant timestamp,
        CheckpointKind kind,
        String description) {
    /**
     * Supported checkpoint kinds.
     */
    public enum CheckpointKind {
        USER_MARKER,
        ASSERTION,
        PAGE_TRANSITION,
        RECOVERY
    }

    /**
     * Creates an immutable checkpoint.
     */
    public Checkpoint {
        id = ModelSupport.requireText(id, "Checkpoint ID");
        if (sequence < 0) {
            throw new IllegalArgumentException("Checkpoint sequence cannot be negative.");
        }
        if (timestamp == null) {
            throw new IllegalArgumentException("Checkpoint timestamp is required.");
        }
        kind = kind == null ? CheckpointKind.USER_MARKER : kind;
        description = ModelSupport.text(description);
    }
}
