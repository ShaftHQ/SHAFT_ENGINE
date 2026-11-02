package com.shaft.coverage.journey;

/**
 * Stable identity for an interactive element or action (FR-1).
 *
 * @param value normalized interaction identity
 */
public record InteractionId(String value) {
    /**
     * Creates an interaction identity from a raw token.
     *
     * @param value raw interaction token
     */
    public InteractionId {
        value = StableIdentity.normalize(value);
    }

    /**
     * @param raw raw interaction token
     * @return interaction identity
     */
    public static InteractionId of(String raw) {
        return new InteractionId(raw);
    }

    @Override
    public String toString() {
        return value;
    }
}
