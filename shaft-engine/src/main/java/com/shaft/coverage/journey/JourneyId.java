package com.shaft.coverage.journey;

/**
 * Stable identity for a user journey (FR-1).
 *
 * @param value normalized journey identity
 */
public record JourneyId(String value) {
    /**
     * Creates a journey identity from a raw token.
     *
     * @param value raw journey token
     */
    public JourneyId {
        value = StableIdentity.normalize(value);
    }

    /**
     * @param raw raw journey token
     * @return journey identity
     */
    public static JourneyId of(String raw) {
        return new JourneyId(raw);
    }

    @Override
    public String toString() {
        return value;
    }
}
