package com.shaft.coverage.journey;

/**
 * Stable identity for a view or page within a journey (FR-1).
 *
 * @param value normalized view identity
 */
public record ViewId(String value) {
    /**
     * Creates a view identity from a raw token.
     *
     * @param value raw view token
     */
    public ViewId {
        value = StableIdentity.normalize(value);
    }

    /**
     * @param raw raw view token
     * @return view identity
     */
    public static ViewId of(String raw) {
        return new ViewId(raw);
    }

    @Override
    public String toString() {
        return value;
    }
}
