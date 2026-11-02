package com.shaft.coverage.journey;

import java.util.Locale;

/**
 * Stable identity for an interactive UI state (FR-1).
 *
 * @param kind state classification
 * @param value normalized state label (defaults to the kind name when blank)
 */
public record StateId(StateKind kind, String value) {
    /**
     * Creates a state identity.
     *
     * @param kind state classification
     * @param value raw state label
     */
    public StateId {
        if (kind == null) {
            throw new IllegalArgumentException("kind must not be null");
        }
        if (value == null || value.isBlank()) {
            value = kind.name().toLowerCase(Locale.ROOT);
        } else {
            value = StableIdentity.normalize(value);
        }
    }

    /**
     * @param kind state classification
     * @return state identity using the kind name as the label
     */
    public static StateId of(StateKind kind) {
        return new StateId(kind, kind.name());
    }

    /**
     * @param kind state classification
     * @param raw raw state label
     * @return state identity
     */
    public static StateId of(StateKind kind, String raw) {
        return new StateId(kind, raw);
    }

    @Override
    public String toString() {
        return kind.name().toLowerCase(Locale.ROOT) + ":" + value;
    }
}
