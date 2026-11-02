package com.shaft.coverage.journey;

/**
 * Stable composite coverage target: journey + view + interaction + state (FR-1).
 *
 * @param journey journey identity
 * @param view view identity
 * @param interaction interaction identity
 * @param state state identity
 */
public record CoverageTarget(JourneyId journey, ViewId view, InteractionId interaction, StateId state) {
    /**
     * Creates an immutable coverage target.
     */
    public CoverageTarget {
        if (journey == null) {
            throw new IllegalArgumentException("journey must not be null");
        }
        if (view == null) {
            throw new IllegalArgumentException("view must not be null");
        }
        if (interaction == null) {
            throw new IllegalArgumentException("interaction must not be null");
        }
        if (state == null) {
            throw new IllegalArgumentException("state must not be null");
        }
    }

    /**
     * Compact key used in maps and reports.
     *
     * @return deterministic key
     */
    public String key() {
        return journey.value() + "|" + view.value() + "|" + interaction.value() + "|" + state;
    }

    @Override
    public String toString() {
        return key();
    }
}
