package com.shaft.heal.internal;

import java.util.Objects;

/**
 * Timestamped healing outcome record.
 *
 * @param result healing result status
 * @param timestamp UTC timestamp
 */
public record HistoryOutcome(String result, String timestamp) {
    /**
     * Creates an immutable outcome record.
     */
    public HistoryOutcome {
        result = Objects.requireNonNullElse(result, "");
        timestamp = Objects.requireNonNullElse(timestamp, "");
    }
}
