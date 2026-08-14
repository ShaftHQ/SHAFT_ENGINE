package com.shaft.infrastructure;

import java.time.Instant;
import java.util.List;
import java.util.Objects;

/** Immutable record of successfully completed setup actions. */
public record SetupReceipt(String planDigest, Instant completedAt, List<SetupAction> completedActions) {
    public SetupReceipt {
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest must not be blank.");
        Objects.requireNonNull(completedAt, "completedAt");
        completedActions = List.copyOf(Objects.requireNonNull(completedActions, "completedActions"));
    }
}
