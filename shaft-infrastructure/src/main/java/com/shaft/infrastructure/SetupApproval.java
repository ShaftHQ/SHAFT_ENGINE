package com.shaft.infrastructure;

import java.time.Instant;
import java.util.Objects;
import java.util.Set;

/** Explicit approval of one exact content-addressed plan. */
public record SetupApproval(String planDigest, Instant approvedAt, Set<String> acceptedLicenses) {
    public SetupApproval {
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest must not be blank.");
        Objects.requireNonNull(approvedAt, "approvedAt");
        acceptedLicenses = Set.copyOf(Objects.requireNonNull(acceptedLicenses, "acceptedLicenses"));
    }
}
