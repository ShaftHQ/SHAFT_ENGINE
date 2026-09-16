package com.shaft.coverage.journey;

import java.time.Instant;
import java.util.Objects;

/**
 * One piece of observed execution evidence for a coverage target (FR-2).
 *
 * @param target observed coverage target
 * @param evidenceId opaque evidence reference (test name, capture event id, etc.)
 * @param observedAt observation timestamp
 */
public record ExecutionObservation(CoverageTarget target, String evidenceId, Instant observedAt) {
    /**
     * Creates an observation.
     */
    public ExecutionObservation {
        if (target == null) {
            throw new IllegalArgumentException("target must not be null");
        }
        if (evidenceId == null || evidenceId.isBlank()) {
            throw new IllegalArgumentException("evidenceId must not be blank");
        }
        observedAt = Objects.requireNonNullElseGet(observedAt, Instant::now);
    }
}
