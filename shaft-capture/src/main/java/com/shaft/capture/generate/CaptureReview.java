package com.shaft.capture.generate;

import java.util.List;

/**
 * Deterministic review of generated Capture output.
 *
 * @param schemaVersion review schema version
 * @param sessionId source Capture session
 * @param readinessScore bounded readiness score from 0 to 100
 * @param blockers generation blockers that must be fixed first
 * @param risks replay or locator risks worth reviewing
 * @param suggestions deterministic next actions
 * @param provider provider identifier, or {@code none} for deterministic review
 */
public record CaptureReview(
        String schemaVersion,
        String sessionId,
        int readinessScore,
        List<String> blockers,
        List<String> risks,
        List<String> suggestions,
        String provider) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable review.
     */
    public CaptureReview {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        sessionId = sessionId == null ? "" : sessionId;
        readinessScore = Math.max(0, Math.min(100, readinessScore));
        blockers = blockers == null ? List.of() : List.copyOf(blockers);
        risks = risks == null ? List.of() : List.copyOf(risks);
        suggestions = suggestions == null ? List.of() : List.copyOf(suggestions);
        provider = provider == null || provider.isBlank() ? "none" : provider;
    }
}
