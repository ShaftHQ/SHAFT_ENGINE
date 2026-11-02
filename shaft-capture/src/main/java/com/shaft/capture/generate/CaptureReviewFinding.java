package com.shaft.capture.generate;

import java.util.List;

/**
 * One deterministic generated-code review finding.
 *
 * @param id stable finding identifier
 * @param category review category
 * @param severity review severity
 * @param summary safe finding summary
 * @param evidenceIds related capture events or trace actions
 * @param recommendation deterministic remediation
 */
public record CaptureReviewFinding(
        String id,
        String category,
        String severity,
        String summary,
        List<String> evidenceIds,
        String recommendation) {
    /**
     * Creates an immutable review finding.
     */
    public CaptureReviewFinding {
        id = text(id);
        category = text(category);
        severity = text(severity).isBlank() ? "WARNING" : text(severity);
        summary = text(summary);
        evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
        recommendation = text(recommendation);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
