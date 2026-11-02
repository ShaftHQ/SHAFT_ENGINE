package com.shaft.doctor.model;

/**
 * Deterministic Doctor result plus a separately identified optional advisory.
 *
 * @param deterministic deterministic analysis result
 * @param advisory optional provider advisory or safe fallback notice
 */
public record DoctorAiAnalysisResult(
        DoctorAnalysisResult deterministic,
        DoctorAdvisory advisory) {
    /**
     * Creates a complete result.
     */
    public DoctorAiAnalysisResult {
        if (deterministic == null || advisory == null) {
            throw new IllegalArgumentException("Deterministic result and advisory are required.");
        }
    }
}
