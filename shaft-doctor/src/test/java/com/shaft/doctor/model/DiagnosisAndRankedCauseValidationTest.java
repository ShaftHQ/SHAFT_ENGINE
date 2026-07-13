package com.shaft.doctor.model;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the normalization and validation branches of the {@link Diagnosis} and
 * {@link RankedCause} records: null-defaulting of enum fields, null-to-empty list
 * normalization, and {@link RankedCause#trustPercentage()} bounds clamping.
 */
class DiagnosisAndRankedCauseValidationTest {

    @Test
    void nullPrimaryCauseAndConfidenceDefaultToUnknown() {
        Diagnosis diagnosis = new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                null,
                null,
                null,
                "Summary text",
                "Rationale text",
                null,
                null,
                null,
                null);

        assertEquals(CauseCategory.UNKNOWN, diagnosis.primaryCause());
        assertEquals(Confidence.UNKNOWN, diagnosis.confidence());
        assertTrue(diagnosis.contributingCauses().isEmpty());
        assertTrue(diagnosis.findings().isEmpty());
        assertTrue(diagnosis.remediations().isEmpty());
        assertTrue(diagnosis.missingEvidence().isEmpty());
        assertTrue(diagnosis.rankedCauses().isEmpty());
    }

    @Test
    void nonNullPrimaryCauseAndConfidenceArePreserved() {
        Diagnosis diagnosis = new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(CauseCategory.TEST),
                Confidence.HIGH,
                "Summary text",
                "Rationale text",
                List.of(),
                List.of(),
                List.of(),
                List.of());

        assertEquals(CauseCategory.LOCATOR, diagnosis.primaryCause());
        assertEquals(Confidence.HIGH, diagnosis.confidence());
        assertEquals(List.of(CauseCategory.TEST), diagnosis.contributingCauses());
    }

    @Test
    void backwardCompatibleNineArgConstructorDelegatesWithEmptyRankedCauses() {
        Diagnosis diagnosis = new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.TIMING_SYNCHRONIZATION,
                List.of(),
                Confidence.MEDIUM,
                "Summary text",
                "Rationale text",
                List.of(),
                List.of(),
                List.of());

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, diagnosis.primaryCause());
        assertTrue(diagnosis.rankedCauses().isEmpty(),
                "The legacy 9-arg constructor must delegate to an empty ranked-causes list.");
    }

    @Test
    void rankedCauseNullCategoryAndConfidenceDefaultToUnknown() {
        RankedCause cause = new RankedCause(null, 50, null, "Rationale text", null, "Fix prompt text");

        assertEquals(CauseCategory.UNKNOWN, cause.category());
        assertEquals(Confidence.UNKNOWN, cause.confidence());
        assertTrue(cause.evidenceIds().isEmpty());
    }

    @Test
    void rankedCauseTrustPercentageBelowZeroIsClampedToZero() {
        RankedCause cause = new RankedCause(
                CauseCategory.LOCATOR, -10, Confidence.HIGH, "Rationale text", List.of("e-1"), "Fix prompt text");

        assertEquals(0, cause.trustPercentage());
    }

    @Test
    void rankedCauseTrustPercentageAboveHundredIsClampedToHundred() {
        RankedCause cause = new RankedCause(
                CauseCategory.LOCATOR, 250, Confidence.HIGH, "Rationale text", List.of("e-1"), "Fix prompt text");

        assertEquals(100, cause.trustPercentage());
    }

    @Test
    void rankedCauseTrustPercentageWithinBoundsIsPreserved() {
        RankedCause cause = new RankedCause(
                CauseCategory.DATA, 42, Confidence.MEDIUM, "Rationale text", List.of("e-1"), "Fix prompt text");

        assertEquals(42, cause.trustPercentage());
        assertEquals(List.of("e-1"), cause.evidenceIds());
    }
}
