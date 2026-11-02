package com.shaft.doctor.history;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DualFlakeComputerTest {
    private static final Path FIXTURES = Path.of("src/test/resources/fixtures/flake");

    @Test
    void retryHiddenFixtureTaggedRetryHiddenNotTransitions() {
        AllureHistoryModels.HistoryView history = AllureHistoryIngestor.ingest(
                FIXTURES.resolve("retry-hidden/history.jsonl"),
                null,
                FIXTURES.resolve("retry-hidden/allure-results"),
                10);
        FlakeModels.FlakeTable table = DualFlakeComputer.compute(history);
        FlakeModels.FlakeRow row = only(table, "hist-retry-only");

        assertTrue(row.retryHidden());
        assertEquals("retry-hidden", row.retryHiddenTag());
        assertTrue(row.tags().contains("retry-hidden"));
        assertFalse(row.tags().contains("transitions"));
        assertEquals("always-passing", row.transitionAssessment());
        assertNotEquals("retry-hidden", row.transitionAssessment());
    }

    @Test
    void transitionsFixtureTaggedTransitionsNotRetryHidden() {
        AllureHistoryModels.HistoryView history = AllureHistoryIngestor.ingest(
                FIXTURES.resolve("transitions/history.jsonl"),
                null,
                null,
                10);
        FlakeModels.FlakeTable table = DualFlakeComputer.compute(history);
        FlakeModels.FlakeRow row = only(table, "hist-transitions");

        assertFalse(row.retryHidden());
        assertEquals("", row.retryHiddenTag());
        assertEquals("transitions", row.transitionAssessment());
        assertTrue(row.transitionCount() != null && row.transitionCount() >= 3);
        assertTrue(row.tags().contains("transitions"));
        assertFalse(row.tags().contains("retry-hidden"));
    }

    @Test
    void twoFixtureTypesProduceTwoDifferentTags() {
        FlakeModels.FlakeRow retryHidden = only(DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("retry-hidden/history.jsonl"),
                null,
                FIXTURES.resolve("retry-hidden/allure-results"),
                10)), "hist-retry-only");
        FlakeModels.FlakeRow transitions = only(DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("transitions/history.jsonl"),
                null,
                null,
                10)), "hist-transitions");

        assertEquals(List.of("retry-hidden"), retryHidden.tags());
        assertEquals(List.of("transitions"), transitions.tags());
        assertNotEquals(retryHidden.tags(), transitions.tags());
    }

    @Test
    void alwaysFailingIsNotFlaky() {
        FlakeModels.FlakeRow row = only(DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("always-failing/history.jsonl"),
                null,
                null,
                10)), "hist-always-fail");
        assertEquals("always-failing", row.transitionAssessment());
        assertFalse(row.tags().contains("transitions"));
        assertFalse(row.retryHidden());
    }

    @Test
    void insufficientHistoryIsUnknownNotZero() {
        FlakeModels.FlakeRow row = only(DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("insufficient/history.jsonl"),
                null,
                null,
                10)), "hist-insufficient");
        assertEquals("unknown", row.transitionAssessment());
        assertNull(row.transitionCount());
        assertFalse(row.tags().contains("transitions"));
    }

    @Test
    void noCombinedScoreIsEmittedAsOnlyDisplay() {
        FlakeModels.FlakeTable table = DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("transitions/history.jsonl"),
                null,
                null,
                10));
        FlakeModels.FlakeRow row = only(table, "hist-transitions");
        // Dual columns exist independently; tags never collapse to a single percentage key.
        assertTrue(row.retryHiddenTag() != null);
        assertTrue(row.transitionAssessment() != null);
        assertFalse(row.tags().stream().anyMatch(tag -> tag.contains("%") || tag.contains("score")));
        assertFalse(table.rows().isEmpty());
    }

    @Test
    void sameShaIsOptionalWhenCiMetadataAbsent() {
        FlakeModels.FlakeRow row = only(DualFlakeComputer.compute(AllureHistoryIngestor.ingest(
                FIXTURES.resolve("transitions/history.jsonl"),
                null,
                null,
                10)), "hist-transitions");
        assertFalse(row.sameShaAvailable());
        assertNull(row.sameShaTransitionCount());
    }

    private static FlakeModels.FlakeRow only(FlakeModels.FlakeTable table, String historyId) {
        assertFalse(table.empty());
        return table.rows().stream()
                .filter(row -> historyId.equals(row.historyId()))
                .findFirst()
                .orElseThrow();
    }
}
