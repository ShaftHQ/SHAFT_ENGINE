package com.shaft.doctor.history;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SmartTagsComputerTest {
    private static final Path FIXTURES = Path.of("src/test/resources/fixtures/smart-tags");

    @Test
    void regressedFixtureIsRegressed() {
        SmartTagModels.SmartTagRow row = only("regressed", "hist-regressed");
        assertEquals(SmartTagModels.TAG_REGRESSED, row.primaryTag());
        assertTrue(row.tags().contains(SmartTagModels.TAG_REGRESSED));
        assertFalse(row.tags().contains(SmartTagModels.TAG_NEW));
        assertFalse(row.tags().contains(SmartTagModels.TAG_ALWAYS_FAILING));
        assertFalse(row.tags().contains(SmartTagModels.TAG_FLAKY));
    }

    @Test
    void firstSeenFixtureIsNewNotRegressed() {
        SmartTagModels.SmartTagRow row = only("new", "hist-new");
        assertEquals(SmartTagModels.TAG_NEW, row.primaryTag());
        assertTrue(row.tags().contains(SmartTagModels.TAG_NEW));
        assertFalse(row.tags().contains(SmartTagModels.TAG_REGRESSED));
        assertFalse(row.tags().contains(SmartTagModels.TAG_FLAKY));
    }

    @Test
    void alwaysFailingFixtureIsAlwaysFailingNotFlaky() {
        SmartTagModels.SmartTagRow row = only("always-failing", "hist-always-fail");
        assertEquals(SmartTagModels.TAG_ALWAYS_FAILING, row.primaryTag());
        assertTrue(row.tags().contains(SmartTagModels.TAG_ALWAYS_FAILING));
        assertFalse(row.tags().contains(SmartTagModels.TAG_FLAKY));
        assertFalse(row.tags().contains(SmartTagModels.TAG_NEW));
    }

    @Test
    void flakyFixtureIsFlaky() {
        SmartTagModels.SmartTagRow row = only("flaky", "hist-flaky");
        assertTrue(row.tags().contains(SmartTagModels.TAG_FLAKY));
        assertEquals(SmartTagModels.TAG_FLAKY, row.primaryTag());
        assertFalse(row.tags().contains(SmartTagModels.TAG_REGRESSED));
        assertFalse(row.tags().contains(SmartTagModels.TAG_NEW));
    }

    @Test
    void fixedFixtureIsFixed() {
        SmartTagModels.SmartTagRow row = only("fixed", "hist-fixed");
        assertEquals(SmartTagModels.TAG_FIXED, row.primaryTag());
        assertTrue(row.tags().contains(SmartTagModels.TAG_FIXED));
    }

    @Test
    void insufficientHistoryDoesNotInventFlaky() {
        SmartTagModels.SmartTagRow row = only("insufficient", "hist-insufficient");
        assertFalse(row.tags().contains(SmartTagModels.TAG_FLAKY));
        assertEquals(SmartTagModels.TAG_NEW, row.primaryTag());
    }

    @Test
    void durationAnomalyOptionalWhenTimingsPresent() {
        SmartTagModels.SmartTagRow row = only("duration-anomaly", "hist-duration");
        assertTrue(row.durationAnomaly());
        assertTrue(row.tags().contains(SmartTagModels.TAG_DURATION_ANOMALY));
    }

    @Test
    void unknownHistoryEmptyTable() {
        SmartTagModels.SmartTagTable table = SmartTagsComputer.compute(
                AllureHistoryIngestor.ingest(Path.of("missing-history.jsonl"), null, null, 10));
        assertTrue(table.empty());
        assertTrue(table.rows().isEmpty());
    }

    private static SmartTagModels.SmartTagRow only(String fixture, String historyId) {
        AllureHistoryModels.HistoryView history = AllureHistoryIngestor.ingest(
                FIXTURES.resolve(fixture).resolve("history.jsonl"), null, null, 10);
        SmartTagModels.SmartTagTable table = SmartTagsComputer.compute(history);
        assertFalse(table.empty());
        return table.rows().stream()
                .filter(row -> historyId.equals(row.historyId()))
                .findFirst()
                .orElseThrow();
    }
}
