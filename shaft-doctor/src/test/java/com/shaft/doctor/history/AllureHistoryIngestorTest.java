package com.shaft.doctor.history;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AllureHistoryIngestorTest {
    private static final Path FIXTURES = Path.of("src/test/resources/fixtures/history");

    @Test
    void missingHistoryJsonlIsEmptyStateNotError() {
        AllureHistoryModels.HistoryView view = AllureHistoryIngestor.ingest(
                Path.of("definitely-missing-history.jsonl"),
                null,
                null,
                5);
        assertTrue(view.empty());
        assertFalse(view.emptyMessage().isBlank());
        assertTrue(view.tests().isEmpty());
        assertFalse(view.allureResultsRootReplaced());
        assertTrue(view.accumulateHistoryHonored());
    }

    @Test
    void twoLaunchesAreVisiblePerHistoryIdAndDoctorJoins() {
        AllureHistoryModels.HistoryView view = AllureHistoryIngestor.ingest(
                FIXTURES.resolve("history.jsonl"),
                FIXTURES.resolve("doctor-report.json"),
                FIXTURES.resolve("allure-results"),
                10);
        assertFalse(view.empty());
        assertEquals(2, view.launchCount());
        assertEquals(2, view.tests().size());

        AllureHistoryModels.TestHistorySeries login = view.tests().stream()
                .filter(series -> "hist-login".equals(series.historyId()))
                .findFirst()
                .orElseThrow();
        assertEquals(2, login.launches().size());
        assertEquals("passed", login.launches().get(0).status());
        assertEquals("failed", login.launches().get(1).status());
        assertEquals("HISTORY", login.launches().get(0).kind());
        assertEquals("LOCATOR", login.doctorCause());

        AllureHistoryModels.RetryGroup retries = view.retries().stream()
                .filter(group -> "hist-login".equals(group.historyId()))
                .findFirst()
                .orElseThrow();
        assertEquals(2, retries.attempts().size());
        assertEquals("RETRY", retries.attempts().get(0).kind());
        assertTrue(view.retries().stream().noneMatch(group -> "hist-checkout".equals(group.historyId())));
        assertFalse(view.allureResultsRootReplaced());
    }

    @Test
    void ingestIsAppendOnlyAndNeverReplacesAllureResultsRoot(@TempDir Path temp) throws IOException {
        Path history = temp.resolve("history.jsonl");
        Path results = temp.resolve("allure-results");
        Files.createDirectories(results);
        Path marker = results.resolve("keep-me.txt");
        Files.writeString(marker, "alive");
        Files.writeString(history, Files.readString(FIXTURES.resolve("history.jsonl")));

        AllureHistoryModels.HistoryView first = AllureHistoryIngestor.ingest(history, null, results, 10);
        assertEquals(2, first.launchCount());

        // Append a third launch — ingest must not wipe prior lines or the results root.
        Files.writeString(history, """
                {"uuid":"launch-3","name":"Nightly #3","timestamp":1700000200000,"testResults":{"hist-login":{"name":"login","fullName":"demo.LoginTest.login","status":"failed","duration":100,"historyId":"hist-login"}}}
                """, java.nio.file.StandardOpenOption.APPEND);

        AllureHistoryModels.HistoryView second = AllureHistoryIngestor.ingest(history, null, results, 10);
        assertEquals(3, second.launchCount());
        AllureHistoryModels.TestHistorySeries login = second.tests().stream()
                .filter(series -> "hist-login".equals(series.historyId()))
                .findFirst()
                .orElseThrow();
        assertEquals(3, login.launches().size());
        assertTrue(Files.isDirectory(results));
        assertEquals("alive", Files.readString(marker));
        assertFalse(second.allureResultsRootReplaced());
        assertTrue(Files.readString(history).contains("launch-1"));
        assertTrue(Files.readString(history).contains("launch-3"));
    }

    @Test
    void limitPerHistoryIdTruncatesNewestFirst() {
        AllureHistoryModels.HistoryView view = AllureHistoryIngestor.ingest(
                FIXTURES.resolve("history.jsonl"),
                null,
                null,
                1);
        AllureHistoryModels.TestHistorySeries login = view.tests().stream()
                .filter(series -> "hist-login".equals(series.historyId()))
                .findFirst()
                .orElseThrow();
        assertEquals(1, login.launches().size());
        assertEquals("launch-2", login.launches().get(0).launchUuid());
    }
}
