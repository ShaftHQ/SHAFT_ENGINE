package com.shaft.doctor.history;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportSummaryComputerTest {

    @Test
    void emptyResultsYieldGenerateTestReportCta(@TempDir Path workspace) {
        ReportSummaryModels.SummaryView summary = ReportSummaryComputer.compute(
                workspace.resolve("allure-results"),
                null,
                null,
                null,
                null,
                null);
        assertAll(
                () -> assertTrue(summary.empty()),
                () -> assertTrue(summary.emptyMessage().contains("generate_test_report")),
                () -> assertTrue(summary.engineerSummary().contains("generate_test_report")));
    }

    @Test
    void reconcilesFinalAttemptCountsWithoutSecrets(@TempDir Path workspace) throws Exception {
        Path results = workspace.resolve("allure-results");
        Files.createDirectories(results);
        writeResult(results, "a-result.json", "h1", "passed", 10);
        writeResult(results, "b-result.json", "h1", "failed", 20); // newer final for h1
        writeResult(results, "c-result.json", "h2", "passed", 15);
        writeResult(results, "d-result.json", "h3", "broken", 15);

        ReportSummaryModels.SummaryView summary = ReportSummaryComputer.compute(
                results, null, null, null, null, null);

        assertAll(
                () -> assertFalse(summary.empty()),
                () -> assertEquals(1, summary.counts().get("passed")),
                () -> assertEquals(1, summary.counts().get("failed")),
                () -> assertEquals(1, summary.counts().get("broken")),
                () -> assertEquals(3, summary.counts().get("selected")),
                () -> assertTrue(summary.engineerSummary().contains("passed=1")),
                () -> assertTrue(summary.stakeholderSummary().contains("Decision:")),
                () -> assertFalse(summary.engineerSummary().toLowerCase().contains("password")),
                () -> assertFalse(summary.stakeholderSummary().toLowerCase().contains("token")));
    }

    @Test
    void openViewEmptyCta(@TempDir Path workspace) {
        ReportSummaryModels.OpenView open =
                ReportSummaryComputer.openView(workspace.resolve("missing.html"), null, false);
        assertAll(
                () -> assertTrue(open.empty()),
                () -> assertEquals("generate_test_report", open.ctaTool()));
    }

    @Test
    void openViewFindsHtml(@TempDir Path workspace) throws Exception {
        Path report = workspace.resolve("allure-report/AllureReport.html");
        Files.createDirectories(report.getParent());
        Files.writeString(report, "<html></html>", StandardCharsets.UTF_8);
        ReportSummaryModels.OpenView open = ReportSummaryComputer.openView(report, null, true);
        assertAll(
                () -> assertFalse(open.empty()),
                () -> assertTrue(open.opened()),
                () -> assertTrue(open.reportPath().endsWith("AllureReport.html")));
    }

    private static void writeResult(Path dir, String name, String historyId, String status, long stop)
            throws Exception {
        String json = """
                {"uuid":"%s","historyId":"%s","name":"%s","status":"%s","start":%d,"stop":%d}
                """.formatted(name, historyId, name, status, stop - 1, stop);
        Files.writeString(dir.resolve(name), json, StandardCharsets.UTF_8);
    }
}
