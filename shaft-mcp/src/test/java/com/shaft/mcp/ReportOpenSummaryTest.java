package com.shaft.mcp;

import com.shaft.doctor.history.ReportSummaryModels;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportOpenSummaryTest {
    @Test
    void reportOpenEmptyStateCitesGenerateTestReport(@TempDir Path workspace) {
        TraceService service = new TraceService(McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());
        ReportSummaryModels.OpenView view = service.reportOpen(null, null, false);
        assertAll(
                () -> assertTrue(view.empty()),
                () -> assertEquals("generate_test_report", view.ctaTool()),
                () -> assertTrue(view.emptyMessage().contains("generate_test_report")));
    }

    @Test
    void reportSummaryReconcilesCountsWithoutSecrets(@TempDir Path workspace) throws Exception {
        Path results = workspace.resolve("target/allure-results");
        Files.createDirectories(results);
        Files.writeString(
                results.resolve("a-result.json"),
                "{\"uuid\":\"a\",\"historyId\":\"h1\",\"name\":\"a\",\"status\":\"passed\",\"start\":1,\"stop\":2}",
                StandardCharsets.UTF_8);
        Files.writeString(
                results.resolve("b-result.json"),
                "{\"uuid\":\"b\",\"historyId\":\"h2\",\"name\":\"b\",\"status\":\"failed\",\"start\":1,\"stop\":2}",
                StandardCharsets.UTF_8);
        TraceService service = new TraceService(McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());
        ReportSummaryModels.SummaryView view = service.reportSummary(
                "target/allure-results", null, null, null, null, null);
        assertAll(
                () -> assertFalse(view.empty()),
                () -> assertEquals(1, view.counts().get("passed")),
                () -> assertEquals(1, view.counts().get("failed")),
                () -> assertTrue(view.engineerSummary().contains("passed=1")),
                () -> assertTrue(view.stakeholderSummary().contains("Decision:")),
                () -> assertFalse(view.engineerSummary().toLowerCase().contains("password")),
                () -> assertFalse(view.stakeholderSummary().toLowerCase().contains("secret")));
    }
}
