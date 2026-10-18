package com.shaft.mcp;

import com.shaft.doctor.history.ReportSummaryModels;
import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.EnumSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TraceServiceCoverageGapsTest {

    @Test
    void reportOpenUsesWorkspaceHtmlWithoutOpeningABrowser(@TempDir Path workspace) throws Exception {
        Path report = workspace.resolve("target/allure-report/CheckoutAllureReport.html");
        Files.createDirectories(report.getParent());
        Files.writeString(report, "<html><body>Allure</body></html>", StandardCharsets.UTF_8);
        Path results = workspace.resolve("nested/allure-results");
        Files.createDirectories(results);
        Files.writeString(results.resolve("a-result.json"), passedResult("a"), StandardCharsets.UTF_8);
        TraceService service = service(workspace);

        ReportSummaryModels.OpenView explicit = service.reportOpen(
                relative(workspace, report), null, false);
        ReportSummaryModels.OpenView discovered = service.reportOpen("   ", relative(workspace, results), false);

        assertFalse(explicit.empty());
        assertFalse(explicit.opened());
        assertEquals("", explicit.ctaTool());
        assertTrue(explicit.reportPath().endsWith("CheckoutAllureReport.html"), explicit.reportPath());
        assertTrue(explicit.resultsPath().contains("allure-results"), explicit.resultsPath());
        assertFalse(discovered.empty());
        assertFalse(discovered.opened());
        assertTrue(discovered.reportPath().endsWith("CheckoutAllureReport.html"), discovered.reportPath());
        assertEquals("<html><body>Allure</body></html>", Files.readString(report));
    }

    @Test
    void reportSummaryUsesHistoryAndDiscoversResultsOrStaysEmpty(@TempDir Path workspace) throws Exception {
        Path results = workspace.resolve("custom/allure-results");
        Files.createDirectories(results);
        Files.writeString(results.resolve("a-result.json"), passedResult("a"), StandardCharsets.UTF_8);
        Path history = workspace.resolve("notes/history.jsonl");
        Files.createDirectories(history.getParent());
        Files.writeString(history, """
                {"uuid":"launch-1","name":"Nightly #1","timestamp":1700000000000,"testResults":{"hist-login":{"name":"login","fullName":"demo.LoginTest.login","status":"passed","duration":100,"historyId":"hist-login"}}}
                """, StandardCharsets.UTF_8);
        TraceService service = service(workspace);

        ReportSummaryModels.SummaryView view = service.reportSummary(
                null, "   ", relative(workspace, history), null, null, null);
        ReportSummaryModels.SummaryView empty = service(Files.createTempDirectory(workspace, "empty"))
                .reportSummary(null, null, null, null, null, null);

        assertFalse(view.empty());
        assertEquals(1, view.counts().get("passed"));
        assertTrue(view.allureResultsPath().contains("custom/allure-results"), view.allureResultsPath());
        assertTrue(view.engineerSummary().contains("passed=1"), view.engineerSummary());
        assertTrue(view.engineerSummary().contains("Stability:"), view.engineerSummary());
        assertTrue(empty.empty());
        assertTrue(empty.emptyMessage().contains("generate_test_report"), empty.emptyMessage());
    }

    @Test
    void reportMuteRejectsUnknownAndNullObserveAndUnmutes(@TempDir Path workspace) {
        TraceService service = service(workspace);

        IllegalArgumentException unknown = assertThrows(IllegalArgumentException.class,
                () -> service.reportMute("forget", "com.example.FlakyTest#flip", null, null, null, null, null));
        IllegalArgumentException missingPassed = assertThrows(IllegalArgumentException.class,
                () -> service.reportMute("observe", "com.example.FlakyTest#flip", null, null, null, null, null));
        assertTrue(service.reportMute("   ", null, null, null, null, null, null).empty());

        assertFalse(service.reportMute(
                "mute", "com.example.FlakyTest#flip", "timeout flake", "custom-mutes.json", 2, null, false).empty());
        assertTrue(Files.isRegularFile(workspace.resolve("custom-mutes.json")));
        assertTrue(service.reportMute(
                "unmute", "com.example.FlakyTest#flip", null, "custom-mutes.json", null, null, null).empty());

        assertTrue(unknown.getMessage().contains("forget"));
        assertTrue(unknown.getMessage().contains("mute, unmute, list, or observe"));
        assertTrue(missingPassed.getMessage().contains("passed=true|false"));
    }

    @Test
    void traceReadAndSummarizeAcceptJsonZipAndRawFiles(@TempDir Path workspace) throws Exception {
        String json = traceJson("""
                "actions": [
                  {"id": "action-1", "category": "element", "name": "CLICK", "status": "failed",
                   "locator": "", "url": "https://shop.example/checkout", "message": "step failed",
                   "exception": {"type": "org.openqa.selenium.TimeoutException", "message": "Timed out waiting"},
                   "durationMs": 15}
                ],
                "network": [{"url": "https://shop.example/ok", "status": 200, "method": "GET"}],
                "console": [{}, {"level": "", "message": ""}]
                """);
        Path jsonDir = Files.createDirectories(workspace.resolve("traces/json-only"));
        Files.writeString(jsonDir.resolve("shaft-trace.json"), json, StandardCharsets.UTF_8);
        Path zipDir = Files.createDirectories(workspace.resolve("traces/zip-only"));
        writeZip(zipDir.resolve("shaft-trace.zip"), json);
        Path raw = workspace.resolve("notes/raw-trace.json");
        Files.createDirectories(raw.getParent());
        Files.writeString(raw, json, StandardCharsets.UTF_8);
        Path indexDir = Files.createDirectories(workspace.resolve("traces/index-only"));
        Files.writeString(indexDir.resolve("index.json"), """
                {"testId":"orphan","generatedAt":"not-a-date","entries":[]}
                """, StandardCharsets.UTF_8);
        TraceService service = service(workspace);

        assertEquals("CLICK", service.traceSummarize(relative(workspace, jsonDir)).failedAction().name());
        assertEquals("CLICK", service.traceSummarize(relative(workspace, zipDir)).failedAction().name());
        var read = service.traceRead(relative(workspace, raw), 0);
        assertFalse(read.truncated());
        assertTrue(read.content().contains("TimeoutException"), read.content());
        assertTrue(service.traceSummarize(relative(workspace, raw)).recommendations().stream()
                .anyMatch(item -> item.contains("evidence-backed SHAFT wait")));
        IllegalArgumentException missing = assertThrows(IllegalArgumentException.class,
                () -> service.traceSummarize(relative(workspace, indexDir)));
        assertTrue(missing.getMessage().contains("shaft-trace.json"));

        var viewer = service.traceOpenViewer(relative(workspace, jsonDir));
        assertEquals("", viewer.viewerPath());
        assertTrue(viewer.warnings().getFirst().contains("No SHAFT Trace Report.html"));

        Files.createDirectories(workspace.resolve("target/shaft-traces/dated"));
        Files.writeString(workspace.resolve("target/shaft-traces/note.txt"), "not an index", StandardCharsets.UTF_8);
        assertTrue(service.traceLatest(5).warnings().stream()
                .anyMatch(warning -> warning.contains("No SHAFT trace indexes")));
        Files.writeString(workspace.resolve("target/shaft-traces/dated/index.json"), """
                {"testId":"dated","generatedAt":"not-a-date","entries":[]}
                """, StandardCharsets.UTF_8);
        assertEquals("dated", service.traceLatest(5).traces().getFirst().testId());
    }

    @Test
    void traceLatestWarnsWhenTheTraceTreeCannotBeListed(@TempDir Path workspace) throws Exception {
        Path locked = Files.createDirectories(workspace.resolve("target/shaft-traces/locked"));
        Set<PosixFilePermission> original = Files.getPosixFilePermissions(locked);
        Files.setPosixFilePermissions(locked, EnumSet.noneOf(PosixFilePermission.class));
        try {
            var latest = service(workspace).traceLatest(5);
            assertTrue(latest.warnings().stream().anyMatch(warning -> warning.contains("Could not scan")),
                    latest.warnings().toString());
        } finally {
            Files.setPosixFilePermissions(locked, original);
        }
    }

    @Test
    void doctorAnalyzeTraceSeparatesTimeoutAssertAndUnknown(@TempDir Path workspace) throws Exception {
        TraceService service = service(workspace);
        Path timeout = writeRaw(workspace, "timeout.json", actionTrace(
                "CLICK", "org.openqa.selenium.TimeoutException", "Timed out waiting", "step failed"));
        Path assertion = writeRaw(workspace, "assert.json", actionTrace(
                "CHECK", "java.lang.AssertionError", "values differ", "step failed"));
        Path unknown = writeRaw(workspace, "unknown.json", actionTrace(
                "CUSTOM", "java.lang.RuntimeException", "Something unexpected", "step failed"));

        var timeoutReport = service.doctorAnalyzeTrace(relative(workspace, timeout), "playwright");
        var assertReport = service.doctorAnalyzeTrace(relative(workspace, assertion), "webdriver");
        var unknownReport = service.doctorAnalyzeTrace(relative(workspace, unknown), null);

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, timeoutReport.primaryCause());
        assertEquals("Add an evidence-backed wait or state assertion",
                timeoutReport.diagnosis().remediations().getFirst().title());
        assertTrue(timeoutReport.diagnosis().remediations().getFirst().action().contains("smallest SHAFT wait"),
                timeoutReport.diagnosis().remediations().getFirst().action());
        assertTrue(timeoutReport.diagnosis().missingEvidence().stream()
                .anyMatch(item -> item.contains("No source context")));
        assertEquals(CauseCategory.TEST, assertReport.primaryCause());
        assertEquals("Inspect trace evidence before changing test code",
                assertReport.diagnosis().remediations().getFirst().title());
        assertTrue(assertReport.diagnosis().remediations().getFirst().action().contains("Classify whether"),
                assertReport.diagnosis().remediations().getFirst().action());
        assertEquals(CauseCategory.UNKNOWN, unknownReport.primaryCause());
        assertEquals("Inspect trace evidence before changing test code",
                unknownReport.diagnosis().remediations().getFirst().title());
        assertTrue(unknownReport.codeBlocks().stream()
                .anyMatch(block -> "Investigation checklist".equals(block.title())
                        && block.kind() == McpCodeBlock.Kind.INVESTIGATION));
    }

    private static TraceService service(Path root) {
        return new TraceService(McpWorkspacePolicy.of(root), new McpDoctorRemediationService());
    }

    private static String passedResult(String id) {
        return """
                {"uuid":"%s","historyId":"%s","name":"%s","status":"passed","start":1,"stop":2}
                """.formatted(id, id, id);
    }

    private static Path writeRaw(Path root, String name, String json) throws Exception {
        Path path = root.resolve(name);
        Files.writeString(path, json, StandardCharsets.UTF_8);
        return path;
    }

    private static String actionTrace(String name, String type, String exceptionMessage, String message) {
        return traceJson("""
                "actions": [
                  {"id": "action-1", "category": "custom", "name": "%s", "status": "failed",
                   "locator": "", "url": "https://shop.example/checkout", "message": "%s",
                   "exception": {"type": "%s", "message": "%s"}, "durationMs": 10}
                ]
                """.formatted(name, message, type, exceptionMessage));
    }

    private static String traceJson(String extraFields) {
        return """
                {
                  "schemaVersion": "1.0",
                  "generatedAt": "2026-06-26T10:00:00Z",
                  "test": {
                    "className": "CheckoutTest",
                    "methodName": "payShouldFail",
                    "displayName": "CheckoutTest.payShouldFail",
                    "status": "failed"
                  },
                  "exception": {"type": "", "message": ""},
                  "source": {"file": "", "line": "", "snippet": ""},
                  %s,
                  "attachments": []
                }
                """.formatted(extraFields);
    }

    private static void writeZip(Path archive, String json) throws Exception {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("shaft-trace.json"));
            zip.write(json.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
    }

    private static String relative(Path root, Path path) {
        return root.toAbsolutePath().normalize()
                .relativize(path.toAbsolutePath().normalize())
                .toString()
                .replace('\\', '/');
    }
}
