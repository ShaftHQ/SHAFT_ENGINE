package com.shaft.mcp;

import com.shaft.doctor.history.LocalMuteModels;
import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TraceReportOfflineTest {
    @TempDir
    Path temp;

    @Test
    void reportOpenStaysClosedWhenTheBrowserIsNotRequested() throws Exception {
        Path report = temp.resolve("target/allure-report/index.html");
        Files.createDirectories(report.getParent());
        Files.writeString(report, "<html><title>Allure</title><body>results</body></html>");
        TraceService service = service();

        var view = service.reportOpen(relative(report), null, false);

        assertFalse(view.empty());
        assertFalse(view.opened());
        assertTrue(view.reportPath().endsWith("index.html"));
        assertTrue(view.ctaTool().isBlank());
    }

    @Test
    void reportMuteRejectsUnknownActionsAndClearsAMute() {
        TraceService service = service();
        IllegalArgumentException unknown = assertThrows(IllegalArgumentException.class,
                () -> service.reportMute("rewind", "com.example.Flaky#test", "reason", null, null, null, null));
        assertTrue(unknown.getMessage().contains("Unknown report_mute action"));
        IllegalArgumentException missingOutcome = assertThrows(IllegalArgumentException.class,
                () -> service.reportMute("observe", "com.example.Flaky#test", null, null, null, null, null));
        assertTrue(missingOutcome.getMessage().contains("passed=true|false"));

        service.reportMute("quarantine", "com.example.Flaky#test", "local timeout", null, 3, null, false);
        assertEquals(1, service.reportMute(null, null, null, null, null, null, null).entries().size());
        assertTrue(service.reportMute("unmute", "com.example.Flaky#test", null, null, null, null, null).empty());
    }

    @Test
    void timingFailureIsNotClassifiedAsALocatorWhenTheLocatorTextIsBlank() throws Exception {
        Path trace = temp.resolve("target/shaft-traces/timing.json");
        Files.createDirectories(trace.getParent());
        Files.writeString(trace, traceJson("""
                "actions": [
                  {"id": "action-1", "category": "element", "name": "CLICK", "status": "failed",
                   "locator": "", "url": "https://example.com", "message": "element was not clickable",
                   "exception": {"type": "org.openqa.selenium.TimeoutException", "message": "Timed out waiting"},
                   "attachments": [], "metadata": {}, "durationMs": 5000}
                ],
                "network": [],
                "console": []
                """), StandardCharsets.UTF_8);

        var analysis = service().doctorAnalyzeTrace(relative(trace), "playwright");

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, analysis.primaryCause());
        assertTrue(analysis.diagnosis().summary().contains("CLICK"));
        assertTrue(analysis.diagnosis().remediations().getFirst().title().toLowerCase().contains("wait")
                || analysis.diagnosis().remediations().getFirst().action().toLowerCase().contains("wait"));
    }

    @Test
    void directoryWithOnlyTraceJsonAndALooseJsonFileBothSummarize() throws Exception {
        Path directory = Files.createDirectories(temp.resolve("target/shaft-traces/json-only"));
        Files.writeString(directory.resolve("shaft-trace.json"), traceJson("""
                "actions": [], "network": [], "console": []
                """), StandardCharsets.UTF_8);
        Path loose = temp.resolve("target/shaft-traces/loose.json");
        Files.writeString(loose, traceJson("""
                "actions": [], "network": [], "console": []
                """), StandardCharsets.UTF_8);
        TraceService service = service();

        assertEquals("CheckoutTest", service.traceSummarize(relative(directory)).testClass());
        assertEquals("CheckoutTest", service.traceSummarize(relative(loose)).testClass());
    }

    private TraceService service() {
        return new TraceService(McpWorkspacePolicy.of(temp), new McpDoctorRemediationService());
    }

    private String relative(Path path) {
        return temp.toAbsolutePath().normalize()
                .relativize(path.toAbsolutePath().normalize())
                .toString()
                .replace('\\', '/');
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
                    "description": "checkout payment",
                    "status": "failed"
                  },
                  "exception": {
                    "type": "",
                    "message": "",
                    "stacktrace": ""
                  },
                  "source": {
                    "frame": "example.CheckoutTest.payShouldFail(CheckoutTest.java:42)",
                    "file": "src/test/java/example/CheckoutTest.java",
                    "line": "42",
                    "snippet": "> 42: driver.element().click(pay);"
                  },
                  %s,
                  "attachments": []
                }
                """.formatted(extraFields);
    }
}
