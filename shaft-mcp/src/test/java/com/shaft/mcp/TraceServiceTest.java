package com.shaft.mcp;

import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TraceServiceTest {
    @Test
    void latestReturnsClearEmptyResultWhenNoTraceIndexesExist(@TempDir Path temp) {
        var latest = service(temp).traceLatest(5);

        assertTrue(latest.traces().isEmpty());
        assertTrue(latest.warnings().stream().anyMatch(warning -> warning.contains("target/shaft-traces")));
    }

    @Test
    void latestAndReadUsePersistedIndexZipAndBoundRedactedOutput(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "checkout-login", traceJson("""
                "actions": [],
                "timeline": ["token=raw-secret-value"],
                "padding": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                """));

        var latest = service(temp).traceLatest(1);
        var read = service(temp).traceRead(latest.traces().getFirst().tracePath(), 900);

        assertEquals("checkout-login", latest.traces().getFirst().testId());
        assertEquals(relative(temp, index), latest.traces().getFirst().tracePath());
        assertTrue(read.truncated(), read.content());
        assertTrue(read.content().length() <= 900, read.content());
        assertFalse(read.content().contains("raw-secret-value"), read.content());
        assertTrue(read.content().contains("********"), read.content());
    }

    @Test
    void summarizeIdentifiesFailedActionSourceAndOptionalEvidence(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "checkout-pay", traceJson("""
                "actions": [
                  {"id": "action-1", "category": "browser", "name": "NAVIGATE", "status": "passed",
                   "startTime": "2026-06-26T10:00:00Z", "durationMs": 12, "locator": "", "url": "https://shop.example/checkout",
                   "message": "Opened checkout", "exception": {"type": "", "message": ""}, "attachments": [], "metadata": {}},
                  {"id": "action-2", "category": "element", "name": "CLICK", "status": "failed",
                   "startTime": "2026-06-26T10:00:01Z", "durationMs": 450, "locator": "By.id: pay",
                   "url": "https://shop.example/checkout", "message": "Click failed",
                   "exception": {"type": "org.openqa.selenium.NoSuchElementException", "message": "Unable to locate By.id: pay"},
                   "attachments": [], "metadata": {"visibleText": "Pay"}, "actionability": {"displayed": false, "enabled": false}}
                ],
                "locatorHealth": {"enabled": true, "healthy": false},
                "network": [
                  {"url": "https://shop.example/api/pay", "status": 500, "method": "POST"},
                  {"url": "https://shop.example/api/bootstrap", "status": 0, "method": "GET", "failureReason": "net::ERR_FAILED"}
                ],
                "console": [{"level": "SEVERE", "message": "Payment widget failed"}]
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals("CheckoutTest", summary.testClass());
        assertEquals("payShouldFail", summary.testMethod());
        assertEquals("CLICK", summary.failedAction().name());
        assertEquals("By.id: pay", summary.failedAction().locator());
        assertEquals("org.openqa.selenium.NoSuchElementException", summary.failedAction().exceptionType());
        assertEquals("src/test/java/example/CheckoutTest.java", summary.sourceFile());
        assertEquals("42", summary.sourceLine());
        assertTrue(summary.actionability().path("displayed").isBoolean());
        assertTrue(summary.locatorHealth().path("healthy").isBoolean());
        assertTrue(summary.networkFindings().getFirst().contains("500"), summary.networkFindings().toString());
        assertTrue(summary.networkFindings().stream().anyMatch(finding -> finding.contains("net::ERR_FAILED")),
                summary.networkFindings().toString());
        assertTrue(summary.consoleFindings().getFirst().contains("Payment widget failed"), summary.consoleFindings().toString());
    }

    @Test
    void doctorAnalyzeTraceReturnsExistingMcpRemediationShape(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "checkout-pay", traceJson("""
                "actions": [
                  {"id": "action-2", "category": "element", "name": "CLICK", "status": "failed",
                   "startTime": "2026-06-26T10:00:01Z", "durationMs": 450, "locator": "By.cssSelector: #pay",
                   "url": "https://shop.example/checkout", "message": "Click failed",
                   "exception": {"type": "org.openqa.selenium.NoSuchElementException", "message": "Unable to locate By.cssSelector: #pay"},
                   "attachments": [], "metadata": {}, "actionability": {"displayed": false}}
                ],
                "locatorHealth": {"enabled": true, "healthy": false},
                "network": [],
                "console": []
                """));

        var analysis = service(temp).doctorAnalyzeTrace(relative(temp, index), "webdriver");

        assertEquals(CauseCategory.LOCATOR, analysis.primaryCause());
        assertTrue(analysis.diagnosis().summary().contains("CLICK"), analysis.diagnosis().summary());
        assertTrue(analysis.diagnosis().summary().contains("By.cssSelector: #pay"), analysis.diagnosis().summary());
        assertTrue(analysis.codeBlocks().stream().anyMatch(block -> block.id().equals("locator-review")));
        assertTrue(analysis.actions().stream().anyMatch(action -> action.evidenceIds().contains("trace-failed-action")));
    }

    @Test
    void tracePathsMustStayInsideWorkspace(@TempDir Path temp) throws Exception {
        Path outside = Files.createTempFile("outside-trace", ".json");
        Files.writeString(outside, "{}", StandardCharsets.UTF_8);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service(temp).traceRead(outside.toString(), 1000));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void indexFallbackArchiveSymlinkCannotEscapeWorkspace(@TempDir Path temp) throws Exception {
        Path outsideDirectory = Files.createTempDirectory("shaft-mcp-outside-trace");
        Path outsideArchive = outsideDirectory.resolve("shaft-trace.zip");
        Path directory = Files.createDirectories(temp.resolve("target/shaft-traces/escape"));
        Path index = directory.resolve("index.json");
        Path symlink = directory.resolve("shaft-trace.zip");
        try {
            writeZip(outsideArchive, traceJson("""
                    "actions": [],
                    "network": [],
                    "console": []
                    """));
            Files.createSymbolicLink(symlink, outsideArchive);
            Files.writeString(index, """
                    {
                      "testId": "escape",
                      "generatedAt": "2026-06-26T10:00:00Z",
                      "entries": {"json": "shaft-trace.json"}
                    }
                    """, StandardCharsets.UTF_8);

            IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                    () -> service(temp).traceRead(relative(temp, index), 1000));

            assertTrue(failure.getMessage().contains("workspace"));
        } catch (UnsupportedOperationException | SecurityException exception) {
            assumeTrue(false, "Symlink creation is unavailable: " + exception.getMessage());
        } catch (java.io.IOException exception) {
            assumeTrue(false, "Symlink creation failed: " + exception.getMessage());
        } finally {
            Files.deleteIfExists(symlink);
            Files.deleteIfExists(outsideArchive);
            Files.deleteIfExists(outsideDirectory);
        }
    }

    @Test
    void openViewerExtractsHtmlFromTraceZipWhenNotAlreadyOnDisk(@TempDir Path temp) throws Exception {
        Path index = writeTraceWithHtml(temp, "checkout-viewer",
                traceJson("\"actions\": [], \"network\": [], \"console\": []"),
                "<html><body>Trace Viewer</body></html>");

        var result = service(temp).traceOpenViewer(relative(temp, index));

        assertTrue(result.warnings().isEmpty(), result.warnings().toString());
        assertTrue(result.extracted());
        assertFalse(result.viewerPath().isBlank());
        Path extracted = temp.resolve(result.viewerPath());
        assertTrue(Files.isRegularFile(extracted));
        assertTrue(Files.readString(extracted).contains("Trace Viewer"));
    }

    @Test
    void openViewerReusesAlreadyExtractedHtmlWithoutReExtracting(@TempDir Path temp) throws Exception {
        Path index = writeTraceWithHtml(temp, "checkout-cached",
                traceJson("\"actions\": [], \"network\": [], \"console\": []"),
                "<html><body>First</body></html>");
        Path directory = index.getParent();
        Path htmlPath = directory.resolve("SHAFT Trace Report.html");
        Files.writeString(htmlPath, "<html><body>Already extracted</body></html>", StandardCharsets.UTF_8);

        var result = service(temp).traceOpenViewer(relative(temp, index));

        assertFalse(result.extracted(), "Should reuse the already-persisted HTML rather than re-extracting.");
        assertTrue(Files.readString(temp.resolve(result.viewerPath())).contains("Already extracted"));
    }

    @Test
    void openViewerWarnsWhenZipHasNoHtmlEntry(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "checkout-no-html",
                traceJson("\"actions\": [], \"network\": [], \"console\": []"));

        var result = service(temp).traceOpenViewer(relative(temp, index));

        assertTrue(result.viewerPath().isBlank());
        assertTrue(result.warnings().stream().anyMatch(w -> w.contains("does not contain")));
    }

    private static TraceService service(Path root) {
        return new TraceService(McpWorkspacePolicy.of(root), new McpDoctorRemediationService());
    }

    private static Path writeTrace(Path root, String id, String json) throws Exception {
        Path directory = Files.createDirectories(root.resolve("target/shaft-traces").resolve(id));
        Path archive = directory.resolve("shaft-trace.zip");
        writeZip(archive, json);
        Path index = directory.resolve("index.json");
        Files.writeString(index, """
                {
                  "testId": "%s",
                  "generatedAt": "2026-06-26T10:00:00Z",
                  "archive": "%s",
                  "entries": {
                    "html": "SHAFT Trace Report.html",
                    "json": "shaft-trace.json",
                    "network": "shaft-network.har"
                  }
                }
                """.formatted(id, relative(root, archive)), StandardCharsets.UTF_8);
        return index;
    }

    private static Path writeTraceWithHtml(Path root, String id, String json, String html) throws Exception {
        Path directory = Files.createDirectories(root.resolve("target/shaft-traces").resolve(id));
        Path archive = directory.resolve("shaft-trace.zip");
        writeZipWithHtml(archive, json, html);
        Path index = directory.resolve("index.json");
        Files.writeString(index, """
                {
                  "testId": "%s",
                  "generatedAt": "2026-06-26T10:00:00Z",
                  "archive": "%s",
                  "entries": {
                    "html": "SHAFT Trace Report.html",
                    "json": "shaft-trace.json",
                    "network": "shaft-network.har"
                  }
                }
                """.formatted(id, relative(root, archive)), StandardCharsets.UTF_8);
        return index;
    }

    private static void writeZip(Path archive, String json) throws Exception {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("shaft-trace.json"));
            zip.write(json.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
    }

    private static void writeZipWithHtml(Path archive, String json, String html) throws Exception {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("shaft-trace.json"));
            zip.write(json.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write(html.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
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
                    "type": "org.openqa.selenium.NoSuchElementException",
                    "message": "Unable to locate By.id: pay",
                    "stacktrace": "stack"
                  },
                  "source": {
                    "frame": "example.CheckoutTest.payShouldFail(CheckoutTest.java:42)",
                    "file": "src/test/java/example/CheckoutTest.java",
                    "line": "42",
                    "snippet": "> 42: driver.element().click(By.id(\\"pay\\"));"
                  },
                  "snapshot": {"type": "webdriver-page-source", "content": "<button>Pay</button>"},
                  %s,
                  "attachments": []
                }
                """.formatted(extraFields);
    }

    private static String relative(Path root, Path path) {
        return root.toAbsolutePath().normalize()
                .relativize(path.toAbsolutePath().normalize())
                .toString()
                .replace('\\', '/');
    }
}
