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
    void mergeShardsCombinesAllureResultsAndDetectsFlakyClusters(@TempDir Path temp) throws Exception {
        Path shard1 = Files.createDirectories(temp.resolve("shard-blobs/1/allure-results"));
        Files.writeString(shard1.resolve("a-result.json"),
                "{\"fullName\":\"com.example.Test.a\",\"status\":\"passed\",\"start\":0,\"stop\":100}",
                StandardCharsets.UTF_8);
        Path shard2 = Files.createDirectories(temp.resolve("shard-blobs/2/allure-results"));
        Files.writeString(shard2.resolve("b-result.json"),
                "{\"fullName\":\"com.example.Test.a\",\"status\":\"failed\",\"start\":0,\"stop\":150}",
                StandardCharsets.UTF_8);

        var result = service(temp).reportMergeShards(
                java.util.List.of("shard-blobs/1", "shard-blobs/2"), "");

        assertEquals(2, result.shardCount());
        assertEquals(2, result.totalResults());
        assertEquals(1, result.flakyClusters().size());
        assertEquals("com.example.Test.a", result.flakyClusters().getFirst().fullName());
        assertTrue(Files.isDirectory(temp.resolve(result.mergedAllureResultsDirectory())));
        assertTrue(Files.isRegularFile(temp.resolve(result.speedboardHtmlPath())));
    }

    @Test
    void openViewerWarnsWhenZipHasNoHtmlEntry(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "checkout-no-html",
                traceJson("\"actions\": [], \"network\": [], \"console\": []"));

        var result = service(temp).traceOpenViewer(relative(temp, index));

        assertTrue(result.viewerPath().isBlank());
        assertTrue(result.warnings().stream().anyMatch(w -> w.contains("does not contain")));
    }

    @Test
    void traceLatestHandlesZeroMaxResultsWithDefault(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "test-one", traceJson("\"actions\": [], \"network\": [], \"console\": []"));
        Path index2 = writeTrace(temp, "test-two", traceJson("\"actions\": [], \"network\": [], \"console\": []"));
        Path index3 = writeTrace(temp, "test-three", traceJson("\"actions\": [], \"network\": [], \"console\": []"));

        var latest = service(temp).traceLatest(0);

        assertEquals(3, latest.traces().size(), "Zero maxResults should use DEFAULT_LATEST_LIMIT and still return all");
    }

    @Test
    void traceLatestCapedAtMaxLimit(@TempDir Path temp) throws Exception {
        for (int i = 0; i < 60; i++) {
            writeTrace(temp, "test-" + i, traceJson("\"actions\": [], \"network\": [], \"console\": []"));
        }

        var latest = service(temp).traceLatest(100);

        assertTrue(latest.traces().size() <= 50, "Should cap at MAX_LATEST_LIMIT of 50");
    }

    @Test
    void traceReadDefaultsMaxCharactersWhenZero(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "test-chars", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], " +
                "\"padding\": \"" + "x".repeat(25_000) + "\""));

        var result = service(temp).traceRead(relative(temp, index), 0);

        assertTrue(result.content().length() <= 20_000, "Zero maxCharacters should use DEFAULT_MAX_CHARACTERS");
    }

    @Test
    void traceReadCapedAtMaxCharacters(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "test-max", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], " +
                "\"padding\": \"" + "x".repeat(150_000) + "\""));

        var result = service(temp).traceRead(relative(temp, index), 150_000);

        assertTrue(result.content().length() <= 100_000, "Should cap at MAX_CHARACTERS");
        assertTrue(result.truncated());
    }

    @Test
    void traceReadNoTruncationWhenContentShorterThanLimit(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "test-short", traceJson("\"actions\": [], \"network\": [], \"console\": []"));

        var result = service(temp).traceRead(relative(temp, index), 50_000);

        assertFalse(result.truncated());
    }

    @Test
    void traceOpenViewerHandlesZipWithoutShaftTraceJson(@TempDir Path temp) throws Exception {
        Path directory = Files.createDirectories(temp.resolve("target/shaft-traces/no-trace"));
        Path archive = directory.resolve("shaft-trace.zip");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("other.txt"));
            zip.write("content".getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
        Path index = directory.resolve("index.json");
        Files.writeString(index, """
                {
                  "testId": "no-trace",
                  "generatedAt": "2026-06-26T10:00:00Z",
                  "archive": "%s",
                  "entries": {}
                }
                """.formatted(relative(temp, archive)), StandardCharsets.UTF_8);

        var result = service(temp).traceOpenViewer(relative(temp, index));

        assertTrue(result.viewerPath().isBlank());
        assertTrue(result.warnings().stream().anyMatch(w -> w.contains("does not contain")));
    }

    @Test
    void traceFromDirectoryFailsWhenNoValidTraceFound(@TempDir Path temp) throws Exception {
        Path directory = Files.createDirectories(temp.resolve("target/shaft-traces/empty"));

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service(temp).traceSummarize(relative(temp, directory)));

        assertTrue(failure.getMessage().contains("does not contain"));
    }

    @Test
    void failedActionReturnsFirstWhenAllActionsPassed(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "all-passed", traceJson("""
                "actions": [
                  {"id": "action-1", "category": "browser", "name": "NAVIGATE", "status": "passed",
                   "locator": "", "url": "https://example.com", "message": "Opened",
                   "exception": {"type": "", "message": ""}, "attachments": [], "metadata": {}, "durationMs": 10},
                  {"id": "action-2", "category": "element", "name": "CLICK", "status": "passed",
                   "locator": "By.id: submit", "url": "https://example.com", "message": "Clicked",
                   "exception": {"type": "", "message": ""}, "attachments": [], "metadata": {}, "durationMs": 20}
                ]
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals("NAVIGATE", summary.failedAction().name(), "Should return first action when all passed");
    }

    @Test
    void failedActionReturnsEmptyObjectWhenNoActionsArray(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "no-actions-array", traceJson("""
                "actions": "not-an-array"
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertTrue(summary.failedAction().id().isBlank(), "Should return empty action when actions is not an array");
    }

    @Test
    void failedActionReturnsEmptyObjectWhenActionsEmpty(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "empty-actions", traceJson("""
                "actions": []
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertTrue(summary.failedAction().id().isBlank(), "Should return empty action when actions array is empty");
    }

    @Test
    void networkFindingsFiltersStatusCodesAndFailureReasons(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "network-test", traceJson("""
                "actions": [],
                "network": [
                  {"url": "https://example.com/ok", "status": 200, "method": "GET"},
                  {"url": "https://example.com/bad", "status": 404, "method": "GET"},
                  {"url": "https://example.com/error", "status": 500, "method": "POST"},
                  {"url": "https://example.com/failed", "status": 0, "method": "GET", "failureReason": "net::ERR_FAILED"},
                  {"url": "https://example.com/timeout", "status": -1, "method": "GET"}
                ],
                "console": []
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(4, summary.networkFindings().size(), "Should include status codes >= 400, <= 0, and any with failureReason");
        assertTrue(summary.networkFindings().stream().anyMatch(f -> f.contains("404")));
        assertTrue(summary.networkFindings().stream().anyMatch(f -> f.contains("500")));
        assertTrue(summary.networkFindings().stream().anyMatch(f -> f.contains("net::ERR_FAILED")));
    }

    @Test
    void networkFindingsTruncatesAtMaxFindings(@TempDir Path temp) throws Exception {
        StringBuilder network = new StringBuilder("\"actions\": [], \"network\": [");
        for (int i = 0; i < 15; i++) {
            if (i > 0) network.append(",");
            network.append("{\"url\": \"https://example.com/").append(i).append("\", \"status\": 500, \"method\": \"GET\"}");
        }
        network.append("], \"console\": []");

        Path index = writeTrace(temp, "network-max", traceJson(network.toString()));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(10, summary.networkFindings().size(), "Should truncate at MAX_FINDINGS (10)");
    }

    @Test
    void consoleFindingsTruncatesAtMaxFindings(@TempDir Path temp) throws Exception {
        StringBuilder console = new StringBuilder("\"actions\": [], \"network\": [], \"console\": [");
        for (int i = 0; i < 15; i++) {
            if (i > 0) console.append(",");
            console.append("{\"level\": \"ERROR\", \"message\": \"Error ").append(i).append("\"}");
        }
        console.append("]");

        Path index = writeTrace(temp, "console-max", traceJson(console.toString()));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(10, summary.consoleFindings().size(), "Should truncate at MAX_FINDINGS (10)");
    }

    @Test
    void redactionRemovesAuthorizationHeaders(@TempDir Path temp) throws Exception {
        String authContent = "Authorization: Bearer secret-token-123";
        Path index = writeTrace(temp, "auth-test", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], \"timeline\": [\"" + authContent + "\"]"));

        var result = service(temp).traceRead(relative(temp, index), 100_000);

        assertFalse(result.content().contains("secret-token-123"), "Authorization tokens should be redacted");
        assertTrue(result.content().contains("********"), "Redacted content should show mask");
    }

    @Test
    void redactionRemovesCookies(@TempDir Path temp) throws Exception {
        String cookieContent = "Set-Cookie: sessionid=abc123xyz789";
        Path index = writeTrace(temp, "cookie-test", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], \"timeline\": [\"" + cookieContent + "\"]"));

        var result = service(temp).traceRead(relative(temp, index), 100_000);

        assertFalse(result.content().contains("abc123xyz789"), "Cookie values should be redacted");
        assertTrue(result.content().contains("********"), "Redacted content should show mask");
    }

    @Test
    void redactionRemovesAssignmentSecrets(@TempDir Path temp) throws Exception {
        String secretContent = "password=super-secret-123 api_key=key-456 token=tok-789";
        Path index = writeTrace(temp, "assignment-secret-test", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], \"timeline\": [\"" + secretContent + "\"]"));

        var result = service(temp).traceRead(relative(temp, index), 100_000);

        assertFalse(result.content().contains("super-secret-123"), "Assignment secrets should be redacted");
        assertFalse(result.content().contains("key-456"), "Assignment API keys should be redacted");
        assertFalse(result.content().contains("tok-789"), "Assignment tokens should be redacted");
    }

    @Test
    void mergeSharesWithEmptyOutputDirectory(@TempDir Path temp) throws Exception {
        Path shard = Files.createDirectories(temp.resolve("test-shard/allure-results"));
        Files.writeString(shard.resolve("result.json"),
                "{\"fullName\":\"com.test.Test.sample\",\"status\":\"passed\",\"start\":0,\"stop\":100}",
                StandardCharsets.UTF_8);

        var result = service(temp).reportMergeShards(java.util.List.of("test-shard"), "");

        assertTrue(result.mergedAllureResultsDirectory().contains("shaft-merged-report"),
                "Empty outputDirectory should default to target/shaft-merged-report");
    }

    @Test
    void traceSummarizeHandlesMissingNetworkAndConsoleArrays(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "missing-arrays", traceJson("""
                "actions": [],
                "network": "not-an-array",
                "console": null
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertTrue(summary.networkFindings().isEmpty(), "Should handle non-array network");
        assertTrue(summary.consoleFindings().isEmpty(), "Should handle null console");
    }

    @Test
    void traceReadRedactsCredentialsInUrls(@TempDir Path temp) throws Exception {
        String urlWithCreds = "https://user:password@example.com/api";
        Path index = writeTrace(temp, "url-creds", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], \"timeline\": [\"" + urlWithCreds + "\"]"));

        var result = service(temp).traceRead(relative(temp, index), 100_000);

        assertFalse(result.content().contains(":password@"), "URL credentials should be redacted");
        assertTrue(result.content().contains("********"), "Credentials should show mask");
    }

    @Test
    void doctorAnalyzeTraceReturnsAnalysisWithValidCause(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "timeout-fail", traceJson("""
                "actions": [
                  {"id": "action-1", "category": "element", "name": "CLICK", "status": "failed",
                   "locator": "custom-locator", "url": "https://example.com", "message": "Click timed out",
                   "exception": {"type": "org.openqa.selenium.TimeoutException", "message": "Timed out waiting"},
                   "attachments": [], "metadata": {}, "durationMs": 5000}
                ],
                "network": [],
                "console": []
                """));

        var analysis = service(temp).doctorAnalyzeTrace(relative(temp, index), "webdriver");

        assertFalse(analysis.diagnosis().summary().isBlank(), "Diagnosis summary should be populated");
        assertTrue(analysis.primaryCause() != null, "Should return a detected cause category");
    }

    @Test
    void doctorAnalyzeTraceDetectsUnknownIssues(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "unknown-fail", traceJson("""
                "actions": [
                  {"id": "action-1", "category": "custom", "name": "CUSTOM_ACTION", "status": "failed",
                   "locator": "", "url": "https://example.com", "message": "Unknown error occurred",
                   "exception": {"type": "java.lang.RuntimeException", "message": "Something unexpected"},
                   "attachments": [], "metadata": {}, "durationMs": 100}
                ],
                "network": [],
                "console": []
                """));

        var analysis = service(temp).doctorAnalyzeTrace(relative(temp, index), null);

        assertEquals(CauseCategory.UNKNOWN, analysis.primaryCause(),
                "Should default to UNKNOWN for unrecognized patterns");
    }

    @Test
    void doctorAnalyzeTraceDetectsAssertionIssues(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "assert-fail", traceJson("""
                "actions": [
                  {"id": "action-1", "category": "assertion", "name": "ASSERT", "status": "failed",
                   "locator": "", "url": "https://example.com", "message": "Assertion failed",
                   "exception": {"type": "java.lang.AssertionError", "message": "Expected true but was false"},
                   "attachments": [], "metadata": {}, "durationMs": 50}
                ],
                "network": [],
                "console": []
                """));

        var analysis = service(temp).doctorAnalyzeTrace(relative(temp, index), null);

        assertEquals(CauseCategory.TEST, analysis.primaryCause(),
                "Should detect test/assertion failures");
    }

    @Test
    void traceFromDirectoryFallbackThroughArchiveAndIndex(@TempDir Path temp) throws Exception {
        Path directory = Files.createDirectories(temp.resolve("target/shaft-traces/fallback"));
        Path archive = directory.resolve("shaft-trace.zip");
        writeZip(archive, traceJson("\"actions\": [], \"network\": [], \"console\": []"));
        // No index.json, no shaft-trace.json, only archive exists
        // Should find and use shaft-trace.zip via fallback

        var summary = service(temp).traceSummarize(relative(temp, directory));

        assertEquals("CheckoutTest", summary.testClass(), "Should load from archive fallback");
    }

    @Test
    void traceLatestSkipsUnreadableIndexes(@TempDir Path temp) throws Exception {
        Path tracesDir = Files.createDirectories(temp.resolve("target/shaft-traces"));
        Path validIndex = writeTrace(temp, "valid", traceJson("\"actions\": [], \"network\": [], \"console\": []"));

        Path badDir = Files.createDirectories(tracesDir.resolve("bad-index"));
        Path badIndex = badDir.resolve("index.json");
        Files.writeString(badIndex, "{ invalid json content", StandardCharsets.UTF_8);

        var latest = service(temp).traceLatest(10);

        assertEquals(1, latest.traces().size(), "Should skip unreadable indexes and return only valid ones");
        assertTrue(latest.warnings().stream().anyMatch(w -> w.contains("Could not read")),
                "Should warn about unreadable index");
    }

    @Test
    void openViewerDoesNotExtractWhenHtmlAlreadyExists(@TempDir Path temp) throws Exception {
        Path index = writeTraceWithHtml(temp, "already-extracted",
                traceJson("\"actions\": [], \"network\": [], \"console\": []"),
                "<html><body>Original</body></html>");
        Path directory = index.getParent();
        Path htmlPath = directory.resolve("SHAFT Trace Report.html");

        var result1 = service(temp).traceOpenViewer(relative(temp, index));
        assertTrue(result1.extracted(), "First call should extract");

        // Verify it exists and update it
        assertTrue(Files.isRegularFile(htmlPath));
        Files.writeString(htmlPath, "<html><body>Modified</body></html>", StandardCharsets.UTF_8);

        // Second call should not re-extract
        var result2 = service(temp).traceOpenViewer(relative(temp, index));
        assertFalse(result2.extracted(), "Second call should reuse existing");
        assertTrue(Files.readString(temp.resolve(result2.viewerPath())).contains("Modified"),
                "Should keep the modified HTML");
    }

    @Test
    void traceSummarizeExtractsEmptyExceptionFromAction(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "no-exception", traceJson("""
                "actions": [
                  {"id": "a1", "category": "browser", "name": "NAVIGATE", "status": "passed",
                   "locator": "", "url": "https://example.com", "message": "OK",
                   "exception": {"type": "", "message": ""}, "attachments": [], "metadata": {}, "durationMs": 10}
                ]
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertTrue(summary.failedAction().exceptionType().isBlank(), "Should handle empty exception type");
        assertTrue(summary.failedAction().exceptionMessage().isBlank(), "Should handle empty exception message");
    }

    @Test
    void traceSummarizeExtractsDurationMs(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "duration-test", traceJson("""
                "actions": [
                  {"id": "a1", "category": "element", "name": "CLICK", "status": "failed",
                   "locator": "By.id: button", "url": "https://example.com", "message": "Fail",
                   "exception": {"type": "Exception", "message": "Error"}, "attachments": [], "metadata": {}, "durationMs": 3500}
                ]
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(3500L, summary.failedAction().durationMs(), "Should extract duration correctly");
    }

    @Test
    void networkFindingsIncludesFailureReason(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "network-reason", traceJson("""
                "actions": [],
                "network": [
                  {"url": "https://api.example.com/data", "status": 0, "method": "GET", "failureReason": "net::ERR_BLOCKED_BY_CLIENT"}
                ],
                "console": []
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(1, summary.networkFindings().size());
        assertTrue(summary.networkFindings().getFirst().contains("net::ERR_BLOCKED_BY_CLIENT"),
                "Should include network failure reason in findings");
    }

    @Test
    void consoleFindingsIncludesLevelAndMessage(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "console-detail", traceJson("""
                "actions": [],
                "network": [],
                "console": [
                  {"level": "WARNING", "message": "Deprecation warning"}
                ]
                """));

        var summary = service(temp).traceSummarize(relative(temp, index));

        assertEquals(1, summary.consoleFindings().size());
        assertTrue(summary.consoleFindings().getFirst().contains("WARNING"));
        assertTrue(summary.consoleFindings().getFirst().contains("Deprecation warning"),
                "Should include both level and message");
    }

    @Test
    void traceReadIncludesWarningsWhenTruncated(@TempDir Path temp) throws Exception {
        Path index = writeTrace(temp, "test-truncated", traceJson(
                "\"actions\": [], \"network\": [], \"console\": [], " +
                "\"padding\": \"" + "x".repeat(25_000) + "\""));

        var result = service(temp).traceRead(relative(temp, index), 1000);

        assertTrue(result.truncated());
        assertTrue(result.warnings().stream().anyMatch(w -> w.contains("truncated")),
                "Should include truncation warning");
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
