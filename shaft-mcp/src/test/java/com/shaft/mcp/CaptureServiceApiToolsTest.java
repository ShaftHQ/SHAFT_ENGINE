package com.shaft.mcp;

import com.shaft.capture.control.CaptureControlFiles;
import com.shaft.capture.control.CaptureControlServer;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;
import com.shaft.capture.runtime.NetworkCaptureOptions;
import com.shaft.capture.runtime.NetworkTransaction;
import com.shaft.capture.storage.NetworkBodyStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureServiceApiToolsTest {
    @TempDir
    Path temp;

    @Test
    void captureStatusIncludesNetworkTransactionCountField() {
        CaptureStatus status = CaptureStatus.notRunning();

        assertTrue(status.networkTransactionCount() >= 0,
                "networkTransactionCount should be non-negative");
        assertNotNull(status.lastEndpoints(),
                "lastEndpoints should not be null");
        assertTrue(status.lastEndpoints().isEmpty(),
                "lastEndpoints should be empty for not-running status");
    }

    @Test
    void networkTransactionRecordHasRequiredFields() {
        NetworkTransaction tx = new NetworkTransaction(
                "tx-123",
                "GET",
                "https://api.example.com/v1/users",
                200,
                "xhr",
                150L,
                java.util.Map.of(),
                List.of(1, 2, 3));

        assertEquals("tx-123", tx.transactionId());
        assertEquals("GET", tx.method());
        assertEquals("https://api.example.com/v1/users", tx.url());
        assertEquals(200, tx.statusCode());
        assertEquals("xhr", tx.resourceKind());
        assertEquals(150L, tx.timingMillis());
        assertNotNull(tx.bodyRefMetadata());
        assertNotNull(tx.correlatedUiSequence());
        assertEquals(3, tx.correlatedUiSequence().size());
    }

    @Test
    void networkCaptureOptionsCanBeCreatedAndConfigured() {
        NetworkCaptureOptions options = new NetworkCaptureOptions();
        assertTrue(options.enabled);
        assertFalse(options.excludeAssets);
        assertTrue(options.excludePattern.isEmpty());
        assertTrue(options.includePattern.isEmpty());
        assertFalse(options.captureResponseBodies);
        assertFalse(options.captureRequestBodies);

        // Verify mutable fields can be changed
        options.enabled = false;
        options.excludeAssets = true;
        options.excludePattern = "*.jpg|*.png";
        options.captureResponseBodies = true;

        assertFalse(options.enabled);
        assertTrue(options.excludeAssets);
        assertEquals("*.jpg|*.png", options.excludePattern);
        assertTrue(options.captureResponseBodies);
    }

    @Test
    void apiToolsExistsAndAreCallable() {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());

        assertNotNull(service);
        // Verify tools exist by checking they don't throw on idle status
        CaptureStatus status = service.apiStatus();
        assertEquals(CaptureStatus.State.NOT_RUNNING, status.state());

        List<NetworkTransaction> transactions = service.apiTransactions(false, "");
        assertTrue(transactions.isEmpty());

        service.close();
    }

    @Test
    void apiStartRejectsSecondConcurrentSessionViaTheSharedSingleSessionLock() {
        CaptureManager manager = new CaptureManager();
        CaptureService service = new CaptureService(
                manager,
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            manager.start(new CaptureStartRequest(
                    "https://example.test",
                    CaptureBrowser.CHROME,
                    temp.resolve("first.json"),
                    temp.resolve("runtime"),
                    true));

            IllegalStateException failure = assertThrows(IllegalStateException.class,
                    () -> service.apiStart("https://example.test", "chrome", true, new NetworkCaptureOptions()));

            assertTrue(failure.getMessage().contains("already active"),
                    "capture_api_start must reject a second concurrent session the same way capture_start does");
        } finally {
            manager.close();
            service.close();
        }
    }

    @Test
    void resolveHeadlessDefaultsToHeadlessPerRepoPolicyWhenUnspecified() {
        assertTrue(CaptureService.resolveHeadless(null),
                "capture_api_start must default to headless execution when headless is unspecified");
        assertTrue(CaptureService.resolveHeadless(true));
        assertFalse(CaptureService.resolveHeadless(false),
                "capture_api_start must honor an explicit request for a visible window");
    }

    @Test
    void apiStartThreadsApiCaptureAndNetworkOptionsWithoutThrowingBeforeBrowserLaunch() {
        // Blank/null target URLs now normalize to about:blank instead of being rejected
        // (CaptureStartRequest#validateUrl, #3673/#3682), so an invalid-scheme URL is used
        // here instead: it still fails CaptureStartRequest validation synchronously, before
        // any browser is touched, so this proves apiStart wires apiCapture=true and
        // networkOptions/headless resolution through startWithOptions without a NullPointerException,
        // for both the unspecified-options and explicit-options cases.
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            IllegalArgumentException unspecified = assertThrows(IllegalArgumentException.class,
                    () -> service.apiStart("ftp://example.test", "chrome", null, null));
            assertTrue(unspecified.getMessage().contains("target URL"));

            IllegalArgumentException explicit = assertThrows(IllegalArgumentException.class,
                    () -> service.apiStart("ftp://example.test", "chrome", false, new NetworkCaptureOptions()));
            assertTrue(explicit.getMessage().contains("target URL"));
        } finally {
            service.close();
        }
    }

    @Test
    void apiTransactionsFilterOverloadIsIdleSafeRegardlessOfRequestedFilters() {
        // The populated/filtered case (asset exclusion, exclude-pattern, and bound) is covered
        // end to end by CaptureManagerNetworkTransactionsTest in shaft-capture, which can inject
        // a fake recorder with real transaction data. Here we confirm the idle path the MCP tool
        // sees before any session starts is empty and bounded no matter what filters are asked for.
        CaptureManager manager = new CaptureManager();

        NetworkCaptureOptions excludeAssets = new NetworkCaptureOptions();
        excludeAssets.excludeAssets = true;
        excludeAssets.excludePattern = "*/admin/*";
        assertTrue(manager.networkTransactions(excludeAssets, 100).isEmpty());

        manager.close();
    }

    @Test
    void generateApiThreadsOpenApiSpecPathIntoTheGenerationReport() throws Exception {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSession();
            Path specPath = temp.resolve("openapi.json");
            Files.writeString(specPath,
                    "{\"paths\": {\"/orders\": {\"post\": {}}}}", StandardCharsets.UTF_8);

            McpCaptureReplayResult result = service.generateApi(
                    "recordings/session-mcp.json", "generated", "tests.generated", "",
                    "SCENARIO", "STATUS", true, false, "openapi.json", List.of(), List.of());

            assertTrue(result.successful(), "Generation report: " + result.report());
            assertNotNull(result.report().openApiCoverage());
            assertTrue(result.report().openApiCoverage().loadable(),
                    "Coverage load failure: " + result.report().openApiCoverage().loadFailureReason());
            assertTrue(result.report().openApiCoverage().coveredOperations().contains("POST /orders"));
        } finally {
            service.close();
        }
    }

    @Test
    void generateApiLeavesCoverageNotRequestedWhenNoSpecPathGiven() throws Exception {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSession();

            McpCaptureReplayResult result = service.generateApi(
                    "recordings/session-mcp.json", "generated", "tests.generated", "",
                    "SCENARIO", "STATUS", true, false, "", List.of(), List.of());

            assertTrue(result.successful(), "Generation report: " + result.report());
            assertFalse(result.report().openApiCoverage().loadable());
        } finally {
            service.close();
        }
    }

    @Test
    void generateApiOmitsExcludedTransactionsFromTheGeneratedTest() throws Exception {
        // Issue #3548 item 3: deselecting a transaction in the recorder table must remove it from
        // generated output, not just decorate the table client-side.
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSessionWithTwoTransactions();

            McpCaptureReplayResult result = service.generateApi(
                    "recordings/session-mcp-two.json", "generated-excluded", "tests.generated", "",
                    "SCENARIO", "STATUS", true, false, "", List.of("tx-2"), List.of());

            assertTrue(result.successful(), "Generation report: " + result.report());
            String source = Files.readString(result.sourcePath());
            assertTrue(source.contains("/orders"), "Included transaction missing from generated source: " + source);
            assertFalse(source.contains("/admin"), "Excluded transaction leaked into generated source: " + source);
        } finally {
            service.close();
        }
    }

    @Test
    void generateApiThreadsPinnedJsonPathsIntoBusinessDepthGenerationWithoutError() throws Exception {
        // Issue #3530 negative-case: capture_api_generate now accepts pinnedJsonPaths so an agent can
        // force-assert an otherwise-skipped response leaf at BUSINESS depth. The pin *semantics*
        // (volatile/correlated pinned -> asserted; sensitive/blank never) are unit-tested in
        // shaft-capture ApiTestRendererTest; here we prove the MCP tool threads the param end to end
        // into real generation without error.
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSessionWithTwoTransactions();

            McpCaptureReplayResult result = service.generateApi(
                    "recordings/session-mcp-two.json", "generated-pinned", "tests.generated", "",
                    "SCENARIO", "BUSINESS", true, false, "", List.of(), List.of("$.id"));

            assertTrue(result.successful(), "Generation report: " + result.report());
            String source = Files.readString(result.sourcePath());
            assertTrue(source.contains("/orders"), "Generated source missing recorded transaction: " + source);
        } finally {
            service.close();
        }
    }

    @Test
    void apiResponseLeavesReturnsClassifiedLeavesWithoutGeneratingATest() throws Exception {
        // Issue #3530 negative-case: capture_api_response_leaves is the data source for a
        // "pin this path" picker -- it must classify leaves without writing any generated source.
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSessionWithTwoTransactions();

            List<com.shaft.capture.generate.api.ApiCaptureGenerator.TransactionLeaves> leaves =
                    service.apiResponseLeaves("recordings/session-mcp-two.json", List.of());

            assertEquals(2, leaves.size());
            assertEquals("tx-1", leaves.get(0).transactionId());
            assertEquals("tx-2", leaves.get(1).transactionId());
            assertFalse(Files.exists(temp.resolve("generated-tests")),
                    "Listing response leaves must not generate any test source");
        } finally {
            service.close();
        }
    }

    @Test
    void apiResponseLeavesHonorsExcludedTransactionIds() throws Exception {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            writeRecordedSessionWithTwoTransactions();

            List<com.shaft.capture.generate.api.ApiCaptureGenerator.TransactionLeaves> leaves =
                    service.apiResponseLeaves("recordings/session-mcp-two.json", List.of("tx-1"));

            assertEquals(1, leaves.size());
            assertEquals("tx-2", leaves.get(0).transactionId());
        } finally {
            service.close();
        }
    }

    private Path writeRecordedSessionWithTwoTransactions() throws Exception {
        Path sessionPath = temp.resolve("recordings/session-mcp-two.json");
        Files.createDirectories(sessionPath.getParent());
        Path bodiesDirectory = sessionPath.getParent().resolve("session-mcp-two-network-bodies");
        Files.createDirectories(bodiesDirectory);
        NetworkBodyStore bodyStore = new NetworkBodyStore();
        BodyRef ordersResponse = bodyStore.store(
                "{\"id\":\"1\"}".getBytes(StandardCharsets.UTF_8), "application/json", bodiesDirectory);
        BodyRef adminResponse = bodyStore.store(
                "{\"id\":\"2\"}".getBytes(StandardCharsets.UTF_8), "application/json", bodiesDirectory);

        Instant started = Instant.parse("2026-01-02T03:04:05Z");
        BrowserMetadata browser = new BrowserMetadata("chrome", "137", "Windows 11", "browser-1", Map.of());
        CaptureEvent.NetworkEvent createOrder = new CaptureEvent.NetworkEvent(
                context(1, started),
                "tx-1",
                ResourceKind.FETCH,
                new HttpRequestRecord("POST", "https://api.example.test/orders", headers(), null),
                new HttpResponseRecord(201, headers(), ordersResponse),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);
        CaptureEvent.NetworkEvent deleteAdmin = new CaptureEvent.NetworkEvent(
                context(2, started),
                "tx-2",
                ResourceKind.FETCH,
                new HttpRequestRecord("DELETE", "https://api.example.test/admin", headers(), null),
                new HttpResponseRecord(204, headers(), adminResponse),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);

        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "session-mcp-two",
                CaptureSession.SessionStatus.COMPLETED,
                started,
                started.plusSeconds(5),
                browser,
                List.of(createOrder, deleteAdmin),
                List.of(),
                List.of(),
                RedactionSummary.empty(),
                Map.of());
        new CaptureJsonCodec().write(sessionPath, session);
        return sessionPath;
    }

    private Path writeRecordedSession() throws Exception {
        Path sessionPath = temp.resolve("recordings/session-mcp.json");
        Files.createDirectories(sessionPath.getParent());
        Path bodiesDirectory = sessionPath.getParent().resolve("session-mcp-network-bodies");
        Files.createDirectories(bodiesDirectory);
        NetworkBodyStore bodyStore = new NetworkBodyStore();
        BodyRef responseRef = bodyStore.store(
                "{\"id\":\"1\"}".getBytes(StandardCharsets.UTF_8), "application/json", bodiesDirectory);

        Instant started = Instant.parse("2026-01-02T03:04:05Z");
        BrowserMetadata browser = new BrowserMetadata("chrome", "137", "Windows 11", "browser-1", Map.of());
        CaptureEvent.NetworkEvent createOrder = new CaptureEvent.NetworkEvent(
                context(1, started),
                "tx-1",
                ResourceKind.FETCH,
                new HttpRequestRecord("POST", "https://api.example.test/orders", headers(), null),
                new HttpResponseRecord(201, headers(), responseRef),
                new NetworkTiming(null, null, null, null, null, null),
                "",
                "https://app.example.test/",
                null);

        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "session-mcp",
                CaptureSession.SessionStatus.COMPLETED,
                started,
                started.plusSeconds(5),
                browser,
                List.of(createOrder),
                List.of(),
                List.of(),
                RedactionSummary.empty(),
                Map.of());
        new CaptureJsonCodec().write(sessionPath, session);
        return sessionPath;
    }

    private static com.shaft.capture.model.EventContext context(long sequence, Instant started) {
        com.shaft.capture.model.PageContext page = new com.shaft.capture.model.PageContext(
                "https://app.example.test/", "App", "window-1", List.of(), 1280, 720);
        return new com.shaft.capture.model.EventContext(sequence, started.plusSeconds(sequence), page,
                com.shaft.capture.model.EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
    }

    private static Map<String, String> headers() {
        Map<String, String> headers = new TreeMap<>();
        headers.put("content-type", "application/json");
        return headers;
    }

    @Test
    void setModeReadsCurrentModeWithBlankArgumentAndTogglesWithAnExplicitOne() {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            assertEquals("record", service.setMode(""));
            assertEquals("inspect", service.setMode("inspect"));
            assertEquals("inspect", service.setMode(null));
            assertEquals("record", service.setMode("RECORD"));
        } finally {
            service.close();
        }
    }

    @Test
    void setModeRejectsUnsupportedMode() {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            assertThrows(IllegalArgumentException.class, () -> service.setMode("not-a-mode"));
        } finally {
            service.close();
        }
    }

    @Test
    void pickLocatorRanksCandidatesBestFirstAndRendersASnippet() {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            List<CaptureService.McpLocatorCandidate> candidates = List.of(
                    new CaptureService.McpLocatorCandidate("CSS", "div.form > input", 1, true, false),
                    new CaptureService.McpLocatorCandidate("ID", "username", 1, true, true));

            CaptureService.McpPickLocatorResult result = service.pickLocator(candidates);

            assertEquals("SHAFT.GUI.Locator.id(\"username\")", result.snippet());
            assertEquals(2, result.ranked().size());
            assertEquals("ID", result.ranked().getFirst().strategy());
        } finally {
            service.close();
        }
    }

    @Test
    void pickLocatorIgnoresUnsupportedStrategiesAndReturnsBlankWhenNoneAreValid() {
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            CaptureService.McpPickLocatorResult result = service.pickLocator(
                    List.of(new CaptureService.McpLocatorCandidate("NOT_A_STRATEGY", "x", 1, true, true)));

            assertTrue(result.snippet().isEmpty());
            assertTrue(result.ranked().isEmpty());
        } finally {
            service.close();
        }
    }

    @Test
    void pickLocatorWithNoCandidatesReturnsThePersistedPickWhenOneExists() throws Exception {
        // Mirrors CaptureService's private RUNTIME_DIRECTORY constant: capture_pick_locator falls
        // back to the pick persisted there by CaptureControlServer's /locator/pick endpoint.
        Path runtimeDirectory = Path.of("target", "shaft-capture-mcp");
        CaptureControlFiles files = new CaptureControlFiles(runtimeDirectory);
        files.writeLastPick(new CaptureControlFiles.LastPick(
                "SHAFT.GUI.Locator.id(\"username\")",
                List.of(new CaptureControlServer.RankedCandidate(
                        "ID", "username", 100, "SHAFT.GUI.Locator.id(\"username\")")),
                System.currentTimeMillis()));
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            CaptureService.McpPickLocatorResult result = service.pickLocator(List.of());

            assertEquals("SHAFT.GUI.Locator.id(\"username\")", result.snippet());
            assertEquals(1, result.ranked().size());
            assertEquals("ID", result.ranked().getFirst().strategy());
        } finally {
            service.close();
            Files.deleteIfExists(files.runtimeDirectory().resolve("lastPick.json"));
        }
    }

    @Test
    void pickLocatorWithNoCandidatesReturnsBlankWhenNoPickIsPersisted() throws Exception {
        Path runtimeDirectory = Path.of("target", "shaft-capture-mcp");
        CaptureControlFiles files = new CaptureControlFiles(runtimeDirectory);
        // Self-contained regardless of test execution order: ensure no stray lastPick.json from a
        // sibling test remains before asserting the blank fallback.
        Files.deleteIfExists(files.runtimeDirectory().resolve("lastPick.json"));
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            assertNull(files.readLastPick(), "Precondition: no pick persisted for this test.");

            CaptureService.McpPickLocatorResult result = service.pickLocator(List.of());

            assertTrue(result.snippet().isEmpty());
            assertTrue(result.ranked().isEmpty());
        } finally {
            service.close();
        }
    }

    @Test
    void networkTransactionRejectsNegativeValues() {
        IllegalArgumentException statusCodeError = assertThrows(IllegalArgumentException.class,
                () -> new NetworkTransaction("tx-1", "GET", "https://example.com", -1, "xhr", 0,
                        java.util.Map.of(), List.of()),
                "Should have thrown IllegalArgumentException for negative status code");
        assertTrue(statusCodeError.getMessage().contains("status code"));

        IllegalArgumentException timingError = assertThrows(IllegalArgumentException.class,
                () -> new NetworkTransaction("tx-1", "GET", "https://example.com", 200, "xhr", -1,
                        java.util.Map.of(), List.of()),
                "Should have thrown IllegalArgumentException for negative timing");
        assertTrue(timingError.getMessage().contains("timing") || timingError.getMessage().contains("Timing"));
    }
}
