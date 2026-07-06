package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;
import com.shaft.capture.runtime.NetworkCaptureOptions;
import com.shaft.capture.runtime.NetworkTransaction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
        // A blank target URL fails CaptureStartRequest validation synchronously, before any
        // browser is touched, so this proves apiStart wires apiCapture=true and
        // networkOptions/headless resolution through startWithOptions without a NullPointerException,
        // for both the unspecified-options and explicit-options cases.
        CaptureService service = new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
        try {
            IllegalArgumentException unspecified = assertThrows(IllegalArgumentException.class,
                    () -> service.apiStart("", "chrome", null, null));
            assertTrue(unspecified.getMessage().contains("target URL"));

            IllegalArgumentException explicit = assertThrows(IllegalArgumentException.class,
                    () -> service.apiStart("", "chrome", false, new NetworkCaptureOptions()));
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
    void networkTransactionRejectsNegativeValues() {
        // Verify status code validation
        try {
            new NetworkTransaction("tx-1", "GET", "https://example.com", -1, "xhr", 0,
                    java.util.Map.of(), List.of());
            assertTrue(false, "Should have thrown IllegalArgumentException for negative status code");
        } catch (IllegalArgumentException ex) {
            assertTrue(ex.getMessage().contains("status code"));
        }

        // Verify timing validation
        try {
            new NetworkTransaction("tx-1", "GET", "https://example.com", 200, "xhr", -1,
                    java.util.Map.of(), List.of());
            assertTrue(false, "Should have thrown IllegalArgumentException for negative timing");
        } catch (IllegalArgumentException ex) {
            assertTrue(ex.getMessage().contains("timing") || ex.getMessage().contains("Timing"));
        }
    }
}
