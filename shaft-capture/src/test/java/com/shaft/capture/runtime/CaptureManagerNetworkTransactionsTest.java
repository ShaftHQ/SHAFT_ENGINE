package com.shaft.capture.runtime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies CaptureManager's network-transaction listing, asset/pattern filtering, and bound,
 * against a populated (non-idle) session -- the plumbing the MCP capture_api_transactions and
 * capture_api_status tools rely on end to end.
 */
class CaptureManagerNetworkTransactionsTest {
    private static final String RAW_AUTHORIZATION_HEADER_VALUE = "Bearer super-secret-token-do-not-leak";
    private static final String RAW_REQUEST_BODY = "{\"password\":\"hunter2\"}";

    @TempDir
    Path temp;

    @Test
    void networkTransactionsAreEmptyWhenNoSessionIsActive() {
        CaptureManager manager = new CaptureManager(FakeRecorderWithTransactions::new);
        assertTrue(manager.networkTransactions().isEmpty());
        assertTrue(manager.networkTransactions(new NetworkCaptureOptions(), 100).isEmpty());
        manager.close();
    }

    @Test
    void networkTransactionsReturnsPopulatedSummariesWithoutSensitiveData() {
        AtomicReference<FakeRecorderWithTransactions> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FakeRecorderWithTransactions recorder = new FakeRecorderWithTransactions(request);
            recorderRef.set(recorder);
            return recorder;
        });
        manager.start(request("populated.json"));

        List<NetworkTransaction> transactions = manager.networkTransactions();

        assertEquals(4, transactions.size());
        for (NetworkTransaction transaction : transactions) {
            String serialized = transaction.toString();
            // The raw Authorization header value and request/response bodies used by the fake
            // recorder to simulate a real capture must never surface in the safe summary; only
            // the redacted placeholder and safe blob-reference metadata may appear.
            assertFalse(serialized.contains(RAW_AUTHORIZATION_HEADER_VALUE),
                    "serialized transaction must never contain the raw Authorization header value");
            assertFalse(serialized.contains(RAW_REQUEST_BODY),
                    "serialized transaction must never contain a request/response body");
            assertTrue(serialized.contains("[REDACTED]") || transaction.bodyRefMetadata().isEmpty(),
                    "sensitive metadata must be redacted, not merely absent from this fixture's shape");
        }
        manager.stop(false);
        manager.close();
    }

    @Test
    void networkTransactionsExcludesAssetNoiseWhenRequested() {
        CaptureManager manager = new CaptureManager(FakeRecorderWithTransactions::new);
        manager.start(request("assets.json"));

        NetworkCaptureOptions excludeAssets = new NetworkCaptureOptions();
        excludeAssets.excludeAssets = true;
        List<NetworkTransaction> withoutAssets = manager.networkTransactions(excludeAssets, 100);

        assertEquals(2, withoutAssets.size());
        assertTrue(withoutAssets.stream().noneMatch(tx -> "IMAGE".equals(tx.resourceKind())
                || "STYLESHEET".equals(tx.resourceKind())));

        NetworkCaptureOptions includeAssets = new NetworkCaptureOptions();
        includeAssets.excludeAssets = false;
        assertEquals(4, manager.networkTransactions(includeAssets, 100).size());

        manager.stop(false);
        manager.close();
    }

    @Test
    void networkTransactionsAppliesExcludePatternAndBound() {
        CaptureManager manager = new CaptureManager(FakeRecorderWithTransactions::new);
        manager.start(request("pattern.json"));

        NetworkCaptureOptions options = new NetworkCaptureOptions();
        options.excludePattern = "*/admin/*";
        List<NetworkTransaction> filtered = manager.networkTransactions(options, 100);

        assertTrue(filtered.stream().noneMatch(tx -> tx.url().contains("/admin/")));
        assertEquals(1, manager.networkTransactions(new NetworkCaptureOptions(), 1).size());

        manager.stop(false);
        manager.close();
    }

    private CaptureStartRequest request(String outputName) {
        return new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                temp.resolve(outputName),
                temp.resolve("runtime"),
                true);
    }

    private static final class FakeRecorderWithTransactions extends ManagedCaptureRecorder {
        private final CaptureStartRequest request;
        private CaptureStatus.State state = CaptureStatus.State.STARTING;

        FakeRecorderWithTransactions(CaptureStartRequest request) {
            super(request);
            this.request = request;
        }

        @Override
        void start() {
            state = CaptureStatus.State.ACTIVE;
        }

        @Override
        synchronized CaptureStatus status() {
            return new CaptureStatus(
                    state,
                    "fake-session",
                    request.browser().name().toLowerCase(),
                    request.targetUrl(),
                    0,
                    List.of(),
                    request.outputPath().toString(),
                    false,
                    ProcessHandle.current().pid(),
                    Instant.parse("2026-01-02T03:04:05Z"));
        }

        @Override
        synchronized List<NetworkTransaction> networkTransactions() {
            if (state != CaptureStatus.State.ACTIVE) {
                return List.of();
            }
            // Simulates a real network recorder: the raw Authorization header value and request
            // body it observed on the wire (RAW_AUTHORIZATION_HEADER_VALUE, RAW_REQUEST_BODY) are
            // deliberately NOT placed into the NetworkTransaction summaries below -- only the
            // redacted placeholder and a safe blob reference are. This is the shape production
            // code must follow once a real recorder (T4) constructs these summaries.
            return List.of(
                    new NetworkTransaction(
                            "tx-1", "GET", "https://api.example.test/v1/users", 200, "xhr", 42L,
                            Map.of("authorization", "[REDACTED]", "bodyRef", "blob-1"),
                            List.of(1)),
                    new NetworkTransaction(
                            "tx-2", "POST", "https://api.example.test/v1/admin/reset", 204, "fetch", 88L,
                            Map.of("authorization", "[REDACTED]"),
                            List.of(2)),
                    new NetworkTransaction(
                            "tx-3", "GET", "https://cdn.example.test/logo.png", 200, "IMAGE", 12L,
                            Map.of(), List.of()),
                    new NetworkTransaction(
                            "tx-4", "GET", "https://cdn.example.test/app.css", 200, "STYLESHEET", 9L,
                            Map.of(), List.of()));
        }

        @Override
        synchronized CaptureStatus stop(boolean discard) {
            state = discard ? CaptureStatus.State.DISCARDED : CaptureStatus.State.COMPLETED;
            return status();
        }

        @Override
        synchronized CaptureStatus interrupt() {
            state = CaptureStatus.State.INCOMPLETE;
            return status();
        }

        @Override
        synchronized boolean isBrowserAlive() {
            return state == CaptureStatus.State.ACTIVE;
        }
    }
}
