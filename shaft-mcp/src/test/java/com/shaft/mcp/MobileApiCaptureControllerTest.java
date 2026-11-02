package com.shaft.mcp;

import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.proxy.CaptureCertificateAuthority;
import com.shaft.capture.proxy.ProxyTransaction;
import com.shaft.capture.storage.CaptureSessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MobileApiCaptureControllerTest {

    @TempDir
    Path tempDir;

    @Test
    void startAcceptStopRoundTripsATransactionIntoAPersistedRedactedNetworkEvent() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/mobile-session.json");

        MobileApiCaptureStatus started = controller.start("Android", "emulator-5554", sessionPath);
        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());
        assertTrue(started.proxyPort() > 0);
        assertTrue(started.caCertificatePem().contains("BEGIN CERTIFICATE"));
        assertTrue(started.warnings().stream().anyMatch(warning -> warning.contains("adb reverse")),
                "expected Android pairing guidance in warnings: " + started.warnings());

        Map<String, String> requestHeaders = new TreeMap<>();
        requestHeaders.put("Authorization", "Bearer super-secret-token");
        requestHeaders.put("Content-Type", "application/json");
        Map<String, String> responseHeaders = new TreeMap<>();
        responseHeaders.put("Content-Type", "application/json");

        controller.acceptTransaction(new ProxyTransaction(
                "POST", "https://api.example.test/orders", requestHeaders,
                "{\"item\":\"widget\"}".getBytes(StandardCharsets.UTF_8),
                201, responseHeaders, "{\"id\":\"abc-123\"}".getBytes(StandardCharsets.UTF_8)));

        MobileApiCaptureStatus afterOneTransaction = controller.status();
        assertEquals(1, afterOneTransaction.transactionCount());

        MobileApiCaptureStatus stopped = controller.stop(false);
        assertFalse(stopped.active());

        CaptureSession persisted = new CaptureSessionStore(sessionPath).read();
        assertEquals(1, persisted.events().size());
        CaptureEvent.NetworkEvent event = (CaptureEvent.NetworkEvent) persisted.events().get(0);
        assertEquals("POST", event.request().method());
        assertEquals("https://api.example.test/orders", event.request().url());
        assertEquals(201, event.response().statusCode());

        String authorizationHeader = event.request().headers().get("authorization");
        assertTrue(authorizationHeader.startsWith("secret-ref:"),
                "The raw bearer token must never be persisted: " + authorizationHeader);
        assertFalse(sessionFileText(sessionPath).contains("super-secret-token"));

        // Bodies are externalized, not inlined -- the raw request/response text must not appear
        // verbatim in the session JSON itself.
        assertFalse(sessionFileText(sessionPath).contains("widget"));
    }

    @Test
    void transactionsReflectsCapturedTransactionsLiveAndOmitsBodies() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/mobile-transactions.json");

        assertTrue(controller.transactions().isEmpty(),
                "transactions() must be empty before a session has started");

        MobileApiCaptureStatus started = controller.start("Android", "emulator-5554", sessionPath);
        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());

        Map<String, String> requestHeaders = new TreeMap<>();
        requestHeaders.put("Content-Type", "application/json");
        Map<String, String> responseHeaders = new TreeMap<>();
        responseHeaders.put("Content-Type", "application/json");

        controller.acceptTransaction(new ProxyTransaction(
                "GET", "https://api.example.test/orders", requestHeaders, new byte[0],
                200, responseHeaders, "{\"status\":\"ok\"}".getBytes(StandardCharsets.UTF_8)));
        controller.acceptTransaction(new ProxyTransaction(
                "POST", "https://api.example.test/orders", requestHeaders,
                "{\"item\":\"widget\"}".getBytes(StandardCharsets.UTF_8),
                201, responseHeaders, "{\"id\":\"abc-123\"}".getBytes(StandardCharsets.UTF_8)));

        var liveTransactions = controller.transactions();
        assertEquals(2, liveTransactions.size(), "transactions() must reflect mid-session appends");
        assertEquals("GET", liveTransactions.get(0).method());
        assertEquals("https://api.example.test/orders", liveTransactions.get(0).url());
        assertEquals(200, liveTransactions.get(0).statusCode());
        assertEquals("POST", liveTransactions.get(1).method());
        assertEquals("https://api.example.test/orders", liveTransactions.get(1).url());
        assertEquals(201, liveTransactions.get(1).statusCode());

        for (var transaction : liveTransactions) {
            for (Object metadata : transaction.bodyRefMetadata().values()) {
                assertFalse(String.valueOf(metadata).contains("widget"),
                        "transaction summaries must not carry raw body content: " + metadata);
                assertFalse(String.valueOf(metadata).contains("abc-123"),
                        "transaction summaries must not carry raw body content: " + metadata);
            }
        }

        controller.stop(false);
    }

    @Test
    void startingTwiceIsIdempotentAndDoesNotReplaceTheActiveSession() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/idempotent.json");

        MobileApiCaptureStatus first = controller.start("Android", "emulator-5554", sessionPath);
        MobileApiCaptureStatus second = controller.start("Android", "emulator-5554", sessionPath);

        assertTrue(first.active(), "mobile API capture failed to start: " + first.warnings());
        assertEquals(first.sessionId(), second.sessionId());
        assertEquals(first.proxyPort(), second.proxyPort());
        controller.stop(true);
    }

    @Test
    void stopWithoutStartIsANoOpNotAFailure() {
        MobileApiCaptureController controller = new MobileApiCaptureController();

        MobileApiCaptureStatus status = controller.stop(false);

        assertFalse(status.active());
    }

    @Test
    void aTransactionWithNoResponseIsRecordedWithoutAResponseRecord() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/no-response.json");
        MobileApiCaptureStatus started = controller.start("iOS", "iPhone-Simulator", sessionPath);
        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());

        controller.acceptTransaction(new ProxyTransaction(
                "GET", "https://api.example.test/timeout", Map.of(), new byte[0], 0, Map.of(), new byte[0]));

        controller.stop(false);
        CaptureSession persisted = new CaptureSessionStore(sessionPath).read();
        CaptureEvent.NetworkEvent event = (CaptureEvent.NetworkEvent) persisted.events().get(0);
        assertEquals(null, event.response());
        assertFalse(event.failureReason().isBlank());
    }

    @Test
    void statusExposesTheSessionPathAndRollsReadinessUpFromWarnings() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/readiness.json");

        MobileApiCaptureStatus started = controller.start("Android", "emulator-5554", sessionPath);
        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());
        // Session path is always visible so the caller can generate an API test from it.
        assertEquals(sessionPath.toString(), started.outputPath());
        // Android start emits pairing-guidance warnings, so readiness is RISKY.
        assertEquals(CaptureReadiness.State.RISKY, started.readiness());

        controller.acceptTransaction(new ProxyTransaction(
                "GET", "https://api.example.test/orders", Map.of(), new byte[0],
                200, Map.of(), "{\"status\":\"ok\"}".getBytes(StandardCharsets.UTF_8)));
        MobileApiCaptureStatus stopped = controller.stop(false);
        assertEquals(1, stopped.transactionCount());
        assertEquals(sessionPath.toString(), stopped.outputPath());
        assertEquals(CaptureReadiness.State.RISKY, stopped.readiness());
    }

    @Test
    void stoppingWithNoTransactionsIsBlocked() {
        MobileApiCaptureController controller = new MobileApiCaptureController(
                () -> new CaptureCertificateAuthority(tempDir.resolve("capture-ca")));
        Path sessionPath = tempDir.resolve("recordings/empty-api.json");

        MobileApiCaptureStatus started = controller.start("iOS", "iPhone-Simulator", sessionPath);
        assertTrue(started.active(), "mobile API capture failed to start: " + started.warnings());

        MobileApiCaptureStatus stopped = controller.stop(false);
        assertEquals(0, stopped.transactionCount());
        assertEquals(CaptureReadiness.State.BLOCKED, stopped.readiness());
    }

    private static String sessionFileText(Path path) {
        try {
            return java.nio.file.Files.readString(path);
        } catch (java.io.IOException e) {
            throw new java.io.UncheckedIOException(e);
        }
    }
}
