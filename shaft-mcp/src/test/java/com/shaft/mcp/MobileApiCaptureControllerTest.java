package com.shaft.mcp;

import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
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
        MobileApiCaptureController controller = new MobileApiCaptureController();
        Path sessionPath = tempDir.resolve("recordings/mobile-session.json");

        MobileApiCaptureStatus started = controller.start("Android", "emulator-5554", sessionPath);
        assertTrue(started.active());
        assertTrue(started.proxyPort() > 0);
        assertTrue(started.caCertificatePem().contains("BEGIN CERTIFICATE"));

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
    void startingTwiceIsIdempotentAndDoesNotReplaceTheActiveSession() {
        MobileApiCaptureController controller = new MobileApiCaptureController();
        Path sessionPath = tempDir.resolve("recordings/idempotent.json");

        MobileApiCaptureStatus first = controller.start("Android", "emulator-5554", sessionPath);
        MobileApiCaptureStatus second = controller.start("Android", "emulator-5554", sessionPath);

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
        MobileApiCaptureController controller = new MobileApiCaptureController();
        Path sessionPath = tempDir.resolve("recordings/no-response.json");
        controller.start("iOS", "iPhone-Simulator", sessionPath);

        controller.acceptTransaction(new ProxyTransaction(
                "GET", "https://api.example.test/timeout", Map.of(), new byte[0], 0, Map.of(), new byte[0]));

        controller.stop(false);
        CaptureSession persisted = new CaptureSessionStore(sessionPath).read();
        CaptureEvent.NetworkEvent event = (CaptureEvent.NetworkEvent) persisted.events().get(0);
        assertEquals(null, event.response());
        assertFalse(event.failureReason().isBlank());
    }

    private static String sessionFileText(Path path) {
        try {
            return java.nio.file.Files.readString(path);
        } catch (java.io.IOException e) {
            throw new java.io.UncheckedIOException(e);
        }
    }
}
