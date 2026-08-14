package com.shaft.tools.io.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.properties.internal.Properties;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import org.openqa.selenium.remote.http.Contents;
import java.util.LinkedHashMap;
import java.util.Map;

public class BrowserObservabilityRecorderSessionTest {
    @AfterMethod
    public void clearRecorder() {
        BrowserObservabilityRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void seleniumCallbackOnAnotherThreadShouldRemainVisibleToItsOwningTraceSession() throws Exception {
        AtomicReference<Filter> filterReference = new AtomicReference<>();
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterReference.set((Filter) context.arguments().get(1)));
             var callbackExecutor = Executors.newSingleThreadExecutor()) {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
            BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver);
            Assert.assertTrue(interceptor.startObserving());

            HttpRequest request = new HttpRequest(HttpMethod.GET, "https://example.com/async");
            HttpResponse response = new HttpResponse().setStatus(204);
            Future<HttpResponse> callback = callbackExecutor.submit(
                    () -> filterReference.get().apply(ignoredRequest -> response).execute(request));

            Assert.assertSame(callback.get(), response);
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot().size(), 1,
                    "The filter callback must record into the session that installed it, not its executor thread.");
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot().getFirst().url(),
                    "https://example.com/async");
        }
    }

    @Test
    public void cachedInterceptorShouldFollowTestSessionAndOwnerCapturePolicy() throws Exception {
        AtomicReference<Filter> filterReference = new AtomicReference<>();
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterReference.set((Filter) context.arguments().get(1)));
             var callbackExecutor = Executors.newSingleThreadExecutor()) {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
            ReportContext.start(info("setup"));
            BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver);
            Assert.assertTrue(interceptor.startObserving());

            ReportContext.start(info("test"));
            Future<HttpResponse> callback = callbackExecutor.submit(() -> {
                SHAFT.Properties.reporting.set().traceIncludeNetwork(false);
                try {
                    return filterReference.get().apply(ignoredRequest -> new HttpResponse().setStatus(200))
                            .execute(new HttpRequest(HttpMethod.GET, "https://example.com/test-session"));
                } finally {
                    Properties.clearForCurrentThread();
                }
            });

            Assert.assertEquals(callback.get().getStatus(), 200);
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot().size(), 1,
                    "A cached interceptor must follow the active test session and its owner-thread capture policy.");
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot().getFirst().url(),
                    "https://example.com/test-session");
        }
    }

    @Test
    public void exchangeOwnerShouldUseIdentityWhenPublicRecordContentsMutate() throws Exception {
        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
            BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
            HttpRequest request = new HttpRequest(HttpMethod.GET, "https://example.com/mutable-record");
            request.addHeader("X-Initial", "one");
            BrowserObservabilityRecorder.NetworkExchange exchange =
                    BrowserObservabilityRecorder.startNetwork(owner, request);
            exchange.requestHeaders().put("X-Later", "two");
            HttpResponse response = new HttpResponse().setStatus(201).setContent(Contents.utf8String("ok"));

            callbackExecutor.submit(() -> BrowserObservabilityRecorder.finishNetwork(exchange, response, "")).get();

            Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).size(), 1,
                    "Mutable public record contents must not change identity-based session ownership.");
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).getFirst().status(), 201);
            Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).getFirst().bodyPreview(), "ok");
        }
    }

    @Test
    public void pendingDetailedExchangeShouldRetainOnlyBoundedRedactedMetadata() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        HttpRequest request = new HttpRequest(HttpMethod.GET,
                "https://example.com/" + "u".repeat(5_000));
        request.addHeader("Authorization", "Bearer raw-pending-secret");
        for (int index = 0; index < 100; index++) {
            request.addHeader("X-Pending-" + index, "v".repeat(300));
        }

        BrowserObservabilityRecorder.NetworkExchange exchange =
                BrowserObservabilityRecorder.startNetwork(owner, request);

        Assert.assertTrue(exchange.enabled());
        Assert.assertTrue(exchange.url().length() <= 2_048, String.valueOf(exchange.url().length()));
        Assert.assertTrue(exchange.url().contains("omitted"), exchange.url());
        Assert.assertTrue(exchange.requestHeaders().containsValue("********"), exchange.requestHeaders().toString());
        Assert.assertFalse(exchange.requestHeaders().toString().contains("raw-pending-secret"));
        Assert.assertTrue(exchange.requestHeaders().size() <= 64,
                String.valueOf(exchange.requestHeaders().size()));
        long retainedHeaderCharacters = exchange.requestHeaders().entrySet().stream()
                .mapToLong(entry -> entry.getKey().length() + entry.getValue().length())
                .sum();
        Assert.assertTrue(retainedHeaderCharacters <= 8_192, String.valueOf(retainedHeaderCharacters));
    }

    @Test
    public void sessionShouldBoundNetworkEvidenceAndReportOldestEventOmission() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession session = BrowserObservabilityRecorder.startSession();

        for (int index = 0; index <= 1000; index++) {
            BrowserObservabilityRecorder.recordNetwork(session, new BrowserObservabilityRecorder.NetworkObservation(
                    "GET", "https://example.com/event-" + index, 200, Map.of(), Map.of(),
                    1, 0, 0, "", ""));
        }

        var snapshot = BrowserObservabilityRecorder.snapshot(session);
        Assert.assertEquals(snapshot.size(), 1000);
        Assert.assertEquals(snapshot.getFirst().url(), "https://example.com/event-1");
        Assert.assertEquals(snapshot.getLast().url(), "https://example.com/event-1000");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().stream()
                .anyMatch(warning -> warning.contains("oldest network")),
                "Trace metadata must explain bounded evidence omission.");
    }

    @Test
    public void explicitCallbackOwnerShouldStayIsolatedFromSiblingSession() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true).traceIncludeConsole(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BrowserObservabilityRecorder.ObservationSession sibling = BrowserObservabilityRecorder.createSession();
        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> {
                BrowserObservabilityRecorder.recordNetwork(owner,
                        new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/owner",
                                200, Map.of(), Map.of(), 1, 0, 0, "", ""));
                BrowserObservabilityRecorder.recordConsole(owner, "bidi", "info", "owner-console", 10);
            }).get();
        }

        Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).size(), 1);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole(owner).size(), 1);
        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(sibling).isEmpty());
        Assert.assertTrue(BrowserObservabilityRecorder.snapshotConsole(sibling).isEmpty());
    }

    @Test
    public void callbackCaptureShouldOmitOversizedFieldsAndRedactCompleteFields() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        String secret = "BOUNDARY_NETWORK_SECRET_984271";
        FailureTraceReporter.registerSensitiveSourceValue(secret);
        String bodyPrefix = "b".repeat(2040);
        LinkedHashMap<String, String> headers = new LinkedHashMap<>();
        headers.put("X-A", "a".repeat(2500));
        headers.put("X-B", "b".repeat(2500));
        headers.put("X-C", "c".repeat(2500));
        headers.put("X-Boundary", "d".repeat(650) + secret + "-header-tail");

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> BrowserObservabilityRecorder.recordNetwork(owner,
                    new BrowserObservabilityRecorder.NetworkObservation("POST", "https://example.com/boundary",
                            200, headers, Map.of(), 1, 0, 0, "", bodyPrefix + secret + "-body-tail"))).get();
        }

        JsonNode event = new ObjectMapper().readTree(BrowserObservabilityRecorder.drainNetworkJson()).get(0);
        String body = event.path("bodyPreview").asText();
        String header = event.path("requestHeaders").path("X-Boundary").asText();
        Assert.assertTrue(body.contains("omitted"), body);
        Assert.assertTrue(header.isEmpty() || header.contains("omitted") || header.contains("********"), header);
        Assert.assertFalse(body.contains("BOUNDARY_N"));
        Assert.assertFalse(header.contains("BOUNDARY_N"));
        Assert.assertTrue(body.length() <= 2048);
        int retainedHeaderCharacters = 0;
        var fields = event.path("requestHeaders").fields();
        while (fields.hasNext()) {
            var field = fields.next();
            retainedHeaderCharacters += field.getKey().length() + field.getValue().asText().length();
        }
        Assert.assertTrue(retainedHeaderCharacters <= 8192);
    }

    @Test
    public void cumulativeSourceRedactionShouldNeverPublishAPartialLaterCredential() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        String first = "FIRST-" + "a".repeat(500) + "-END";
        String second = "SECOND-" + "b".repeat(499) + "-END";
        String later = "LATER-" + "z".repeat(500) + "-END";
        FailureTraceReporter.registerSensitiveSourceValue(first);
        FailureTraceReporter.registerSensitiveSourceValue(second);
        FailureTraceReporter.registerSensitiveSourceValue(later);
        LinkedHashMap<String, String> headers = new LinkedHashMap<>();
        headers.put("A", first);
        headers.put("B", second);
        headers.put("C", "c".repeat(2048));
        headers.put("D", "d".repeat(2048));
        headers.put("E", "e".repeat(2048));
        headers.put("F", "f".repeat(1400) + later + "-tail");

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> BrowserObservabilityRecorder.recordNetwork(owner,
                    new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/cumulative",
                            200, headers, Map.of(), 1, 0, 0, "", ""))).get();
        }

        String persisted = BrowserObservabilityRecorder.drainNetworkJson();
        Assert.assertFalse(persisted.contains("LATER-"), persisted);
        Assert.assertFalse(persisted.contains(later.substring(0, 24)), persisted);
    }

    @Test
    public void responsePreviewShouldFailClosedBeforeCredentialBoundaryTruncation() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        String secret = "RESPONSE-BOUNDARY-SECRET-771923";
        FailureTraceReporter.registerSensitiveSourceValue(secret);
        BrowserObservabilityRecorder.NetworkExchange exchange = BrowserObservabilityRecorder.startNetwork(owner,
                new HttpRequest(HttpMethod.GET, "https://example.com/response-boundary"));
        HttpResponse response = new HttpResponse().setStatus(200)
                .setContent(Contents.utf8String("r".repeat(2_040) + secret + "-tail"));

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> BrowserObservabilityRecorder.finishNetwork(exchange, response, "")).get();
        }

        String preview = BrowserObservabilityRecorder.snapshot(owner).getFirst().bodyPreview();
        Assert.assertTrue(preview.contains("omitted"), preview);
        Assert.assertFalse(preview.contains(secret.substring(0, 16)), preview);
    }

    @Test
    public void largeResponsePreviewShouldBeOmittedBeforeFullPayloadRedaction() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BrowserObservabilityRecorder.NetworkExchange exchange = BrowserObservabilityRecorder.startNetwork(owner,
                new HttpRequest(HttpMethod.GET, "https://example.com/large-response"));
        HttpResponse response = new HttpResponse().setStatus(200)
                .setContent(Contents.utf8String("large-response-".repeat(100_000)));

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> BrowserObservabilityRecorder.finishNetwork(exchange, response, "")).get();
        }

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(owner).getFirst().bodyPreview().contains("omitted"));
    }

    @Test
    public void publicSnapshotShouldRedactSourceSensitiveMimeTypeOnOwnerThread() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        String secret = "mime-secret-771923";
        FailureTraceReporter.registerSensitiveSourceValue(secret);

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> BrowserObservabilityRecorder.recordNetwork(owner,
                    new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/mime", 200,
                            Map.of(), Map.of("Content-Type", "application/" + secret + "; charset=utf-8"),
                            1, 0, 0, "", ""))).get();
        }

        String mimeType = BrowserObservabilityRecorder.snapshot(owner).getFirst().mimeType();
        Assert.assertTrue(mimeType.contains("********"), mimeType);
        Assert.assertFalse(mimeType.contains(secret), mimeType);
    }

    @Test
    public void closedOwnerShouldRejectLateCallbackWithoutFallingBackToCurrentSession() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true).traceIncludeConsole(true);
        BrowserObservabilityRecorder.ObservationSession closed = BrowserObservabilityRecorder.startSession();
        BrowserObservabilityRecorder.NetworkExchange exchange = BrowserObservabilityRecorder.startNetwork(
                closed, new HttpRequest(HttpMethod.GET, "https://example.com/late"));
        for (int index = 0; index <= 1000; index++) {
            BrowserObservabilityRecorder.recordNetwork(closed,
                    new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/seed-" + index,
                            200, Map.of(), Map.of(), 1, 0, 0, "", ""));
        }
        Assert.assertFalse(BrowserObservabilityRecorder.drainWarnings(closed).isEmpty(),
                "The close regression must first prove an omission marker exists.");
        BrowserObservabilityRecorder.recordNetwork(closed,
                new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/seed-again",
                        200, Map.of(), Map.of(), 1, 0, 0, "", ""));
        closed.close();
        BrowserObservabilityRecorder.ObservationSession current = BrowserObservabilityRecorder.startSession();

        try (var callbackExecutor = Executors.newSingleThreadExecutor()) {
            callbackExecutor.submit(() -> {
                BrowserObservabilityRecorder.finishNetwork(exchange, new HttpResponse().setStatus(200), "");
                BrowserObservabilityRecorder.recordConsole(closed, "bidi", "error", "late-console", 20);
            }).get();
        }

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(closed).isEmpty());
        Assert.assertTrue(BrowserObservabilityRecorder.snapshotConsole(closed).isEmpty());
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(closed).isEmpty());
        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(current).isEmpty(),
                "A late callback must not fall back to the current test session.");
        Assert.assertTrue(BrowserObservabilityRecorder.snapshotConsole(current).isEmpty());
    }

    @Test
    public void detachedSessionShouldKeepOmissionMarkersWithinWarningLimit() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession detached = BrowserObservabilityRecorder.createSession();
        for (int index = 0; index <= 1000; index++) {
            BrowserObservabilityRecorder.recordNetwork(detached,
                    new BrowserObservabilityRecorder.NetworkObservation("GET", "https://example.com/" + index,
                            200, Map.of(), Map.of(), 1, 0, 0, "", ""));
        }
        for (int index = 0; index <= 100; index++) {
            BrowserObservabilityRecorder.recordWarning(detached, "provider", "warning-" + index);
        }

        var warnings = BrowserObservabilityRecorder.drainWarnings(detached);
        Assert.assertTrue(warnings.size() <= 100);
        Assert.assertTrue(warnings.stream().anyMatch(warning -> warning.contains("oldest network")));
        Assert.assertTrue(warnings.stream().anyMatch(warning -> warning.contains("oldest browser observability")));
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(detached).isEmpty());
    }

    @Test
    public void sessionShouldBoundConsoleEvidenceAndReportOldestEventOmission() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        BrowserObservabilityRecorder.ObservationSession session = BrowserObservabilityRecorder.startSession();
        for (int index = 0; index <= 1000; index++) {
            BrowserObservabilityRecorder.recordConsole(session, "bidi", "info", "console-" + index, index);
        }

        var snapshot = BrowserObservabilityRecorder.snapshotConsole(session);
        Assert.assertEquals(snapshot.size(), 1000);
        Assert.assertEquals(snapshot.getFirst().message(), "console-1");
        Assert.assertEquals(snapshot.getLast().message(), "console-1000");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(session).stream()
                .anyMatch(warning -> warning.contains("oldest console")));
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(session).isEmpty());
    }

    @Test
    public void closedOwnerShouldRejectLateProviderOmissionMarker() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        BrowserObservabilityRecorder.ObservationSession session = BrowserObservabilityRecorder.startSession();
        session.close();

        BrowserObservabilityRecorder.recordConsoleOmission(session);

        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(session).isEmpty());
    }

    @Test
    public void disabledOwnerShouldRejectProviderOmissionMarker() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(false);
        BrowserObservabilityRecorder.ObservationSession session = BrowserObservabilityRecorder.startSession();

        BrowserObservabilityRecorder.recordConsoleOmission(session);

        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings(session).isEmpty());
    }

    private static TestExecutionInfo info(String method) {
        return new TestExecutionInfo("observability-" + method, BrowserObservabilityRecorderSessionTest.class.getName(),
                method, method, method, null, null, false);
    }
}
