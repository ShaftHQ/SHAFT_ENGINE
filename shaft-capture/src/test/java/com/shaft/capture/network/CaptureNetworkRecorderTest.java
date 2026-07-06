package com.shaft.capture.network;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpHandler;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureNetworkRecorderTest {
    @TempDir
    Path temp;

    private final List<String> warnings = new ArrayList<>();

    @AfterEach
    void resetWarnings() {
        warnings.clear();
    }

    @Test
    void copyRestoresFullRequestAndResponseBodiesEvenWhenStoredCopyIsTruncated() throws Exception {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        NetworkCaptureOptions options = new NetworkCaptureOptions(
                true, List.of(), List.of(), false, 500, 5L);
        WebDriver driver = devToolsDriver("https://example.test/app");

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        AtomicReference<HttpRequest> observedByNext = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "session-1", events::add, warnings::add);
            assertTrue(recorder.start());

            String requestPayload = "abcdefghij"; // 10 bytes, exceeds maxBodyBytes=5
            String responsePayload = "0123456789012345"; // 16 bytes, exceeds maxBodyBytes=5
            HttpRequest request = new HttpRequest(HttpMethod.POST, "https://example.test/api/orders");
            request.setContent(Contents.utf8String(requestPayload));

            HttpHandler next = req -> {
                observedByNext.set(req);
                HttpResponse response = new HttpResponse().setStatus(200);
                response.setContent(Contents.utf8String(responsePayload));
                return response;
            };

            HttpResponse callerResponse = filterRef.get().apply(next).execute(request);

            // The downstream handler must still see the full, untruncated request body.
            assertEquals(requestPayload, new String(
                    Contents.bytes(observedByNext.get().getContent()), java.nio.charset.StandardCharsets.UTF_8));
            // The caller must still see the full, untruncated response body.
            assertEquals(responsePayload, new String(
                    Contents.bytes(callerResponse.getContent()), java.nio.charset.StandardCharsets.UTF_8));

            assertEquals(1, events.size());
            CaptureNetworkRecorder.RecordedTransaction event = events.getFirst();
            assertTrue(event.request().body().truncated());
            assertEquals(10, event.request().body().sizeBytes());
            assertTrue(event.response().body().truncated());
            assertEquals(16, event.response().body().sizeBytes());

            // The persisted copies on disk must actually be truncated to maxBodyBytes.
            byte[] storedRequestBytes = Files.readAllBytes(temp.resolve("bodies").resolve(
                    event.request().body().ref()));
            assertEquals(5, storedRequestBytes.length);
            recorder.close();
        }
    }

    @Test
    void dropsAssetRequestsWhenIncludeAssetTypesIsFalse() throws Exception {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        NetworkCaptureOptions options = new NetworkCaptureOptions(
                false, List.of(), List.of(), false, 500, 0);
        WebDriver driver = devToolsDriver("https://example.test/app");

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "session-1", events::add, warnings::add);
            assertTrue(recorder.start());

            HttpRequest assetRequest = new HttpRequest(HttpMethod.GET, "https://example.test/static/app.css");
            HttpHandler next = req -> new HttpResponse().setStatus(200).addHeader("Content-Type", "text/css");
            filterRef.get().apply(next).execute(assetRequest);

            assertTrue(events.isEmpty(), "Asset requests must be dropped when includeAssetTypes is false.");
            recorder.close();
        }
    }

    @Test
    void dropsCrossOriginRequestsWhenFirstPartyOnlyIsTrue() throws Exception {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        NetworkCaptureOptions options = new NetworkCaptureOptions(
                true, List.of(), List.of(), true, 500, 0);
        WebDriver driver = devToolsDriver("https://first-party.test/app");

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "session-1", events::add, warnings::add);
            assertTrue(recorder.start());

            HttpRequest crossOrigin = new HttpRequest(HttpMethod.GET, "https://third-party.test/pixel.gif");
            HttpHandler next = req -> new HttpResponse().setStatus(200);
            filterRef.get().apply(next).execute(crossOrigin);

            assertTrue(events.isEmpty(), "Cross-origin requests must be dropped when firstPartyOnly is true.");

            HttpRequest sameOrigin = new HttpRequest(HttpMethod.GET, "https://first-party.test/api/data");
            filterRef.get().apply(next).execute(sameOrigin);

            assertEquals(1, events.size(), "Same-origin requests must still be recorded.");
            recorder.close();
        }
    }

    @Test
    void honorsIncludeAndExcludeGlobsWithExcludeWinning() throws Exception {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        NetworkCaptureOptions options = new NetworkCaptureOptions(
                true, List.of("*/api/*"), List.of("*/api/secrets*"), false, 500, 0);
        WebDriver driver = devToolsDriver("https://example.test/app");

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "session-1", events::add, warnings::add);
            assertTrue(recorder.start());
            HttpHandler next = req -> new HttpResponse().setStatus(200);

            // Not matching the include glob: dropped.
            filterRef.get().apply(next).execute(new HttpRequest(HttpMethod.GET, "https://example.test/assets/logo.png"));
            assertTrue(events.isEmpty());

            // Matches include glob: recorded.
            filterRef.get().apply(next).execute(new HttpRequest(HttpMethod.GET, "https://example.test/api/orders"));
            assertEquals(1, events.size());

            // Matches both include and exclude globs: exclude wins, dropped.
            filterRef.get().apply(next).execute(new HttpRequest(HttpMethod.GET, "https://example.test/api/secrets/dump"));
            assertEquals(1, events.size(), "Exclude globs must win over include globs.");
            recorder.close();
        }
    }

    @Test
    void dropsTransactionsPastMaxTransactionsWithOneTimeWarning() throws Exception {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        NetworkCaptureOptions options = new NetworkCaptureOptions(
                true, List.of(), List.of(), false, 2, 0);
        WebDriver driver = devToolsDriver("https://example.test/app");

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "session-1", events::add, warnings::add);
            assertTrue(recorder.start());
            HttpHandler next = req -> new HttpResponse().setStatus(200);

            for (int i = 0; i < 5; i++) {
                filterRef.get().apply(next).execute(new HttpRequest(HttpMethod.GET, "https://example.test/api/item/" + i));
            }

            assertEquals(2, events.size(), "Recording must stop at the configured transaction cap.");
            long capWarnings = warnings.stream().filter(warning -> warning.contains("transaction cap")).count();
            assertEquals(1, capWarnings, "The transaction cap warning must be recorded exactly once.");
            recorder.close();
        }
    }

    @Test
    void warnsAndSkipsWhenDriverDoesNotSupportDevTools() {
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        WebDriver driver = Mockito.mock(WebDriver.class);

        CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                driver, temp.resolve("bodies"), NetworkCaptureOptions.defaults(), "session-1",
                events::add, warnings::add);

        assertFalse(recorder.start());
        assertTrue(warnings.stream().anyMatch(warning -> warning.contains("DevTools") || warning.contains("driver")));
        recorder.close();
    }

    @Test
    void becomesSoleInterceptorOwnerByReleasingPassiveTraceObservationOnTheSameDriver() throws Exception {
        // Simulates DriverFactoryHelper.startBrowserObservability() already having registered a
        // BrowserNetworkInterceptor-owned NetworkInterceptor for passive trace/HAR observation on
        // this driver (shaft.trace.enabled + shaft.trace.includeNetwork), independent of apiCapture,
        // before ManagedCaptureRecorder.start() constructs CaptureNetworkRecorder on the same driver.
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        WebDriver driver = devToolsDriver("https://example.test/app");
        DriverFactoryHelper helper = new DriverFactoryHelper();

        AtomicInteger interceptorConstructions = new AtomicInteger();
        AtomicReference<Filter> lastFilter = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> {
                    interceptorConstructions.incrementAndGet();
                    lastFilter.set((Filter) context.arguments().get(1));
                })) {
            try {
                helper.setDriver(driver);
                assertTrue(helper.startBrowserNetworkObservation(),
                        "Passive trace/HAR observation must start for the DevTools-capable driver.");
                assertEquals(1, interceptorConstructions.get(),
                        "The trace/HAR observer must have installed one DevTools network filter.");

                CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                        driver, temp.resolve("bodies"), NetworkCaptureOptions.defaults(), "session-1",
                        events::add, warnings::add);

                assertTrue(recorder.start(),
                        "CaptureNetworkRecorder must become the sole interceptor owner instead of "
                                + "silently losing the registration race.");

                // Sole ownership: the trace observer's filter was released (deactivated), and this
                // recorder's own filter is now the one and only active DevTools network filter.
                assertFalse(DriverFactoryHelper.hasBlockingBrowserNetworkInterceptionRules(driver));
                HttpHandler next = req -> new HttpResponse().setStatus(200);
                lastFilter.get().apply(next).execute(new HttpRequest(HttpMethod.GET, "https://example.test/api/data"));
                assertEquals(1, events.size(),
                        "The recorder's own filter (the surviving, most-recently-installed one) must "
                                + "still observe traffic after the handoff.");

                recorder.close();
            } finally {
                helper.setDriver(null);
            }
        }
    }

    @Test
    void doesNotDoubleRegisterAndWarnsWhenActiveMockValidateRulesWouldBeBroken() throws Exception {
        // When the driver's BrowserNetworkInterceptor instead owns active mock/validate rules
        // (registered via DriverFactoryHelper.registerBrowserNetworkInterceptionRule), replacing its
        // filter would silently break those rules with no warning. CaptureNetworkRecorder must
        // refuse to become the sole owner in that case rather than double-registering.
        List<CaptureNetworkRecorder.RecordedTransaction> events = new ArrayList<>();
        WebDriver driver = devToolsDriver("https://example.test/app");
        DriverFactoryHelper helper = new DriverFactoryHelper();

        AtomicInteger interceptorConstructions = new AtomicInteger();
        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> interceptorConstructions.incrementAndGet())) {
            try {
                helper.setDriver(driver);
                helper.registerBrowserNetworkInterceptionRule(BrowserNetworkInterceptionRule.mock(
                        request -> true, request -> new HttpResponse().setStatus(200)));
                assertEquals(1, interceptorConstructions.get(),
                        "Registering a rule must have installed one DevTools network filter.");
                assertTrue(DriverFactoryHelper.hasBlockingBrowserNetworkInterceptionRules(driver));

                CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                        driver, temp.resolve("bodies"), NetworkCaptureOptions.defaults(), "session-1",
                        events::add, warnings::add);

                assertFalse(recorder.start(),
                        "The recorder must not double-register a competing DevTools network filter "
                                + "while active mock/validate rules would be silently broken.");
                assertEquals(1, interceptorConstructions.get(),
                        "No second NetworkInterceptor may be constructed on this driver.");
                assertTrue(warnings.stream().anyMatch(warning -> warning.contains("mock")
                        || warning.contains("rules") || warning.contains("sole")));

                recorder.close();
            } finally {
                helper.setDriver(null);
            }
        }
    }

    @Test
    void persistedSessionJsonContainsOnlyBodyRefMetadataAndScrubsSecretHeaders() throws Exception {
        Path sessionPath = temp.resolve("session.json");
        CaptureSessionStore store = new CaptureSessionStore(sessionPath);
        store.start(CaptureSession.start("network-session", CaptureFixtures.STARTED, CaptureFixtures.browser()));

        WebDriver driver = devToolsDriver("https://example.test/app");
        NetworkCaptureOptions options = new NetworkCaptureOptions(true, List.of(), List.of(), false, 500, 0);

        AtomicReference<Filter> filterRef = new AtomicReference<>();
        try (MockedConstruction<NetworkInterceptor> ignored = mockInterceptor(filterRef)) {
            CaptureNetworkRecorder recorder = new CaptureNetworkRecorder(
                    driver, temp.resolve("bodies"), options, "network-session",
                    transaction -> store.append(toNetworkEvent(store, transaction)),
                    warnings::add);
            assertTrue(recorder.start());

            HttpRequest request = new HttpRequest(HttpMethod.POST, "https://example.test/api/login");
            request.addHeader("Authorization", "Bearer super-secret-token-value");
            request.setContent(Contents.utf8String("{\"password\":\"raw-secret-password\"}"));

            HttpHandler next = req -> {
                HttpResponse response = new HttpResponse().setStatus(200)
                        .addHeader("Set-Cookie", "session=raw-secret-cookie");
                response.setContent(Contents.utf8String("{\"token\":\"raw-secret-response-token\"}"));
                return response;
            };
            filterRef.get().apply(next).execute(request);
            recorder.close();
        }

        store.stop(CaptureFixtures.STARTED.plusSeconds(1));
        String json = Files.readString(sessionPath);

        assertTrue(json.contains("\"network\""), json);
        assertTrue(json.contains("\"ref\""), json);
        assertTrue(json.contains("\"request\""), json);
        assertTrue(json.contains("\"response\""), json);

        // No raw sensitive header values leaked into the persisted session.
        assertFalse(json.contains("super-secret-token-value"), json);
        assertFalse(json.contains("raw-secret-cookie"), json);
        // No inline bodies leaked into the persisted session; only BodyRef metadata.
        assertFalse(json.contains("raw-secret-password"), json);
        assertFalse(json.contains("raw-secret-response-token"), json);

        // The session must still be readable and schema-valid.
        CaptureSession reread = new CaptureJsonCodec().read(sessionPath);
        assertTrue(reread.events().stream().anyMatch(event -> event instanceof CaptureEvent.NetworkEvent));
    }

    private static CaptureEvent.NetworkEvent toNetworkEvent(
            CaptureSessionStore store, CaptureNetworkRecorder.RecordedTransaction transaction) {
        long sequence = store.nextSequence();
        PageContext page = new PageContext(transaction.initiatorPageUrl(), "", "window-1", List.of(), 0, 0);
        EventContext context = new EventContext(
                sequence, Instant.now(), page, EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        return new CaptureEvent.NetworkEvent(
                context,
                transaction.transactionId(),
                transaction.resourceKind(),
                transaction.request(),
                transaction.response(),
                transaction.timing(),
                transaction.failureReason(),
                transaction.initiatorPageUrl(),
                null);
    }

    private static WebDriver devToolsDriver(String currentUrl) {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        Mockito.when(driver.getCurrentUrl()).thenReturn(currentUrl);
        return driver;
    }

    private static MockedConstruction<NetworkInterceptor> mockInterceptor(AtomicReference<Filter> filterRef) {
        return Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterRef.set((Filter) context.arguments().get(1)));
    }
}
