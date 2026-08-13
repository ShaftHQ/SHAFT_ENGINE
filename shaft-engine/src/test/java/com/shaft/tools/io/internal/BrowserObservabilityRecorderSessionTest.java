package com.shaft.tools.io.internal;

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
        }
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

    private static TestExecutionInfo info(String method) {
        return new TestExecutionInfo("observability-" + method, BrowserObservabilityRecorderSessionTest.class.getName(),
                method, method, method, null, null, false);
    }
}
