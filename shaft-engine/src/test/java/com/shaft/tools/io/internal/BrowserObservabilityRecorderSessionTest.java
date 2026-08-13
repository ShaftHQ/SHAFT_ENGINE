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

    private static TestExecutionInfo info(String method) {
        return new TestExecutionInfo("observability-" + method, BrowserObservabilityRecorderSessionTest.class.getName(),
                method, method, method, null, null, false);
    }
}
