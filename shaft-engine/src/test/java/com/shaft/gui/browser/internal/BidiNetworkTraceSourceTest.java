package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.mockito.Mockito;
import org.openqa.selenium.bidi.network.BeforeRequestSent;
import org.openqa.selenium.bidi.network.BytesValue;
import org.openqa.selenium.bidi.network.Header;
import org.openqa.selenium.bidi.network.RequestData;
import org.openqa.selenium.bidi.network.ResponseData;
import org.openqa.selenium.bidi.network.ResponseDetails;
import org.openqa.selenium.bidi.network.FetchError;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.lang.reflect.Constructor;

public class BidiNetworkTraceSourceTest {
    @AfterMethod
    public void clearState() {
        BrowserObservabilityRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void bidiRequestCompletionShouldPublishBoundedMetadataToCapturedOwner() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        long[] nanos = {1_000L};
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(() -> nanos[0]);
        RequestData request = request("request-1", "POST", "https://example.test/orders", 12L,
                List.of(header("Authorization", "Bearer private")));
        BeforeRequestSent before = Mockito.mock(BeforeRequestSent.class);
        Mockito.when(before.getRequest()).thenReturn(request);
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getStatus()).thenReturn(201);
        Mockito.when(response.getHeaders()).thenReturn(List.of(header("Content-Type", "application/json")));
        Mockito.when(response.getBodySize()).thenReturn(34L);
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleBeforeRequestSent(before);
        nanos[0] += 5_000_000L;
        source.handleResponseCompleted(completed);

        var events = BrowserObservabilityRecorder.snapshot(owner);
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().method(), "POST");
        Assert.assertEquals(events.getFirst().url(), "https://example.test/orders");
        Assert.assertEquals(events.getFirst().status(), 201);
        Assert.assertEquals(events.getFirst().durationMs(), 5L);
        Assert.assertEquals(events.getFirst().requestSizeBytes(), 12L);
        Assert.assertEquals(events.getFirst().responseSizeBytes(), 34L);
        Assert.assertEquals(events.getFirst().requestHeaders().get("Authorization"), "********");
        Assert.assertEquals(events.getFirst().mimeType(), "application/json");
        try {
            var drain = BrowserObservabilityRecorder.class.getDeclaredMethod("drainNetworkJson");
            drain.setAccessible(true);
            String persisted = (String) drain.invoke(null);
            Assert.assertTrue(persisted.contains("\"provider\": \"bidi\""), persisted);
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
    }

    @Test
    public void lateBidiCompletionShouldNotCrossReportSessionRollover() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession first = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        RequestData request = request("late", "GET", "https://example.test/late", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        BrowserObservabilityRecorder.ObservationSession second = BrowserObservabilityRecorder.startSession();
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(first).isEmpty());
        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(second).isEmpty());
    }

    @Test
    public void longLivedUpgradeAndFetchFailureShouldRemainExplicitAndPayloadFree() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        RequestData upgrade = request("socket", "GET", "wss://example.test/socket", 0L,
                List.of(header("Upgrade", "websocket")));
        source.handleBeforeRequestSent(before(upgrade));
        RequestData failed = request("failed", "GET", "https://example.test/fail", 0L, List.of());
        source.handleBeforeRequestSent(before(failed));
        FetchError error = Mockito.mock(FetchError.class);
        Mockito.when(error.getRequest()).thenReturn(failed);
        Mockito.when(error.getErrorText()).thenReturn("provider secret parser detail");
        source.handleFetchError(error);

        var events = BrowserObservabilityRecorder.snapshot(owner);
        Assert.assertEquals(events.size(), 2);
        Assert.assertTrue(events.get(0).failureReason().contains("long-lived"));
        Assert.assertEquals(events.get(0).status(), 0);
        Assert.assertEquals(events.get(1).failureReason(), "BiDi request failed.");
        Assert.assertFalse(events.get(1).failureReason().contains("provider secret"));
        Assert.assertTrue(events.stream().allMatch(event -> event.bodyPreview().isEmpty()));
    }

    @Test
    public void incompleteBidiRequestsShouldAgeOutIntoExplicitBoundedEvidence() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        long[] nanos = {0L};
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(() -> nanos[0]);
        source.handleBeforeRequestSent(before(request(
                "stale", "GET", "https://example.test/stale", 0L, List.of())));

        nanos[0] = BidiNetworkActivitySource.IN_FLIGHT_AGE_OUT_WINDOW.toNanos();
        source.inFlightCount();

        var events = BrowserObservabilityRecorder.snapshot(owner);
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().url(), "https://example.test/stale");
        Assert.assertTrue(events.getFirst().failureReason().contains("age-out"));
    }

    @Test
    public void detailedInterceptorShouldSuppressDuplicateBidiMetadata() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        EqualWebDriver driver = new EqualWebDriver();
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver, (ignored, filter) -> () -> { });
        Assert.assertTrue(interceptor.startObserving());
        Constructor<BidiNetworkActivitySource> constructor = BidiNetworkActivitySource.class
                .getDeclaredConstructor(org.openqa.selenium.WebDriver.class, java.util.function.LongSupplier.class);
        constructor.setAccessible(true);
        BidiNetworkActivitySource source = constructor.newInstance(driver, (java.util.function.LongSupplier) System::nanoTime);
        RequestData request = request("duplicate", "GET", "https://example.test/duplicate", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(owner).isEmpty());
        source.close();
        interceptor.close();
    }

    @Test
    public void orphanCompletionShouldNeverSynthesizeARequestOwner() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        RequestData request = request("orphan", "GET", "https://example.test/orphan", 0L, List.of());
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(owner).isEmpty());
    }

    @Test
    public void bidiInflightTraceMapShouldRejectLimitPlusOneWithOneWarning() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        for (int index = 0; index <= 1_000; index++) {
            source.handleBeforeRequestSent(before(request(
                    "request-" + index, "GET", "https://example.test/" + index, 0L, List.of())));
        }

        Assert.assertEquals(source.retainedTraceRequestCount(), 1_000);
        Assert.assertEquals(source.inFlightCount(), 1_000);
        try {
            var field = BidiNetworkActivitySource.class.getDeclaredField("traceRequests");
            field.setAccessible(true);
            var retained = (java.util.Map<?, ?>) field.get(source);
            Assert.assertTrue(retained.containsKey("request-999"));
            Assert.assertFalse(retained.containsKey("request-1000"));
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        RequestData completedRequest = request(
                "request-0", "GET", "https://example.test/0", 0L, List.of());
        Mockito.when(completed.getRequest()).thenReturn(completedRequest);
        Mockito.when(completed.getResponseData()).thenReturn(response);
        source.handleResponseCompleted(completed);
        Assert.assertEquals(source.retainedTraceRequestCount(), 999);
        source.handleBeforeRequestSent(before(request(
                "replacement", "GET", "https://example.test/replacement", 0L, List.of())));
        Assert.assertEquals(source.retainedTraceRequestCount(), 1_000);

        long warnings = BrowserObservabilityRecorder.drainWarnings(owner).stream()
                .filter(value -> value.contains("in-flight trace limit")).count();
        Assert.assertEquals(warnings, 1L);
    }

    @Test
    public void stoppedDetailedInterceptorShouldAllowBidiFallbackMetadata() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        EqualWebDriver driver = new EqualWebDriver();
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver, (ignored, filter) -> () -> { });
        Assert.assertTrue(interceptor.startObserving());
        interceptor.stopObserving();
        Constructor<BidiNetworkActivitySource> constructor = BidiNetworkActivitySource.class
                .getDeclaredConstructor(org.openqa.selenium.WebDriver.class, java.util.function.LongSupplier.class);
        constructor.setAccessible(true);
        BidiNetworkActivitySource source = constructor.newInstance(driver, (java.util.function.LongSupplier) System::nanoTime);
        RequestData request = request("fallback", "GET", "https://example.test/fallback", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).size(), 1);
        source.close();
        interceptor.close();
    }

    @Test
    public void closedSourceShouldRejectLateRequestStartAndCompletion() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        source.close();
        BrowserObservabilityRecorder.ObservationSession next = BrowserObservabilityRecorder.startSession();
        RequestData request = request("closed", "GET", "https://example.test/closed", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertTrue(BrowserObservabilityRecorder.snapshot(next).isEmpty());
    }

    @Test
    public void bidiMetadataShouldRedactThenBoundLargeFieldsAndHeaders() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        List<Header> headers = new java.util.ArrayList<>();
        headers.add(header("X-Secret", "x".repeat(3_000)));
        for (int index = 0; index < 100; index++) {
            headers.add(header("X-Header-" + index, "v".repeat(300)));
        }
        RequestData request = request("bounded", "POST", "https://example.test/" + "u".repeat(5_000), 0L, headers);
        source.handleBeforeRequestSent(before(request));
        try {
            var field = BidiNetworkActivitySource.class.getDeclaredField("traceRequests");
            field.setAccessible(true);
            Object retainedRequest = ((java.util.Map<?, ?>) field.get(source)).values().iterator().next();
            var retained = retainedRequest.toString();
            Assert.assertFalse(retained.contains("u".repeat(2_049)), retained.length() + "");
            Assert.assertFalse(retained.contains("x".repeat(100)), retained);
            Assert.assertTrue(retained.contains("********"), retained);
            var headersMethod = retainedRequest.getClass().getDeclaredMethod("requestHeaders");
            headersMethod.setAccessible(true);
            var retainedHeaders = (java.util.Map<?, ?>) headersMethod.invoke(retainedRequest);
            Assert.assertTrue(retainedHeaders.size() <= 64, String.valueOf(retainedHeaders.size()));
            long retainedCharacters = retainedHeaders.entrySet().stream()
                    .mapToLong(entry -> entry.getKey().toString().length() + entry.getValue().toString().length())
                    .sum();
            Assert.assertTrue(retainedCharacters <= 8_192, String.valueOf(retainedCharacters));
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(headers);
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);
        source.handleResponseCompleted(completed);

        var event = BrowserObservabilityRecorder.snapshot(owner).getFirst();
        Assert.assertTrue(event.url().length() <= 2_048, String.valueOf(event.url().length()));
        Assert.assertTrue(event.requestHeaders().size() <= 64, String.valueOf(event.requestHeaders().size()));
        Assert.assertTrue(event.responseHeaders().size() <= 64, String.valueOf(event.responseHeaders().size()));
        Assert.assertEquals(event.requestHeaders().get("X-Secret"), "********");
        Assert.assertTrue(event.requestHeaders().values().stream().allMatch(value -> value.length() <= 2_048));
    }

    @Test
    public void bidiOwnedRequestShouldCompleteAfterDetailedInterceptorStarts() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        EqualWebDriver driver = new EqualWebDriver();
        Constructor<BidiNetworkActivitySource> constructor = BidiNetworkActivitySource.class
                .getDeclaredConstructor(org.openqa.selenium.WebDriver.class, java.util.function.LongSupplier.class);
        constructor.setAccessible(true);
        BidiNetworkActivitySource source = constructor.newInstance(driver, (java.util.function.LongSupplier) System::nanoTime);
        RequestData request = request("transition", "GET", "https://example.test/transition", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver, (ignored, filter) -> () -> { });
        Assert.assertTrue(interceptor.startObserving());
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);

        source.handleResponseCompleted(completed);

        Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).size(), 1);
        source.close();
        interceptor.close();
    }

    @Test
    public void sensitiveHeaderSuffixBeyondNameLimitShouldRemainRedacted() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        String name = "X".repeat(2_048) + "Authorization";
        RequestData request = request("suffix", "GET", "https://example.test", 0L,
                List.of(header(name, "private-value")));
        source.handleBeforeRequestSent(before(request));
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);
        source.handleResponseCompleted(completed);

        Assert.assertEquals(BrowserObservabilityRecorder.snapshot(owner).getFirst().requestHeaders().values()
                .iterator().next(), "********");
    }

    @Test
    public void oversizedResponseHeadersShouldEmitOneExplicitTruncationWarning() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        RequestData request = request("response-bound", "GET", "https://example.test", 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        List<Header> responseHeaders = new java.util.ArrayList<>();
        for (int index = 0; index < 100; index++) {
            responseHeaders.add(header("Response-" + index, "v".repeat(300)));
        }
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(responseHeaders);
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);
        source.handleResponseCompleted(completed);

        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(owner).stream()
                .filter(value -> value.contains("metadata was truncated")).count(), 1L);
    }

    @Test
    public void metadataLimitWarningShouldRepeatOnceAfterSessionRollover() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession first = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(System::nanoTime);
        publishOversizedMetadata(source, "first");
        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(first).stream()
                .filter(value -> value.contains("metadata was truncated")).count(), 1L);

        BrowserObservabilityRecorder.ObservationSession second = BrowserObservabilityRecorder.startSession();
        publishOversizedMetadata(source, "second");

        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(second).stream()
                .filter(value -> value.contains("metadata was truncated")).count(), 1L);
    }

    @Test
    public void requestLimitWarningShouldRepeatOnceAfterSessionRollover() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        long[] nanos = {0L};
        BrowserObservabilityRecorder.ObservationSession first = BrowserObservabilityRecorder.startSession();
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(() -> nanos[0]);
        fillRequestLimit(source, "first-");
        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(first).stream()
                .filter(value -> value.contains("in-flight trace limit")).count(), 1L);
        nanos[0] = BidiNetworkActivitySource.IN_FLIGHT_AGE_OUT_WINDOW.toNanos() + 1;
        source.inFlightCount();

        BrowserObservabilityRecorder.ObservationSession second = BrowserObservabilityRecorder.startSession();
        fillRequestLimit(source, "second-");

        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(second).stream()
                .filter(value -> value.contains("in-flight trace limit")).count(), 1L);
    }

    private static void publishOversizedMetadata(BidiNetworkActivitySource source, String id) {
        RequestData request = request(id, "GET", "https://example.test/" + "u".repeat(3_000), 0L, List.of());
        source.handleBeforeRequestSent(before(request));
        ResponseData response = Mockito.mock(ResponseData.class);
        Mockito.when(response.getHeaders()).thenReturn(List.of());
        ResponseDetails completed = Mockito.mock(ResponseDetails.class);
        Mockito.when(completed.getRequest()).thenReturn(request);
        Mockito.when(completed.getResponseData()).thenReturn(response);
        source.handleResponseCompleted(completed);
    }

    private static void fillRequestLimit(BidiNetworkActivitySource source, String prefix) {
        for (int index = 0; index <= 1_000; index++) {
            source.handleBeforeRequestSent(before(request(
                    prefix + index, "GET", "https://example.test/" + index, 0L, List.of())));
        }
    }

    private static BeforeRequestSent before(RequestData request) {
        BeforeRequestSent event = Mockito.mock(BeforeRequestSent.class);
        Mockito.when(event.getRequest()).thenReturn(request);
        return event;
    }

    private static RequestData request(String id, String method, String url, long bodySize, List<Header> headers) {
        RequestData request = Mockito.mock(RequestData.class);
        Mockito.when(request.getRequestId()).thenReturn(id);
        Mockito.when(request.getMethod()).thenReturn(method);
        Mockito.when(request.getUrl()).thenReturn(url);
        Mockito.when(request.getBodySize()).thenReturn(bodySize);
        Mockito.when(request.getHeaders()).thenReturn(headers);
        return request;
    }

    private static Header header(String name, String value) {
        return new Header(name, new BytesValue(BytesValue.Type.STRING, value));
    }
}
