package com.shaft.test.unitTests;

import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.LongSupplier;

/**
 * Unit tests for {@code BidiNetworkActivitySource} (issue #3749, Increment B).
 * <p>
 * Exercises the pure in-flight/aging/marker state machine and the construction-failure fallback
 * path without a real browser, following the same reflective-access idiom as
 * {@link JavaScriptWaitManagerUnitTest} for package-private members.
 */
public class BidiNetworkActivitySourceUnitTest {
    private static final String CLASS_NAME = "com.shaft.gui.browser.internal.BidiNetworkActivitySource";

    private static Object newSourceWithClock(long[] currentNanos) throws Exception {
        Constructor<?> constructor = Class.forName(CLASS_NAME).getDeclaredConstructor(LongSupplier.class);
        constructor.setAccessible(true);
        LongSupplier clock = () -> currentNanos[0];
        return constructor.newInstance(clock);
    }

    private static void invokeVoid(Object target, String methodName, Class<?>[] paramTypes, Object... args) throws Exception {
        Method method = Class.forName(CLASS_NAME).getDeclaredMethod(methodName, paramTypes);
        method.setAccessible(true);
        method.invoke(target, args);
    }

    private static <T> T invoke(Object target, String methodName, Class<?>[] paramTypes, Object... args) throws Exception {
        Method method = Class.forName(CLASS_NAME).getDeclaredMethod(methodName, paramTypes);
        method.setAccessible(true);
        @SuppressWarnings("unchecked")
        T result = (T) method.invoke(target, args);
        return result;
    }

    private static int inFlightCount(Object source) throws Exception {
        return invoke(source, "inFlightCount", new Class<?>[0]);
    }

    private static String activityMarker(Object source) throws Exception {
        return invoke(source, "activityMarker", new Class<?>[0]);
    }

    private static boolean healthy(Object source) throws Exception {
        return invoke(source, "healthy", new Class<?>[0]);
    }

    private static void recordRequestStart(Object source, String requestId, boolean excludeFromInFlight) throws Exception {
        invokeVoid(source, "recordRequestStart", new Class<?>[]{String.class, boolean.class}, requestId, excludeFromInFlight);
    }

    private static void recordRequestEnd(Object source, String requestId) throws Exception {
        invokeVoid(source, "recordRequestEnd", new Class<?>[]{String.class}, requestId);
    }

    // --- pure in-flight tracking ---

    @Test(description = "Verify a started request is counted as in-flight")
    public void testRecordRequestStartIncrementsInFlightCount() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, "req-1", false);

        Assert.assertEquals(inFlightCount(source), 1, "Started request should be counted as in-flight");
    }

    @Test(description = "Verify a completed request is removed from the in-flight count")
    public void testRecordRequestEndRemovesFromInFlightCount() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, "req-1", false);
        recordRequestEnd(source, "req-1");

        Assert.assertEquals(inFlightCount(source), 0, "Completed request should no longer be counted as in-flight");
    }

    @Test(description = "Verify multiple concurrent requests are all counted until they individually complete")
    public void testMultipleInFlightRequestsTrackedIndependently() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, "req-1", false);
        recordRequestStart(source, "req-2", false);
        recordRequestStart(source, "req-3", false);
        Assert.assertEquals(inFlightCount(source), 3, "All three started requests should be in-flight");

        recordRequestEnd(source, "req-2");
        Assert.assertEquals(inFlightCount(source), 2, "Only the completed request should be removed");
    }

    @Test(description = "Verify a request excluded as a long-lived upgrade is never counted as in-flight")
    public void testExcludedLongLivedRequestNeverCountedInFlight() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, "sse-1", true);

        Assert.assertEquals(inFlightCount(source), 0,
                "SSE/WebSocket-upgrade requests must be excluded from the advisory in-flight count");
    }

    @Test(description = "Verify a null requestId is a safe no-op for start and end")
    public void testNullRequestIdIsNoOp() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, null, false);
        recordRequestEnd(source, null);

        Assert.assertEquals(inFlightCount(source), 0, "Null request id must never be tracked");
    }

    // --- advisory age-out ---

    @Test(description = "Verify an in-flight request that never completes ages out after the bounded threshold")
    public void testInFlightRequestAgesOutAfterThreshold() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);
        long ageOutNanos = ((Duration) getStaticField("IN_FLIGHT_AGE_OUT_WINDOW")).toNanos();

        recordRequestStart(source, "sse-1", false);
        Assert.assertEquals(inFlightCount(source), 1, "Request should be counted immediately after starting");

        nanos[0] = ageOutNanos - 1;
        Assert.assertEquals(inFlightCount(source), 1, "Request must still be counted just under the age-out threshold");

        nanos[0] = ageOutNanos;
        Assert.assertEquals(inFlightCount(source), 0, "Request must age out once the threshold elapses without completion");
    }

    @Test(description = "Verify age-out never removes a request that is still fresh")
    public void testFreshInFlightRequestDoesNotAgeOutEarly() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        recordRequestStart(source, "req-1", false);
        nanos[0] = Duration.ofSeconds(1).toNanos();

        Assert.assertEquals(inFlightCount(source), 1, "A one-second-old request must not age out under the 10s threshold");
    }

    // --- activity marker (sequence) semantics ---

    @Test(description = "Verify the activity marker changes on every beforeRequestSent/responseCompleted/fetchError-equivalent event")
    public void testActivityMarkerChangesOnEveryEvent() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);
        String initialMarker = activityMarker(source);

        bumpSequence(source);
        String afterFirstEvent = activityMarker(source);
        bumpSequence(source);
        String afterSecondEvent = activityMarker(source);

        Assert.assertNotEquals(afterFirstEvent, initialMarker, "Marker must change after an observed event");
        Assert.assertNotEquals(afterSecondEvent, afterFirstEvent, "Marker must change again after a second observed event");
    }

    @Test(description = "Verify the activity marker is stable when no event has been observed")
    public void testActivityMarkerStableWithoutEvents() throws Exception {
        long[] nanos = {0L};
        Object source = newSourceWithClock(nanos);

        String firstRead = activityMarker(source);
        String secondRead = activityMarker(source);

        Assert.assertEquals(secondRead, firstRead, "Marker must not change between reads without an intervening event");
    }

    // --- construction-failure fallback (no browser required) ---

    @Test(description = "Verify a driver that does not support BiDi leaves the source unhealthy, never throwing")
    public void testConstructionFailureMarksSourceUnhealthy() throws Exception {
        WebDriver nonBidiDriver = Mockito.mock(WebDriver.class);

        Constructor<?> constructor = Class.forName(CLASS_NAME)
                .getDeclaredConstructor(WebDriver.class, LongSupplier.class);
        constructor.setAccessible(true);
        Object source;
        try {
            source = constructor.newInstance(nonBidiDriver, (LongSupplier) System::nanoTime);
        } catch (InvocationTargetException e) {
            throw new AssertionError("Construction must never propagate an exception for a non-BiDi driver", e.getCause());
        }

        Assert.assertFalse(healthy(source), "A driver that does not implement HasBiDi must leave the source unhealthy");
    }

    @Test(description = "Verify forDriver() returns null when BiDi is disabled via SHAFT.Properties.platform.enableBiDi()")
    public void testForDriverReturnsNullWhenBiDiDisabled() throws Exception {
        boolean original = com.shaft.driver.SHAFT.Properties.platform.enableBiDi();
        try {
            com.shaft.driver.SHAFT.Properties.platform.set().enableBiDi(false);
            WebDriver driver = Mockito.mock(WebDriver.class);
            Method forDriver = Class.forName(CLASS_NAME).getDeclaredMethod("forDriver", WebDriver.class);
            forDriver.setAccessible(true);

            Object result = forDriver.invoke(null, driver);

            Assert.assertNull(result, "forDriver() must return null when enableBiDi is false");
        } finally {
            com.shaft.driver.SHAFT.Properties.platform.set().enableBiDi(original);
        }
    }

    @Test(description = "Verify forDriver() returns null for a null driver")
    public void testForDriverReturnsNullForNullDriver() throws Exception {
        Method forDriver = Class.forName(CLASS_NAME).getDeclaredMethod("forDriver", WebDriver.class);
        forDriver.setAccessible(true);

        Object result = forDriver.invoke(null, (Object) null);

        Assert.assertNull(result, "forDriver() must return null for a null driver");
    }

    @Test(description = "Verify closeAndRemove() is a safe no-op for a driver with no cached source")
    public void testCloseAndRemoveNoOpForUnknownDriver() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Method closeAndRemove = Class.forName(CLASS_NAME).getDeclaredMethod("closeAndRemove", WebDriver.class);
        closeAndRemove.setAccessible(true);

        // must not throw
        closeAndRemove.invoke(null, driver);
    }

    @Test(description = "Verify forDriver() caches: repeated calls for the same driver return the same instance")
    public void testForDriverCachesPerDriver() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Method forDriver = Class.forName(CLASS_NAME).getDeclaredMethod("forDriver", WebDriver.class);
        forDriver.setAccessible(true);
        Method closeAndRemove = Class.forName(CLASS_NAME).getDeclaredMethod("closeAndRemove", WebDriver.class);
        closeAndRemove.setAccessible(true);
        try {
            Object first = forDriver.invoke(null, driver);
            Object second = forDriver.invoke(null, driver);

            Assert.assertSame(second, first, "forDriver() must return the cached instance for the same driver, never construct twice");
        } finally {
            closeAndRemove.invoke(null, driver);
        }
    }

    // --- isLongLivedUpgrade header detection (RequestData built directly, not via a fake browser) ---

    @Test(description = "Verify a WebSocket upgrade request (Upgrade: websocket header) is detected as long-lived")
    public void testWebSocketUpgradeHeaderDetected() throws Exception {
        Object request = buildRequestData(Map.of("Upgrade", "websocket"));
        boolean result = invoke(null, "isLongLivedUpgrade",
                new Class<?>[]{Class.forName("org.openqa.selenium.bidi.network.RequestData")}, request);
        Assert.assertTrue(result, "A request with an Upgrade: websocket header must be detected as long-lived");
    }

    @Test(description = "Verify an EventSource request (Accept: text/event-stream header) is detected as long-lived")
    public void testEventSourceAcceptHeaderDetected() throws Exception {
        Object request = buildRequestData(Map.of("Accept", "text/event-stream"));
        boolean result = invoke(null, "isLongLivedUpgrade",
                new Class<?>[]{Class.forName("org.openqa.selenium.bidi.network.RequestData")}, request);
        Assert.assertTrue(result, "A request with an Accept: text/event-stream header must be detected as long-lived");
    }

    @Test(description = "Verify an ordinary request is not detected as long-lived")
    public void testOrdinaryRequestNotDetectedAsLongLived() throws Exception {
        Object request = buildRequestData(Map.of("Accept", "application/json"));
        boolean result = invoke(null, "isLongLivedUpgrade",
                new Class<?>[]{Class.forName("org.openqa.selenium.bidi.network.RequestData")}, request);
        Assert.assertFalse(result, "An ordinary JSON-accepting request must not be detected as long-lived");
    }

    @Test(description = "Verify a null request is not detected as long-lived")
    public void testNullRequestNotDetectedAsLongLived() throws Exception {
        boolean result = invoke(null, "isLongLivedUpgrade",
                new Class<?>[]{Class.forName("org.openqa.selenium.bidi.network.RequestData")}, (Object) null);
        Assert.assertFalse(result, "A null request must not be detected as long-lived");
    }

    private static Object buildRequestData(Map<String, String> headerNameToValue) throws Exception {
        Class<?> headerClass = Class.forName("org.openqa.selenium.bidi.network.Header");
        Class<?> bytesValueClass = Class.forName("org.openqa.selenium.bidi.network.BytesValue");
        Class<?> bytesValueTypeClass = Class.forName("org.openqa.selenium.bidi.network.BytesValue$Type");
        Object stringType = bytesValueTypeClass.getEnumConstants()[0];
        for (Object constant : bytesValueTypeClass.getEnumConstants()) {
            if (((Enum<?>) constant).name().toUpperCase(java.util.Locale.ROOT).contains("STRING")) {
                stringType = constant;
                break;
            }
        }
        Constructor<?> bytesValueConstructor = bytesValueClass.getDeclaredConstructor(bytesValueTypeClass, String.class);
        bytesValueConstructor.setAccessible(true);
        Constructor<?> headerConstructor = headerClass.getDeclaredConstructor(String.class, bytesValueClass);
        headerConstructor.setAccessible(true);

        java.util.List<Object> headers = new java.util.ArrayList<>();
        for (Map.Entry<String, String> entry : headerNameToValue.entrySet()) {
            Object bytesValue = bytesValueConstructor.newInstance(stringType, entry.getValue());
            headers.add(headerConstructor.newInstance(entry.getKey(), bytesValue));
        }

        Class<?> requestDataClass = Class.forName("org.openqa.selenium.bidi.network.RequestData");
        Class<?> fetchTimingInfoClass = Class.forName("org.openqa.selenium.bidi.network.FetchTimingInfo");
        Constructor<?> requestDataConstructor = requestDataClass.getDeclaredConstructor(
                String.class, String.class, String.class, java.util.List.class, java.util.List.class,
                long.class, Long.class, String.class, String.class, fetchTimingInfoClass);
        requestDataConstructor.setAccessible(true);
        return requestDataConstructor.newInstance(
                "req-1", "https://example.test/stream", "GET", headers, java.util.List.of(),
                0L, null, "document", "other", null);
    }

    private static void bumpSequence(Object source) throws Exception {
        Field field = Class.forName(CLASS_NAME).getDeclaredField("activitySequence");
        field.setAccessible(true);
        ((AtomicLong) field.get(source)).incrementAndGet();
    }

    private static Object getStaticField(String name) throws Exception {
        Field field = Class.forName(CLASS_NAME).getDeclaredField(name);
        field.setAccessible(true);
        return field.get(null);
    }
}
