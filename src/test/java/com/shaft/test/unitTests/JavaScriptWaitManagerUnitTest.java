package com.shaft.test.unitTests;

import com.shaft.tools.internal.support.JavaScriptHelper;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;

/**
 * Unit tests for JavaScriptWaitManager and JavaScriptHelper.
 * Validates the JavaScript snippets used for network and framework readiness checks
 * without requiring a running browser.
 */
public class JavaScriptWaitManagerUnitTest {
    private static long getIdleWindowNotStartedMarker() throws Exception {
        var field = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                .getDeclaredField("IDLE_WINDOW_NOT_STARTED");
        field.setAccessible(true);
        return field.getLong(null);
    }

    private static Method getHasMetMinimumIdleWindowMethod() throws Exception {
        Method method = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                .getDeclaredMethod("hasMetMinimumIdleWindow", long.class, long[].class, boolean[].class, long.class);
        method.setAccessible(true);
        return method;
    }

    private static boolean hasMetMinimumIdleWindow(Method method, long activeRequests, long[] idleSinceMillis,
                                                  boolean[] networkActivityObserved, long nowMillis) throws Exception {
        return (boolean) method.invoke(null, activeRequests, idleSinceMillis, networkActivityObserved, nowMillis);
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script is defined and non-empty")
    public void testActiveNetworkRequestsCountScriptIsDefined() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertNotNull(script, "Script should not be null");
        Assert.assertFalse(script.isBlank(), "Script should not be blank");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script instruments XHR and fetch")
    public void testActiveNetworkRequestsCountScriptInstrumentsXHRAndFetch() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("XMLHttpRequest"),
                "Script should reference XMLHttpRequest to track AJAX requests");
        Assert.assertTrue(script.contains("fetch"),
                "Script should reference the fetch API to track fetch requests");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script is idempotent via tracker flag")
    public void testActiveNetworkRequestsCountScriptIsIdempotent() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("_shaftNetworkTracker"),
                "Script should check a tracker flag to prevent double instrumentation");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script protects against negative counts")
    public void testActiveNetworkRequestsCountScriptProtectsAgainstNegativeCounts() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("Math.max(0,"),
                "Script should use Math.max(0, ...) to prevent the counter from going negative");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script patches XMLHttpRequest.prototype.open")
    public void testActiveNetworkRequestsCountScriptPatchesXHROpen() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("XMLHttpRequest.prototype.open"),
                "Script should patch XMLHttpRequest.prototype.open to intercept XHR calls");
        Assert.assertTrue(script.contains("loadend"),
                "Script should listen for loadend to finalize XHR tracking");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script is defined and checks Angular 2+ testabilities")
    public void testAngular2ReadyStateScriptIsDefined() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertNotNull(script, "Angular 2+ readiness script should not be null");
        Assert.assertFalse(script.isBlank(), "Angular 2+ readiness script should not be blank");
        Assert.assertTrue(script.contains("getAllAngularTestabilities"),
                "Script should use getAllAngularTestabilities for Angular 2+ support");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script returns 0 when Angular 2+ is absent")
    public void testAngular2ReadyStateScriptFallsBackToZeroWhenAngularAbsent() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertTrue(script.contains("return 0"),
                "Script should return 0 (stable) when Angular 2+ is not present on the page");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script checks zone stability")
    public void testAngular2ReadyStateScriptChecksZoneStability() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertTrue(script.contains("isStable"),
                "Script should call isStable() on each Angular testability to check for pending async tasks");
    }

    @Test(description = "Verify legacy ANGULAR_READY_STATE script is still present for AngularJS 1.x support")
    public void testLegacyAngularReadyStateScriptIsPresent() {
        String script = JavaScriptHelper.ANGULAR_READY_STATE.getValue();
        Assert.assertNotNull(script, "Legacy AngularJS 1.x script should not be null");
        Assert.assertTrue(script.contains("angular"),
                "Legacy script should reference the AngularJS global");
    }

    @Test(description = "Verify DOCUMENT_READY_STATE script checks document.readyState")
    public void testDocumentReadyStateScriptChecksReadyState() {
        String script = JavaScriptHelper.DOCUMENT_READY_STATE.getValue();
        Assert.assertNotNull(script, "DOCUMENT_READY_STATE script should not be null");
        Assert.assertTrue(script.contains("readyState"),
                "Script should check document.readyState");
    }

    @Test(description = "Verify JQUERY_ACTIVE_STATE script checks jQuery.active count")
    public void testJQueryActiveStateScriptChecksActiveCount() {
        String script = JavaScriptHelper.JQUERY_ACTIVE_STATE.getValue();
        Assert.assertNotNull(script, "JQUERY_ACTIVE_STATE script should not be null");
        Assert.assertTrue(script.contains("jQuery"),
                "Script should reference jQuery to check for active AJAX requests");
    }

    @Test(description = "Verify idle window check uses a short observation window when there were no network requests")
    public void testShortObservationWindowWhenIdle() throws Exception {
        Method method = getHasMetMinimumIdleWindowMethod();
        long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
        boolean[] networkActivityObserved = {false};
        boolean firstIdlePoll = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1000L);
        boolean secondIdlePoll = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1200L);

        Assert.assertFalse(firstIdlePoll, "First zero-request poll should not complete before a follow-up observation");
        Assert.assertTrue(secondIdlePoll, "No network activity should only wait for the short initial observation window");
        Assert.assertFalse(networkActivityObserved[0], "No-activity path should not be marked as activity-observed");
    }

    @Test(description = "Verify deferred network activity after an idle first poll is still synchronized")
    public void testDeferredNetworkActivityDetection() throws Exception {
        Method method = getHasMetMinimumIdleWindowMethod();
        long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
        boolean[] networkActivityObserved = {false};
        boolean firstIdlePoll = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1000L);
        boolean deferredActivityPoll = hasMetMinimumIdleWindow(method, 1L, idleSinceMillis, networkActivityObserved, 1200L);
        boolean firstIdlePollAfterActivity = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1400L);
        boolean beforeQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1800L);
        boolean afterQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1900L);

        Assert.assertFalse(firstIdlePoll, "First zero-request poll should not bypass deferred network startup");
        Assert.assertFalse(deferredActivityPoll, "Should not pass while deferred requests are in flight");
        Assert.assertFalse(firstIdlePollAfterActivity, "First idle poll after deferred activity should start the quiet window");
        Assert.assertFalse(beforeQuietWindowEnds, "Should wait until the full quiet window elapses after deferred activity");
        Assert.assertTrue(afterQuietWindowEnds, "Should pass once deferred activity is followed by the quiet window");
    }

    @Test(description = "Verify idle window still applies after real network activity is observed")
    public void testIdleWindowAppliesAfterNetworkActivity() throws Exception {
        Method method = getHasMetMinimumIdleWindowMethod();
        long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
        boolean[] networkActivityObserved = {false};
        boolean duringActivity = hasMetMinimumIdleWindow(method, 2L, idleSinceMillis, networkActivityObserved, 1000L);
        boolean firstIdlePoll = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1200L);
        boolean beforeQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1600L);
        boolean afterQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, idleSinceMillis, networkActivityObserved, 1800L);

        Assert.assertFalse(duringActivity, "Should not pass while active requests are still in flight");
        Assert.assertFalse(firstIdlePoll, "First idle poll after network activity should start the quiet window");
        Assert.assertFalse(beforeQuietWindowEnds, "Should wait until the full quiet window elapses");
        Assert.assertTrue(afterQuietWindowEnds, "Should pass once the quiet window has elapsed");
    }
}
