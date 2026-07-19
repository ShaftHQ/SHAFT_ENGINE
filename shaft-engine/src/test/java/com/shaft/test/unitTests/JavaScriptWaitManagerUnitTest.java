package com.shaft.test.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.JavaScriptHelper;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.time.Duration;

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

    private static Method getHasMetMinimumIdleWindowWithMarkerMethod() throws Exception {
        Method method = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                .getDeclaredMethod("hasMetMinimumIdleWindow", long.class, String.class, long[].class, String[].class,
                        boolean[].class, long.class);
        method.setAccessible(true);
        return method;
    }

    private static boolean hasMetMinimumIdleWindow(Method method, long activeRequests, long[] idleSinceMillis,
                                                  boolean[] networkActivityObserved, long nowMillis) throws Exception {
        return (boolean) method.invoke(null, activeRequests, idleSinceMillis, networkActivityObserved, nowMillis);
    }

    private static boolean hasMetMinimumIdleWindow(Method method, long activeRequests, String networkActivityMarker,
                                                  long[] idleSinceMillis, String[] lastNetworkActivityMarker,
                                                  boolean[] networkActivityObserved, long nowMillis) throws Exception {
        return (boolean) method.invoke(null, activeRequests, networkActivityMarker, idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, nowMillis);
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

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script patches XMLHttpRequest.prototype.send")
    public void testActiveNetworkRequestsCountScriptPatchesXHRSend() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("XMLHttpRequest.prototype.send"),
                "Script should count XHR when requests are actually sent");
        Assert.assertTrue(script.contains("loadend"),
                "Script should listen for loadend to finalize XHR tracking");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script always decrements fetch counters")
    public void testActiveNetworkRequestsCountScriptFinalizesFetchOnAllPaths() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("finally"),
                "Fetch tracking should decrement counters after success or rejection");
        Assert.assertTrue(script.contains("catch"),
                "Fetch tracking should decrement counters when the original fetch throws synchronously");
    }

    @Test(description = "Verify browser readiness script observes resource timing")
    public void testBrowserReadinessStateScriptUsesResourceTiming() {
        String script = JavaScriptHelper.BROWSER_READINESS_STATE.getValue();
        Assert.assertTrue(script.contains("performance.getEntriesByType('resource')"),
                "Readiness state should observe completed resources that started before SHAFT instrumentation");
        Assert.assertTrue(script.contains("networkActivityMarker"),
                "Readiness state should expose a network activity marker for quiet-window resets");
        Assert.assertTrue(script.contains("angularActive === 0 && window.getAllAngularTestabilities"),
                "Readiness state should check Angular 2+ testabilities even when AngularJS is absent");
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

    @Test(description = "Verify resource timing marker changes reset the network quiet window")
    public void testNetworkActivityMarkerChangesResetQuietWindow() throws Exception {
        Method method = getHasMetMinimumIdleWindowWithMarkerMethod();
        long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
        String[] lastNetworkActivityMarker = {null};
        boolean[] networkActivityObserved = {false};

        boolean firstIdlePoll = hasMetMinimumIdleWindow(method, 0L, "0:1:100", idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, 1000L);
        boolean initialObservationPassed = hasMetMinimumIdleWindow(method, 0L, "0:1:100", idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, 1200L);
        boolean markerChanged = hasMetMinimumIdleWindow(method, 0L, "0:2:300", idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, 1300L);
        boolean beforeQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, "0:2:300", idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, 1700L);
        boolean afterQuietWindowEnds = hasMetMinimumIdleWindow(method, 0L, "0:2:300", idleSinceMillis,
                lastNetworkActivityMarker, networkActivityObserved, 1800L);

        Assert.assertFalse(firstIdlePoll, "First zero-request poll should start the initial observation window");
        Assert.assertTrue(initialObservationPassed, "Stable marker should pass the initial observation window");
        Assert.assertFalse(markerChanged, "Resource timing changes should reset the quiet window");
        Assert.assertFalse(beforeQuietWindowEnds, "Observed resource activity should wait for the full quiet window");
        Assert.assertTrue(afterQuietWindowEnds, "Stable marker should pass after the full quiet window");
    }

    // --- Issue #3749 Increment A: polling-interval property consumption ---

    @Test(description = "Verify the readiness fluentWait polling interval is sourced from the lazyLoadingPollingIntervalMillis property")
    public void testPollingIntervalSourcedFromProperty() throws Exception {
        int original = SHAFT.Properties.timeouts.lazyLoadingPollingIntervalMillis();
        try {
            SHAFT.Properties.timeouts.set().lazyLoadingPollingIntervalMillis(350);
            Method method = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                    .getDeclaredMethod("pollingInterval");
            method.setAccessible(true);
            Duration interval = (Duration) method.invoke(null);
            Assert.assertEquals(interval, Duration.ofMillis(350),
                    "Polling interval should reflect the configured lazyLoadingPollingIntervalMillis property value");
        } finally {
            SHAFT.Properties.timeouts.set().lazyLoadingPollingIntervalMillis(original);
        }
    }

    @Test(description = "Verify the readiness fluentWait polling interval falls back to the documented default of 200ms")
    public void testPollingIntervalDefaultsTo200Millis() {
        Assert.assertEquals(SHAFT.Properties.timeouts.lazyLoadingPollingIntervalMillis(), 200,
                "lazyLoadingPollingIntervalMillis should default to 200 (today's hardcoded interval)");
    }

    // --- Issue #3749 Increment A: BROWSER_READINESS_STATE snippet extensions (observed-only) ---

    @Test(description = "Verify browser readiness script counts navigator.sendBeacon calls idempotently as network activity")
    public void testBrowserReadinessStateScriptTracksSendBeacon() {
        String script = JavaScriptHelper.BROWSER_READINESS_STATE.getValue();
        Assert.assertTrue(script.contains("sendBeacon"), "Readiness state should patch navigator.sendBeacon");
        Assert.assertTrue(script.contains("beaconCount"), "Readiness state should expose a sendBeacon call count");
    }

    @Test(description = "Verify browser readiness script observes EventSource/WebSocket construction as observed-only counters")
    public void testBrowserReadinessStateScriptTracksEventSourceAndWebSocketAsObservedOnly() {
        String script = JavaScriptHelper.BROWSER_READINESS_STATE.getValue();
        Assert.assertTrue(script.contains("EventSource"), "Readiness state should observe EventSource construction");
        Assert.assertTrue(script.contains("WebSocket"), "Readiness state should observe WebSocket construction");
        Assert.assertTrue(script.contains("eventSourceCount"), "Readiness state should expose an EventSource construction count");
        Assert.assertTrue(script.contains("webSocketCount"), "Readiness state should expose a WebSocket construction count");
        Assert.assertTrue(script.contains("long-lived connections must never pin the quiet window"),
                "Script must document why EventSource/WebSocket are excluded from the idle/activity marker");
    }

    @Test(description = "Verify browser readiness script exposes a re-armed, idempotent main-thread idle signal that never gates the pass condition")
    public void testBrowserReadinessStateScriptExposesMainThreadIdleSignal() {
        String script = JavaScriptHelper.BROWSER_READINESS_STATE.getValue();
        Assert.assertTrue(script.contains("requestIdleCallback"), "Script should prefer requestIdleCallback for the idle probe");
        Assert.assertTrue(script.contains("requestAnimationFrame"), "Script should fall back to requestAnimationFrame");
        Assert.assertTrue(script.contains("mainThreadIdleSeen"), "Script should expose the idle signal in the returned state map");
        Assert.assertTrue(script.contains("never a hard gate"),
                "Script must document that the idle signal is observed-only and never a hard gate in this increment");
    }

    @Test(description = "Verify browser readiness script installs an idempotent MutationObserver excluding attributes/characterData")
    public void testBrowserReadinessStateScriptTracksDomMutations() {
        String script = JavaScriptHelper.BROWSER_READINESS_STATE.getValue();
        Assert.assertTrue(script.contains("MutationObserver"), "Script should install a MutationObserver");
        Assert.assertTrue(script.contains("_shaftDomMutationSeq"), "Script should expose a DOM-mutation sequence counter");
        Assert.assertTrue(script.contains("domMutationMarker"), "Script should return the DOM-mutation marker in the state map");
        Assert.assertTrue(script.contains("childList: true"), "MutationObserver should observe childList mutations");
        Assert.assertTrue(script.contains("subtree: true"), "MutationObserver should observe subtree mutations");
        Assert.assertTrue(script.contains("attributes: false"), "MutationObserver must exclude attribute mutations");
        Assert.assertTrue(script.contains("characterData: false"), "MutationObserver must exclude characterData mutations");
    }

    // --- Issue #3749 Increment A: DOM-stability quiet-window folding (gated by lazyLoadingDomStabilityQuietWindowMillis) ---

    private static Method getHasMetDomStabilityQuietWindowMethod() throws Exception {
        Method method = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                .getDeclaredMethod("hasMetDomStabilityQuietWindow", String.class, long[].class, String[].class,
                        long.class, long.class);
        method.setAccessible(true);
        return method;
    }

    private static Method getIsDomStableMethod() throws Exception {
        Method method = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager")
                .getDeclaredMethod("isDomStable", String.class, long[].class, String[].class, long.class);
        method.setAccessible(true);
        return method;
    }

    @Test(description = "Verify DOM-mutation marker changes reset the DOM-stability quiet window")
    public void testDomMutationMarkerChangeResetsQuietWindow() throws Exception {
        Method method = getHasMetDomStabilityQuietWindowMethod();
        long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
        String[] lastMarker = {null};

        boolean firstPoll = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 300L, 1000L);
        boolean beforeWindowEnds = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 300L, 1200L);
        boolean afterWindowEnds = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 300L, 1300L);
        boolean markerChanged = (boolean) method.invoke(null, "2", idleSinceMillis, lastMarker, 300L, 1350L);
        boolean afterSecondWindowEnds = (boolean) method.invoke(null, "2", idleSinceMillis, lastMarker, 300L, 1650L);

        Assert.assertFalse(firstPoll, "First observation should establish the baseline marker, not pass immediately");
        Assert.assertFalse(beforeWindowEnds, "Should wait until the full DOM-stability quiet window elapses");
        Assert.assertTrue(afterWindowEnds, "Should pass once the DOM-stability quiet window has elapsed with a stable marker");
        Assert.assertFalse(markerChanged, "A DOM-mutation marker change should reset the quiet window");
        Assert.assertTrue(afterSecondWindowEnds, "Should pass again once the quiet window elapses after the marker change");
    }

    @Test(description = "Verify DOM stability is a no-op (always stable) when lazyLoadingDomStabilityQuietWindowMillis is 0 (default = today's behavior)")
    public void testDomStabilityDisabledWhenPropertyIsZero() throws Exception {
        int original = SHAFT.Properties.timeouts.lazyLoadingDomStabilityQuietWindowMillis();
        try {
            SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(0);
            Method method = getIsDomStableMethod();
            long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
            String[] lastMarker = {null};

            boolean stableOnFirstPoll = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 1000L);
            boolean stableAfterImmediateMarkerChange = (boolean) method.invoke(null, "2", idleSinceMillis, lastMarker, 1001L);

            Assert.assertTrue(stableOnFirstPoll, "DOM stability must default to true (no-op) when the property is 0");
            Assert.assertTrue(stableAfterImmediateMarkerChange,
                    "A DOM-mutation marker change must never gate the wait when the property is 0 (bit-for-bit today's behavior)");
        } finally {
            SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(original);
        }
    }

    @Test(description = "Verify DOM stability folds the marker into the quiet-window decision once lazyLoadingDomStabilityQuietWindowMillis > 0")
    public void testDomStabilityEnabledFoldsMarkerIntoQuietWindow() throws Exception {
        int original = SHAFT.Properties.timeouts.lazyLoadingDomStabilityQuietWindowMillis();
        try {
            SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(300);
            Method method = getIsDomStableMethod();
            long[] idleSinceMillis = {getIdleWindowNotStartedMarker()};
            String[] lastMarker = {null};

            boolean firstPoll = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 1000L);
            boolean beforeWindowEnds = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 1200L);
            boolean afterWindowEnds = (boolean) method.invoke(null, "1", idleSinceMillis, lastMarker, 1300L);

            Assert.assertFalse(firstPoll, "Enabling DOM stability should require an observation baseline first");
            Assert.assertFalse(beforeWindowEnds, "Enabling DOM stability should require the configured quiet window to elapse");
            Assert.assertTrue(afterWindowEnds, "Should pass once the configured DOM-stability quiet window elapses");
        } finally {
            SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(original);
        }
    }
}
