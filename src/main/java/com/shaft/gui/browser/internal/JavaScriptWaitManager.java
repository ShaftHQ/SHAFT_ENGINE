package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import java.time.Duration;
import java.util.List;

public class JavaScriptWaitManager {
    private static final List<String> COMPLETE_READY_STATES = List.of("loaded", "complete");
    private static final Duration ACTIVE_REQUEST_POLLING_INTERVAL = Duration.ofMillis(200);
    // Short initial observation window that lets deferred XHR/fetch startup appear before declaring no-activity idle.
    private static final Duration INITIAL_IDLE_OBSERVATION_WINDOW = ACTIVE_REQUEST_POLLING_INTERVAL;
    private static final Duration MINIMUM_IDLE_WINDOW = Duration.ofMillis(500);
    private static final long IDLE_WINDOW_NOT_STARTED = -1L;

    private JavaScriptWaitManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Waits for jQuery, Angular, and/or Javascript if present on the current page.
     */
    public static void waitForLazyLoading(WebDriver driver) {
        try {
        if (SHAFT.Properties.timeouts.waitForLazyLoading()
                && !DriverFactoryHelper.isMobileNativeExecution()) {
            var lazyLoadingThreads = List.of(
                    Thread.ofVirtual().start(() -> waitForJQuery(driver)),
                    Thread.ofVirtual().start(() -> waitForAngular(driver)),
                    Thread.ofVirtual().start(() -> waitForDocumentReadyState(driver)),
                    Thread.ofVirtual().start(() -> waitUntilNoActiveNetworkRequests(driver))
            );
            lazyLoadingThreads.forEach(thread -> {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    //do nothing
                }
            });
        }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    private static void waitUntilNoActiveNetworkRequests(WebDriver driver) {
        //Wait for active XHR/fetch requests to remain idle for the minimum quiet window
        final long[] idleSinceMillis = {IDLE_WINDOW_NOT_STARTED};
        final boolean[] networkActivityObserved = {false};
        new SynchronizationManager(driver).fluentWait().pollingEvery(ACTIVE_REQUEST_POLLING_INTERVAL).until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    var returnedValue = javascriptExecutor.executeScript(JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue());
                    long activeRequests = 0L;
                    if (returnedValue instanceof Number numberValue) {
                        activeRequests = numberValue.longValue();
                    } else if (returnedValue != null) {
                        activeRequests = Long.parseLong(returnedValue.toString());
                    }
                    return hasMetMinimumIdleWindow(activeRequests, idleSinceMillis, networkActivityObserved, System.currentTimeMillis());
                } catch (Exception exception) {
                    // force return in case of unexpected exception
                    // e.g. org.openqa.selenium.JavascriptException if the script cannot execute
                    ReportManagerHelper.logDiscrete(exception);
                    return true;
                }
            }
            return true;
        });
    }

    /**
     * Evaluates whether the network has remained idle for the configured quiet window.
     * <p>
     * State machine behavior:
     * <ul>
     *   <li>If there has been no observed network activity, the method waits for one follow-up poll.</li>
     *   <li>After observing {@code activeRequests > 0}, the first zero-activity poll captures the quiet-window start time.</li>
     *   <li>On subsequent zero-activity polls, the method returns {@code true} once the quiet window is reached.</li>
     * </ul>
     *
     * @param activeRequests          current number of active network requests
     * @param idleSinceMillis         single-element state holder for quiet-window start timestamp
     * @param networkActivityObserved single-element state holder that tracks whether any request was observed
     * @param nowMillis               current time in milliseconds
     * @return {@code true} when the quiet window has been satisfied, otherwise {@code false}
     */
    private static boolean hasMetMinimumIdleWindow(long activeRequests, long[] idleSinceMillis, boolean[] networkActivityObserved, long nowMillis) {
        if (activeRequests > 0) {
            networkActivityObserved[0] = true;
            idleSinceMillis[0] = IDLE_WINDOW_NOT_STARTED;
            return false;
        }

        if (idleSinceMillis[0] == IDLE_WINDOW_NOT_STARTED) {
            idleSinceMillis[0] = nowMillis;
            return false;
        }

        var requiredIdleWindow = networkActivityObserved[0] ? MINIMUM_IDLE_WINDOW : INITIAL_IDLE_OBSERVATION_WINDOW;
        return (nowMillis - idleSinceMillis[0]) >= requiredIdleWindow.toMillis();
    }

    private static void waitForDocumentReadyState(WebDriver driver) {
        new SynchronizationManager(driver).fluentWait().until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    return COMPLETE_READY_STATES.contains(String.valueOf(javascriptExecutor.executeScript(JavaScriptHelper.DOCUMENT_READY_STATE.getValue())));
                } catch (Exception exception) {
                    // force return in case of unexpected exception
                    return true;
                }
            }
            return true;
        });
    }

    private static void waitForJQuery(WebDriver driver) {
        new SynchronizationManager(driver).fluentWait().until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    return Long.parseLong(String.valueOf(javascriptExecutor.executeScript(JavaScriptHelper.JQUERY_ACTIVE_STATE.getValue()))) == 0;
                } catch (Exception exception) {
                    // force return in case of unexpected exception
                    // org.openqa.selenium.JavascriptException: javascript error: jQuery is not defined
                    return true;
                }
            }
            return true;
        });
    }

    private static void waitForAngular(WebDriver driver) {
        new SynchronizationManager(driver).fluentWait().until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    // Try AngularJS (1.x) first
                    return Long.parseLong(String.valueOf(javascriptExecutor.executeScript(JavaScriptHelper.ANGULAR_READY_STATE.getValue()))) == 0;
                } catch (Exception exception) {
                    // AngularJS not found on this page; try Angular 2+
                    // org.openqa.selenium.JavascriptException: javascript error: angular is not defined
                    try {
                        // Try Angular 2+ if AngularJS is not available on the page
                        return Long.parseLong(String.valueOf(javascriptExecutor.executeScript(JavaScriptHelper.ANGULAR2_READY_STATE.getValue()))) == 0;
                    } catch (Exception angularException) {
                        // force return if Angular is not present on this page
                        return true;
                    }
                }
            }
            return true;
        });
    }
}
