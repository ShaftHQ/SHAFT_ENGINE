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
import java.util.Map;

public class JavaScriptWaitManager {
    private static final List<String> COMPLETE_READY_STATES = List.of("loaded", "complete");
    private static final Duration ACTIVE_REQUEST_POLLING_INTERVAL = Duration.ofMillis(200);
    private static final Duration MINIMUM_IDLE_WINDOW = Duration.ofMillis(500);

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
                    Thread.ofVirtual().start(() -> waitUntilWebsiteIsIdle(driver))
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

    private static void waitUntilWebsiteIsIdle(WebDriver driver) {
        //Wait for full async activity to stay idle for a minimum quiet window
        if (driver instanceof JavascriptExecutor javascriptExecutor) {
            installAsyncActivityMonitor(javascriptExecutor);
        }
        new SynchronizationManager(driver).fluentWait().pollingEvery(ACTIVE_REQUEST_POLLING_INTERVAL).until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    var snapshot = readAsyncActivitySnapshot(javascriptExecutor);
                    if (snapshot == null) {
                        return true;
                    }
                    if (!Boolean.TRUE.equals(snapshot.get("idle"))) {
                        return false;
                    }
                    var quietForMs = snapshot.get("quietForMs");
                    if (!(quietForMs instanceof Number quietForMsNumber)) {
                        return false;
                    }
                    return quietForMsNumber.longValue() >= MINIMUM_IDLE_WINDOW.toMillis();
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

    private static void installAsyncActivityMonitor(JavascriptExecutor javascriptExecutor) {
        javascriptExecutor.executeScript(JavaScriptHelper.INSTALL_ASYNC_ACTIVITY_MONITOR.getValue());
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> readAsyncActivitySnapshot(JavascriptExecutor javascriptExecutor) {
        var result = javascriptExecutor.executeScript(JavaScriptHelper.GET_ASYNC_ACTIVITY_SNAPSHOT.getValue());
        if (result instanceof Map<?, ?> mapResult) {
            return (Map<String, Object>) mapResult;
        }
        return null;
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
