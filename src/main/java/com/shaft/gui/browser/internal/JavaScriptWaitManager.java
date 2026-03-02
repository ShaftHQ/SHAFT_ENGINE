package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import java.util.ArrayList;
import java.util.Arrays;

public class JavaScriptWaitManager {
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
            ArrayList<Thread> lazyLoadingThreads = new ArrayList<>();
            lazyLoadingThreads.add(Thread.ofVirtual().start(() -> waitForJQuery(driver)));
            lazyLoadingThreads.add(Thread.ofVirtual().start(() -> waitForAngular(driver)));
            lazyLoadingThreads.add(Thread.ofVirtual().start(() -> waitForDocumentReadyState(driver)));
            lazyLoadingThreads.add(Thread.ofVirtual().start(() -> waitUntilNoActiveNetworkFetchRequests(driver)));
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

    private static void waitUntilNoActiveNetworkFetchRequests(WebDriver driver) {
        //Wait for active requests to be zero using native XHR/fetch interceptors
        new SynchronizationManager(driver).fluentWait().until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    var returnedValue = javascriptExecutor.executeScript(JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue());
                    return Long.parseLong(String.valueOf(returnedValue)) == 0;
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

    private static void waitForDocumentReadyState(WebDriver driver) {
        new SynchronizationManager(driver).fluentWait().until(f -> {
            if (f instanceof JavascriptExecutor javascriptExecutor) {
                try {
                    var ready = Arrays.asList("loaded", "complete");
                    return ready.contains(String.valueOf(javascriptExecutor.executeScript(JavaScriptHelper.DOCUMENT_READY_STATE.getValue())));
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