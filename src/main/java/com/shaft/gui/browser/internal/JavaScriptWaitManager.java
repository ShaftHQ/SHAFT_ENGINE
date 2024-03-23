package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.tools.internal.support.JavaScriptHelper;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import java.util.ArrayList;
import java.util.Arrays;

public class JavaScriptWaitManager {
    private static final ThreadLocal<WebDriver> jsWaitDriver = new ThreadLocal<>();
    private static final ThreadLocal<JavascriptExecutor> jsExec = new ThreadLocal<>();

    private JavaScriptWaitManager() {
        throw new IllegalStateException("Utility class");
    }

    private static void setDriver(WebDriver driver) {
        jsWaitDriver.set(driver);
        jsExec.set((JavascriptExecutor) jsWaitDriver.get());
    }

    /**
     * Waits for jQuery, Angular, and/or Javascript if present on the current page.
     */
    public static void waitForLazyLoading(WebDriver driver) {
        setDriver(driver);
        if (SHAFT.Properties.timeouts.waitForLazyLoading()
                && !DriverFactoryHelper.isMobileNativeExecution()) {
            ArrayList<Thread> lazyLoadingThreads = new ArrayList<>();
            lazyLoadingThreads.add(Thread.ofVirtual().start(JavaScriptWaitManager::waitForJQuery));
            lazyLoadingThreads.add(Thread.ofVirtual().start(JavaScriptWaitManager::waitForAngular));
            lazyLoadingThreads.add(Thread.ofVirtual().start(JavaScriptWaitManager::waitForDocumentReadyState));
            lazyLoadingThreads.forEach(thread -> {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    //do nothing
                }
            });
        }
    }

    private static void waitForDocumentReadyState() {
        new SynchronizationManager(jsWaitDriver.get()).fluentWait().until(f -> {
            try {
                var ready = Arrays.asList("loaded", "complete");
                return ready.contains(jsExec.get().executeScript(JavaScriptHelper.DOCUMENT_READY_STATE.getValue()).toString());
            } catch (Exception exception) {
                // force return in case of unexpected exception
                return true;
            }
        });
    }

    private static void waitForJQuery() {
        new SynchronizationManager(jsWaitDriver.get()).fluentWait().until(f -> {
            try {
                return Long.parseLong(jsExec.get().executeScript(JavaScriptHelper.JQUERY_ACTIVE_STATE.getValue()).toString()) == 0;
            } catch (Exception exception) {
                // force return in case of unexpected exception
                // org.openqa.selenium.JavascriptException: javascript error: jQuery is not defined
                return true;
            }
        });
    }

    private static void waitForAngular() {
        new SynchronizationManager(jsWaitDriver.get()).fluentWait().until(f -> {
            try {
                return Long.parseLong(jsExec.get().executeScript(JavaScriptHelper.ANGULAR_READY_STATE.getValue()).toString()) == 0;
            } catch (Exception exception) {
                // force return in case of unexpected exception
                // org.openqa.selenium.JavascriptException: javascript error: angular is not defined
                return true;
            }
        });
    }
}