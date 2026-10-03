package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class BrowserConsoleErrorGateTest {

    private static BrowserObservabilityRecorder.ConsoleSnapshotEntry entry(String level, String message) {
        return BrowserObservabilityRecorder.consoleEntry("javascript", level, message, 1L);
    }

    @Test(description = "Only non-allowlisted error-level messages fail the test")
    public void failureListsOnlyNonAllowlistedErrors() {
        AssertionError failure = BrowserConsoleErrorGate.failureFor(List.of(
                entry("info", "hello"),
                entry("error", "Uncaught TypeError: x is undefined"),
                entry("SEVERE", "favicon.ico 404 (Not Found)")), "favicon\\.ico");

        Assert.assertNotNull(failure);
        Assert.assertTrue(failure.getMessage().contains("Uncaught TypeError"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().contains("favicon"), failure.getMessage());
    }

    @Test(description = "No failure when every error is allowlisted or there are no errors")
    public void noFailureWhenClean() {
        Assert.assertNull(BrowserConsoleErrorGate.failureFor(List.of(entry("warning", "deprecated")), ""));
        Assert.assertNull(BrowserConsoleErrorGate.failureFor(List.of(entry("error", "ResizeObserver loop")), "ResizeObserver"));
    }

    @Test(description = "Gate is off by default so existing suites are unaffected")
    public void gateIsOffByDefault() {
        Assert.assertFalse(com.shaft.driver.SHAFT.Properties.web.failOnBrowserConsoleErrors());
        Assert.assertEquals(com.shaft.driver.SHAFT.Properties.web.browserConsoleErrorAllowlist(), "");
        Assert.assertNull(BrowserConsoleErrorGate.checkCurrentTest());
    }
}
