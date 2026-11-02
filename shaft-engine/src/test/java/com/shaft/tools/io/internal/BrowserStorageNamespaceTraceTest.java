package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class BrowserStorageNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void storageValueShouldNeverBeCopiedIntoTraceEvidence() {
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));

        new com.shaft.gui.browser.BrowserActions(driver, true).storage().local()
                .set("authToken", "Bearer top-secret");

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        var event = events.getFirst();
        Assert.assertEquals(event.category(), "storage");
        Assert.assertEquals(event.name(), "localStorage/set");
        Assert.assertEquals(event.locator(), "authToken");
        Assert.assertEquals(event.status(), "passed");
        Assert.assertEquals(event.backend(), AutomationBackend.SELENIUM_WEBDRIVER);
        Assert.assertFalse(event.toString().contains("top-secret"));
    }

    @Test
    public void unsupportedStorageShouldEmitOneFailedEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).storage().session().clear());

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "storage");
        Assert.assertEquals(events.getFirst().name(), "sessionStorage/clear");
        Assert.assertEquals(events.getFirst().status(), "failed");
    }

    @Test
    public void seleniumStorageFailureShouldRedactTheExactValueButRethrowTheOriginalFailure() {
        String secret = "opaque-value-7821";
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        JavascriptExecutor scripts = (JavascriptExecutor) driver;
        IllegalStateException providerFailure = new IllegalStateException("Rejected " + secret);
        Mockito.doThrow(providerFailure).when(scripts).executeScript(
                "window[arguments[0]].setItem(arguments[1], arguments[2]);",
                "localStorage", "key", secret);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).storage().local().set("key", secret));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertFalse(TraceEventRecorder.snapshot().getFirst().exceptionMessage().contains(secret));
        String report = FailureTraceReporter.renderTraceJson(info("seleniumStorageFailure", thrown), "", java.util.List.of());
        Assert.assertFalse(report.contains(secret), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void playwrightStorageFailureShouldRedactTheExactValueButRethrowTheOriginalFailure() {
        String secret = "opaque-value-9914";
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        IllegalStateException providerFailure = new IllegalStateException("Rejected " + secret);
        Mockito.doThrow(providerFailure).when(page).evaluate(
                "([scope, key, value]) => window[scope].setItem(key, value)",
                java.util.List.of("sessionStorage", "key", secret));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).storage().session()
                        .set("key", secret));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertFalse(TraceEventRecorder.snapshot().getFirst().exceptionMessage().contains(secret));
        String report = FailureTraceReporter.renderTraceJson(info("playwrightStorageFailure", thrown), "", java.util.List.of());
        Assert.assertFalse(report.contains(secret), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void shortFailedStorageValueShouldOmitOnlyThrowableTextWithoutCorruptingEvidence() {
        String secret = "a";
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        IllegalStateException providerFailure = new IllegalStateException("Rejected value " + secret);
        Mockito.doThrow(providerFailure).when((JavascriptExecutor) driver).executeScript(
                "window[arguments[0]].setItem(arguments[1], arguments[2]);",
                "localStorage", "flag", secret);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).storage().local().set("flag", secret));
        String report = FailureTraceReporter.renderTraceJson(info("shortStorageFailure", thrown), "unrelated log", java.util.List.of());

        Assert.assertFalse(report.contains("Rejected value a"), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
        Assert.assertTrue(report.contains("localStorage/set"), report);
        Assert.assertTrue(report.contains("unrelated log"), report);
    }

    private static TestExecutionInfo info(String methodName, Throwable throwable) {
        return new TestExecutionInfo("id-" + methodName, BrowserStorageNamespaceTraceTest.class.getName(),
                methodName, methodName, "storage trace", null, throwable, false);
    }
}
