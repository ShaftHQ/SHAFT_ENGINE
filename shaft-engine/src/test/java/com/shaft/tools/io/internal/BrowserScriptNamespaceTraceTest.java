package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Iterator;

public class BrowserScriptNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void seleniumProviderFailureShouldRedactNestedArgumentAndRethrowOriginalException() {
        String secret = "opaque-script-value-7102";
        Map<String, Object> argument = Map.of("items", List.of("public", secret));
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        IllegalStateException providerFailure = new IllegalStateException("Rejected " + secret);
        Mockito.doThrow(providerFailure).when((JavascriptExecutor) driver)
                .executeScript("return arguments[0]", argument);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).script()
                        .evaluate("return arguments[0]", argument));

        Assert.assertSame(thrown, providerFailure);
        assertSecretSafeReport("seleniumScriptFailure", secret, thrown);
    }

    @Test
    public void playwrightProviderFailureShouldRedactArgumentWithoutRecordingScriptSource() {
        String secret = "opaque-script-value-8304";
        String source = "value => { const credential = 'opaque-source-1942'; throw new Error(value); }";
        PlaywrightSession session = livePlaywrightSession();
        Page page = session.page();
        IllegalStateException providerFailure = new IllegalStateException(
                "Rejected " + source + " with " + secret + " and opaque-source-1942");
        Mockito.doThrow(providerFailure).when(page).evaluate(source, secret);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).script().evaluate(source, secret));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertFalse(TraceEventRecorder.snapshot().getFirst().toString().contains(source));
        boolean originalCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        try {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(true);
            String report = assertSecretSafeReport("playwrightScriptFailure", secret, thrown);
            Assert.assertFalse(report.contains("opaque-source-1942"), report);
            Assert.assertFalse(report.contains(source), report);
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(originalCodeContext);
        }
    }

    @Test
    public void hostileArgumentTraversalShouldNeverReplaceTheProviderFailure() {
        Iterable<String> hostile = () -> new Iterator<>() {
            @Override public boolean hasNext() { throw new IllegalArgumentException("iterator failure"); }
            @Override public String next() { throw new IllegalArgumentException("iterator failure"); }
        };
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        IllegalStateException providerFailure = new IllegalStateException("provider failure");
        Mockito.doThrow(providerFailure).when((JavascriptExecutor) driver)
                .executeScript("return arguments[0]", hostile);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).script()
                        .evaluate("return arguments[0]", hostile));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertEquals(TraceEventRecorder.snapshot().size(), 1);
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().status(), "failed");
        IllegalArgumentException unrelated = new IllegalArgumentException("unrelated diagnostic");
        Assert.assertEquals(FailureTraceReporter.redactThrowableText(unrelated, unrelated.getMessage()),
                "unrelated diagnostic");
    }

    @Test
    public void wrapperAroundSensitiveProviderFailureShouldOmitTheWholeCauseChainAndSourceContext() {
        String secret = "opaque-wrapped-value-6027";
        String source = "value => { const wrappedSecret = 'opaque-wrapped-source-9921'; throw value; }";
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        IllegalStateException providerFailure = new IllegalStateException(
                "Provider echoed " + source + " and " + secret);
        Mockito.doThrow(providerFailure).when((JavascriptExecutor) driver).executeScript(source, secret);
        Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).script().evaluate(source, secret));
        RuntimeException wrapper = new RuntimeException("script execution failed", providerFailure);
        boolean originalCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        try {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(true);
            String report = FailureTraceReporter.renderTraceJson(info("wrappedScriptFailure", wrapper), "", List.of());
            Assert.assertFalse(report.contains(secret), report);
            Assert.assertFalse(report.contains("opaque-wrapped-source-9921"), report);
            Assert.assertFalse(report.contains(source), report);
            Assert.assertTrue(report.contains(RuntimeException.class.getName()), report);
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(originalCodeContext);
        }
    }

    private static String assertSecretSafeReport(String method, String secret, Throwable throwable) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "script");
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertFalse(events.getFirst().exceptionMessage().contains(secret));
        String report = FailureTraceReporter.renderTraceJson(info(method, throwable), "", List.of());
        Assert.assertFalse(report.contains(secret), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
        return report;
    }

    private static TestExecutionInfo info(String method, Throwable throwable) {
        return new TestExecutionInfo("id-" + method, BrowserScriptNamespaceTraceTest.class.getName(),
                method, method, "script trace", null, throwable, false);
    }

    private static PlaywrightSession livePlaywrightSession() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        return session;
    }
}
