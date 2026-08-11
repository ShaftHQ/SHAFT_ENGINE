package com.shaft.tools.io.internal;

import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.driver.SHAFT;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.Logs;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Set;
import java.util.logging.Level;

public class BrowserConsoleNamespaceTraceTest {
    @AfterMethod
    public void clear() {
        TraceEventRecorder.clear();
        BrowserObservabilityRecorder.clear();
    }

    @Test
    public void supportedConsoleQueryShouldEmitOnePassedOwnerEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        Logs logs = Mockito.mock(Logs.class);
        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.logs()).thenReturn(logs);
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        Mockito.when(logs.get("browser")).thenReturn(new LogEntries(List.of(
                new LogEntry(Level.WARNING, 10, "warning"))));

        Assert.assertEquals(new com.shaft.gui.browser.BrowserActions(driver, true).console().messages().size(), 1);

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "console");
        Assert.assertEquals(events.getFirst().name(), "messages");
        Assert.assertEquals(events.getFirst().status(), "passed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void unsupportedConsoleQueryShouldEmitOneFailedOwnerEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).console().errors());

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "console");
        Assert.assertEquals(events.getFirst().name(), "errors");
        Assert.assertEquals(events.getFirst().status(), "failed");
    }

    @Test
    public void disabledTraceConsoleCaptureShouldNotConsumeProviderLogs() {
        boolean originalTrace = SHAFT.Properties.reporting.traceEnabled();
        boolean originalConsole = SHAFT.Properties.reporting.traceIncludeConsole();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(false);
            WebDriver driver = Mockito.mock(WebDriver.class);
            WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
            Logs logs = Mockito.mock(Logs.class);
            Mockito.when(driver.manage()).thenReturn(options);
            Mockito.when(options.logs()).thenReturn(logs);

            BrowserObservabilityRecorder.collectConsole(driver);

            Mockito.verify(logs, Mockito.never()).get(Mockito.anyString());
            Mockito.verify(logs, Mockito.never()).getAvailableLogTypes();
        } finally {
            SHAFT.Properties.reporting.set().traceEnabled(originalTrace).traceIncludeConsole(originalConsole);
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void successfulBidiFallbackShouldNotPublishAnUnsupportedWarning() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasBiDi.class));
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        Logs logs = Mockito.mock(Logs.class);
        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.logs()).thenReturn(logs);
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of());
        var sourceType = com.shaft.gui.browser.internal.BidiConsoleLogSource.class;
        var constructor = sourceType.getDeclaredConstructor();
        constructor.setAccessible(true);
        var source = constructor.newInstance();
        var install = sourceType.getDeclaredMethod("install", WebDriver.class, sourceType);
        install.setAccessible(true);
        install.invoke(null, driver, source);
        var record = sourceType.getDeclaredMethod("record", String.class, String.class, long.class);
        record.setAccessible(true);
        record.invoke(source, "error", "bidi boom", 42L);

        Assert.assertEquals(new com.shaft.gui.browser.BrowserActions(driver, true).console().messages().size(), 1);
        String metadata = BrowserObservabilityRecorder.drainMetadataJson();
        Assert.assertFalse(metadata.contains("\"source\": \"console\""), metadata);
        com.shaft.gui.browser.internal.BidiConsoleLogSource.closeAndRemove(driver);
    }
}
