package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.Tracing;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

public class PlaywrightSessionSensitiveTraceTest {
    @AfterMethod(alwaysRun = true)
    public void clearSensitiveState() {
        TraceEventRecorder.clear();
        PlaywrightTraceManager.clearLastTracePath();
    }

    @Test
    public void closeShouldDiscardNativeTraceBeforeClearingSensitiveSessionState() throws Exception {
        Path artifacts = Files.createTempDirectory("shaft-playwright-sensitive-close-");
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Tracing tracing = Mockito.mock(Tracing.class);
        Mockito.when(context.tracing()).thenReturn(tracing);
        PlaywrightTraceManager traceManager = new PlaywrightTraceManager(context, artifacts);
        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                context, Mockito.mock(Page.class), traceManager);
        try {
            traceManager.start();
            FailureTraceReporter.registerPersistentSensitiveBrowserState(session, "geolocation", 30.0444, 31.2357);

            session.close();

            Mockito.verify(tracing).stop();
            Mockito.verify(tracing, Mockito.never()).stop(Mockito.any(Tracing.StopOptions.class));
            Assert.assertNull(PlaywrightTraceManager.getLastTracePath());
            try (var files = Files.list(artifacts)) {
                Assert.assertEquals(files.count(), 0);
            }
        } finally {
            try (var files = Files.walk(artifacts)) {
                files.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception exception) {
                        throw new IllegalStateException(exception);
                    }
                });
            }
        }
    }
}
