package com.shaft.gui.driver;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.Logs;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

public class BrowserConsoleNamespaceTest {
    @AfterMethod
    public void clearObservations() {
        BrowserObservabilityRecorder.clear();
    }

    @Test
    public void consoleNamespaceShouldBeDiscoverableFromTheGenericFacade() {
        Assert.assertTrue(Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("console")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("ConsoleActionsContract")));
        Assert.assertEquals(ConsoleActionsContract.class.getDeclaredMethods().length, 5);
        Assert.assertEquals(BrowserConsoleMessage.class.getRecordComponents().length, 4);
    }

    @Test
    public void seleniumConsoleNamespaceShouldCollectFilterAndClearBrowserLogs() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        Logs logs = Mockito.mock(Logs.class);
        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.logs()).thenReturn(logs);
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        Mockito.when(logs.get("browser")).thenReturn(new LogEntries(List.of(
                        new LogEntry(Level.INFO, 10, "ready"),
                        new LogEntry(Level.SEVERE, 20, "boom"))),
                new LogEntries(List.of()), new LogEntries(List.of()), new LogEntries(List.of()));
        var browser = new com.shaft.gui.browser.BrowserActions(driver, true);

        var console = browser.console();
        Assert.assertEquals(console.messages().size(), 2);
        Assert.assertEquals(console.errors().size(), 1);
        Assert.assertTrue(console.hasErrors());
        Assert.assertSame(console.clear().and(), browser);
        Assert.assertTrue(BrowserObservabilityRecorder.snapshotConsole().isEmpty());
    }

    @Test
    public void playwrightConsoleNamespaceShouldUseTheSharedBoundedRecorder() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.consoleSnapshot()).thenReturn(List.of(
                BrowserObservabilityRecorder.consoleEntry("playwright", "log", "ready", 10),
                BrowserObservabilityRecorder.consoleEntry("playwright", "pageerror", "boom", 20)));
        var browser = new com.shaft.gui.playwright.browser.BrowserActions(session);

        var console = browser.console();
        Assert.assertEquals(console.messages().stream().map(BrowserConsoleMessage::message).toList(),
                List.of("ready", "boom"));
        Assert.assertTrue(console.hasErrors());
        Assert.assertEquals(console.errors().getFirst().level(), "pageerror");
        Assert.assertSame(console.clear().and(), browser);
        Mockito.verify(session).clearConsole();
    }

    @Test
    public void unsupportedSeleniumConsoleShouldFailClosed() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).console().messages());
    }

    @Test
    public void consoleRecorderShouldStayBoundedAndRedactKnownSecretShapes() {
        for (int index = 0; index < 1001; index++) {
            BrowserObservabilityRecorder.recordConsole("playwright", "log", "message-" + index, index);
        }
        BrowserObservabilityRecorder.recordConsole("playwright", "log", "token=raw-secret", 1002);

        var snapshot = BrowserObservabilityRecorder.snapshotConsole();
        Assert.assertEquals(snapshot.size(), 1000);
        Assert.assertEquals(snapshot.getFirst().message(), "message-2");
        Assert.assertFalse(snapshot.getLast().message().contains("raw-secret"));
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
