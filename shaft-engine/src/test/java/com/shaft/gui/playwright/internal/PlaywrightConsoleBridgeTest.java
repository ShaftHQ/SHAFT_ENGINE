package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.ConsoleMessage;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.function.Consumer;
import java.util.concurrent.Executors;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportContext;
import com.shaft.listeners.internal.TestExecutionInfo;

public class PlaywrightConsoleBridgeTest {
    @AfterMethod
    public void clearObservations() {
        BrowserObservabilityRecorder.clear();
    }

    @Test
    public void sessionShouldBridgeConsoleMessagesAndPageErrorsExactlyOncePerPage() {
        Page page = Mockito.mock(Page.class);
        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                Mockito.mock(BrowserContext.class), page, null);
        ArgumentCaptor<Consumer<ConsoleMessage>> consoleListener = consumerCaptor();
        ArgumentCaptor<Consumer<String>> errorListener = consumerCaptor();
        Mockito.verify(page).onConsoleMessage(consoleListener.capture());
        Mockito.verify(page).onPageError(errorListener.capture());

        ConsoleMessage message = Mockito.mock(ConsoleMessage.class);
        Mockito.when(message.type()).thenReturn("warning");
        Mockito.when(message.text()).thenReturn("deprecated API");
        consoleListener.getValue().accept(message);
        errorListener.getValue().accept("uncaught boom");
        session.setPage(page);

        Assert.assertEquals(session.consoleSnapshot().size(), 2);
        Assert.assertEquals(session.consoleSnapshot().getLast().level(), "pageerror");
        Mockito.verify(page, Mockito.times(1)).onConsoleMessage(Mockito.any());
        Mockito.verify(page, Mockito.times(1)).onPageError(Mockito.any());
    }

    @Test
    public void simultaneousSessionsShouldKeepConsoleEvidenceIsolatedWhenEitherCloses() {
        Page firstPage = Mockito.mock(Page.class);
        Page secondPage = Mockito.mock(Page.class);
        Browser firstBrowser = Mockito.mock(Browser.class);
        Browser secondBrowser = Mockito.mock(Browser.class);
        Mockito.when(firstBrowser.isConnected()).thenReturn(true);
        Mockito.when(secondBrowser.isConnected()).thenReturn(true);
        Mockito.when(firstPage.isClosed()).thenReturn(false);
        Mockito.when(secondPage.isClosed()).thenReturn(false);
        PlaywrightSession first = new PlaywrightSession(Mockito.mock(Playwright.class), firstBrowser,
                Mockito.mock(BrowserContext.class), firstPage, null);
        PlaywrightSession second = new PlaywrightSession(Mockito.mock(Playwright.class), secondBrowser,
                Mockito.mock(BrowserContext.class), secondPage, null);
        ArgumentCaptor<Consumer<ConsoleMessage>> firstListener = consumerCaptor();
        ArgumentCaptor<Consumer<ConsoleMessage>> secondListener = consumerCaptor();
        Mockito.verify(firstPage).onConsoleMessage(firstListener.capture());
        Mockito.verify(secondPage).onConsoleMessage(secondListener.capture());
        firstListener.getValue().accept(message("first-session"));
        secondListener.getValue().accept(message("second-session"));

        var firstConsole = new com.shaft.gui.playwright.browser.BrowserActions(first).console();
        var secondConsole = new com.shaft.gui.playwright.browser.BrowserActions(second).console();
        Assert.assertEquals(firstConsole.messages().getFirst().message(), "first-session");
        Assert.assertEquals(secondConsole.messages().getFirst().message(), "second-session");
        first.close();
        Assert.assertEquals(secondConsole.messages().getFirst().message(), "second-session");
        second.close();
    }

    @Test
    public void asyncDrainShouldFollowReportSessionRolloverInsteadOfExecutorThread() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        ReportContext.start(info("setup"));
        Page page = Mockito.mock(Page.class);
        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                Mockito.mock(BrowserContext.class), page, null);
        ArgumentCaptor<Consumer<ConsoleMessage>> listener = consumerCaptor();
        Mockito.verify(page).onConsoleMessage(listener.capture());
        ReportContext.start(info("test"));

        try (var executor = Executors.newSingleThreadExecutor()) {
            executor.submit(() -> listener.getValue().accept(message("async-playwright-owner"))).get();
        }
        session.drainConsoleToRecorder();

        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(),
                "async-playwright-owner");
    }

    @Test
    public void providerOverflowShouldReportOldestConsoleOmission() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        ReportContext.start(info("overflow"));
        Page page = Mockito.mock(Page.class);
        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                Mockito.mock(BrowserContext.class), page, null);
        ArgumentCaptor<Consumer<ConsoleMessage>> listener = consumerCaptor();
        Mockito.verify(page).onConsoleMessage(listener.capture());
        for (int index = 0; index <= 1000; index++) {
            listener.getValue().accept(message("playwright-" + index));
        }
        Assert.assertEquals(session.consoleSnapshot().size(), 1000);
        Assert.assertEquals(session.consoleSnapshot().getFirst().message(), "playwright-1");
        Assert.assertEquals(session.consoleSnapshot().getLast().message(), "playwright-1000");

        session.drainConsoleToRecorder();

        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1000);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(), "playwright-1");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().stream()
                .anyMatch(warning -> warning.contains("oldest console")));
        BrowserObservabilityRecorder.clearConsole();
        listener.getValue().accept(message("next-batch"));
        session.drainConsoleToRecorder();
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(), "next-batch");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().isEmpty());
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static <T> ArgumentCaptor<Consumer<T>> consumerCaptor() {
        return ArgumentCaptor.forClass((Class) Consumer.class);
    }

    private static ConsoleMessage message(String text) {
        ConsoleMessage message = Mockito.mock(ConsoleMessage.class);
        Mockito.when(message.type()).thenReturn("log");
        Mockito.when(message.text()).thenReturn(text);
        return message;
    }

    private static TestExecutionInfo info(String method) {
        return new TestExecutionInfo("playwright-" + method, PlaywrightConsoleBridgeTest.class.getName(),
                method, method, method, null, null, false);
    }
}
