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
}
