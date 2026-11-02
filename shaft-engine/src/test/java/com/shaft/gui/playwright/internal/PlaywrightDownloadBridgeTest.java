package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Download;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

public class PlaywrightDownloadBridgeTest {
    @Test
    @SuppressWarnings("unchecked")
    public void sessionShouldObserveDownloadsBeforeNavigationAndKeepThemSessionScoped() throws Exception {
        Page firstPage = Mockito.mock(Page.class);
        Page secondPage = Mockito.mock(Page.class);
        AtomicReference<Consumer<Download>> firstListener = new AtomicReference<>();
        AtomicReference<Consumer<Download>> secondListener = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            firstListener.set(invocation.getArgument(0));
            return null;
        }).when(firstPage).onDownload(Mockito.any());
        Mockito.doAnswer(invocation -> {
            secondListener.set(invocation.getArgument(0));
            return null;
        }).when(secondPage).onDownload(Mockito.any());
        PlaywrightSession first = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                Mockito.mock(BrowserContext.class), firstPage, Mockito.mock(PlaywrightTraceManager.class));
        PlaywrightSession second = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                Mockito.mock(BrowserContext.class), secondPage, Mockito.mock(PlaywrightTraceManager.class));

        Method snapshot = null;
        try {
            snapshot = PlaywrightSession.class.getMethod("downloadSnapshot");
        } catch (NoSuchMethodException missingBehavior) {
            // RED: the session does not own download observations yet.
        }
        Assert.assertNotNull(snapshot);
        Download firstDownload = Mockito.mock(Download.class);
        Download secondDownload = Mockito.mock(Download.class);
        firstListener.get().accept(firstDownload);
        secondListener.get().accept(secondDownload);

        Assert.assertEquals((List<Download>) snapshot.invoke(first), List.of(firstDownload));
        Assert.assertEquals((List<Download>) snapshot.invoke(second), List.of(secondDownload));

        first.close();
        Assert.assertEquals((List<Download>) snapshot.invoke(first), List.of());
        Assert.assertEquals((List<Download>) snapshot.invoke(second), List.of(secondDownload));
    }

    @Test
    public void clearShouldNotForgetADownloadThatArrivesWhileEarlierFilesAreDeleted() {
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(browser.isConnected()).thenReturn(true);
        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), browser,
                context, page, Mockito.mock(PlaywrightTraceManager.class));
        Download first = Mockito.mock(Download.class);
        Download late = Mockito.mock(Download.class);
        session.trackDownload(first);
        Mockito.doAnswer(invocation -> {
            session.trackDownload(late);
            return null;
        }).when(first).delete();

        new com.shaft.gui.playwright.browser.BrowserActions(session).downloads().clear();

        Mockito.verify(first).delete();
        Mockito.verify(late, Mockito.never()).delete();
        Assert.assertEquals(session.downloadSnapshot(), List.of(late));
    }

    @Test
    public void sessionShouldObserveExistingAndFutureContextPagesBeforeTheyDownload() {
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page current = Mockito.mock(Page.class);
        Page existingPopup = Mockito.mock(Page.class);
        Page futurePopup = Mockito.mock(Page.class);
        Mockito.when(context.pages()).thenReturn(List.of(current, existingPopup));
        AtomicReference<Consumer<Download>> existingPopupDownload = new AtomicReference<>();
        AtomicReference<Consumer<Download>> futurePopupDownload = new AtomicReference<>();
        AtomicReference<Consumer<Page>> pageListener = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            existingPopupDownload.set(invocation.getArgument(0));
            return null;
        }).when(existingPopup).onDownload(Mockito.any());
        Mockito.doAnswer(invocation -> {
            futurePopupDownload.set(invocation.getArgument(0));
            return null;
        }).when(futurePopup).onDownload(Mockito.any());
        Mockito.doAnswer(invocation -> {
            pageListener.set(invocation.getArgument(0));
            return null;
        }).when(context).onPage(Mockito.any());

        PlaywrightSession session = new PlaywrightSession(Mockito.mock(Playwright.class), Mockito.mock(Browser.class),
                context, current, Mockito.mock(PlaywrightTraceManager.class));

        Assert.assertNotNull(existingPopupDownload.get());
        Assert.assertNotNull(pageListener.get());
        Download existingDownload = Mockito.mock(Download.class);
        existingPopupDownload.get().accept(existingDownload);
        pageListener.get().accept(futurePopup);
        Assert.assertNotNull(futurePopupDownload.get());
        Download futureDownload = Mockito.mock(Download.class);
        futurePopupDownload.get().accept(futureDownload);
        Assert.assertEquals(session.downloadSnapshot(), List.of(existingDownload, futureDownload));
    }
}
