package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Download;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.options.Media;
import com.microsoft.playwright.options.ReducedMotion;
import com.microsoft.playwright.options.ViewportSize;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Unit coverage for {@link PlaywrightSession} public seams (issue #7077).
 */
public class PlaywrightSessionCoverageUnitTest {

    @Test
    public void dialogViewportMediaAndHandlesShouldBeSessionScoped() {
        Page page = Mockito.mock(Page.class);
        Page other = Mockito.mock(Page.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Browser browser = Mockito.mock(Browser.class);
        AtomicReference<Consumer<?>> dialogListener = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            dialogListener.set(invocation.getArgument(0));
            return null;
        }).when(page).onDialog(Mockito.any());
        Mockito.when(context.pages()).thenReturn(List.of(page, other));
        Mockito.when(page.viewportSize()).thenReturn(new ViewportSize(1280, 720));

        PlaywrightSession session = new PlaywrightSession(
                Mockito.mock(Playwright.class), browser, context, page,
                Mockito.mock(PlaywrightTraceManager.class));

        Assert.assertSame(session.browser(), browser);
        Assert.assertSame(session.browserContext(), context);
        Assert.assertSame(session.page(), page);
        Assert.assertNotNull(session.playwright());
        Assert.assertNotNull(session.traceManager());
        Assert.assertNotNull(session.networkInterceptor());

        Assert.assertFalse(session.isDialogSeen());
        Assert.assertNull(session.lastDialogText());
        session.acceptNextDialog();
        session.dismissNextDialog();
        session.typeIntoNextPrompt("answer");
        Assert.assertFalse(session.isDialogSeen());

        String handle = session.pageHandle(page);
        Assert.assertTrue(handle.startsWith("page-"));
        Assert.assertEquals(session.pageHandle(page), handle);
        Assert.assertEquals(session.pageByHandle(handle), page);
        Assert.assertNull(session.pageByHandle("missing"));

        session.setViewport(page, 800, 600);
        Mockito.verify(page).setViewportSize(800, 600);
        session.clearViewport(page);
        Mockito.verify(page, Mockito.atLeastOnce()).setViewportSize(1280, 720);

        session.setMediaType(page, Media.PRINT);
        Mockito.verify(page).emulateMedia(Mockito.any());
        session.setColorScheme(page, ColorScheme.DARK);
        session.setReducedMotion(page, ReducedMotion.REDUCE);
        session.resetMedia(page);

        Download download = Mockito.mock(Download.class);
        session.trackDownload(download);
        Assert.assertEquals(session.downloadSnapshot(), List.of(download));
        session.forgetDownload(download);
        Assert.assertTrue(session.downloadSnapshot().isEmpty());
        session.trackDownload(download);
        session.clearDownloadHandles();
        Assert.assertTrue(session.downloadSnapshot().isEmpty());

        session.setPage(other);
        Assert.assertSame(session.page(), other);
        session.clearConsole();
        Assert.assertNotNull(session.consoleSnapshot());
    }
}
