package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Route;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.io.internal.HttpContractRecorder;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.mockito.MockedStatic;
import org.openqa.selenium.remote.http.HttpResponse;
import com.microsoft.playwright.options.Cookie;
import com.shaft.enums.internal.Screenshots;
import static org.mockito.ArgumentMatchers.contains;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightBrowserActionsUnitTest {
    private Path storageStateFile;
    private Path harFile;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        if (storageStateFile != null) {
            Files.deleteIfExists(storageStateFile);
        }
        if (harFile != null) {
            Files.deleteIfExists(harFile);
        }
    }

    @Test
    public void shouldSaveAndLoadStorageStateThroughPlaywrightSession() throws Exception {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(page.url()).thenReturn("https://example.com/app");
        when(page.evaluate("() => ({localStorage: Object.fromEntries(Object.entries(window.localStorage)), "
                + "sessionStorage: Object.fromEntries(Object.entries(window.sessionStorage))})"))
                .thenReturn(Map.of("localStorage", Map.of("authToken", "storage-secret"), "sessionStorage", Map.of()));
        BrowserActions actions = new BrowserActions(session);

        storageStateFile = Files.createTempFile("shaft-pw-wiring-storage-state", ".json");
        Assert.assertSame(actions.saveStorageState(storageStateFile.toString()), actions);
        Assert.assertTrue(Files.readString(storageStateFile).contains("storage-secret"));

        Assert.assertSame(actions.loadStorageState(storageStateFile.toString()), actions);
        verify(context, times(1)).clearCookies();
        verify(context, never()).addCookies(any());
        verify(page).evaluate(eq("(state) => { window.localStorage.clear(); window.sessionStorage.clear(); "
                        + "for (const [key, value] of Object.entries(state.localStorage)) { window.localStorage.setItem(key, value); } "
                        + "for (const [key, value] of Object.entries(state.sessionStorage)) { window.sessionStorage.setItem(key, value); } }"),
                eq(Map.of("localStorage", Map.of("authToken", "storage-secret"), "sessionStorage", Map.of())));
    }

    @Test
    public void shouldWireNetworkInterceptionAndAccessibilityEntryPoints() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(session.networkInterceptor()).thenReturn(new PlaywrightNetworkInterceptor(context));
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> { };
        });
        BrowserActions actions = new BrowserActions(session);

        Assert.assertSame(actions.mock(request -> true, new HttpResponse()), actions);
        Assert.assertNotNull(handler.get());
        Assert.assertNotNull(actions.interceptRequest());
        AccessibilityActions accessibility = actions.accessibility();

        Assert.assertNotNull(accessibility);
    }

    @Test
    public void shouldRegisterRouteFromHarThroughPlaywrightNetworkInterceptor() throws Exception {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(session.networkInterceptor()).thenReturn(new PlaywrightNetworkInterceptor(context));
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> { };
        });
        BrowserActions actions = new BrowserActions(session);

        harFile = Files.createTempFile("shaft-pw-wiring-har", ".har");
        Files.writeString(harFile, """
                {
                  "log": {
                    "version": "1.2",
                    "entries": [
                      {
                        "request": {"method": "GET", "url": "https://shop.test/api/items"},
                        "response": {"status": 200, "headers": [], "content": {"text": "{\\"ok\\":true}"}}
                      }
                    ]
                  }
                }
                """);

        Assert.assertSame(actions.routeFromHar(harFile.toString()), actions);
        Assert.assertNotNull(handler.get());
    }

    @Test
    public void shouldNavigateWithBasicAuthenticationWithoutEmbeddingCredentialsInTheUrl() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Browser browser = mock(Browser.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        PlaywrightNetworkInterceptor interceptor = mock(PlaywrightNetworkInterceptor.class);
        when(session.page()).thenReturn(page);
        when(session.browser()).thenReturn(browser);
        when(session.browserContext()).thenReturn(context);
        when(session.networkInterceptor()).thenReturn(interceptor);
        when(browser.isConnected()).thenReturn(true);
        when(page.isClosed()).thenReturn(false);
        BrowserActions actions = new BrowserActions(session);

        actions.navigateToURLWithBasicAuthentication(
                "https://example.com/secure", "user", "secretPass", null);

        verify(page).navigate("https://example.com/secure");
        verify(interceptor).registerBasicAuthentication("https://example.com", "Basic dXNlcjpzZWNyZXRQYXNz");
    }

    @Test
    public void shouldSwitchToWindowByHandleUrlTitleOrIndex() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page first = mock(Page.class);
        Page second = mock(Page.class);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(first);
        when(context.pages()).thenReturn(List.of(first, second));
        when(first.url()).thenReturn("https://example.com/one");
        when(second.url()).thenReturn("https://example.com/two");
        when(first.title()).thenReturn("One");
        when(second.title()).thenReturn("Two");
        when(session.pageByHandle("page-2")).thenReturn(second);
        BrowserActions actions = new BrowserActions(session);

        Assert.assertSame(actions.switchToWindow("page-2"), actions);

        when(session.pageByHandle("https://example.com/two")).thenReturn(null);
        Assert.assertSame(actions.switchToWindow("https://example.com/two"), actions);

        when(session.pageByHandle("Two")).thenReturn(null);
        Assert.assertSame(actions.switchToWindow("Two"), actions);

        when(session.pageByHandle("1")).thenReturn(null);
        when(first.url()).thenReturn("https://example.com/other");
        when(second.url()).thenReturn("https://example.com/else");
        when(first.title()).thenReturn("Alpha");
        when(second.title()).thenReturn("Beta");
        Assert.assertSame(actions.switchToWindow("1"), actions);
        verify(session, times(4)).setPage(second);

        when(session.pageByHandle("9")).thenReturn(null);
        IllegalArgumentException missingIndex = Assert.expectThrows(IllegalArgumentException.class,
                () -> actions.switchToWindow("9"));
        Assert.assertTrue(missingIndex.getMessage().contains("index"));

        when(session.pageByHandle("missing")).thenReturn(null);
        IllegalArgumentException missingPage = Assert.expectThrows(IllegalArgumentException.class,
                () -> actions.switchToWindow("missing"));
        Assert.assertTrue(missingPage.getMessage().contains("No Playwright page matches"));
    }

    @Test
    public void shouldCloseTheCurrentWindowAndKeepTheNextOpenPage() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page current = mock(Page.class);
        Page remaining = mock(Page.class);
        when(session.page()).thenReturn(current);
        when(session.browserContext()).thenReturn(context);
        when(context.pages()).thenReturn(List.of(remaining), List.of());
        BrowserActions actions = new BrowserActions(session);

        actions.closeCurrentWindow();
        verify(current).close();
        verify(session).setPage(remaining);

        actions.closeCurrentWindow();
        verify(session, times(1)).setPage(any(Page.class));
    }

    @Test
    public void shouldStartPlaywrightContractsAndRollBackWhenSetupFails() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        PlaywrightNetworkInterceptor interceptor = mock(PlaywrightNetworkInterceptor.class);
        when(session.networkInterceptor()).thenReturn(interceptor);
        when(session.page()).thenReturn(mock(Page.class));
        BrowserActions actions = new BrowserActions(session);
        try {
            Assert.assertSame(actions.startContractRecording("target/pw-contract.json", "/api"), actions);
            HttpContractRecorder.stopRecording();
            Assert.assertSame(actions.assertContract("target/pw-contract.json", "/api"), actions);
            Assert.assertSame(actions.verifyContract("target/pw-contract.json", "/api"), actions);
            verify(interceptor, times(3)).startObserving();

            IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                    () -> actions.startNetworkContractRecording(" "));
            Assert.assertTrue(failure.getMessage().contains("contractFilePath"));
            verify(interceptor).stopObserving();
        } finally {
            HttpContractRecorder.clear();
        }
    }
    @Test
    public void shouldCoverGettersNavigationCookiesAndAlerts() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        Page newPage = mock(Page.class);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(session.pageHandle(page)).thenReturn("page-1");
        when(session.isDialogSeen()).thenReturn(true);
        when(session.lastDialogText()).thenReturn("confirm?");
        when(page.url()).thenReturn("https://example.test/app");
        when(page.title()).thenReturn("Coverage");
        when(page.content()).thenReturn("<html></html>");
        when(page.evaluate("() => `(${window.screenX}, ${window.screenY})`")).thenReturn("(0, 0)");
        when(page.evaluate("() => window.innerHeight")).thenReturn(800);
        when(page.evaluate("() => window.innerWidth")).thenReturn(1200);
        when(page.locator("#ok")).thenReturn(mock(com.microsoft.playwright.Locator.class));
        when(page.isClosed()).thenReturn(false);
        when(context.isClosed()).thenReturn(false);
        when(context.newPage()).thenReturn(newPage);
        Cookie cookie = new Cookie("sid", "abc");
        cookie.domain = "example.test";
        cookie.path = "/";
        when(context.cookies()).thenReturn(List.of(cookie));

        BrowserActions actions = new BrowserActions(session);
        Assert.assertSame(actions.and(), actions);
        Assert.assertNotNull(actions.network());
        Assert.assertNotNull(actions.dialog());
        Assert.assertNotNull(actions.context());
        Assert.assertNotNull(actions.storage());
        Assert.assertNotNull(actions.console());
        Assert.assertNotNull(actions.script());
        Assert.assertNotNull(actions.permissions());
        Assert.assertNotNull(actions.authentication());
        Assert.assertNotNull(actions.downloads());
        Assert.assertNotNull(actions.emulation());
        Assert.assertNotNull(actions.assertThat());
        Assert.assertNotNull(actions.verifyThat());
        Assert.assertEquals(actions.getCurrentURL(), "https://example.test/app");
        Assert.assertEquals(actions.getCurrentWindowTitle(), "Coverage");
        Assert.assertEquals(actions.getPageSource(), "<html></html>");
        Assert.assertEquals(actions.getWindowHandle(), "page-1");
        Assert.assertEquals(actions.getWindowPosition(), "(0, 0)");
        Assert.assertEquals(actions.getWindowHeight(), "800");
        Assert.assertEquals(actions.getWindowWidth(), "1200");
        Assert.assertEquals(actions.getWindowSize(), "1200x800");

        Assert.assertSame(actions.navigateToURL("https://example.test/next"), actions);
        verify(page).navigate("https://example.test/next");
        Assert.assertSame(actions.openNewTab("https://example.test/tab"), actions);
        verify(context).newPage();
        verify(session).setPage(newPage);
        Assert.assertSame(actions.navigateBack(), actions);
        Assert.assertSame(actions.navigateForward(), actions);
        Assert.assertSame(actions.refreshCurrentPage(), actions);
        verify(page).goBack();
        verify(page).goForward();
        verify(page).reload();
        Assert.assertSame(actions.setWindowSize(1024, 768), actions);
        verify(page).setViewportSize(1024, 768);

        Assert.assertTrue(actions.isAlertPresent());
        Assert.assertEquals(actions.getAlertText(), "confirm?");
        Assert.assertSame(actions.acceptAlert(), actions);
        Assert.assertSame(actions.dismissAlert(), actions);
        Assert.assertSame(actions.typeIntoPromptAlert("typed"), actions);
        verify(session).acceptNextDialog();
        verify(session).dismissNextDialog();
        verify(session).typeIntoNextPrompt("typed");

        Assert.assertSame(actions.addCookie("sid", "abc"), actions);
        Assert.assertEquals(actions.getCookieValue("sid"), "abc");
        Assert.assertEquals(actions.getCookieDomain("sid"), "example.test");
        Assert.assertEquals(actions.getCookiePath("sid"), "/");
        Assert.assertNotNull(actions.getCookie("sid"));
        Assert.assertEquals(actions.getAllCookies().size(), 1);
        Assert.assertNull(actions.getCookie("missing"));
        Assert.assertSame(actions.deleteCookie("sid"), actions);
        Assert.assertSame(actions.deleteAllCookies(), actions);
        verify(context).clearCookies();
        verify(page).evaluate(contains("document.cookie"), eq("sid"));

        when(session.networkInterceptor()).thenReturn(new com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor(context));
        Assert.assertSame(actions.capturePageSnapshot(), actions);
        Assert.assertSame(actions.clearNetworkInterceptors(), actions);
        Assert.assertSame(actions.getNativeContext(), context);
        Assert.assertSame(actions.getNativePage(), page);
        Assert.assertSame(actions.fullScreenWindow(), actions);
        Assert.assertSame(actions.maximizeWindow(), actions);
        Assert.assertSame(actions.navigateToURL("https://example.test/a", "https://example.test/b"), actions);
        verify(page).waitForURL("https://example.test/b");
        Assert.assertSame(actions.openNewWindow("https://example.test/win"), actions);
        Assert.assertNotNull(actions.locator("#ok"));
        verify(page).locator("#ok");
    }

    @Test
    public void shouldCoverScreenshotHandlesContextAndLazyLoad() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Browser browser = mock(Browser.class);
        Page page = mock(Page.class);
        Page other = mock(Page.class);
        when(session.browserContext()).thenReturn(context);
        when(session.browser()).thenReturn(browser);
        when(session.page()).thenReturn(page);
        when(session.pageHandle(page)).thenReturn("page-1");
        when(session.pageHandle(other)).thenReturn("page-2");
        when(context.pages()).thenReturn(List.of(page, other));
        when(context.isClosed()).thenReturn(false);
        when(page.isClosed()).thenReturn(false);
        when(browser.isConnected()).thenReturn(true);
        when(page.url()).thenReturn("https://example.test/");
        when(page.content()).thenReturn("<html/>");
        when(page.screenshot(any())).thenReturn(new byte[] {9, 9, 9});

        BrowserActions actions = new BrowserActions(session);
        Assert.assertEquals(actions.getContext(), "PLAYWRIGHT");
        Assert.assertEquals(actions.getContextHandles(), List.of("PLAYWRIGHT"));
        Assert.assertSame(actions.setContext("PLAYWRIGHT"), actions);
        Assert.assertSame(actions.setContext("WEB"), actions);
        Assert.expectThrows(UnsupportedOperationException.class, () -> actions.setContext("NATIVE"));
        Assert.expectThrows(UnsupportedOperationException.class, actions::generateLightHouseReport);
        Assert.assertEquals(actions.getWindowHandles(), List.of("page-1", "page-2"));
        Assert.assertSame(actions.waitForLazyLoading(), actions);
        verify(page).waitForLoadState(any());
        Assert.assertSame(actions.captureScreenshot(), actions);
        Assert.assertSame(actions.captureScreenshot(Screenshots.VIEWPORT), actions);
        Assert.assertSame(actions.captureSnapshot(), actions);
        verify(page, org.mockito.Mockito.atLeast(2)).screenshot(any());
    }

}
