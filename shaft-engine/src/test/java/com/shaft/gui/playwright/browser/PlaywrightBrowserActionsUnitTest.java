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
}
