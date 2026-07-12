package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Route;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
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
}
