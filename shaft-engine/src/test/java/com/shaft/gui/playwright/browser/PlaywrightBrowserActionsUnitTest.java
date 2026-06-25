package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Route;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class PlaywrightBrowserActionsUnitTest {
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
}
