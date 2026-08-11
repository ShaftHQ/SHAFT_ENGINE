package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class BrowserPermissionsNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void playwrightGrantShouldEmitOneBackendOwnedPermissionEvent() {
        PlaywrightSession session = livePlaywrightSession();

        new com.shaft.gui.playwright.browser.BrowserActions(session).permissions()
                .grantFor("https://example.test", "geolocation");

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "permissions");
        Assert.assertEquals(events.getFirst().name(), "grant");
        Assert.assertEquals(events.getFirst().locator(), "https://example.test");
        Assert.assertEquals(events.getFirst().status(), "passed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void unsupportedSeleniumPermissionShouldEmitOneFailedEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).permissions()
                        .grantFor("https://example.test", "geolocation"));

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "permissions");
        Assert.assertEquals(events.getFirst().name(), "grant");
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.SELENIUM_WEBDRIVER);
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
