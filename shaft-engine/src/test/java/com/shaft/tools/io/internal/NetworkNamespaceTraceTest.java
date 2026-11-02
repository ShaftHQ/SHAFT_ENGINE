package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.openqa.selenium.WebDriver;

public class NetworkNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void playwrightNetworkNamespaceShouldTracePassedAndUnsupportedActions() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);

        try (MockedStatic<PlaywrightSessionManager> manager = Mockito.mockStatic(PlaywrightSessionManager.class)) {
            manager.when(PlaywrightSessionManager::currentSession).thenReturn(null);
            var network = new com.shaft.gui.playwright.browser.BrowserActions(session).network();
            network.offline();
            Assert.expectThrows(UnsupportedOperationException.class, () -> network.throttle(1, 2, 3));
            Assert.expectThrows(UnsupportedOperationException.class, () -> network.block("*.png"));
        }

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 3);
        Assert.assertEquals(events.get(0).category(), "network");
        Assert.assertEquals(events.get(0).name(), "offline");
        Assert.assertEquals(events.get(0).status(), "passed");
        Assert.assertEquals(events.get(0).backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertEquals(events.get(1).name(), "throttling");
        Assert.assertEquals(events.get(1).status(), "failed");
        Assert.assertEquals(events.get(1).backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertEquals(events.get(2).category(), "network");
        Assert.assertEquals(events.get(2).name(), "resource blocking");
        Assert.assertEquals(events.get(2).status(), "failed");
        Assert.assertEquals(events.get(2).backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void unsupportedSeleniumNamespaceActionShouldEmitOnlyFailedNetworkEvidence() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        var network = new com.shaft.gui.browser.BrowserActions(driver, true).network();

        Assert.expectThrows(UnsupportedOperationException.class, network::offline);

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "network");
        Assert.assertEquals(events.getFirst().name(), "offline");
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void unsupportedBuilderDiscoveryShouldEmitFailedEvidenceForEachBackend() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).network().interceptRequest());
        var selenium = TraceEventRecorder.snapshot().getLast();
        Assert.assertEquals(selenium.category(), "network");
        Assert.assertEquals(selenium.name(), "intercept-request");
        Assert.assertEquals(selenium.status(), "failed");
        Assert.assertEquals(selenium.backend(), AutomationBackend.SELENIUM_WEBDRIVER);

        TraceEventRecorder.clear();
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(null).network().interceptRequest());
        var playwright = TraceEventRecorder.snapshot().getLast();
        Assert.assertEquals(playwright.category(), "network");
        Assert.assertEquals(playwright.name(), "intercept-request");
        Assert.assertEquals(playwright.status(), "failed");
        Assert.assertEquals(playwright.backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }
}
