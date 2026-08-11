package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;

import java.util.concurrent.atomic.AtomicReference;

public class BrowserStateNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void unsupportedSeleniumContextShouldEmitFailedContextEvidence() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).context().current());

        var event = TraceEventRecorder.snapshot().getLast();
        Assert.assertEquals(TraceEventRecorder.snapshot().size(), 1);
        Assert.assertEquals(event.category(), "context");
        Assert.assertEquals(event.name(), "current");
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void playwrightNextDialogPolicyShouldEmitPassedDialogEvidence() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);

        new com.shaft.gui.playwright.browser.BrowserActions(session).dialog().next().accept();

        var event = TraceEventRecorder.snapshot().getLast();
        Assert.assertEquals(TraceEventRecorder.snapshot().size(), 1);
        Assert.assertEquals(event.category(), "dialog-policy");
        Assert.assertEquals(event.name(), "arm-accept-next");
        Assert.assertEquals(event.status(), "passed");
        Assert.assertEquals(event.backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void unsupportedDialogSemanticBranchesShouldEmitOneFailedEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).dialog().next());
        var selenium = TraceEventRecorder.snapshot();
        Assert.assertEquals(selenium.size(), 1);
        Assert.assertEquals(selenium.getFirst().category(), "dialog");
        Assert.assertEquals(selenium.getFirst().name(), "next-unsupported");
        Assert.assertEquals(selenium.getFirst().status(), "failed");

        TraceEventRecorder.clear();
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).dialog().current());
        var playwright = TraceEventRecorder.snapshot();
        Assert.assertEquals(playwright.size(), 1);
        Assert.assertEquals(playwright.getFirst().category(), "dialog");
        Assert.assertEquals(playwright.getFirst().name(), "current-unsupported");
        Assert.assertEquals(playwright.getFirst().status(), "failed");
        Assert.assertEquals(playwright.getFirst().backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void supportedSeleniumDialogProbeShouldEmitOnePassedDialogEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebDriver.TargetLocator target = Mockito.mock(WebDriver.TargetLocator.class);
        Mockito.when(driver.switchTo()).thenReturn(target);
        Mockito.when(target.alert()).thenReturn(Mockito.mock(org.openqa.selenium.Alert.class));

        Assert.assertTrue(new com.shaft.gui.browser.BrowserActions(driver, true).dialog().current().isPresent());

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "dialog");
        Assert.assertEquals(events.getFirst().name(), "is-present");
        Assert.assertEquals(events.getFirst().status(), "passed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void supportedSeleniumDialogTextShouldSuppressTheNestedLegacyEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        var browser = Mockito.spy(new com.shaft.gui.browser.BrowserActions(driver, true));
        Mockito.doReturn("Confirm purchase?").when(browser).getAlertText();

        Assert.assertEquals(browser.dialog().current().text(), "Confirm purchase?");

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "dialog");
        Assert.assertEquals(events.getFirst().name(), "text");
        Assert.assertEquals(events.getFirst().status(), "passed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void successfulAppiumContextSwitchShouldKeepOwnerEvidenceWithoutDuplicates() {
        AtomicReference<String> current = new AtomicReference<>("WEBVIEW_1");
        AppiumDriver driver = liveContextDriver(current);

        new com.shaft.gui.browser.BrowserActions(driver, true).context().switchTo("NATIVE_APP");

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        var event = events.getFirst();
        Assert.assertEquals(event.category(), "context");
        Assert.assertEquals(event.name(), "switch-to");
        Assert.assertEquals(event.status(), "passed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.locator(), "NATIVE_APP");
        Assert.assertEquals(event.metadata().get("requestedContext"), "NATIVE_APP");
        Assert.assertEquals(event.metadata().get("contextBefore"), "WEBVIEW_1");
        Assert.assertEquals(event.metadata().get("contextAfter"), "NATIVE_APP");
    }

    @Test
    public void failedAppiumContextSwitchShouldKeepFailureAndNativeEvidence() {
        AtomicReference<String> current = new AtomicReference<>("WEBVIEW_1");
        AppiumDriver driver = liveContextDriver(current);
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        Mockito.doThrow(new IllegalStateException("provider rejected context"))
                .when(contexts).context("NATIVE_APP");

        Assert.expectThrows(IllegalStateException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).context().switchTo("NATIVE_APP"));

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        var event = events.getFirst();
        Assert.assertEquals(event.category(), "context");
        Assert.assertEquals(event.name(), "switch-to");
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.metadata().get("requestedContext"), "NATIVE_APP");
        Assert.assertEquals(event.metadata().get("contextBefore"), "WEBVIEW_1");
        Assert.assertEquals(event.metadata().get("contextAfter"), "WEBVIEW_1");
        Assert.assertEquals(event.metadata().get("platformName"), "Android");
        Assert.assertEquals(event.exceptionType(), IllegalStateException.class.getName());
        Assert.assertEquals(event.exceptionMessage(), "provider rejected context");
    }

    private static AppiumDriver liveContextDriver(AtomicReference<String> current) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-session"));
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("platformName", "Android");
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        Mockito.when(contexts.getContext()).thenAnswer(ignored -> current.get());
        Mockito.doAnswer(invocation -> {
            current.set(invocation.getArgument(0));
            return null;
        }).when(contexts).context(Mockito.anyString());
        return driver;
    }
}
