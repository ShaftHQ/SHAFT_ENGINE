package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.mockito.Mockito;
import org.openqa.selenium.HasAuthentication;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Optional;

public class BrowserAuthenticationNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void authenticationFailureShouldRethrowProviderErrorWithoutLeakingCredentials() {
        String username = "opaque-user-431";
        String password = "opaque-password-927";
        PlaywrightSession session = livePlaywrightSession();
        PlaywrightNetworkInterceptor interceptor = session.networkInterceptor();
        IllegalStateException providerFailure = new IllegalStateException(
                "Rejected " + username + " with " + password);
        Mockito.doThrow(providerFailure).when(interceptor)
                .registerBasicAuthentication(Mockito.any(), Mockito.anyString());

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).authentication()
                        .basicFor("https://example.test", username, password));

        Assert.assertSame(thrown, providerFailure);
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "authentication");
        Assert.assertEquals(events.getFirst().name(), "register-basic");
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertFalse(events.getFirst().toString().contains(username));
        Assert.assertFalse(events.getFirst().toString().contains(password));

        String report = FailureTraceReporter.renderTraceJson(info(thrown), "", List.of());
        Assert.assertFalse(report.contains(username), report);
        Assert.assertFalse(report.contains(password), report);
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    @Test
    public void authenticationSuccessShouldEmitOneBackendOwnedEvent() {
        PlaywrightSession session = livePlaywrightSession();

        new com.shaft.gui.playwright.browser.BrowserActions(session).authentication()
                .basicFor("https://example.test", "user", "password");

        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "authentication");
        Assert.assertEquals(events.getFirst().name(), "register-basic");
        Assert.assertEquals(events.getFirst().status(), "passed");
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertFalse(events.getFirst().toString().contains("password"));
    }

    @Test
    public void playwrightCurrentOriginAuthenticationShouldUseSemanticTraceLocator() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.page().url()).thenReturn("https://current.test/page");

        new com.shaft.gui.playwright.browser.BrowserActions(session).authentication()
                .basic("user", "password");

        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().locator(), "<current-origin>");
    }

    @Test
    public void seleniumCurrentOriginAuthenticationShouldUseSemanticTraceLocator() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class, HasDevTools.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("selenium"));
        Mockito.when(driver.getCurrentUrl()).thenReturn("https://current.test/page");
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(Mockito.mock(DevTools.class)));

        new com.shaft.gui.browser.BrowserActions(driver, true).authentication()
                .basic("user", "password");

        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().locator(), "<current-origin>");
    }

    @Test
    public void successfulCredentialsShouldBeRemovedFromLaterSourceContextEvidence() {
        String username = "source-user-6137";
        String password = "source-password-8452";
        PlaywrightSession session = livePlaywrightSession();
        boolean originalCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        try {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(true);
            new com.shaft.gui.playwright.browser.BrowserActions(session).authentication()
                    .basicFor("https://example.test", username, password);
            AssertionError unrelatedFailure = new AssertionError("unrelated terminal failure");

            String report = FailureTraceReporter.renderTraceJson(info(unrelatedFailure), "", List.of());

            Assert.assertFalse(report.contains(username), report);
            Assert.assertFalse(report.contains(password), report);
            Assert.assertTrue(report.contains("unrelated terminal failure"), report);
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeCodeContext(originalCodeContext);
        }
    }

    @Test
    public void rejectedCredentialBearingPlaywrightUrlShouldNeverEnterTraceEvidence() {
        String embeddedUser = String.join("", "visible", "-user-781");
        String embeddedPassword = String.join("", "visible", "-password-294");
        String url = "https://" + embeddedUser + ":" + embeddedPassword + "@example.test/private";
        PlaywrightSession session = livePlaywrightSession();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).authentication()
                        .navigateTo(url, "safe-user", "safe-password"));

        assertCredentialBearingUrlOmitted(embeddedUser, embeddedPassword, thrown);
    }

    @Test
    public void rejectedCredentialBearingSeleniumUrlShouldNeverEnterTraceEvidence() {
        String embeddedUser = String.join("", "selenium", "-user-537");
        String embeddedPassword = String.join("", "selenium", "-password-862");
        String url = "https://" + embeddedUser + ":" + embeddedPassword + "@example.test/private";
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class, HasDevTools.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("selenium"));
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(Mockito.mock(DevTools.class)));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).authentication()
                        .navigateTo(url, "safe-user", "safe-password"));

        assertCredentialBearingUrlOmitted(embeddedUser, embeddedPassword, thrown);
    }

    @Test
    public void rejectedCredentialBearingPlaywrightOriginShouldNeverEnterTraceEvidence() {
        String embeddedUser = String.join("", "origin-user", "-418");
        String embeddedPassword = String.join("", "origin-password", "-735");
        String origin = "https://" + embeddedUser + ":" + embeddedPassword + "@example.test";

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(livePlaywrightSession()).authentication()
                        .basicFor(origin, "safe-user", "safe-password"));

        assertCredentialBearingOriginOmitted(embeddedUser, embeddedPassword, thrown);
    }

    @Test
    public void rejectedCredentialBearingSeleniumOriginShouldNeverEnterTraceEvidence() {
        String embeddedUser = String.join("", "origin-selenium-user", "-842");
        String embeddedPassword = String.join("", "origin-selenium-password", "-196");
        String origin = "https://" + embeddedUser + ":" + embeddedPassword + "@example.test";
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class, HasDevTools.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("selenium"));
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(Mockito.mock(DevTools.class)));

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).authentication()
                        .basicFor(origin, "safe-user", "safe-password"));

        assertCredentialBearingOriginOmitted(embeddedUser, embeddedPassword, thrown);
    }

    private static void assertCredentialBearingUrlOmitted(String embeddedUser, String embeddedPassword,
                                                           RuntimeException thrown) {
        var event = TraceEventRecorder.snapshot().getFirst();
        Assert.assertEquals(event.locator(), "<credential-bearing-url>");
        Assert.assertFalse(event.toString().contains(embeddedUser));
        Assert.assertFalse(event.toString().contains(embeddedPassword));
        String report = FailureTraceReporter.renderTraceJson(info(thrown), "", List.of());
        Assert.assertFalse(report.contains(embeddedUser), report);
        Assert.assertFalse(report.contains(embeddedPassword), report);
    }

    private static void assertCredentialBearingOriginOmitted(String embeddedUser, String embeddedPassword,
                                                              RuntimeException thrown) {
        var event = TraceEventRecorder.snapshot().getFirst();
        Assert.assertEquals(event.locator(), "<credential-bearing-origin>");
        Assert.assertFalse(event.toString().contains(embeddedUser));
        Assert.assertFalse(event.toString().contains(embeddedPassword));
        String report = FailureTraceReporter.renderTraceJson(info(thrown), "", List.of());
        Assert.assertFalse(report.contains(embeddedUser), report);
        Assert.assertFalse(report.contains(embeddedPassword), report);
    }

    private static TestExecutionInfo info(Throwable throwable) {
        return new TestExecutionInfo("auth-id", BrowserAuthenticationNamespaceTraceTest.class.getName(),
                "authenticationFailure", "authenticationFailure", "authentication trace", null,
                throwable, false);
    }

    private static PlaywrightSession livePlaywrightSession() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightNetworkInterceptor interceptor = Mockito.mock(PlaywrightNetworkInterceptor.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(session.networkInterceptor()).thenReturn(interceptor);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        return session;
    }
}
