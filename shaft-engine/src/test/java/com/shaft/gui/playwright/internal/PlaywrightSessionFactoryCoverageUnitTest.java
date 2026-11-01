package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Playwright;
import com.shaft.driver.SHAFT;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Unit coverage for {@link PlaywrightSessionFactory} private helpers (issue #7077).
 */
@Test(singleThreaded = true)
public class PlaywrightSessionFactoryCoverageUnitTest {
    private String savedMode;
    private String savedEndpoint;
    private String savedBrowserName;

    @BeforeMethod
    public void saveProps() {
        savedMode = SHAFT.Properties.playwright.connectionMode();
        savedEndpoint = SHAFT.Properties.playwright.endpoint();
        savedBrowserName = SHAFT.Properties.playwright.browserName();
    }

    @AfterMethod(alwaysRun = true)
    public void restoreProps() {
        SHAFT.Properties.playwright.set()
                .connectionMode(savedMode)
                .endpoint(savedEndpoint)
                .browserName(savedBrowserName);
    }

    @Test
    public void utilityConstructorShouldBeBlocked() throws Exception {
        var ctor = PlaywrightSessionFactory.class.getDeclaredConstructor();
        ctor.setAccessible(true);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class, ctor::newInstance);
        Assert.assertTrue(thrown.getCause() instanceof IllegalStateException);
    }

    @Test
    public void createBrowserShouldRejectUnknownConnectionMode() throws Exception {
        SHAFT.Properties.playwright.set().connectionMode("warp-drive").endpoint("ws://localhost:1");
        BrowserType browserType = Mockito.mock(BrowserType.class);
        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("createBrowser", BrowserType.class, String.class);
        method.setAccessible(true);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(null, browserType, "chromium"));
        Assert.assertTrue(thrown.getCause() instanceof IllegalArgumentException);
        Assert.assertTrue(thrown.getCause().getMessage().contains("Unsupported Playwright connection mode"));
    }

    @Test
    public void createBrowserConnectOverCdpShouldRejectNonChromium() throws Exception {
        SHAFT.Properties.playwright.set().connectionMode("connectOverCDP").endpoint("http://localhost:9222");
        BrowserType browserType = Mockito.mock(BrowserType.class);
        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("createBrowser", BrowserType.class, String.class);
        method.setAccessible(true);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(null, browserType, "firefox"));
        Assert.assertTrue(thrown.getCause() instanceof IllegalArgumentException);
        Assert.assertTrue(thrown.getCause().getMessage().contains("Chromium only"));
    }

    @Test
    public void createBrowserLocalShouldLaunch() throws Exception {
        SHAFT.Properties.playwright.set().connectionMode("local");
        BrowserType browserType = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        Mockito.when(browserType.launch(Mockito.any(BrowserType.LaunchOptions.class))).thenReturn(browser);
        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("createBrowser", BrowserType.class, String.class);
        method.setAccessible(true);
        Assert.assertSame(method.invoke(null, browserType, "chromium"), browser);
        Mockito.verify(browserType).launch(Mockito.any(BrowserType.LaunchOptions.class));
    }

    @Test
    public void createBrowserConnectShouldUseEndpoint() throws Exception {
        SHAFT.Properties.playwright.set().connectionMode("connect").endpoint("ws://127.0.0.1:9");
        BrowserType browserType = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        Mockito.when(browserType.connect(Mockito.eq("ws://127.0.0.1:9"), Mockito.any(BrowserType.ConnectOptions.class)))
                .thenReturn(browser);
        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("createBrowser", BrowserType.class, String.class);
        method.setAccessible(true);
        Assert.assertSame(method.invoke(null, browserType, "chromium"), browser);
    }

    @Test
    public void resolveBrowserTypeShouldMapKnownNames() throws Exception {
        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType chromium = Mockito.mock(BrowserType.class);
        BrowserType firefox = Mockito.mock(BrowserType.class);
        BrowserType webkit = Mockito.mock(BrowserType.class);
        Mockito.when(playwright.chromium()).thenReturn(chromium);
        Mockito.when(playwright.firefox()).thenReturn(firefox);
        Mockito.when(playwright.webkit()).thenReturn(webkit);

        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("resolveBrowserType", Playwright.class, String.class);
        method.setAccessible(true);
        Assert.assertSame(method.invoke(null, playwright, "chrome"), chromium);
        Assert.assertSame(method.invoke(null, playwright, "firefox"), firefox);
        Assert.assertSame(method.invoke(null, playwright, "webkit"), webkit);
    }

    @Test
    public void attachShouldRegisterSession() {
        Playwright playwright = Mockito.mock(Playwright.class);
        Browser browser = Mockito.mock(Browser.class);
        com.microsoft.playwright.BrowserContext context = Mockito.mock(com.microsoft.playwright.BrowserContext.class);
        com.microsoft.playwright.Page page = Mockito.mock(com.microsoft.playwright.Page.class);
        Mockito.when(context.pages()).thenReturn(java.util.List.of(page));

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try {
            PlaywrightSession session = PlaywrightSessionFactory.attach(playwright, browser, context, page);
            Assert.assertSame(PlaywrightSessionManager.currentSession(), session);
            Assert.assertSame(session.page(), page);
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }
}
