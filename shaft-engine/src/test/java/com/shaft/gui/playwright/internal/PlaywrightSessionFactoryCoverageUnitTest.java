package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.driver.SHAFT;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Map;
import java.util.Optional;

/**
 * Unit coverage for {@link PlaywrightSessionFactory} via public {@code create}/{@code attach}
 * (issue #7077). No reflection.
 */
@Test(singleThreaded = true)
public class PlaywrightSessionFactoryCoverageUnitTest {
    private String savedMode;
    private String savedEndpoint;
    private String savedBrowserName;
    private String savedChannel;
    private String savedDownloads;
    private String savedTimezone;
    private String savedLocale;
    private String savedUserAgent;
    private String savedBaseUrl;
    private String savedStorageState;
    private boolean savedHeadless;
    private boolean savedIsMobileEmulation;
    private boolean savedAcceptDownloads;
    private boolean savedJsEnabled;

    @BeforeMethod
    public void saveProps() {
        savedMode = SHAFT.Properties.playwright.connectionMode();
        savedEndpoint = SHAFT.Properties.playwright.endpoint();
        savedBrowserName = SHAFT.Properties.playwright.browserName();
        savedChannel = SHAFT.Properties.playwright.channel();
        savedDownloads = SHAFT.Properties.playwright.downloadsDirectory();
        savedTimezone = SHAFT.Properties.playwright.timezoneId();
        savedLocale = SHAFT.Properties.playwright.locale();
        savedUserAgent = SHAFT.Properties.playwright.userAgent();
        savedBaseUrl = SHAFT.Properties.web.baseURL();
        savedStorageState = SHAFT.Properties.web.storageStatePath();
        savedHeadless = SHAFT.Properties.web.headlessExecution();
        savedIsMobileEmulation = SHAFT.Properties.web.isMobileEmulation();
        savedAcceptDownloads = SHAFT.Properties.playwright.acceptDownloads();
        savedJsEnabled = SHAFT.Properties.playwright.javaScriptEnabled();
    }

    @AfterMethod(alwaysRun = true)
    public void restoreProps() {
        SHAFT.Properties.playwright.set()
                .connectionMode(savedMode)
                .endpoint(savedEndpoint)
                .browserName(savedBrowserName)
                .channel(savedChannel)
                .downloadsDirectory(savedDownloads)
                .timezoneId(savedTimezone)
                .locale(savedLocale)
                .userAgent(savedUserAgent)
                .acceptDownloads(savedAcceptDownloads)
                .javaScriptEnabled(savedJsEnabled);
        SHAFT.Properties.web.set()
                .baseURL(savedBaseUrl)
                .storageStatePath(savedStorageState)
                .headlessExecution(savedHeadless)
                .isMobileEmulation(savedIsMobileEmulation);
    }

    @Test
    public void createLocalShouldLaunchChromiumAndRegisterSession() {
        SHAFT.Properties.playwright.set()
                .connectionMode("local")
                .browserName("chromium")
                .channel("chrome")
                .downloadsDirectory("target/pw-downloads")
                .timezoneId("Africa/Cairo")
                .locale("en-US")
                .userAgent("shaft-coverage-ua")
                .acceptDownloads(true)
                .javaScriptEnabled(true);
        SHAFT.Properties.web.set().baseURL("https://example.test").storageStatePath("").headlessExecution(true);

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType chromium = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightTraceManager traceManager = Mockito.mock(PlaywrightTraceManager.class);

        Mockito.when(playwright.chromium()).thenReturn(chromium);
        Mockito.when(chromium.launch(Mockito.any(BrowserType.LaunchOptions.class))).thenReturn(browser);
        Mockito.when(browser.newContext(Mockito.any(Browser.NewContextOptions.class))).thenReturn(context);
        Mockito.when(context.newPage()).thenReturn(page);

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class);
             MockedStatic<PlaywrightTraceManager> traces = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            traces.when(() -> PlaywrightTraceManager.startIfEnabled(Mockito.any(), Mockito.any()))
                    .thenReturn(traceManager);
            playwrightStatic.when(Playwright::create).thenReturn(playwright);
            playwrightStatic.when(() -> Playwright.create(Mockito.any(Playwright.CreateOptions.class)))
                    .thenReturn(playwright);

            PlaywrightSession session = PlaywrightSessionFactory.create();
            Assert.assertSame(PlaywrightSessionManager.currentSession(), session);
            Assert.assertSame(session.page(), page);
            Mockito.verify(chromium).launch(Mockito.any(BrowserType.LaunchOptions.class));
            Mockito.verify(page).setDefaultTimeout(Mockito.anyDouble());
            Mockito.verify(page).setDefaultNavigationTimeout(Mockito.anyDouble());
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }

    @Test
    public void createConnectShouldUseEndpoint() {
        SHAFT.Properties.playwright.set()
                .connectionMode("connect")
                .endpoint("ws://127.0.0.1:9")
                .browserName("firefox");
        SHAFT.Properties.web.set().storageStatePath("");

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType firefox = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightTraceManager traceManager = Mockito.mock(PlaywrightTraceManager.class);

        Mockito.when(playwright.firefox()).thenReturn(firefox);
        Mockito.when(firefox.connect(Mockito.eq("ws://127.0.0.1:9"), Mockito.any(BrowserType.ConnectOptions.class)))
                .thenReturn(browser);
        Mockito.when(browser.newContext(Mockito.any(Browser.NewContextOptions.class))).thenReturn(context);
        Mockito.when(context.newPage()).thenReturn(page);

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class);
             MockedStatic<PlaywrightTraceManager> traces = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            traces.when(() -> PlaywrightTraceManager.startIfEnabled(Mockito.any(), Mockito.any()))
                    .thenReturn(traceManager);
            playwrightStatic.when(Playwright::create).thenReturn(playwright);

            PlaywrightSession session = PlaywrightSessionFactory.create();
            Assert.assertSame(session.page(), page);
            Mockito.verify(firefox).connect(Mockito.eq("ws://127.0.0.1:9"), Mockito.any(BrowserType.ConnectOptions.class));
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }

    @Test
    public void createConnectOverCdpShouldRejectNonChromium() {
        SHAFT.Properties.playwright.set()
                .connectionMode("connectOverCDP")
                .endpoint("http://localhost:9222")
                .browserName("firefox");
        SHAFT.Properties.web.set().storageStatePath("");

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType firefox = Mockito.mock(BrowserType.class);
        Mockito.when(playwright.firefox()).thenReturn(firefox);

        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            playwrightStatic.when(Playwright::create).thenReturn(playwright);

            IllegalArgumentException thrown = Assert.expectThrows(IllegalArgumentException.class,
                    PlaywrightSessionFactory::create);
            Assert.assertTrue(thrown.getMessage().contains("Chromium only"));
        }
    }

    @Test
    public void createConnectOverCdpShouldConnectChromium() {
        SHAFT.Properties.playwright.set()
                .connectionMode("cdp")
                .endpoint("http://localhost:9222")
                .browserName("chrome");
        SHAFT.Properties.web.set().storageStatePath("");

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType chromium = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightTraceManager traceManager = Mockito.mock(PlaywrightTraceManager.class);

        Mockito.when(playwright.chromium()).thenReturn(chromium);
        Mockito.when(chromium.connectOverCDP(Mockito.eq("http://localhost:9222"),
                        Mockito.any(BrowserType.ConnectOverCDPOptions.class)))
                .thenReturn(browser);
        Mockito.when(browser.newContext(Mockito.any(Browser.NewContextOptions.class))).thenReturn(context);
        Mockito.when(context.newPage()).thenReturn(page);

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class);
             MockedStatic<PlaywrightTraceManager> traces = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            traces.when(() -> PlaywrightTraceManager.startIfEnabled(Mockito.any(), Mockito.any()))
                    .thenReturn(traceManager);
            playwrightStatic.when(Playwright::create).thenReturn(playwright);

            Assert.assertSame(PlaywrightSessionFactory.create().page(), page);
            Mockito.verify(chromium).connectOverCDP(Mockito.eq("http://localhost:9222"),
                    Mockito.any(BrowserType.ConnectOverCDPOptions.class));
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }

    @Test
    public void createShouldRejectUnknownConnectionMode() {
        SHAFT.Properties.playwright.set().connectionMode("warp-drive").browserName("chromium");
        SHAFT.Properties.web.set().storageStatePath("");

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType chromium = Mockito.mock(BrowserType.class);
        Mockito.when(playwright.chromium()).thenReturn(chromium);

        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            playwrightStatic.when(Playwright::create).thenReturn(playwright);

            IllegalArgumentException thrown = Assert.expectThrows(IllegalArgumentException.class,
                    PlaywrightSessionFactory::create);
            Assert.assertTrue(thrown.getMessage().contains("Unsupported Playwright connection mode"));
        }
    }

    @Test
    public void createShouldMapWebkitBrowserName() {
        SHAFT.Properties.playwright.set().connectionMode("local").browserName("webkit");
        SHAFT.Properties.web.set().storageStatePath("");

        Playwright playwright = Mockito.mock(Playwright.class);
        BrowserType webkit = Mockito.mock(BrowserType.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightTraceManager traceManager = Mockito.mock(PlaywrightTraceManager.class);

        Mockito.when(playwright.webkit()).thenReturn(webkit);
        Mockito.when(webkit.launch(Mockito.any(BrowserType.LaunchOptions.class))).thenReturn(browser);
        Mockito.when(browser.newContext(Mockito.any(Browser.NewContextOptions.class))).thenReturn(context);
        Mockito.when(context.newPage()).thenReturn(page);

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try (MockedStatic<Playwright> playwrightStatic = Mockito.mockStatic(Playwright.class);
             MockedStatic<PlaywrightManagedRuntime> runtime = Mockito.mockStatic(PlaywrightManagedRuntime.class);
             MockedStatic<PlaywrightDeviceDescriptor> devices = Mockito.mockStatic(PlaywrightDeviceDescriptor.class);
             MockedStatic<PlaywrightTraceManager> traces = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            runtime.when(() -> PlaywrightManagedRuntime.environment(
                            Mockito.any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyMap()))
                    .thenReturn(Map.of());
            devices.when(() -> PlaywrightDeviceDescriptor.resolve(Mockito.any(), Mockito.any()))
                    .thenReturn(Optional.empty());
            traces.when(() -> PlaywrightTraceManager.startIfEnabled(Mockito.any(), Mockito.any()))
                    .thenReturn(traceManager);
            playwrightStatic.when(Playwright::create).thenReturn(playwright);

            Assert.assertSame(PlaywrightSessionFactory.create().page(), page);
            Mockito.verify(webkit).launch(Mockito.any(BrowserType.LaunchOptions.class));
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }

    @Test
    public void attachShouldRegisterSession() {
        Playwright playwright = Mockito.mock(Playwright.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(context.pages()).thenReturn(java.util.List.of(page));

        PlaywrightSession previous = PlaywrightSessionManager.currentSession();
        try (MockedStatic<PlaywrightTraceManager> traces = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traces.when(() -> PlaywrightTraceManager.startIfEnabled(Mockito.any(), Mockito.any()))
                    .thenReturn(Mockito.mock(PlaywrightTraceManager.class));
            PlaywrightSession session = PlaywrightSessionFactory.attach(playwright, browser, context, page);
            Assert.assertSame(PlaywrightSessionManager.currentSession(), session);
            Assert.assertSame(session.page(), page);
        } finally {
            PlaywrightSessionManager.setSession(previous);
        }
    }
}
