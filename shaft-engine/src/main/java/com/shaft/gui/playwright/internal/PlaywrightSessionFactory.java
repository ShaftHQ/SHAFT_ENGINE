package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;

import java.nio.file.Path;
import java.util.Locale;

/**
 * Creates Playwright sessions from SHAFT web and Playwright properties.
 */
public final class PlaywrightSessionFactory {
    private PlaywrightSessionFactory() {
        throw new IllegalStateException("Utility class");
    }

    public static PlaywrightSession create() {
        Playwright playwright = Playwright.create();
        BrowserType browserType = resolveBrowserType(playwright);
        Browser browser = createBrowser(browserType);
        BrowserContext browserContext = browser.newContext(createContextOptions());
        Page page = browserContext.newPage();
        page.setDefaultTimeout(SHAFT.Properties.playwright.defaultTimeoutMilliseconds());
        page.setDefaultNavigationTimeout(SHAFT.Properties.playwright.navigationTimeoutMilliseconds());

        PlaywrightTraceManager traceManager = PlaywrightTraceManager.startIfEnabled(browserContext, artifactsPath());
        PlaywrightSession session = new PlaywrightSession(playwright, browser, browserContext, page, traceManager);
        PlaywrightSessionManager.setSession(session);
        ReportManager.logDiscrete("Created Playwright GUI session using browser '" + browserName() + "'.", Level.INFO);
        return session;
    }

    public static PlaywrightSession attach(Playwright playwright, Browser browser, BrowserContext browserContext, Page page) {
        PlaywrightTraceManager traceManager = PlaywrightTraceManager.startIfEnabled(browserContext, artifactsPath());
        PlaywrightSession session = new PlaywrightSession(playwright, browser, browserContext, page, traceManager);
        PlaywrightSessionManager.setSession(session);
        return session;
    }

    private static Browser createBrowser(BrowserType browserType) {
        String connectionMode = SHAFT.Properties.playwright.connectionMode().trim().toLowerCase(Locale.ROOT);
        String endpoint = SHAFT.Properties.playwright.endpoint().trim();
        return switch (connectionMode) {
            case "connect" -> browserType.connect(endpoint, new BrowserType.ConnectOptions()
                    .setTimeout(SHAFT.Properties.playwright.launchTimeoutMilliseconds())
                    .setSlowMo(SHAFT.Properties.playwright.slowMo()));
            case "connectovercdp", "cdp" -> {
                if (!isChromium()) {
                    throw new IllegalArgumentException("Playwright connectOverCDP supports Chromium only.");
                }
                yield browserType.connectOverCDP(endpoint, new BrowserType.ConnectOverCDPOptions()
                        .setTimeout(SHAFT.Properties.playwright.launchTimeoutMilliseconds())
                        .setSlowMo(SHAFT.Properties.playwright.slowMo()));
            }
            case "local", "" -> browserType.launch(createLaunchOptions());
            default -> throw new IllegalArgumentException("Unsupported Playwright connection mode: " + connectionMode);
        };
    }

    private static BrowserType.LaunchOptions createLaunchOptions() {
        BrowserType.LaunchOptions launchOptions = new BrowserType.LaunchOptions()
                .setHeadless(SHAFT.Properties.web.headlessExecution())
                .setTimeout(SHAFT.Properties.playwright.launchTimeoutMilliseconds())
                .setSlowMo(SHAFT.Properties.playwright.slowMo());

        if (!SHAFT.Properties.playwright.channel().isBlank()) {
            launchOptions.setChannel(SHAFT.Properties.playwright.channel());
        }
        if (!SHAFT.Properties.playwright.downloadsDirectory().isBlank()) {
            launchOptions.setDownloadsPath(Path.of(SHAFT.Properties.playwright.downloadsDirectory()));
        }
        return launchOptions;
    }

    private static Browser.NewContextOptions createContextOptions() {
        Browser.NewContextOptions contextOptions = new Browser.NewContextOptions()
                .setAcceptDownloads(SHAFT.Properties.playwright.acceptDownloads())
                .setViewportSize(SHAFT.Properties.web.browserWindowWidth(), SHAFT.Properties.web.browserWindowHeight());

        if (!SHAFT.Properties.web.baseURL().isBlank()) {
            contextOptions.setBaseURL(SHAFT.Properties.web.baseURL());
        }
        if (SHAFT.Properties.web.isMobileEmulation()) {
            contextOptions.setIsMobile(true)
                    .setDeviceScaleFactor(SHAFT.Properties.web.mobileEmulationPixelRatio());
            if (!SHAFT.Properties.web.mobileEmulationUserAgent().isBlank()) {
                contextOptions.setUserAgent(SHAFT.Properties.web.mobileEmulationUserAgent());
            }
            if (SHAFT.Properties.web.mobileEmulationWidth() > 0 && SHAFT.Properties.web.mobileEmulationHeight() > 0) {
                contextOptions.setViewportSize(SHAFT.Properties.web.mobileEmulationWidth(), SHAFT.Properties.web.mobileEmulationHeight());
            }
        }
        return contextOptions;
    }

    private static BrowserType resolveBrowserType(Playwright playwright) {
        return switch (browserName()) {
            case "firefox" -> playwright.firefox();
            case "safari", "webkit" -> playwright.webkit();
            default -> playwright.chromium();
        };
    }

    private static String browserName() {
        String playwrightBrowser = SHAFT.Properties.playwright.browserName();
        String browserName = playwrightBrowser == null || playwrightBrowser.isBlank()
                ? SHAFT.Properties.web.targetBrowserName()
                : playwrightBrowser;
        return browserName.trim().toLowerCase(Locale.ROOT);
    }

    private static boolean isChromium() {
        String browser = browserName();
        return !("firefox".equals(browser) || "safari".equals(browser) || "webkit".equals(browser));
    }

    private static Path artifactsPath() {
        return Path.of(SHAFT.Properties.playwright.artifactsDirectory());
    }
}
