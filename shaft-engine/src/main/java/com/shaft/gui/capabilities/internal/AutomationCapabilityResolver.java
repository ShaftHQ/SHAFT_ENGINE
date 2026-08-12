package com.shaft.gui.capabilities.internal;

import com.microsoft.playwright.Browser;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.browser.internal.BidiConsoleLogSource;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.InteractsWithApps;
import io.appium.java_client.LocksDevice;
import io.appium.java_client.PerformsTouchActions;
import io.appium.java_client.PullsFiles;
import io.appium.java_client.PushesFiles;
import io.appium.java_client.android.AuthenticatesByFinger;
import io.appium.java_client.android.HasSupportedPerformanceDataType;
import io.appium.java_client.ios.PerformsTouchID;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.screenrecording.CanRecordScreen;
import io.appium.java_client.windows.WindowsDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ios.ListensToSyslogMessages;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.HasAuthentication;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.virtualauthenticator.HasVirtualAuthenticator;

import java.util.Locale;

/**
 * Resolves effective capability snapshots without opening protocol channels or mutating sessions.
 */
public final class AutomationCapabilityResolver {
    private AutomationCapabilityResolver() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves features proven by the live Selenium/Appium instance and negotiated capabilities.
     *
     * @param driver active driver
     * @return immutable fail-closed snapshot
     */
    public static AutomationCapabilities forWebDriver(WebDriver driver) {
        if (driver == null) {
            return AutomationCapabilities.unknown("No active Selenium or Appium driver exists.");
        }

        if (driver instanceof AppiumDriver appiumDriver && !isLiveAppium(appiumDriver)) {
            return AutomationCapabilities.unknown("No live Appium session exists.");
        }

        Capabilities rawCapabilities = driver instanceof HasCapabilities hasCapabilities
                ? hasCapabilities.getCapabilities()
                : null;
        if (driver instanceof AppiumDriver appiumDriver) {
            return appium(appiumDriver, rawCapabilities);
        }
        return selenium(driver, rawCapabilities);
    }

    private static boolean isLiveAppium(AppiumDriver driver) {
        try {
            return driver.getSessionId() != null;
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    /**
     * Resolves features for a connected Playwright session with a live page and context.
     *
     * @param session active session
     * @return immutable fail-closed snapshot
     */
    public static AutomationCapabilities forPlaywright(PlaywrightSession session) {
        if (session == null || session.browserContext() == null || session.browserContext().isClosed()) {
            return AutomationCapabilities.unknown("No live Playwright browser context exists.");
        }
        Browser browser = session.browser();
        if (browser == null || !browser.isConnected()) {
            return AutomationCapabilities.unknown("No connected Playwright browser exists.");
        }
        String runtime = browser.browserType() == null
                ? "Playwright browser"
                : browser.browserType().name() + " " + browser.version();
        AutomationCapabilities.Builder builder = AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .runtime(runtime)
                .platform("Playwright browser host")
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "Playwright BrowserContext")
                .nativeFeature(AutomationFeature.NETWORK_OBSERVATION, "Playwright request and response events")
                .nativeFeature(AutomationFeature.NETWORK_INTERCEPTION, "Playwright routing")
                .nativeFeature(AutomationFeature.BROWSING_CONTEXTS, "Playwright BrowserContext")
                .nativeFeature(AutomationFeature.STORAGE, "Playwright storage state")
                .nativeFeature(AutomationFeature.PERMISSIONS, "Playwright BrowserContext permissions")
                .nativeFeature(AutomationFeature.GEOLOCATION_EMULATION, "Playwright BrowserContext geolocation")
                .nativeFeature(AutomationFeature.DOWNLOADS, "Playwright BrowserContext download lifecycle")
                .adaptedFeature(AutomationFeature.AUTHENTICATION, "SHAFT HTTP authentication routing")
                .nativeFeature(AutomationFeature.TRACE, "Playwright BrowserContext trace with SHAFT evidence integration");
        if (session.page() != null && !session.page().isClosed()) {
            builder.nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "Playwright Browser and Page")
                    .nativeFeature(AutomationFeature.CONSOLE_LOGS, "Playwright console and page-error events")
                    .nativeFeature(AutomationFeature.SCRIPT_EXECUTION, "Playwright evaluate and bindings")
                    .nativeFeature(AutomationFeature.VIEWPORT_EMULATION, "Playwright Page viewport")
                    .nativeFeature(AutomationFeature.MEDIA_EMULATION, "Playwright Page media emulation");
        }
        return builder.build();
    }

    private static AutomationCapabilities selenium(WebDriver driver, Capabilities capabilities) {
        AutomationCapabilities.Builder builder = AutomationCapabilities
                .builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .runtime(browserRuntime(capabilities))
                .platform(platform(capabilities))
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "W3C WebDriver")
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "Selenium WebDriver")
                .nativeFeature(AutomationFeature.BROWSING_CONTEXTS, "W3C WebDriver windows and frames")
                .nativeFeature(AutomationFeature.STORAGE, "W3C WebDriver cookies and SHAFT storage state")
                .adaptedFeature(AutomationFeature.TRACE, "SHAFT unified trace");

        if (driver instanceof JavascriptExecutor) {
            builder.nativeFeature(AutomationFeature.SCRIPT_EXECUTION, "W3C WebDriver script execution");
        }
        if (BidiConsoleLogSource.isHealthy(driver)) {
            builder.adaptedFeature(AutomationFeature.CONSOLE_LOGS,
                    "Selenium session-bound BiDi events through SHAFT");
        } else if (hasBrowserConsoleLogs(driver)) {
            builder.adaptedFeature(AutomationFeature.CONSOLE_LOGS,
                    "Selenium browser logs through SHAFT");
        }
        if (BrowserNetworkInterceptor.observationCountIfPresent(driver).isPresent()) {
            builder.adaptedFeature(AutomationFeature.NETWORK_OBSERVATION,
                    "Selenium session-bound DevTools observations through SHAFT");
        }

        boolean bidiAdvertised = hasNegotiatedBiDi(driver);
        if (bidiAdvertised) {
            builder.nativeFeature(AutomationFeature.BIDI, "W3C WebDriver BiDi")
                    .adaptedFeature(AutomationFeature.PERMISSIONS, "Selenium BiDi through SHAFT")
                    .nativeFeature(AutomationFeature.SCREEN_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.GEOLOCATION_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.TIMEZONE_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.LOCALE_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.USER_AGENT_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.SCRIPTING_EMULATION, "W3C WebDriver BiDi emulation");
        }
        if (hasLiveDevTools(driver)) {
            builder.adaptedFeature(AutomationFeature.NETWORK_INTERCEPTION, "Selenium DevTools through SHAFT")
                    .adaptedFeature(AutomationFeature.VIEWPORT_EMULATION, "Selenium DevTools through SHAFT")
                    .adaptedFeature(AutomationFeature.MEDIA_EMULATION, "Selenium DevTools through SHAFT");
            if (driver instanceof HasAuthentication
                    && (!(driver instanceof RemoteWebDriver remote) || remote.getSessionId() != null)) {
                builder.nativeFeature(AutomationFeature.AUTHENTICATION, "Selenium CDP-backed HasAuthentication");
            }
        }
        if (hasEnabledDownloads(driver)) {
            builder.nativeFeature(AutomationFeature.DOWNLOADS, "W3C WebDriver downloads");
        }
        if (driver instanceof HasVirtualAuthenticator) {
            builder.nativeFeature(AutomationFeature.WEBAUTHN, "W3C WebDriver virtual authenticator");
        }
        return builder.build();
    }

    private static boolean hasEnabledDownloads(WebDriver driver) {
        if (!(driver instanceof HasDownloads downloads)
                || (driver instanceof RemoteWebDriver remote && remote.getSessionId() == null)) {
            return false;
        }
        try {
            return downloads.isDownloadsEnabled();
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static AutomationCapabilities appium(AppiumDriver driver, Capabilities capabilities) {
        String automationName = capabilityValue(capabilities, "appium:automationName");
        AutomationCapabilities.Builder builder = AutomationCapabilities.builder(AutomationBackend.APPIUM)
                .runtime(automationName)
                .platform(platform(capabilities))
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "AppiumDriver")
                .adaptedFeature(AutomationFeature.TRACE, "SHAFT unified mobile trace");
        addAppiumAutomationFeatures(builder, driver);
        addAppiumDeviceFeatures(builder, driver);
        addAppiumBiDiFeatures(builder, driver);
        addAppiumObservabilityFeatures(builder, driver);
        return builder.build();
    }

    private static void addAppiumAutomationFeatures(AutomationCapabilities.Builder builder, AppiumDriver driver) {
        if (driver instanceof AndroidDriver || driver instanceof IOSDriver) {
            builder.nativeFeature(AutomationFeature.MOBILE_AUTOMATION, "Appium mobile driver extensions");
        }
        if (driver instanceof SupportsContextSwitching) {
            builder.nativeFeature(AutomationFeature.BROWSING_CONTEXTS, "Appium native and web contexts");
        }
        if (driver instanceof SupportsContextSwitching contexts
                && isWebContext(contexts)) {
            builder.nativeFeature(AutomationFeature.SCRIPT_EXECUTION, "JavaScript in the active Appium web context")
                    .nativeFeature(AutomationFeature.STORAGE, "Web Storage in the active Appium web context");
        }
        if (driver instanceof PerformsTouchActions) {
            builder.nativeFeature(AutomationFeature.TOUCH_GESTURES, "Appium touch actions");
        }
        if (driver instanceof InteractsWithApps || driver instanceof WindowsDriver) {
            builder.nativeFeature(AutomationFeature.APP_LIFECYCLE, "Appium application commands");
        }
    }

    private static void addAppiumDeviceFeatures(AutomationCapabilities.Builder builder, AppiumDriver driver) {
        if (driver instanceof LocksDevice) {
            builder.nativeFeature(AutomationFeature.DEVICE_CONTROL, "Appium device lock controls");
        }
        if (driver instanceof HasSupportedPerformanceDataType) {
            builder.nativeFeature(AutomationFeature.PERFORMANCE_DATA, "Appium Android performance commands");
        }
        if (driver instanceof PullsFiles && driver instanceof PushesFiles) {
            builder.nativeFeature(AutomationFeature.FILE_TRANSFER, "Appium push and pull file commands");
        }
        if (driver instanceof CanRecordScreen) {
            builder.nativeFeature(AutomationFeature.SCREEN_RECORDING, "Appium screen recording");
        }
    }

    private static void addAppiumObservabilityFeatures(AutomationCapabilities.Builder builder, AppiumDriver driver) {
        if (driver instanceof ListensToLogcatMessages || driver instanceof ListensToSyslogMessages) {
            builder.nativeFeature(AutomationFeature.DEVICE_LOGS, "Appium continuous device-log broadcast");
        }
        if (driver instanceof AuthenticatesByFinger || driver instanceof PerformsTouchID) {
            builder.nativeFeature(AutomationFeature.BIOMETRICS, "Platform Appium biometric extensions");
        }
        if (BidiConsoleLogSource.isHealthy(driver)) {
            builder.adaptedFeature(AutomationFeature.CONSOLE_LOGS,
                    "Appium session-bound BiDi events through SHAFT");
        } else if (hasBrowserConsoleLogs(driver)) {
            builder.adaptedFeature(AutomationFeature.CONSOLE_LOGS,
                    "Appium browser logs through SHAFT");
        }
        if (BrowserNetworkInterceptor.observationCountIfPresent(driver).isPresent()) {
            builder.adaptedFeature(AutomationFeature.NETWORK_OBSERVATION,
                    "Appium session-bound DevTools observations through SHAFT");
        }
    }

    private static void addAppiumBiDiFeatures(AutomationCapabilities.Builder builder, AppiumDriver driver) {
        if (hasNegotiatedBiDi(driver)) {
            builder.nativeFeature(AutomationFeature.BIDI, "W3C WebDriver BiDi")
                    .nativeFeature(AutomationFeature.SCREEN_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.GEOLOCATION_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.TIMEZONE_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.LOCALE_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.USER_AGENT_EMULATION, "W3C WebDriver BiDi emulation")
                    .nativeFeature(AutomationFeature.SCRIPTING_EMULATION, "W3C WebDriver BiDi emulation");
        }
    }

    private static boolean hasBrowserConsoleLogs(WebDriver driver) {
        try {
            return driver != null && driver.manage().logs().getAvailableLogTypes().contains("browser");
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static boolean isWebContext(SupportsContextSwitching contexts) {
        try {
            String context = contexts.getContext();
            String normalized = context == null ? "" : context.toUpperCase(Locale.ROOT);
            return normalized.contains("WEB") || normalized.contains("CHROMIUM");
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    /**
     * Returns whether one live driver has both a usable BiDi object and the negotiated websocket capability.
     * Runtime protocol users call this same predicate so advertised and executable support cannot diverge.
     */
    @SuppressWarnings("removal")
    public static boolean hasNegotiatedBiDi(WebDriver driver) {
        try {
            if ((driver instanceof RemoteWebDriver remote && remote.getSessionId() == null)
                    || !(driver instanceof HasBiDi hasBiDi)
                    || !(driver instanceof HasCapabilities hasCapabilities)
                    || hasBiDi.maybeGetBiDi().isEmpty()) {
                return false;
            }
            Capabilities capabilities = hasCapabilities.getCapabilities();
            Object webSocketUrl = capabilities == null ? null : capabilities.getCapability("webSocketUrl");
            return webSocketUrl instanceof String url && !url.isBlank();
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static boolean hasLiveDevTools(WebDriver driver) {
        return (!(driver instanceof RemoteWebDriver remote) || remote.getSessionId() != null)
                && driver instanceof HasDevTools hasDevTools
                && hasDevTools.maybeGetDevTools().isPresent();
    }

    private static String browserRuntime(Capabilities capabilities) {
        if (capabilities == null) {
            return "Selenium WebDriver";
        }
        return (capabilities.getBrowserName() + " " + capabilities.getBrowserVersion()).trim();
    }

    private static String platform(Capabilities capabilities) {
        return capabilities == null || capabilities.getPlatformName() == null
                ? "unknown"
                : capabilities.getPlatformName().toString();
    }

    private static String capabilityValue(Capabilities capabilities, String key) {
        if (capabilities == null || capabilities.getCapability(key) == null) {
            return "Appium";
        }
        return capabilities.getCapability(key).toString();
    }
}
