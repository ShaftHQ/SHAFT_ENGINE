package com.shaft.gui.capabilities.internal;

import com.microsoft.playwright.Browser;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
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
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.devtools.HasDevTools;
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

        Capabilities rawCapabilities = driver instanceof HasCapabilities hasCapabilities
                ? hasCapabilities.getCapabilities()
                : null;
        if (driver instanceof AppiumDriver) {
            return appium((AppiumDriver) driver, rawCapabilities);
        }
        return selenium(driver, rawCapabilities);
    }

    /**
     * Resolves features for a connected Playwright session with a live page and context.
     *
     * @param session active session
     * @return immutable fail-closed snapshot
     */
    public static AutomationCapabilities forPlaywright(PlaywrightSession session) {
        if (session == null || session.page() == null || session.page().isClosed()
                || session.browserContext() == null) {
            return AutomationCapabilities.unknown("No live Playwright page and browser context exist.");
        }
        Browser browser = session.browser();
        if (browser != null && !browser.isConnected()) {
            return AutomationCapabilities.unknown("The Playwright browser is disconnected.");
        }
        String runtime = browser == null || browser.browserType() == null
                ? "Playwright browser"
                : browser.browserType().name() + " " + browser.version();
        return AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .runtime(runtime)
                .platform("Playwright browser host")
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "Playwright Browser and Page")
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "Playwright Page")
                .nativeFeature(AutomationFeature.NETWORK_OBSERVATION, "Playwright request and response events")
                .nativeFeature(AutomationFeature.NETWORK_INTERCEPTION, "Playwright routing")
                .nativeFeature(AutomationFeature.CONSOLE_LOGS, "Playwright console and page-error events")
                .nativeFeature(AutomationFeature.BROWSING_CONTEXTS, "Playwright BrowserContext")
                .nativeFeature(AutomationFeature.SCRIPT_EXECUTION, "Playwright evaluate and bindings")
                .nativeFeature(AutomationFeature.STORAGE, "Playwright storage state")
                .nativeFeature(AutomationFeature.PERMISSIONS, "Playwright BrowserContext permissions")
                .nativeFeature(AutomationFeature.DOWNLOADS, "Playwright download events")
                .nativeFeature(AutomationFeature.TRACE, "Playwright trace with SHAFT evidence integration")
                .build();
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

        boolean bidiAdvertised = hasNegotiatedBiDi(driver, capabilities);
        if (bidiAdvertised) {
            builder.nativeFeature(AutomationFeature.BIDI, "W3C WebDriver BiDi")
                    .adaptedFeature(AutomationFeature.NETWORK_OBSERVATION, "Selenium BiDi through SHAFT")
                    .adaptedFeature(AutomationFeature.CONSOLE_LOGS, "Selenium BiDi through SHAFT")
                    .adaptedFeature(AutomationFeature.PERMISSIONS, "Selenium BiDi through SHAFT");
        }
        if (driver instanceof HasDevTools hasDevTools && hasDevTools.maybeGetDevTools().isPresent()) {
            builder.adaptedFeature(AutomationFeature.NETWORK_INTERCEPTION, "Selenium DevTools through SHAFT");
        }
        if (driver instanceof HasDownloads) {
            builder.nativeFeature(AutomationFeature.DOWNLOADS, "W3C WebDriver downloads");
        }
        if (driver instanceof HasVirtualAuthenticator) {
            builder.nativeFeature(AutomationFeature.WEBAUTHN, "W3C WebDriver virtual authenticator");
        }
        return builder.build();
    }

    private static AutomationCapabilities appium(AppiumDriver driver, Capabilities capabilities) {
        String automationName = capabilityValue(capabilities, "appium:automationName");
        AutomationCapabilities.Builder builder = AutomationCapabilities.builder(AutomationBackend.APPIUM)
                .runtime(automationName)
                .platform(platform(capabilities))
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "AppiumDriver")
                .adaptedFeature(AutomationFeature.TRACE, "SHAFT unified mobile trace");
        if (driver instanceof AndroidDriver || driver instanceof IOSDriver) {
            builder.nativeFeature(AutomationFeature.MOBILE_AUTOMATION, "Appium mobile driver extensions");
        }
        if (driver instanceof SupportsContextSwitching) {
            builder.nativeFeature(AutomationFeature.BROWSING_CONTEXTS, "Appium native and web contexts");
        }
        if (driver instanceof JavascriptExecutor
                && driver instanceof SupportsContextSwitching contexts
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
        if (driver instanceof AuthenticatesByFinger || driver instanceof PerformsTouchID) {
            builder.nativeFeature(AutomationFeature.BIOMETRICS, "Platform Appium biometric extensions");
        }
        if (hasNegotiatedBiDi(driver, capabilities)) {
            builder.nativeFeature(AutomationFeature.BIDI, "W3C WebDriver BiDi")
                    .adaptedFeature(AutomationFeature.NETWORK_OBSERVATION, "Appium BiDi through SHAFT")
                    .adaptedFeature(AutomationFeature.CONSOLE_LOGS, "Appium BiDi through SHAFT");
        }
        return builder.build();
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

    @SuppressWarnings("removal")
    private static boolean hasNegotiatedBiDi(WebDriver driver, Capabilities capabilities) {
        if (!(driver instanceof HasBiDi hasBiDi) || capabilities == null
                || hasBiDi.maybeGetBiDi().isEmpty()) {
            return false;
        }
        Object webSocketUrl = capabilities.getCapability("webSocketUrl");
        return webSocketUrl instanceof String url && !url.isBlank();
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
