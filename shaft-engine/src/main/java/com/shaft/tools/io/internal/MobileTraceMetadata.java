package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Collects mobile and mobile-emulation metadata for SHAFT trace events.
 */
public final class MobileTraceMetadata {
    private static final int SOURCE_EXCERPT_LIMIT = 1200;

    private MobileTraceMetadata() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Builds Appium/mobile metadata for an action trace event.
     *
     * @param driver active WebDriver session
     * @param includeNativeSource whether to include a bounded native source excerpt when enabled
     * @return trace metadata; empty when the driver/properties do not describe a mobile session
     */
    public static Map<String, String> mobileMetadata(WebDriver driver, boolean includeNativeSource) {
        if (!isMobileSession(driver)) {
            return Map.of();
        }
        Map<String, String> metadata = new LinkedHashMap<>();
        Capabilities capabilities = capabilities(driver);
        put(metadata, "platformName", normalizePlatform(firstNonBlank(capability(capabilities, "platformName"),
                platformName(capabilities), SHAFT.Properties.platform.targetPlatform())));
        put(metadata, "automationName", firstNonBlank(capability(capabilities, "appium:automationName"),
                capability(capabilities, "automationName"), SHAFT.Properties.mobile.automationName()));
        put(metadata, "appPackage", firstNonBlank(capability(capabilities, "appium:appPackage"),
                capability(capabilities, "appPackage"), SHAFT.Properties.mobile.appPackage(), "unavailable"));
        put(metadata, "appActivity", firstNonBlank(capability(capabilities, "appium:appActivity"),
                capability(capabilities, "appActivity"), SHAFT.Properties.mobile.appActivity(), "unavailable"));
        put(metadata, "bundleId", firstNonBlank(capability(capabilities, "appium:bundleId"),
                capability(capabilities, "bundleId"), SHAFT.Properties.mobile.bundleId(), "unavailable"));
        put(metadata, "context", context(driver));
        put(metadata, "orientation", orientation(driver));
        put(metadata, "windowSize", windowSize(driver));
        if (includeNativeSource
                && DriverFactoryHelper.isMobileNativeExecution()
                && SHAFT.Properties.reporting.traceIncludeNativePageSource()) {
            put(metadata, "nativePageSourceExcerpt", nativeSourceExcerpt(driver));
        }
        return metadata;
    }

    /**
     * Builds the Selenium Chrome/Edge mobile-emulation profile from current SHAFT properties.
     *
     * @param browserName active browser name
     * @return trace metadata using {@code deviceProfile.*} keys
     */
    public static Map<String, String> seleniumMobileEmulationProfile(String browserName) {
        if (!SHAFT.Properties.web.isMobileEmulation() || !DriverFactoryHelper.isNotMobileExecution()) {
            return Map.of();
        }
        Map<String, String> metadata = new LinkedHashMap<>();
        String browser = value(browserName);
        put(metadata, "deviceProfile.browserName", browser);
        put(metadata, "deviceProfile.mobile", "true");
        put(metadata, "deviceProfile.touch", "true");
        put(metadata, "deviceProfile.source", "selenium-mobile-emulation");
        if (SHAFT.Properties.web.mobileEmulationIsCustomDevice()) {
            put(metadata, "deviceProfile.mode", "custom");
            put(metadata, "deviceProfile.viewport", viewport(
                    SHAFT.Properties.web.mobileEmulationWidth(),
                    SHAFT.Properties.web.mobileEmulationHeight()));
            put(metadata, "deviceProfile.deviceScaleFactor",
                    scale(SHAFT.Properties.web.mobileEmulationPixelRatio()));
            put(metadata, "deviceProfile.userAgent",
                    firstNonBlank(SHAFT.Properties.web.mobileEmulationUserAgent(), "browser-default"));
            return metadata;
        }
        String deviceName = firstNonBlank(SHAFT.Properties.web.mobileEmulationDeviceName(), "Pixel 5");
        put(metadata, "deviceProfile.mode", "devtools-device");
        put(metadata, "deviceProfile.deviceName", deviceName);
        var pinnedProfile = com.shaft.driver.internal.DriverFactory.EmulatedDeviceProfiles.of(deviceName);
        if (pinnedProfile.isPresent()) {
            put(metadata, "deviceProfile.viewport", pinnedProfile.get().viewport());
            put(metadata, "deviceProfile.deviceScaleFactor", String.valueOf(pinnedProfile.get().pixelRatio()));
            put(metadata, "deviceProfile.userAgent", pinnedProfile.get().userAgent());
        } else {
            put(metadata, "deviceProfile.viewport", "resolved-by-devtools");
            put(metadata, "deviceProfile.deviceScaleFactor", "resolved-by-devtools");
            put(metadata, "deviceProfile.userAgent", "resolved-by-devtools");
        }
        return metadata;
    }

    /**
     * Builds the Selenium mobile-emulation profile and enriches it with live browser values when available.
     *
     * @param driver active emulated browser session
     * @param browserName active browser name
     * @return trace metadata using {@code deviceProfile.*} keys
     */
    public static Map<String, String> seleniumMobileEmulationProfile(WebDriver driver, String browserName) {
        Map<String, String> metadata = new LinkedHashMap<>(seleniumMobileEmulationProfile(browserName));
        if (metadata.isEmpty() || !(driver instanceof JavascriptExecutor javascriptExecutor)) {
            return metadata;
        }
        put(metadata, "deviceProfile.viewport", liveString(javascriptExecutor,
                "return window.innerWidth + 'x' + window.innerHeight;"));
        put(metadata, "deviceProfile.deviceScaleFactor", liveString(javascriptExecutor,
                "return String(window.devicePixelRatio || 1);"));
        put(metadata, "deviceProfile.userAgent", liveString(javascriptExecutor,
                "return navigator.userAgent || '';"));
        put(metadata, "deviceProfile.touch", liveString(javascriptExecutor,
                "return String((navigator.maxTouchPoints || 0) > 0);"));
        put(metadata, "deviceProfile.mobile", liveString(javascriptExecutor,
                "return String(/Mobi|Android|iPhone|iPad/i.test(navigator.userAgent || ''));"));
        return metadata;
    }

    /**
     * Converts Selenium trace metadata into the unprefixed MCP response shape.
     *
     * @param browserName active browser name
     * @return device profile details for MCP session responses
     */
    public static Map<String, String> mcpDeviceProfile(String browserName) {
        return mcpDeviceProfile(null, browserName);
    }

    /**
     * Converts Selenium trace metadata into the unprefixed MCP response shape.
     *
     * @param driver active emulated browser session
     * @param browserName active browser name
     * @return device profile details for MCP session responses
     */
    public static Map<String, String> mcpDeviceProfile(WebDriver driver, String browserName) {
        Map<String, String> traceProfile = driver == null
                ? seleniumMobileEmulationProfile(browserName)
                : seleniumMobileEmulationProfile(driver, browserName);
        if (traceProfile.isEmpty()) {
            return Map.of();
        }
        Map<String, String> profile = new LinkedHashMap<>();
        traceProfile.forEach((key, value) -> {
            if (key.startsWith("deviceProfile.")) {
                profile.put(key.substring("deviceProfile.".length()), value);
            }
        });
        return profile;
    }

    private static boolean isMobileSession(WebDriver driver) {
        return driver instanceof AppiumDriver
                || DriverFactoryHelper.isMobileNativeExecution()
                || DriverFactoryHelper.isMobileWebExecution();
    }

    private static Capabilities capabilities(WebDriver driver) {
        if (driver instanceof RemoteWebDriver remoteWebDriver) {
            try {
                return remoteWebDriver.getCapabilities();
            } catch (RuntimeException ignored) {
                return null;
            }
        }
        return null;
    }

    private static String capability(Capabilities capabilities, String key) {
        if (capabilities == null || key == null || key.isBlank()) {
            return "";
        }
        Object value = capabilities.getCapability(key);
        return value == null ? "" : String.valueOf(value);
    }

    private static String platformName(Capabilities capabilities) {
        if (capabilities == null) {
            return "";
        }
        Platform platform = capabilities.getPlatformName();
        return platform == null ? "" : platform.name();
    }

    private static String normalizePlatform(String platformName) {
        if ("ANDROID".equalsIgnoreCase(platformName)) {
            return "Android";
        }
        if ("IOS".equalsIgnoreCase(platformName)) {
            return "iOS";
        }
        return platformName;
    }

    private static String context(WebDriver driver) {
        if (driver instanceof SupportsContextSwitching contextDriver) {
            try {
                return firstNonBlank(contextDriver.getContext(), "unavailable");
            } catch (RuntimeException ignored) {
                return "unsupported by active provider";
            }
        }
        return "unsupported by active driver";
    }

    private static String orientation(WebDriver driver) {
        try {
            if (driver instanceof AndroidDriver androidDriver) {
                return String.valueOf(androidDriver.getOrientation());
            }
            if (driver instanceof IOSDriver iosDriver) {
                return String.valueOf(iosDriver.getOrientation());
            }
            return "unsupported by active driver";
        } catch (RuntimeException ignored) {
            return "unsupported by active provider";
        }
    }

    private static String windowSize(WebDriver driver) {
        try {
            Dimension size = driver.manage().window().getSize();
            return size.getWidth() + "x" + size.getHeight();
        } catch (RuntimeException ignored) {
            return "unsupported by active provider";
        }
    }

    private static String nativeSourceExcerpt(WebDriver driver) {
        try {
            String source = driver.getPageSource();
            if (source == null || source.isBlank()) {
                return "unavailable";
            }
            String redacted = FailureTraceReporter.redact(source);
            return redacted.length() <= SOURCE_EXCERPT_LIMIT
                    ? redacted
                    : redacted.substring(0, SOURCE_EXCERPT_LIMIT) + "...";
        } catch (RuntimeException ignored) {
            return "unsupported by active provider";
        }
    }

    private static String viewport(int width, int height) {
        return width > 0 && height > 0 ? width + "x" + height : "unavailable";
    }

    private static String scale(double pixelRatio) {
        return pixelRatio > 0 ? String.valueOf(pixelRatio) : "1.0";
    }

    private static String liveString(JavascriptExecutor javascriptExecutor, String script) {
        try {
            Object value = javascriptExecutor.executeScript(script);
            return value == null ? "" : String.valueOf(value);
        } catch (RuntimeException ignored) {
            return "";
        }
    }

    private static String firstNonBlank(String... candidates) {
        if (candidates == null) {
            return "";
        }
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate.trim();
            }
        }
        return "";
    }

    private static String value(String value) {
        return value == null ? "" : value.trim();
    }

    private static void put(Map<String, String> metadata, String key, String value) {
        if (value != null && !value.isBlank()) {
            metadata.put(key, value);
        }
    }
}
