package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.Platform;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Saves and restores Appium session capabilities and mobile app-state snapshots for one WebDriver session.
 */
public final class MobileSessionStateManager {
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .build();

    private MobileSessionStateManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Saves the active Appium session capabilities to a JSON file.
     *
     * @param driver   active WebDriver (must be a RemoteWebDriver/Appium session)
     * @param filePath target JSON file
     */
    public static void saveCapabilities(WebDriver driver, String filePath) {
        requireDriver(driver);
        Path path = path(filePath);
        RemoteWebDriver remoteWebDriver = requireRemoteWebDriver(driver);
        Capabilities capabilities = remoteWebDriver.getCapabilities();
        SessionCapabilitiesState state = new SessionCapabilitiesState();
        state.schemaVersion = "1.0";
        state.platformName = firstNonBlank(capability(capabilities, "platformName"), platformName(capabilities));
        state.capabilities = capabilities == null ? Map.of() : new LinkedHashMap<>(capabilities.asMap());
        write(path, state);
    }

    /**
     * Loads previously saved Appium session capabilities from a JSON file.
     *
     * <p>This is a static method because capabilities must be known before a driver session exists.
     * Example:
     * <pre>{@code
     * MutableCapabilities capabilities = MobileSessionStateManager.loadCapabilities("target/mobile-session-cache/device.json");
     * DriverFactory.getHelper(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, capabilities);
     * }</pre>
     *
     * @param filePath source JSON file
     * @return capabilities restored from the file
     */
    public static MutableCapabilities loadCapabilities(String filePath) {
        Path path = path(filePath);
        try {
            SessionCapabilitiesState state = JSON.readValue(path.toFile(), SessionCapabilitiesState.class);
            Map<String, Object> capabilities = new LinkedHashMap<>();
            if (state.capabilities != null) {
                state.capabilities.forEach((key, value) -> {
                    if (value != null) {
                        capabilities.put(key, value);
                    }
                });
            }
            return new MutableCapabilities(capabilities);
        } catch (RuntimeException e) {
            throw new IllegalStateException("Could not load mobile session capabilities from `" + path + "`.", e);
        }
    }

    /**
     * Saves a snapshot of the current mobile app state (active app, context, orientation, window size)
     * to a JSON file.
     *
     * @param driver   active WebDriver
     * @param filePath target JSON file
     */
    public static void saveAppState(WebDriver driver, String filePath) {
        requireDriver(driver);
        Path path = path(filePath);
        Capabilities capabilities = capabilities(driver);
        AppStateSnapshot state = new AppStateSnapshot();
        state.schemaVersion = "1.0";
        state.appPackage = firstNonBlank(capability(capabilities, "appium:appPackage"),
                capability(capabilities, "appPackage"), SHAFT.Properties.mobile.appPackage());
        state.appActivity = firstNonBlank(capability(capabilities, "appium:appActivity"),
                capability(capabilities, "appActivity"), SHAFT.Properties.mobile.appActivity());
        state.bundleId = firstNonBlank(capability(capabilities, "appium:bundleId"),
                capability(capabilities, "bundleId"), SHAFT.Properties.mobile.bundleId());
        state.context = context(driver);
        state.orientation = orientation(driver);
        state.windowSize = windowSize(driver);
        write(path, state);
    }

    /**
     * Restores what is restorable of a previously saved mobile app-state snapshot on a live session:
     * re-activates the app, switches context, and rotates the device when the corresponding
     * capability is supported by the active driver. Unsupported restore steps are skipped silently.
     *
     * @param driver   active WebDriver
     * @param filePath source JSON file
     */
    public static void loadAppState(WebDriver driver, String filePath) {
        requireDriver(driver);
        Path path = path(filePath);
        AppStateSnapshot state;
        try {
            state = JSON.readValue(path.toFile(), AppStateSnapshot.class);
        } catch (RuntimeException e) {
            throw new IllegalStateException("Could not load mobile app state from `" + path + "`.", e);
        }
        restoreActiveApp(driver, state);
        restoreContext(driver, state);
        restoreOrientation(driver, state);
    }

    private static void restoreActiveApp(WebDriver driver, AppStateSnapshot state) {
        try {
            if (driver instanceof AndroidDriver androidDriver) {
                String appId = firstNonBlank(state.appPackage, state.bundleId);
                if (appId != null) {
                    androidDriver.activateApp(appId);
                }
            } else if (driver instanceof IOSDriver iosDriver) {
                String appId = firstNonBlank(state.bundleId, state.appPackage);
                if (appId != null) {
                    iosDriver.activateApp(appId);
                }
            }
        } catch (RuntimeException ignored) {
            // restoring the active app is best-effort; skip silently when unsupported
        }
    }

    private static void restoreContext(WebDriver driver, AppStateSnapshot state) {
        if (state.context == null || state.context.isBlank()) {
            return;
        }
        if (driver instanceof SupportsContextSwitching contextDriver) {
            try {
                String current = contextDriver.getContext();
                if (!state.context.equals(current)) {
                    contextDriver.context(state.context);
                }
            } catch (RuntimeException ignored) {
                // context switching unsupported by the active provider; skip silently
            }
        }
    }

    private static void restoreOrientation(WebDriver driver, AppStateSnapshot state) {
        if (state.orientation == null || state.orientation.isBlank()) {
            return;
        }
        ScreenOrientation orientation;
        try {
            orientation = ScreenOrientation.valueOf(state.orientation.toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException ignored) {
            return;
        }
        try {
            if (driver instanceof AndroidDriver androidDriver) {
                androidDriver.rotate(orientation);
            } else if (driver instanceof IOSDriver iosDriver) {
                iosDriver.rotate(orientation);
            }
        } catch (RuntimeException ignored) {
            // rotation unsupported by the active provider; skip silently
        }
    }

    private static void requireDriver(WebDriver driver) {
        if (driver == null) {
            throw new IllegalArgumentException("A WebDriver session is required for mobile session state.");
        }
    }

    private static RemoteWebDriver requireRemoteWebDriver(WebDriver driver) {
        if (driver instanceof RemoteWebDriver remoteWebDriver) {
            return remoteWebDriver;
        }
        throw new IllegalArgumentException("Mobile session capabilities require a RemoteWebDriver (Appium) session.");
    }

    private static Path path(String filePath) {
        if (filePath == null || filePath.isBlank()) {
            throw new IllegalArgumentException("Mobile session state path must not be blank.");
        }
        return Path.of(filePath).toAbsolutePath().normalize();
    }

    private static void write(Path path, Object state) {
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            JSON.writeValue(path.toFile(), state);
        } catch (IOException e) {
            throw new IllegalStateException("Could not save mobile session state to `" + path + "`.", e);
        }
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
            return null;
        }
        Object value = capabilities.getCapability(key);
        return value == null ? null : String.valueOf(value);
    }

    private static String platformName(Capabilities capabilities) {
        if (capabilities == null) {
            return null;
        }
        Platform platform = capabilities.getPlatformName();
        return platform == null ? null : platform.name();
    }

    private static String context(WebDriver driver) {
        if (driver instanceof SupportsContextSwitching contextDriver) {
            try {
                String context = contextDriver.getContext();
                return (context == null || context.isBlank()) ? null : context;
            } catch (RuntimeException ignored) {
                return null;
            }
        }
        return null;
    }

    private static String orientation(WebDriver driver) {
        try {
            if (driver instanceof AndroidDriver androidDriver) {
                return String.valueOf(androidDriver.getOrientation());
            }
            if (driver instanceof IOSDriver iosDriver) {
                return String.valueOf(iosDriver.getOrientation());
            }
        } catch (RuntimeException ignored) {
            return null;
        }
        return null;
    }

    private static String windowSize(WebDriver driver) {
        try {
            Dimension size = driver.manage().window().getSize();
            return size.getWidth() + "x" + size.getHeight();
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static String firstNonBlank(String... candidates) {
        if (candidates == null) {
            return null;
        }
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate.trim();
            }
        }
        return null;
    }

    @SuppressWarnings("java:S1104")
    public static class SessionCapabilitiesState {
        public String schemaVersion;
        public String platformName;
        public Map<String, Object> capabilities = new LinkedHashMap<>();
    }

    @SuppressWarnings("java:S1104")
    public static class AppStateSnapshot {
        public String schemaVersion;
        public String appPackage;
        public String appActivity;
        public String bundleId;
        public String context;
        public String orientation;
        public String windowSize;
    }
}
