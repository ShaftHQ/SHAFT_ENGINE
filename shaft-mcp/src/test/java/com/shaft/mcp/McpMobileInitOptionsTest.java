package com.shaft.mcp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * {@link McpMobileInitOptions} coerces every optional field to a safe default (blank string, zero,
 * {@code false}) before forwarding to {@link MobileService}'s real initializers (design doc
 * amendment A9). {@link MobileService#initializeWebEmulation}/{@code initializeNative} themselves
 * launch a real WebDriver/Appium session, so this test overrides them on a subclass instead of
 * calling the real ones -- proving only the coercion/forwarding this class is responsible for,
 * without needing a live browser or device.
 */
class McpMobileInitOptionsTest {

    @AfterEach
    void unregisterBridge() {
        EngineService.registerMobileInitBridge(null);
    }

    @Test
    void initializeWebEmulationCoercesAllNullOrUnsetFieldsToSafeDefaults() {
        RecordingMobileService mobileService = new RecordingMobileService();
        McpMobileInitOptions allUnset = new McpMobileInitOptions(
                null, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null);

        allUnset.initializeWebEmulation(mobileService);

        assertEquals("", mobileService.targetUrl);
        assertEquals("", mobileService.browser);
        assertEquals("", mobileService.deviceName);
        assertEquals(0, mobileService.width);
        assertEquals(0, mobileService.height);
        assertEquals(0.0, mobileService.pixelRatio);
        assertEquals("", mobileService.userAgent);
        assertFalse(mobileService.headless);
    }

    @Test
    void initializeWebEmulationForwardsExplicitValuesUnchanged() {
        RecordingMobileService mobileService = new RecordingMobileService();
        McpMobileInitOptions explicit = new McpMobileInitOptions(
                "https://example.com", "EDGE", "Galaxy S22", 400, 800, 2.5, "custom-agent", true,
                null, null, null, null, null, null, null, null, null);

        explicit.initializeWebEmulation(mobileService);

        assertEquals("https://example.com", mobileService.targetUrl);
        assertEquals("EDGE", mobileService.browser);
        assertEquals("Galaxy S22", mobileService.deviceName);
        assertEquals(400, mobileService.width);
        assertEquals(800, mobileService.height);
        assertEquals(2.5, mobileService.pixelRatio);
        assertEquals("custom-agent", mobileService.userAgent);
        assertEquals(true, mobileService.headless);
    }

    @Test
    void initializeNativeCoercesAllNullFieldsToSafeDefaults() {
        RecordingMobileService mobileService = new RecordingMobileService();
        McpMobileInitOptions allUnset = new McpMobileInitOptions(
                null, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null);

        allUnset.initializeNative(mobileService);

        assertEquals("", mobileService.platformName);
        assertEquals("", mobileService.deviceName);
        assertEquals("", mobileService.appiumServerUrl);
        assertEquals("", mobileService.automationName);
        assertEquals("", mobileService.platformVersion);
        assertEquals("", mobileService.udid);
        assertEquals("", mobileService.app);
        assertEquals("", mobileService.appPackage);
        assertEquals("", mobileService.appActivity);
        assertEquals("", mobileService.bundleId);
    }

    @Test
    void initializeNativeForwardsExplicitValuesUnchanged() {
        RecordingMobileService mobileService = new RecordingMobileService();
        McpMobileInitOptions explicit = new McpMobileInitOptions(
                null, null, "Pixel 7", null, null, null, null, null,
                "Android", "http://localhost:4723", "UiAutomator2", "14", "udid-1",
                "/path/app.apk", "com.example", ".MainActivity", null);

        explicit.initializeNative(mobileService);

        assertEquals("Android", mobileService.platformName);
        assertEquals("Pixel 7", mobileService.deviceName);
        assertEquals("http://localhost:4723", mobileService.appiumServerUrl);
        assertEquals("UiAutomator2", mobileService.automationName);
        assertEquals("14", mobileService.platformVersion);
        assertEquals("udid-1", mobileService.udid);
        assertEquals("/path/app.apk", mobileService.app);
        assertEquals("com.example", mobileService.appPackage);
        assertEquals(".MainActivity", mobileService.appActivity);
        assertEquals("", mobileService.bundleId);
    }

    @Test
    void emptyConstantHasEveryFieldUnset() {
        assertEquals(null, McpMobileInitOptions.EMPTY.targetUrl());
        assertEquals(null, McpMobileInitOptions.EMPTY.browser());
        assertEquals(null, McpMobileInitOptions.EMPTY.platformName());
        assertEquals(null, McpMobileInitOptions.EMPTY.headless());
    }

    /**
     * Overrides the two package-private initializers so no real WebDriver/Appium session is ever
     * launched; just captures the coerced arguments {@link McpMobileInitOptions} forwarded.
     */
    private static final class RecordingMobileService extends MobileService {
        String targetUrl;
        String browser;
        String deviceName;
        int width;
        int height;
        double pixelRatio;
        String userAgent;
        boolean headless;
        String platformName;
        String appiumServerUrl;
        String automationName;
        String platformVersion;
        String udid;
        String app;
        String appPackage;
        String appActivity;
        String bundleId;

        RecordingMobileService() {
            super(new EngineService());
        }

        @Override
        McpMobileSessionResult initializeWebEmulation(
                String targetUrl, String browser, String deviceName, int width, int height,
                double pixelRatio, String userAgent, boolean headless) {
            this.targetUrl = targetUrl;
            this.browser = browser;
            this.deviceName = deviceName;
            this.width = width;
            this.height = height;
            this.pixelRatio = pixelRatio;
            this.userAgent = userAgent;
            this.headless = headless;
            return new McpMobileSessionResult("web-emulation", "", deviceName, browser, true, List.of(), List.of());
        }

        @Override
        McpMobileSessionResult initializeNative(
                String platformName, String deviceName, String appiumServerUrl, String automationName,
                String platformVersion, String udid, String app, String appPackage, String appActivity,
                String bundleId) {
            this.platformName = platformName;
            this.deviceName = deviceName;
            this.appiumServerUrl = appiumServerUrl;
            this.automationName = automationName;
            this.platformVersion = platformVersion;
            this.udid = udid;
            this.app = app;
            this.appPackage = appPackage;
            this.appActivity = appActivity;
            this.bundleId = bundleId;
            return new McpMobileSessionResult("native", platformName, deviceName, "", true, List.of(), List.of());
        }
    }
}
