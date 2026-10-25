package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.MutableCapabilities;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MobileTraceMetadataOfflineContractTest {
    @Test
    public void appiumMetadataReadsCapabilitiesWithoutOpeningADevice() {
        AndroidDriver driver = mock(AndroidDriver.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("platformName", "ANDROID");
        capabilities.setCapability("appium:automationName", "UiAutomator2");
        capabilities.setCapability("appium:appPackage", "com.example.app");
        capabilities.setCapability("appium:appActivity", ".Main");
        capabilities.setCapability("appium:bundleId", "com.example.ios");
        when(driver.getCapabilities()).thenReturn(capabilities);
        when(driver.getContext()).thenReturn("NATIVE_APP");
        when(driver.getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(390, 844));

        Map<String, String> metadata = MobileTraceMetadata.mobileMetadata(driver, false);

        Assert.assertEquals(metadata.get("platformName"), "Android");
        Assert.assertEquals(metadata.get("automationName"), "UiAutomator2");
        Assert.assertEquals(metadata.get("appPackage"), "com.example.app");
        Assert.assertEquals(metadata.get("appActivity"), ".Main");
        Assert.assertEquals(metadata.get("bundleId"), "com.example.ios");
        Assert.assertEquals(metadata.get("context"), "NATIVE_APP");
        Assert.assertEquals(metadata.get("orientation"), "PORTRAIT");
        Assert.assertEquals(metadata.get("windowSize"), "390x844");
        Assert.assertFalse(metadata.containsKey("nativePageSourceExcerpt"));
    }

    @Test
    public void closedAppiumSessionReportsUnsupportedProviders() {
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getCapabilities()).thenThrow(new IllegalStateException("closed session"));
        when(driver.getContext()).thenThrow(new IllegalStateException("closed session"));
        when(driver.getOrientation()).thenThrow(new IllegalStateException("closed session"));
        when(driver.manage()).thenThrow(new IllegalStateException("closed session"));

        Map<String, String> metadata = MobileTraceMetadata.mobileMetadata(driver, false);

        Assert.assertEquals(metadata.get("context"), "unsupported by active provider");
        Assert.assertEquals(metadata.get("orientation"), "unsupported by active provider");
        Assert.assertEquals(metadata.get("windowSize"), "unsupported by active provider");
    }

    @Test
    public void plainDriverStaysEmptyWhenThePlatformIsNotMobile() {
        String platform = SHAFT.Properties.platform.targetPlatform();
        try {
            SHAFT.Properties.platform.set().targetPlatform("Linux");
            Map<String, String> metadata = MobileTraceMetadata.mobileMetadata(mock(WebDriver.class), true);
            Assert.assertTrue(metadata.isEmpty(), metadata.toString());
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(platform);
        }
    }

    @Test
    public void emulationProfilesCoverCustomPinnedAndUnknownDevices() {
        String platform = SHAFT.Properties.platform.targetPlatform();
        boolean emulation = SHAFT.Properties.web.isMobileEmulation();
        boolean custom = SHAFT.Properties.web.mobileEmulationIsCustomDevice();
        double ratio = SHAFT.Properties.web.mobileEmulationPixelRatio();
        String agent = SHAFT.Properties.web.mobileEmulationUserAgent();
        String deviceName = SHAFT.Properties.web.mobileEmulationDeviceName();
        try {
            SHAFT.Properties.platform.set().targetPlatform("Linux");
            SHAFT.Properties.web.set()
                    .isMobileEmulation(false);
            Assert.assertTrue(MobileTraceMetadata.seleniumMobileEmulationProfile("chrome").isEmpty());
            Assert.assertTrue(MobileTraceMetadata.mcpDeviceProfile("chrome").isEmpty());

            SHAFT.Properties.web.set()
                    .isMobileEmulation(true)
                    .mobileEmulationIsCustomDevice(true)
                    .mobileEmulationWidth(0)
                    .mobileEmulationHeight(844)
                    .mobileEmulationPixelRatio(0)
                    .mobileEmulationUserAgent("   ");
            Map<String, String> unavailable = MobileTraceMetadata.seleniumMobileEmulationProfile(" edge ");
            Assert.assertEquals(unavailable.get("deviceProfile.mode"), "custom");
            Assert.assertEquals(unavailable.get("deviceProfile.browserName"), "edge");
            Assert.assertEquals(unavailable.get("deviceProfile.viewport"), "unavailable");
            Assert.assertEquals(unavailable.get("deviceProfile.deviceScaleFactor"), "1.0");
            Assert.assertEquals(unavailable.get("deviceProfile.userAgent"), "browser-default");
            Assert.assertEquals(MobileTraceMetadata.mcpDeviceProfile("edge").get("mode"), "custom");

            SHAFT.Properties.web.set()
                    .mobileEmulationIsCustomDevice(false)
                    .mobileEmulationDeviceName("Pixel 5");
            Map<String, String> pinned = MobileTraceMetadata.seleniumMobileEmulationProfile("CHROME");
            Assert.assertEquals(pinned.get("deviceProfile.mode"), "devtools-device");
            Assert.assertEquals(pinned.get("deviceProfile.deviceName"), "Pixel 5");
            Assert.assertEquals(pinned.get("deviceProfile.viewport"), "393x851");
            Assert.assertEquals(pinned.get("deviceProfile.deviceScaleFactor"), "2.75");
            Assert.assertTrue(pinned.get("deviceProfile.userAgent").contains("Pixel 5"), pinned.get("deviceProfile.userAgent"));

            SHAFT.Properties.web.set().mobileEmulationDeviceName("Not A Real Phone");
            Map<String, String> unknown = MobileTraceMetadata.seleniumMobileEmulationProfile(null);
            Assert.assertEquals(unknown.get("deviceProfile.viewport"), "resolved-by-devtools");
            Assert.assertEquals(unknown.get("deviceProfile.deviceScaleFactor"), "resolved-by-devtools");
            Assert.assertEquals(unknown.get("deviceProfile.userAgent"), "resolved-by-devtools");

            RemoteWebDriver live = mock(RemoteWebDriver.class);
            doThrow(new IllegalStateException("script closed")).when(live).executeScript(org.mockito.ArgumentMatchers.anyString());
            Map<String, String> failedLive = MobileTraceMetadata.seleniumMobileEmulationProfile(live, "CHROME");
            Assert.assertEquals(failedLive.get("deviceProfile.deviceName"), "Not A Real Phone");
            Assert.assertEquals(failedLive.get("deviceProfile.viewport"), "resolved-by-devtools");
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(platform);
            SHAFT.Properties.web.set()
                    .isMobileEmulation(emulation)
                    .mobileEmulationIsCustomDevice(custom)
                    .mobileEmulationPixelRatio(ratio)
                    .mobileEmulationUserAgent(agent)
                    .mobileEmulationDeviceName(deviceName);
        }
    }

    @Test
    public void nativeSourceExcerptIsRedactedAndBounded() {
        String platform = SHAFT.Properties.platform.targetPlatform();
        String browserName = SHAFT.Properties.mobile.browserName();
        boolean includeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        try {
            SHAFT.Properties.platform.set().targetPlatform("Android");
            SHAFT.Properties.mobile.set().browserName("");
            SHAFT.Properties.reporting.set().traceIncludeNativePageSource(true);
            AndroidDriver driver = mock(AndroidDriver.class);
            when(driver.getCapabilities()).thenReturn(new MutableCapabilities());
            when(driver.getPageSource()).thenReturn("password=offline-secret " + "m".repeat(1300));

            Map<String, String> metadata = MobileTraceMetadata.mobileMetadata(driver, true);
            String excerpt = metadata.get("nativePageSourceExcerpt");
            Assert.assertNotNull(excerpt);
            Assert.assertTrue(excerpt.startsWith("password=********"), excerpt);
            Assert.assertFalse(excerpt.contains("offline-secret"), excerpt);
            Assert.assertTrue(excerpt.endsWith("..."), excerpt);
            Assert.assertTrue(excerpt.length() > 1200, String.valueOf(excerpt.length()));

            when(driver.getPageSource()).thenReturn("   ");
            Assert.assertEquals(MobileTraceMetadata.mobileMetadata(driver, true).get("nativePageSourceExcerpt"), "unavailable");

            when(driver.getPageSource()).thenThrow(new IllegalStateException("source closed"));
            Assert.assertEquals(MobileTraceMetadata.mobileMetadata(driver, true).get("nativePageSourceExcerpt"),
                    "unsupported by active provider");
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(platform);
            SHAFT.Properties.mobile.set().browserName(browserName);
            SHAFT.Properties.reporting.set().traceIncludeNativePageSource(includeSource);
        }
    }
}
