package com.shaft.gui.element.internal;

import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import tools.jackson.databind.json.JsonMapper;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class MobileSessionStateManagerTest {
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private Path stateFile;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        if (stateFile != null) {
            Files.deleteIfExists(stateFile);
        }
    }

    @Test
    public void shouldRejectNullDriverForSaveMethods() {
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.saveCapabilities(null, "target/state.json"));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.saveAppState(null, "target/state.json"));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.loadAppState(null, "target/state.json"));
    }

    @Test
    public void shouldRejectBlankPathForSaveMethods() {
        AndroidDriver driver = mock(AndroidDriver.class);
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.saveCapabilities(driver, " "));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.saveAppState(driver, null));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.loadAppState(driver, ""));
    }

    @Test
    public void shouldRejectBlankPathForLoadCapabilities() {
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.loadCapabilities(" "));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.loadCapabilities(null));
    }

    @Test
    public void shouldRejectNonRemoteWebDriverForSaveCapabilities() {
        WebDriver driver = mock(WebDriver.class);
        Assert.assertThrows(IllegalArgumentException.class,
                () -> MobileSessionStateManager.saveCapabilities(driver, "target/state.json"));
    }

    @Test
    public void shouldSaveCapabilitiesToJsonFile() throws Exception {
        AndroidDriver driver = mock(AndroidDriver.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("platformName", "Android");
        capabilities.setCapability("appium:automationName", "UiAutomator2");
        capabilities.setCapability("appium:appPackage", "com.example.app");
        when(driver.getCapabilities()).thenReturn(capabilities);

        stateFile = Files.createTempFile("shaft-mobile-capabilities", ".json");
        MobileSessionStateManager.saveCapabilities(driver, stateFile.toString());

        MobileSessionStateManager.SessionCapabilitiesState state =
                JSON.readValue(stateFile.toFile(), MobileSessionStateManager.SessionCapabilitiesState.class);

        Assert.assertEquals(state.schemaVersion, "1.0");
        // Selenium's MutableCapabilities canonicalizes "platformName" into the Platform enum's name.
        Assert.assertEquals(state.platformName, "ANDROID");
        Assert.assertEquals(state.capabilities.get("appium:automationName"), "UiAutomator2");
        Assert.assertEquals(state.capabilities.get("appium:appPackage"), "com.example.app");
    }

    @Test
    public void shouldLoadCapabilitiesRoundTrip() throws Exception {
        stateFile = Files.createTempFile("shaft-mobile-capabilities-load", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "platformName" : "Android",
                  "capabilities" : {
                    "platformName" : "Android",
                    "appium:appPackage" : "com.example.app",
                    "appium:noReset" : true,
                    "appium:unused" : null
                  }
                }
                """);

        MutableCapabilities capabilities = MobileSessionStateManager.loadCapabilities(stateFile.toString());

        // Selenium's MutableCapabilities canonicalizes "platformName" into a Platform enum value.
        Assert.assertEquals(String.valueOf(capabilities.getCapability("platformName")), "ANDROID");
        Assert.assertEquals(capabilities.getCapability("appium:appPackage"), "com.example.app");
        Assert.assertEquals(capabilities.getCapability("appium:noReset"), true);
        Assert.assertFalse(capabilities.getCapabilityNames().contains("appium:unused"));
    }

    @Test
    public void shouldRaiseIllegalStateWhenLoadCapabilitiesFileMissing() {
        Assert.assertThrows(IllegalStateException.class,
                () -> MobileSessionStateManager.loadCapabilities("target/does-not-exist-" + System.nanoTime() + ".json"));
    }

    @Test
    public void shouldSaveAppStateToJsonFile() throws Exception {
        AndroidDriver driver = mock(AndroidDriver.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("appium:appPackage", "com.example.app");
        capabilities.setCapability("appium:appActivity", ".MainActivity");
        when(driver.getCapabilities()).thenReturn(capabilities);
        when(driver.getContext()).thenReturn("NATIVE_APP");
        when(driver.getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1080, 1920));

        stateFile = Files.createTempFile("shaft-mobile-app-state", ".json");
        MobileSessionStateManager.saveAppState(driver, stateFile.toString());

        MobileSessionStateManager.AppStateSnapshot state =
                JSON.readValue(stateFile.toFile(), MobileSessionStateManager.AppStateSnapshot.class);

        Assert.assertEquals(state.schemaVersion, "1.0");
        Assert.assertEquals(state.appPackage, "com.example.app");
        Assert.assertEquals(state.appActivity, ".MainActivity");
        Assert.assertEquals(state.context, "NATIVE_APP");
        Assert.assertEquals(state.orientation, "PORTRAIT");
        Assert.assertEquals(state.windowSize, "1080x1920");
    }

    @Test
    public void shouldRaiseIllegalStateWhenLoadAppStateFileMissing() {
        AndroidDriver driver = mock(AndroidDriver.class);
        Assert.assertThrows(IllegalStateException.class,
                () -> MobileSessionStateManager.loadAppState(driver, "target/does-not-exist-" + System.nanoTime() + ".json"));
    }

    @Test
    public void shouldRestoreAndroidAppStateFromJsonFile() throws Exception {
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getContext()).thenReturn("NATIVE_APP");

        stateFile = Files.createTempFile("shaft-mobile-app-state-load", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "appPackage" : "com.example.app",
                  "appActivity" : ".MainActivity",
                  "bundleId" : null,
                  "context" : "WEBVIEW_1",
                  "orientation" : "LANDSCAPE",
                  "windowSize" : "1080x1920"
                }
                """);

        MobileSessionStateManager.loadAppState(driver, stateFile.toString());

        verify(driver).activateApp("com.example.app");
        verify(driver).context("WEBVIEW_1");
        verify(driver).rotate(ScreenOrientation.LANDSCAPE);
    }

    @Test
    public void shouldSkipContextSwitchWhenSnapshotContextMatchesCurrentContext() throws Exception {
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getContext()).thenReturn("NATIVE_APP");

        stateFile = Files.createTempFile("shaft-mobile-app-state-same-context", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "appPackage" : null,
                  "appActivity" : null,
                  "bundleId" : null,
                  "context" : "NATIVE_APP",
                  "orientation" : null,
                  "windowSize" : null
                }
                """);

        MobileSessionStateManager.loadAppState(driver, stateFile.toString());

        verify(driver, never()).context(org.mockito.ArgumentMatchers.anyString());
        verify(driver, never()).activateApp(org.mockito.ArgumentMatchers.anyString());
        verify(driver, never()).rotate(org.mockito.ArgumentMatchers.any(ScreenOrientation.class));
    }

    @Test
    public void shouldSkipMissingFieldsWithoutThrowing() throws Exception {
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getContext()).thenReturn("NATIVE_APP");

        stateFile = Files.createTempFile("shaft-mobile-app-state-empty", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "appPackage" : null,
                  "appActivity" : null,
                  "bundleId" : null,
                  "context" : null,
                  "orientation" : null,
                  "windowSize" : null
                }
                """);

        MobileSessionStateManager.loadAppState(driver, stateFile.toString());

        verify(driver, never()).activateApp(org.mockito.ArgumentMatchers.anyString());
        verify(driver, never()).context(org.mockito.ArgumentMatchers.anyString());
        verify(driver, never()).rotate(org.mockito.ArgumentMatchers.any(ScreenOrientation.class));
    }

    @Test
    public void shouldRestoreIosAppStateUsingBundleId() throws Exception {
        IOSDriver driver = mock(IOSDriver.class);
        when(driver.getContext()).thenReturn("NATIVE_APP");

        stateFile = Files.createTempFile("shaft-mobile-app-state-ios", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "appPackage" : null,
                  "appActivity" : null,
                  "bundleId" : "com.example.iosApp",
                  "context" : "NATIVE_APP",
                  "orientation" : "PORTRAIT",
                  "windowSize" : "750x1334"
                }
                """);

        MobileSessionStateManager.loadAppState(driver, stateFile.toString());

        verify(driver).activateApp("com.example.iosApp");
        verify(driver).rotate(ScreenOrientation.PORTRAIT);
        verify(driver, never()).context(org.mockito.ArgumentMatchers.anyString());
    }

    @Test
    public void shouldSkipRestoreStepsWhenUnsupportedByDriver() throws Exception {
        WebDriver driver = mock(WebDriver.class);

        stateFile = Files.createTempFile("shaft-mobile-app-state-unsupported", ".json");
        Files.writeString(stateFile, """
                {
                  "schemaVersion" : "1.0",
                  "appPackage" : "com.example.app",
                  "appActivity" : null,
                  "bundleId" : null,
                  "context" : "WEBVIEW_1",
                  "orientation" : "PORTRAIT",
                  "windowSize" : null
                }
                """);

        MobileSessionStateManager.loadAppState(driver, stateFile.toString());
        // No interfaces implemented by the plain WebDriver mock, so every restore step must be skipped silently.
    }
}
