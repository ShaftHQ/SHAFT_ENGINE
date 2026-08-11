package com.shaft.gui.driver;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidBatteryInfo;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.appmanagement.ApplicationState;
import io.appium.java_client.windows.WindowsDriver;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class MobileNamespaceTest {
    private static final String LIVE_APPIUM_REQUIRED = "Mobile actions require a live Appium session.";

    @Test
    public void mobileNamespaceShouldBeDiscoverableWithoutAddingTopLevelClutter() throws Exception {
        Method mobile = DriverContract.class.getMethod("mobile");

        Assert.assertTrue(mobile.isDefault());
        Assert.assertEquals(mobile.getReturnType().getSimpleName(), "MobileActionsContract");
        Assert.assertEquals(SHAFT.GUI.WebDriver.class.getDeclaredMethod("mobile")
                .getReturnType().getSimpleName(), "MobileActions");
        Set<String> intendedSurface = Set.of(
                "and[]->DriverContract",
                "app[]->MobileApplicationActionsContract",
                "biometrics[]->MobileBiometricActionsContract",
                "context[]->MobileContextActionsContract",
                "device[]->MobileDeviceActionsContract",
                "evidence[]->MobileEvidenceActionsContract",
                "files[]->MobileFileActionsContract",
                "gestures[]->MobileGestureActionsContract",
                "logs[]->MobileLogActionsContract",
                "performance[]->MobilePerformanceActionsContract",
                "recording[]->MobileRecordingActionsContract");
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileActionsContract"), intendedSurface);
        Assert.assertEquals(descriptors("com.shaft.gui.mobile.MobileActions"), intendedSurface);
        Assert.assertEquals(Arrays.stream(Class.forName("com.shaft.gui.mobile.MobileActions").getConstructors())
                .map(constructor -> Arrays.toString(constructor.getParameterTypes()))
                .collect(Collectors.toSet()), Set.of("[class com.shaft.driver.SHAFT$GUI$WebDriver]"));

        for (String contract : Set.of(
                "MobileBiometricActionsContract",
                "MobileEvidenceActionsContract",
                "MobileFileActionsContract",
                "MobileGestureActionsContract",
                "MobileLogActionsContract",
                "MobilePerformanceActionsContract",
                "MobileRecordingActionsContract")) {
            Assert.assertNotNull(Class.forName("com.shaft.gui.driver." + contract));
            Assert.assertEquals(descriptors("com.shaft.gui.driver." + contract),
                    Set.of("and[]->MobileActionsContract"));
        }
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileContextActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "current[]->String",
                "handles[]->List",
                "nativeApp[]->MobileContextActionsContract",
                "switchTo[class java.lang.String]->MobileContextActionsContract",
                "webView[]->MobileContextActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileDeviceActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "battery[]->MobileBatteryInfo",
                "clipboard[]->MobileClipboardActionsContract",
                "isLocked[]->boolean",
                "keyboard[]->MobileKeyboardActionsContract",
                "lock[]->MobileDeviceActionsContract",
                "lock[class java.time.Duration]->MobileDeviceActionsContract",
                "orientation[]->ScreenOrientation",
                "orientation[class org.openqa.selenium.ScreenOrientation]->MobileDeviceActionsContract",
                "time[]->String",
                "time[class java.lang.String]->String",
                "unlock[]->MobileDeviceActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileKeyboardActionsContract"), Set.of(
                "and[]->MobileDeviceActionsContract",
                "hide[]->MobileKeyboardActionsContract",
                "isShown[]->boolean"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileClipboardActionsContract"), Set.of(
                "and[]->MobileDeviceActionsContract",
                "text[]->String",
                "text[class java.lang.String]->MobileClipboardActionsContract"));
        Class<?> batteryInfo = Class.forName("com.shaft.gui.driver.MobileBatteryInfo");
        Assert.assertTrue(batteryInfo.isRecord());
        Assert.assertEquals(Arrays.stream(batteryInfo.getRecordComponents())
                .map(component -> component.getName() + ":" + component.getType().getSimpleName())
                .collect(Collectors.toSet()), Set.of("level:double", "state:String"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileApplicationActionsContract"), Set.of(
                "activate[class java.lang.String]->MobileApplicationActionsContract",
                "and[]->MobileActionsContract",
                "background[class java.time.Duration]->MobileApplicationActionsContract",
                "closeConfiguredApp[]->MobileApplicationActionsContract",
                "install[class java.lang.String]->MobileApplicationActionsContract",
                "install[interface java.nio.file.Path]->MobileApplicationActionsContract",
                "isInstalled[class java.lang.String]->boolean",
                "launchConfiguredApp[]->MobileApplicationActionsContract",
                "remove[class java.lang.String]->boolean",
                "state[class java.lang.String]->MobileApplicationState",
                "terminate[class java.lang.String]->boolean"));
        Assert.assertEquals(enumValues("com.shaft.gui.driver.MobileApplicationState"), Set.of(
                "NOT_INSTALLED", "NOT_RUNNING", "RUNNING_IN_BACKGROUND_SUSPENDED",
                "RUNNING_IN_BACKGROUND", "RUNNING_IN_FOREGROUND"));
    }

    @Test
    public void concreteNamespaceShouldFailClosedUnlessTheSessionIsLiveAppium() {
        assertUnsupported(() -> new com.shaft.gui.mobile.MobileActions(null));

        AppiumDriver appium = Mockito.mock(AppiumDriver.class);
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("mobile-actions"));
        SHAFT.GUI.WebDriver live = new SHAFT.GUI.WebDriver(appium);

        Assert.assertSame(live.mobile().and(), live);

        AppiumDriver closedAppium = Mockito.mock(AppiumDriver.class);
        SHAFT.GUI.WebDriver closed = new SHAFT.GUI.WebDriver(closedAppium);
        assertUnsupported(closed::mobile);

        SHAFT.GUI.WebDriver selenium = new SHAFT.GUI.WebDriver(Mockito.mock(WebDriver.class));
        assertUnsupported(selenium::mobile);
        assertUnsupported(() -> new com.shaft.gui.mobile.MobileActions(selenium));

        live.quit();
        assertUnsupported(live::mobile);
        assertUnsupported(() -> new com.shaft.gui.mobile.MobileActions(live));

        DriverContract legacy = Mockito.mock(DriverContract.class, Mockito.CALLS_REAL_METHODS);
        assertUnsupported(legacy::mobile);
    }

    @Test
    public void androidApplicationLifecycleShouldDelegateAndReturnNativeState() throws Exception {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-apps"));
        Mockito.when(driver.isAppInstalled("com.example.app")).thenReturn(true);
        Mockito.when(driver.queryAppState("com.example.app"))
                .thenReturn(ApplicationState.RUNNING_IN_FOREGROUND);
        Mockito.when(driver.terminateApp("com.example.app")).thenReturn(true);
        Mockito.when(driver.removeApp("com.example.app")).thenReturn(true);
        SHAFT.GUI.WebDriver owner = new SHAFT.GUI.WebDriver(driver);
        MobileActionsContract mobile = owner.mobile();
        MobileApplicationActionsContract app = mobile.app();
        Path localApp = Files.createTempFile("shaft-mobile-app", ".apk");

        Assert.assertSame(app.install("https://example.test/app.apk"), app);
        Assert.assertSame(app.install(localApp), app);
        Assert.assertTrue(app.isInstalled("com.example.app"));
        Assert.assertEquals(app.state("com.example.app"), MobileApplicationState.RUNNING_IN_FOREGROUND);
        Assert.assertSame(app.activate("com.example.app"), app);
        Assert.assertTrue(app.terminate("com.example.app"));
        Assert.assertTrue(app.remove("com.example.app"));
        Assert.assertSame(app.background(Duration.ofSeconds(3)), app);
        Assert.assertSame(app.and(), mobile);

        Mockito.verify(driver).installApp("https://example.test/app.apk");
        Mockito.verify(driver).installApp(localApp.toAbsolutePath().normalize().toString());
        Mockito.verify(driver).activateApp("com.example.app");
        Mockito.verify(driver).runAppInBackground(Duration.ofSeconds(3));
    }

    @Test
    public void windowsApplicationLifecycleShouldExposeOnlyConfiguredAppLaunchAndClose() {
        WindowsDriver driver = Mockito.mock(WindowsDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("windows-app"));
        MobileApplicationActionsContract app = new SHAFT.GUI.WebDriver(driver).mobile().app();

        Assert.assertSame(app.launchConfiguredApp(), app);
        Assert.assertSame(app.closeConfiguredApp(), app);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> app.activate("com.example.not-windows"));

        Mockito.verify(driver).launchApp();
        Mockito.verify(driver).closeApp();
    }

    @Test
    public void previouslyAcquiredApplicationFacadeShouldFailClosedAfterSessionTeardown() {
        AndroidDriver android = Mockito.mock(AndroidDriver.class);
        Mockito.when(android.getSessionId()).thenReturn(new SessionId("android-live"), null);
        MobileApplicationActionsContract androidApp = new SHAFT.GUI.WebDriver(android).mobile().app();
        assertUnsupported(androidApp::launchConfiguredApp);
        assertUnsupported(() -> androidApp.activate("com.example.app"));
        Mockito.verify(android, Mockito.never()).activateApp(Mockito.anyString());

        WindowsDriver windows = Mockito.mock(WindowsDriver.class);
        Mockito.when(windows.getSessionId()).thenReturn(new SessionId("windows-live"), null);
        MobileApplicationActionsContract windowsApp = new SHAFT.GUI.WebDriver(windows).mobile().app();
        assertUnsupported(windowsApp::launchConfiguredApp);
        Mockito.verify(windows, Mockito.never()).launchApp();
    }

    @Test
    public void commonAndroidDeviceControlsShouldDelegateThroughSmallNamespaces() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        AndroidBatteryInfo battery = Mockito.mock(AndroidBatteryInfo.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-device"));
        Mockito.when(driver.isDeviceLocked()).thenReturn(true);
        Mockito.when(driver.getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        Mockito.when(driver.getDeviceTime()).thenReturn("2026-08-11T14:00:00Z");
        Mockito.when(driver.getDeviceTime("yyyy-MM-dd")).thenReturn("2026-08-11");
        Mockito.when(driver.getBatteryInfo()).thenReturn(battery);
        Mockito.when(battery.getLevel()).thenReturn(0.75);
        Mockito.when(battery.getState()).thenReturn(AndroidBatteryInfo.BatteryState.CHARGING);
        Mockito.when(driver.isKeyboardShown()).thenReturn(true);
        Mockito.when(driver.getClipboardText()).thenReturn("copied text");

        MobileActionsContract mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        MobileDeviceActionsContract device = mobile.device();

        Assert.assertSame(device.lock(), device);
        Assert.assertSame(device.lock(Duration.ofSeconds(4)), device);
        Assert.assertTrue(device.isLocked());
        Assert.assertSame(device.unlock(), device);
        Assert.assertEquals(device.orientation(), ScreenOrientation.PORTRAIT);
        Assert.assertSame(device.orientation(ScreenOrientation.LANDSCAPE), device);
        Assert.assertEquals(device.time(), "2026-08-11T14:00:00Z");
        Assert.assertEquals(device.time("yyyy-MM-dd"), "2026-08-11");
        Assert.assertEquals(device.battery(), new MobileBatteryInfo(0.75, "CHARGING"));
        Assert.assertTrue(device.keyboard().isShown());
        Assert.assertSame(device.keyboard().hide().and(), device);
        Assert.assertEquals(device.clipboard().text(), "copied text");
        Assert.assertSame(device.clipboard().text("new text").and(), device);
        Assert.assertSame(device.and(), mobile);

        Mockito.verify(driver).lockDevice();
        Mockito.verify(driver).lockDevice(Duration.ofSeconds(4));
        Mockito.verify(driver).unlockDevice();
        Mockito.verify(driver).rotate(ScreenOrientation.LANDSCAPE);
        Mockito.verify(driver).hideKeyboard();
        Mockito.verify(driver).setClipboardText("new text");
    }

    @Test
    public void missingProviderBatteryStateShouldNormalizeToUnknown() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        AndroidBatteryInfo battery = Mockito.mock(AndroidBatteryInfo.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-battery-unknown"));
        Mockito.when(driver.getBatteryInfo()).thenReturn(battery);
        Mockito.when(battery.getLevel()).thenReturn(0.5);
        Mockito.when(battery.getState()).thenReturn(null);

        Assert.assertEquals(new SHAFT.GUI.WebDriver(driver).mobile().device().battery(),
                new MobileBatteryInfo(0.5, "unknown"));
    }

    @Test
    public void mobileContextShouldExposeExactNativeAndWebViewConveniences() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-context"));
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP");
        Mockito.when(driver.getContextHandles()).thenReturn(new LinkedHashSet<>(List.of(
                "NATIVE_APP", "WEBVIEW_com.example", "WEBVIEW_com.other")));
        MobileActionsContract mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        MobileContextActionsContract context = mobile.context();

        Assert.assertEquals(context.current(), "NATIVE_APP");
        Assert.assertEquals(context.handles(), List.of("NATIVE_APP", "WEBVIEW_com.example", "WEBVIEW_com.other"));
        Assert.assertSame(context.switchTo("WEBVIEW_com.other"), context);
        Assert.assertSame(context.nativeApp(), context);
        Assert.assertSame(context.webView(), context);
        Assert.assertSame(context.and(), mobile);

        Mockito.verify(driver).context("WEBVIEW_com.other");
        Mockito.verify(driver).context("NATIVE_APP");
        Mockito.verify(driver).context("WEBVIEW_com.example");
    }

    private static Set<String> descriptors(String className) throws ClassNotFoundException {
        return Arrays.stream(Class.forName(className).getDeclaredMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                        + "->" + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
    }

    private static Set<String> enumValues(String className) throws ClassNotFoundException {
        Object[] values = Class.forName(className).getEnumConstants();
        return Arrays.stream(values).map(String::valueOf).collect(Collectors.toSet());
    }

    private static void assertUnsupported(Runnable action) {
        UnsupportedOperationException exception = Assert.expectThrows(UnsupportedOperationException.class, action::run);
        Assert.assertEquals(exception.getMessage(), LIVE_APPIUM_REQUIRED);
    }
}
