package com.shaft.gui.driver;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidBatteryInfo;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.ios.ListensToSyslogMessages;
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
import java.time.Instant;
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
                "MobileEvidenceActionsContract",
                "MobilePerformanceActionsContract",
                "MobileRecordingActionsContract")) {
            Assert.assertNotNull(Class.forName("com.shaft.gui.driver." + contract));
            Assert.assertEquals(descriptors("com.shaft.gui.driver." + contract),
                    Set.of("and[]->MobileActionsContract"));
        }
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileBiometricActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "fingerprint[]->MobileFingerprintActionsContract",
                "touchId[]->MobileTouchIdActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileFingerprintActionsContract"), Set.of(
                "and[]->MobileBiometricActionsContract",
                "authenticate[int]->MobileFingerprintActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileTouchIdActionsContract"), Set.of(
                "and[]->MobileBiometricActionsContract",
                "enroll[]->MobileTouchIdActionsContract",
                "match[]->MobileTouchIdActionsContract",
                "reject[]->MobileTouchIdActionsContract",
                "unenroll[]->MobileTouchIdActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileLogActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "clear[]->MobileLogActionsContract",
                "errors[]->List",
                "messages[]->List",
                "start[]->MobileLogActionsContract",
                "stop[]->MobileLogActionsContract"));
        Class<?> logContract = Class.forName("com.shaft.gui.driver.MobileLogActionsContract");
        Assert.assertEquals(logContract.getMethod("messages").getGenericReturnType().getTypeName(),
                "java.util.List<com.shaft.gui.driver.MobileLogMessage>");
        Assert.assertEquals(logContract.getMethod("errors").getGenericReturnType().getTypeName(),
                "java.util.List<com.shaft.gui.driver.MobileLogError>");
        assertRecord("com.shaft.gui.driver.MobileLogMessage",
                List.of("capturedAt:java.time.Instant", "source:java.lang.String", "text:java.lang.String"));
        assertRecord("com.shaft.gui.driver.MobileLogError", List.of(
                "capturedAt:java.time.Instant", "source:java.lang.String", "type:java.lang.String",
                "message:java.lang.String"));
        Assert.assertTrue(enumValues("com.shaft.gui.capabilities.AutomationFeature").contains("DEVICE_LOGS"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileFileActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "pull[class java.lang.String]->byte[]",
                "pullFolder[class java.lang.String]->byte[]",
                "pullText[class java.lang.String]->String",
                "pullTo[class java.lang.String, interface java.nio.file.Path]->Path",
                "push[class java.lang.String, class [B]->MobileFileActionsContract",
                "pushFrom[class java.lang.String, interface java.nio.file.Path]->MobileFileActionsContract",
                "pushText[class java.lang.String, class java.lang.String]->MobileFileActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileGestureActionsContract"), Set.of(
                "and[]->MobileActionsContract",
                "drag[]->MobileDragActionsContract",
                "swipe[]->MobileSwipeActionsContract",
                "tap[]->MobileTapActionsContract",
                "zoom[]->MobileZoomActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileTapActionsContract"), Set.of(
                "and[]->MobileGestureActionsContract",
                "at[int, int]->MobileTapActionsContract",
                "doubleOn[class org.openqa.selenium.By]->MobileTapActionsContract",
                "longPress[class org.openqa.selenium.By]->MobileTapActionsContract",
                "on[class org.openqa.selenium.By]->MobileTapActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileSwipeActionsContract"), Set.of(
                "and[]->MobileGestureActionsContract",
                "byOffset[class org.openqa.selenium.By, int, int]->MobileSwipeActionsContract",
                "fromTo[class org.openqa.selenium.By, class org.openqa.selenium.By]->MobileSwipeActionsContract",
                "fromTo[int, int, int, int, class java.time.Duration]->MobileSwipeActionsContract",
                "intoView[class org.openqa.selenium.By, class com.shaft.gui.driver.MobileSwipeDirection]->MobileSwipeActionsContract",
                "toEnd[class com.shaft.gui.driver.MobileSwipeDirection]->MobileSwipeActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileDragActionsContract"), Set.of(
                "and[]->MobileGestureActionsContract",
                "fromTo[class org.openqa.selenium.By, class org.openqa.selenium.By]->MobileDragActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MobileZoomActionsContract"), Set.of(
                "and[]->MobileGestureActionsContract",
                "in[]->MobileZoomActionsContract",
                "out[]->MobileZoomActionsContract"));
        Assert.assertEquals(enumValues("com.shaft.gui.driver.MobileSwipeDirection"),
                Set.of("UP", "DOWN", "LEFT", "RIGHT"));
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
    public void mobileLogModelsShouldEnforceTheirPublicNullAndDefaultInvariants() {
        Instant capturedAt = Instant.parse("2026-08-11T12:00:00Z");

        Assert.assertEquals(new MobileLogMessage(capturedAt, "logcat", null),
                new MobileLogMessage(capturedAt, "logcat", ""));
        Assert.expectThrows(NullPointerException.class, () -> new MobileLogMessage(null, "logcat", "message"));
        Assert.expectThrows(NullPointerException.class, () -> new MobileLogMessage(capturedAt, null, "message"));
        Assert.assertEquals(new MobileLogError(capturedAt, "syslog", null, null),
                new MobileLogError(capturedAt, "syslog", Throwable.class.getName(), ""));
        Assert.assertEquals(new MobileLogError(capturedAt, "syslog", " ", "message").type(),
                Throwable.class.getName());
        Assert.expectThrows(NullPointerException.class,
                () -> new MobileLogError(null, "syslog", "type", "message"));
        Assert.expectThrows(NullPointerException.class,
                () -> new MobileLogError(capturedAt, null, "type", "message"));
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

    @Test
    public void gestureCategoriesShouldRemainUnderOneMobileNamespace() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-gestures"));
        MobileActionsContract mobile = new SHAFT.GUI.WebDriver(driver).mobile();

        MobileGestureActionsContract gestures = mobile.gestures();

        Assert.assertSame(gestures.and(), mobile);
        Assert.assertSame(gestures.tap().and(), gestures);
        Assert.assertSame(gestures.swipe().and(), gestures);
        Assert.assertSame(gestures.drag().and(), gestures);
        Assert.assertSame(gestures.zoom().and(), gestures);
    }

    @Test
    public void gestureNamespaceShouldRequireThePinnedAppiumTouchInterface() {
        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("generic-no-touch"));

        UnsupportedOperationException exception = Assert.expectThrows(UnsupportedOperationException.class,
                () -> new SHAFT.GUI.WebDriver(generic).mobile().gestures());

        Assert.assertTrue(exception.getMessage().contains("touch gestures"));
    }

    @Test
    public void mobileLogNamespaceShouldRequireAnExactLiveListenerInterface() throws Exception {
        AndroidDriver android = Mockito.mock(AndroidDriver.class);
        SessionId liveSession = new SessionId("android-device-logs");
        Mockito.when(android.getSessionId()).thenReturn(liveSession, liveSession, (SessionId) null);
        MobileActionsContract mobile = new SHAFT.GUI.WebDriver(android).mobile();

        MobileLogActionsContract logs;
        try {
            logs = mobile.logs();
        } catch (UnsupportedOperationException missingImplementation) {
            logs = null;
        }
        Assert.assertNotNull(logs);
        Assert.assertSame(logs.and(), mobile);
        Assert.expectThrows(UnsupportedOperationException.class, mobile::logs);

        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("generic-no-device-logs"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new SHAFT.GUI.WebDriver(generic).mobile().logs());

        AppiumDriver customLogcat = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(ListensToLogcatMessages.class));
        Mockito.when(customLogcat.getSessionId()).thenReturn(new SessionId("custom-logcat-runtime"));
        MobileActionsContract customLogcatMobile = new SHAFT.GUI.WebDriver(customLogcat).mobile();
        Assert.assertSame(customLogcatMobile.logs().and(), customLogcatMobile);

        AppiumDriver customSyslog = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(ListensToSyslogMessages.class));
        Mockito.when(customSyslog.getSessionId()).thenReturn(new SessionId("custom-syslog-runtime"));
        MobileActionsContract customSyslogMobile = new SHAFT.GUI.WebDriver(customSyslog).mobile();
        Assert.assertSame(customSyslogMobile.logs().and(), customSyslogMobile);

        Class<?> implementation = Class.forName("com.shaft.gui.mobile.LogActions");
        Assert.assertFalse(Modifier.isPublic(implementation.getModifiers()));
        Assert.assertEquals(implementation.getConstructors().length, 0);
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

    private static void assertRecord(String className, List<String> components) throws Exception {
        Class<?> type = Class.forName(className);
        Assert.assertTrue(type.isRecord(), className);
        Assert.assertEquals(Arrays.stream(type.getRecordComponents())
                .map(component -> component.getName() + ":" + component.getType().getName())
                .toList(), components);
        Class<?>[] componentTypes = Arrays.stream(type.getRecordComponents())
                .map(java.lang.reflect.RecordComponent::getType)
                .toArray(Class<?>[]::new);
        Assert.assertEquals(type.getDeclaredConstructors().length, 1);
        Assert.assertNotNull(type.getDeclaredConstructor(componentTypes));
    }

    private static void assertUnsupported(Runnable action) {
        UnsupportedOperationException exception = Assert.expectThrows(UnsupportedOperationException.class, action::run);
        Assert.assertEquals(exception.getMessage(), LIVE_APPIUM_REQUIRED);
    }
}
