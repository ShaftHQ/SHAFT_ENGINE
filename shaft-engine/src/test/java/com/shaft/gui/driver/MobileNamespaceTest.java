package com.shaft.gui.driver;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumDriver;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
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
                "MobileApplicationActionsContract",
                "MobileBiometricActionsContract",
                "MobileContextActionsContract",
                "MobileDeviceActionsContract",
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

    private static Set<String> descriptors(String className) throws ClassNotFoundException {
        return Arrays.stream(Class.forName(className).getDeclaredMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                        + "->" + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
    }

    private static void assertUnsupported(Runnable action) {
        UnsupportedOperationException exception = Assert.expectThrows(UnsupportedOperationException.class, action::run);
        Assert.assertEquals(exception.getMessage(), LIVE_APPIUM_REQUIRED);
    }
}
