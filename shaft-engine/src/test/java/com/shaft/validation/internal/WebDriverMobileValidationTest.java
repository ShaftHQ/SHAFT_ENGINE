package com.shaft.validation.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.driver.MobileApplicationState;
import com.shaft.gui.driver.MobileAssertions;
import com.shaft.gui.driver.MobileBatteryInfo;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.InteractsWithApps;
import io.appium.java_client.HasDeviceTime;
import io.appium.java_client.LocksDevice;
import io.appium.java_client.android.AndroidBatteryInfo;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.battery.HasBattery;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.remote.SupportsRotation;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.validation.ValidationEnums;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.remote.SessionId;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.TimeoutException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.Executors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class WebDriverMobileValidationTest {
    @BeforeMethod
    public void disableValidationArtifacts() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
    }

    @AfterMethod(alwaysRun = true)
    public void resetValidationState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void publicHardAndSoftRootsShouldReadAllFocusedMobileValues() {
        AndroidDriver driver = mock(AndroidDriver.class);
        AndroidBatteryInfo battery = mock(AndroidBatteryInfo.class);
        SessionId session = new SessionId("mobile-validation-values");
        when(driver.getSessionId()).thenReturn(session);
        when(driver.getContext()).thenReturn("NATIVE_APP");
        when(driver.getContextHandles()).thenReturn(new LinkedHashSet<>(Set.of("NATIVE_APP", "WEBVIEW_app")));
        when(driver.isAppInstalled("com.example.app")).thenReturn(true);
        when(driver.queryAppState("com.example.app"))
                .thenReturn(io.appium.java_client.appmanagement.ApplicationState.RUNNING_IN_FOREGROUND);
        when(driver.isDeviceLocked()).thenReturn(true);
        when(driver.getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        when(driver.getDeviceTime()).thenReturn("2026-08-12T09:00:00Z");
        when(driver.getBatteryInfo()).thenReturn(battery);
        when(battery.getLevel()).thenReturn(0.75);
        when(battery.getState()).thenReturn(AndroidBatteryInfo.BatteryState.CHARGING);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            MobileAssertions hard = new WizardHelpers.WebDriverAssertions(helper).mobileValues();
            var context = hard.currentContextValue();
            var contextCount = hard.contextCountValue();
            var installed = hard.appInstalledValue("com.example.app");
            var appState = hard.appStateValue("com.example.app");
            var locked = hard.deviceLockedValue();
            var orientation = hard.deviceOrientationValue();
            var time = hard.deviceTimeValue();
            var batteryValue = hard.batteryValue();
            verify(driver, never()).getSessionId();

            context.isEqualTo("NATIVE_APP");
            contextCount.isEqualTo(2);
            installed.isTrue();
            appState.isEqualTo(MobileApplicationState.RUNNING_IN_FOREGROUND);
            locked.isTrue();
            orientation.isEqualTo(ScreenOrientation.PORTRAIT);
            time.isEqualTo("2026-08-12T09:00:00Z");
            batteryValue.isEqualTo(new MobileBatteryInfo(0.75, "CHARGING"));

            new WizardHelpers.WebDriverVerifications(helper).mobileValues()
                    .currentContextValue().isEqualTo("NATIVE_APP");
        }

        verify(driver, times(2)).getContext();
        verify(driver).getContextHandles();
        verify(driver).isAppInstalled("com.example.app");
        verify(driver).queryAppState("com.example.app");
        verify(driver).isDeviceLocked();
        verify(driver).getOrientation();
        verify(driver).getDeviceTime();
        verify(driver).getBatteryInfo();
    }

    @Test
    public void mobileValuesShouldResolveLazilyAndRetryCurrentProviderState() {
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-lazy"));
        AtomicInteger reads = new AtomicInteger();
        when(contexts.getContext()).thenAnswer(invocation -> reads.incrementAndGet() == 1
                ? "NATIVE_APP" : "WEBVIEW_app");
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);

        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper).mobileValues();
        var terminal = values.currentContextValue();
        verify(contexts, never()).getContext();
        verify(driver, never()).getSessionId();

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 2);
             MockedStatic<JavaScriptWaitManager> waits = Mockito.mockStatic(JavaScriptWaitManager.class)) {
            terminal.isEqualTo("WEBVIEW_app");
            waits.verifyNoInteractions();
        }

        verify(contexts, times(2)).getContext();
    }

    @Test
    public void mobileValuesShouldRequireTheExactInterfaceAtTerminalExecution() {
        AppiumDriver generic = mock(AppiumDriver.class);
        when(generic.getSessionId()).thenReturn(new SessionId("mobile-validation-generic"));
        DriverFactoryHelper genericHelper = mock(DriverFactoryHelper.class);
        when(genericHelper.getDriver()).thenReturn(generic);
        MobileAssertions genericValues = new WizardHelpers.WebDriverAssertions(genericHelper).mobileValues();

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(generic, 1)) {
            UnsupportedOperationException unsupported = Assert.expectThrows(UnsupportedOperationException.class,
                    () -> genericValues.currentContextValue().isEqualTo("NATIVE_APP"));
            Assert.assertTrue(unsupported.getMessage().contains("context"));
        }

        AppiumDriver custom = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) custom;
        when(custom.getSessionId()).thenReturn(new SessionId("mobile-validation-custom"));
        when(contexts.getContext()).thenReturn("CUSTOM_CONTEXT");
        DriverFactoryHelper customHelper = mock(DriverFactoryHelper.class);
        when(customHelper.getDriver()).thenReturn(custom);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(custom, 1)) {
            new WizardHelpers.WebDriverAssertions(customHelper).mobileValues()
                    .currentContextValue().isEqualTo("CUSTOM_CONTEXT");
        }
        verify(contexts).getContext();
    }

    @Test
    public void staleMobileValuePlanShouldFailBeforeItsProviderCommand() {
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        when(driver.getSessionId()).thenReturn(null);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        var terminal = new WizardHelpers.WebDriverAssertions(helper).mobileValues().currentContextValue();

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            Assert.expectThrows(UnsupportedOperationException.class, () -> terminal.isEqualTo("NATIVE_APP"));
        }

        verify(contexts, never()).getContext();
    }

    @Test
    public void invalidApplicationIdShouldFailWithoutProviderAccessOrSecretEcho() {
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(InteractsWithApps.class));
        InteractsWithApps apps = (InteractsWithApps) driver;
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-app-id"));
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper).mobileValues();

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> values.appInstalledValue(" ").isTrue());
            Assert.expectThrows(NullPointerException.class,
                    () -> values.appStateValue(null).isEqualTo(MobileApplicationState.NOT_RUNNING));
        }

        verify(apps, never()).isAppInstalled(any());
        verify(apps, never()).queryAppState(any());
        verify(driver, never()).getSessionId();
    }

    @Test
    public void providerFailureShouldPreserveTheOriginalException() {
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-provider-failure"));
        WebDriverException sentinel = new WebDriverException("provider-mobile-value-sentinel");
        when(contexts.getContext()).thenThrow(sentinel);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);

        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            WebDriverException thrown = Assert.expectThrows(WebDriverException.class,
                    () -> new WizardHelpers.WebDriverAssertions(helper).mobileValues()
                            .currentContextValue().isEqualTo("NATIVE_APP"));
            Assert.assertSame(thrown, sentinel);
        }
    }

    @Test
    public void hardAndSoftRootsShouldRetainTheirFailureSemantics() {
        AppiumDriver driver = contextDriver("NATIVE_APP", "mobile-validation-failure-semantics");
        DriverFactoryHelper helper = helper(driver);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            Assert.expectThrows(AssertionError.class, () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().currentContextValue().isEqualTo("WEBVIEW_app"));

            new WizardHelpers.WebDriverVerifications(helper).mobileValues()
                    .currentContextValue().isEqualTo("WEBVIEW_app");
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        } finally {
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @Test
    public void eachProviderFamilyShouldRequireItsExactInterface() {
        AppiumDriver generic = mock(AppiumDriver.class);
        when(generic.getSessionId()).thenReturn(new SessionId("mobile-validation-provider-families"));
        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper(generic)).mobileValues();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(generic, 1)) {
            assertUnsupported(() -> values.currentContextValue().isEqualTo("NATIVE_APP"));
            assertUnsupported(() -> values.appInstalledValue("com.example.app").isTrue());
            assertUnsupported(() -> values.deviceLockedValue().isTrue());
            assertUnsupported(() -> values.deviceOrientationValue().isEqualTo(ScreenOrientation.PORTRAIT));
            assertUnsupported(() -> values.deviceTimeValue().isEqualTo("time"));
            assertUnsupported(() -> values.batteryValue().isEqualTo(new MobileBatteryInfo(1, "FULL")));
        }
    }

    @Test
    public void customExactInterfacesShouldRouteEveryProviderFamily() {
        AppiumDriver driver = mock(AppiumDriver.class, Mockito.withSettings().extraInterfaces(
                SupportsContextSwitching.class, InteractsWithApps.class, LocksDevice.class,
                SupportsRotation.class, HasDeviceTime.class, HasBattery.class));
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-custom-families"));
        when(((SupportsContextSwitching) driver).getContext()).thenReturn("CUSTOM");
        when(((InteractsWithApps) driver).isAppInstalled("com.example.app")).thenReturn(true);
        when(((LocksDevice) driver).isDeviceLocked()).thenReturn(true);
        when(((SupportsRotation) driver).getOrientation()).thenReturn(ScreenOrientation.LANDSCAPE);
        when(((HasDeviceTime) driver).getDeviceTime()).thenReturn("custom-time");
        AndroidBatteryInfo battery = mock(AndroidBatteryInfo.class);
        when(battery.getLevel()).thenReturn(0.5);
        when(battery.getState()).thenReturn(AndroidBatteryInfo.BatteryState.NOT_CHARGING);
        when(((HasBattery<?>) driver).getBatteryInfo()).thenReturn(battery);
        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            values.currentContextValue().isEqualTo("CUSTOM");
            values.appInstalledValue("com.example.app").isTrue();
            values.deviceLockedValue().isTrue();
            values.deviceOrientationValue().isEqualTo(ScreenOrientation.LANDSCAPE);
            values.deviceTimeValue().isEqualTo("custom-time");
            values.batteryValue().isEqualTo(new MobileBatteryInfo(0.5, "NOT_CHARGING"));
        }
    }

    @Test
    public void nullProviderValuesShouldFailClosedWithSpecificDiagnostics() {
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-null-values"));
        when(driver.getContext()).thenReturn(null);
        when(driver.getContextHandles()).thenReturn(null);
        when(driver.queryAppState("com.example.app")).thenReturn(null);
        when(driver.getOrientation()).thenReturn(null);
        when(driver.getDeviceTime()).thenReturn(null);
        when(driver.getBatteryInfo()).thenReturn(null);
        DriverFactoryHelper helper = helper(driver);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            assertNullProvider("current context", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().currentContextValue().isEqualTo("NATIVE_APP"));
            assertNullProvider("context handles", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().contextCountValue().isEqualTo(0));
            assertNullProvider("application state", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().appStateValue("com.example.app").isEqualTo(MobileApplicationState.NOT_RUNNING));
            assertNullProvider("device orientation", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().deviceOrientationValue().isEqualTo(ScreenOrientation.PORTRAIT));
            assertNullProvider("device time", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().deviceTimeValue().isEqualTo("time"));
            assertNullProvider("battery information", () -> new WizardHelpers.WebDriverAssertions(helper)
                    .mobileValues().batteryValue().isEqualTo(new MobileBatteryInfo(0, "unknown")));
        }
    }

    @Test
    public void applicationIdentifierShouldRedactLaterFailureSinksAndPreserveProviderIdentity() {
        String secret = "private.application.identifier";
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(InteractsWithApps.class));
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-app-privacy"));
        WebDriverException sentinel = new WebDriverException("provider rejected " + secret);
        when(((InteractsWithApps) driver).isAppInstalled(secret)).thenThrow(sentinel);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            WebDriverException thrown = Assert.expectThrows(WebDriverException.class,
                    () -> new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues()
                            .appInstalledValue(secret).isTrue());
            Assert.assertSame(thrown, sentinel);
            Assert.assertFalse(FailureTraceReporter.redactInvocationText("later " + thrown.getMessage())
                    .contains(secret));
        }
    }

    @Test
    public void retainedRootShouldStayBoundToItsOriginalDriver() {
        AppiumDriver first = contextDriver("FIRST", "mobile-validation-first");
        AppiumDriver second = contextDriver("SECOND", "mobile-validation-second");
        MobileAssertions retained = new WizardHelpers.WebDriverAssertions(helper(first)).mobileValues();
        new WizardHelpers.WebDriverAssertions(helper(second));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(first, 1)) {
            retained.currentContextValue().isEqualTo("FIRST");
        }
        verify((SupportsContextSwitching) first).getContext();
        verify((SupportsContextSwitching) second, never()).getContext();
    }

    @Test
    public void retainedSoftRootShouldStayBoundToItsOriginalDriver() {
        AppiumDriver first = contextDriver("FIRST", "mobile-validation-soft-first");
        AppiumDriver second = contextDriver("SECOND", "mobile-validation-soft-second");
        MobileAssertions retained = new WizardHelpers.WebDriverVerifications(helper(first)).mobileValues();
        new WizardHelpers.WebDriverVerifications(helper(second));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(first, 1)) {
            retained.currentContextValue().isEqualTo("FIRST");
        }
        verify((SupportsContextSwitching) first).getContext();
        verify((SupportsContextSwitching) second, never()).getContext();
    }

    @Test
    public void providerTimeoutShouldBeRethrownByIdentity() {
        WebDriver driver = mock(WebDriver.class);
        TimeoutException sentinel = new TimeoutException("provider timeout sentinel");
        TimeoutException thrown = Assert.expectThrows(TimeoutException.class,
                () -> new ValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT)
                        .validateMobileValue(driver, "current context", () -> {
                                    throw sentinel;
                                }, "NATIVE_APP", ValidationEnums.ValidationComparisonType.EQUALS,
                                ValidationEnums.ValidationType.POSITIVE));
        Assert.assertSame(thrown, sentinel);
    }

    @Test
    public void retainedTerminalsShouldKeepIndependentReportNames() {
        AppiumDriver driver = contextDriver("NATIVE_APP", "mobile-validation-report-names");
        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues();
        var context = values.currentContextValue();
        var count = values.contextCountValue();
        Assert.assertEquals(context.reportMessageBuilder.toString(), "the mobile session current context ");
        Assert.assertEquals(count.reportMessageBuilder.toString(), "the mobile session context count ");
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            Assert.expectThrows(AssertionError.class, () -> context.isEqualTo("WEBVIEW_app"));
            Assert.expectThrows(AssertionError.class, () -> count.isEqualTo(1));
        }
    }


    @Test
    public void nullBatteryStateShouldNormalizeToUnknown() {
        AndroidDriver driver = mock(AndroidDriver.class);
        AndroidBatteryInfo battery = mock(AndroidBatteryInfo.class);
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-null-battery-state"));
        when(driver.getBatteryInfo()).thenReturn(battery);
        when(battery.getLevel()).thenReturn(0.25);
        when(battery.getState()).thenReturn(null);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues().batteryValue()
                    .isEqualTo(new MobileBatteryInfo(0.25, "unknown"));
        }
    }

    @Test
    public void applicationIdentifierShouldPopulateBothPrivacyRegistries() throws Exception {
        String secret = "isolated.private.application.identifier";
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(InteractsWithApps.class));
        when(driver.getSessionId()).thenReturn(new SessionId("mobile-validation-registry-privacy"));
        when(((InteractsWithApps) driver).isAppInstalled(secret)).thenReturn(true);
        try {
            try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
                new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues().appInstalledValue(secret).isTrue();
            }
            clearSensitiveRegistry("SOURCE_SENSITIVE_VALUES");
            Assert.assertFalse(FailureTraceReporter.redactInvocationText("exact " + secret).contains(secret));
            try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
                new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues().appInstalledValue(secret).isTrue();
            }
            clearSensitiveRegistry("EXACT_SENSITIVE_VALUES");
            Method redactSource = FailureTraceReporter.class.getDeclaredMethod("redactSourceText", String.class);
            redactSource.setAccessible(true);
            Assert.assertFalse(String.valueOf(redactSource.invoke(null, "source " + secret)).contains(secret));
        } finally {
            clearSensitiveRegistry("EXACT_SENSITIVE_VALUES");
            clearSensitiveRegistry("SOURCE_SENSITIVE_VALUES");
        }
    }

    @Test
    public void concurrentStartersShouldProduceIndependentImmutablePlans() throws Exception {
        AppiumDriver driver = contextDriver("NATIVE_APP", "mobile-validation-concurrent-plans");
        when(((SupportsContextSwitching) driver).getContextHandles())
                .thenReturn(new LinkedHashSet<>(Set.of("NATIVE_APP", "WEBVIEW_app")));
        MobileAssertions values = new WizardHelpers.WebDriverAssertions(helper(driver)).mobileValues();
        try (var executor = Executors.newFixedThreadPool(2)) {
            for (int iteration = 0; iteration < 200; iteration++) {
                CyclicBarrier start = new CyclicBarrier(2);
                var context = executor.submit(() -> {
                    start.await();
                    return values.currentContextValue();
                });
                var count = executor.submit(() -> {
                    start.await();
                    return values.contextCountValue();
                });
                NativeValidationsBuilder contextPlan = context.get();
                NativeValidationsBuilder countPlan = count.get();
                Assert.assertEquals(contextPlan.reportMessageBuilder.toString(),
                        "the mobile session current context ");
                Assert.assertEquals(countPlan.reportMessageBuilder.toString(),
                        "the mobile session context count ");
                Assert.assertEquals(contextPlan.mobileValueName, "current context");
                Assert.assertEquals(countPlan.mobileValueName, "context count");
                Assert.assertEquals(contextPlan.mobileValueReader.get(), "NATIVE_APP");
                Assert.assertEquals(countPlan.mobileValueReader.get(), 2);
            }
        }
    }

    private static void clearSensitiveRegistry(String name) throws Exception {
        Field field = FailureTraceReporter.class.getDeclaredField(name);
        field.setAccessible(true);
        ((ThreadLocal<?>) field.get(null)).remove();
    }

    private static AppiumDriver contextDriver(String context, String sessionName) {
        AppiumDriver driver = mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        when(driver.getSessionId()).thenReturn(new SessionId(sessionName));
        when(((SupportsContextSwitching) driver).getContext()).thenReturn(context);
        return driver;
    }

    private static DriverFactoryHelper helper(WebDriver driver) {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        return helper;
    }

    private static void assertUnsupported(Runnable operation) {
        Assert.expectThrows(UnsupportedOperationException.class, operation::run);
    }

    private static void assertNullProvider(String diagnostic, Runnable operation) {
        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, operation::run);
        Assert.assertTrue(thrown.getMessage().contains(diagnostic), thrown.getMessage());
    }

    @SuppressWarnings("unchecked")
    private static MockedConstruction<SynchronizationManager> waitApplying(WebDriver driver, int attempts) {
        return Mockito.mockConstruction(SynchronizationManager.class, (manager, context) -> {
            FluentWait<WebDriver> wait = mock(FluentWait.class);
            when(manager.fluentWait(anyBoolean())).thenReturn(wait);
            when(wait.until(any())).thenAnswer(invocation -> {
                Function<? super WebDriver, ?> condition = invocation.getArgument(0);
                Object result = null;
                for (int attempt = 0; attempt < attempts; attempt++) {
                    result = condition.apply(driver);
                }
                return result;
            });
        });
    }
}
