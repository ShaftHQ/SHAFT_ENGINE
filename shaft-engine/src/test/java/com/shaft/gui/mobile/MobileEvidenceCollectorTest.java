package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.FailureTraceReporter;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.appmanagement.ApplicationState;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.remote.SupportsRotation;
import org.mockito.Mockito;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Platform;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class MobileEvidenceCollectorTest {
    private static final byte[] PNG = new byte[]{
            (byte) 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x01};

    @Test
    @SuppressWarnings("unchecked")
    public void collectorShouldBoundCurrentContextArtifactsAndAllowlistedMetadataWithoutLeaking() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        boolean domSource = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(true)
                    .traceIncludeNativePageSource(true)
                    .traceIncludeDomSnapshots(true);

            ExplosiveValue hostileCapability = new ExplosiveValue();
            Map<String, Object> capabilities = new LinkedHashMap<>();
            capabilities.put("platformName", "Android");
            capabilities.put("appium:platformVersion", "15");
            capabilities.put("appium:automationName", "UiAutomator2");
            capabilities.put("appium:appPackage", "secret.app");
            capabilities.put("appium:appActivity", "secret.Activity");
            capabilities.put("appium:bundleId", "secret.bundle");
            capabilities.put("appium:udid", "forbidden-udid");
            capabilities.put("bstack:options", hostileCapability);
            AppiumDriver nativeDriver = driver("evidence-native", capabilities);
            Mockito.when(nativeDriver.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(nativeDriver.getPageSource())
                    .thenReturn("<node package=\"secret.app\">visible</node>");
            Mockito.when(((SupportsContextSwitching) nativeDriver).getContext())
                    .thenReturn("NATIVE_APP", "NATIVE_APP");
            Mockito.when(nativeDriver.manage().window().getSize()).thenReturn(new Dimension(1080, 1920));

            Object nativeCapture = collect(nativeDriver, 1024);
            Assert.assertEquals(value(nativeCapture, "context"), "NATIVE_APP");
            Assert.assertEquals(value(nativeCapture, "sourceKind"), "native-accessibility-source");
            Assert.assertEquals(value(nativeCapture, "applicationMetadata"), Map.of(
                    "appPackage", "secret.app",
                    "appActivity", "secret.Activity",
                    "bundleId", "secret.bundle"));
            Map<String, String> deviceMetadata = (Map<String, String>) value(nativeCapture, "deviceMetadata");
            Assert.assertEquals(deviceMetadata, Map.of(
                    "platformName", "Android",
                    "platformVersion", "15",
                    "automationName", "UiAutomator2",
                    "windowSize", "1080x1920"));
            Assert.assertFalse(deviceMetadata.containsValue("forbidden-udid"));
            Assert.assertFalse(hostileCapability.stringified);
            Assert.assertEquals(value(nativeCapture, "omissions"), Map.of("applicationState", "unsupported"));

            byte[] screenshot = (byte[]) value(nativeCapture, "screenshot");
            Assert.assertEquals(screenshot, PNG);
            screenshot[0] = 0;
            Assert.assertEquals(((byte[]) value(nativeCapture, "screenshot"))[0], PNG[0]);
            String source = new String((byte[]) value(nativeCapture, "source"), StandardCharsets.UTF_8);
            Assert.assertFalse(source.contains("secret.app"));
            Assert.assertTrue(source.contains("********"));
            Assert.assertFalse(FailureTraceReporter.redactInvocationText("later secret.app").contains("secret.app"));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> deviceMetadata.put("deviceName", "forbidden"));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> ((Map<String, String>) value(nativeCapture, "applicationMetadata"))
                            .put("appPackage", "changed"));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> ((Map<String, String>) value(nativeCapture, "omissions"))
                            .put("source", "empty"));
            byte[] returnedSource = (byte[]) value(nativeCapture, "source");
            returnedSource[0] = 0;
            Assert.assertNotEquals(((byte[]) value(nativeCapture, "source"))[0], 0);
            Assert.assertFalse(nativeCapture.toString().contains("secret.app"));
            Assert.assertFalse(nativeCapture.toString().contains("visible"));
            Assert.assertFalse(nativeCapture.toString().contains("NATIVE_APP"));

            AndroidDriver queriedMetadata = Mockito.mock(AndroidDriver.class,
                    Mockito.withSettings().defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
            Mockito.when(queriedMetadata.getSessionId()).thenReturn(new SessionId("evidence-live-metadata"));
            Mockito.when(queriedMetadata.getCapabilities()).thenReturn(new ImmutableCapabilities(Map.of(
                    "platformName", "Android",
                    "appium:appPackage", "queried.app")));
            Mockito.when(queriedMetadata.getOrientation()).thenReturn(ScreenOrientation.LANDSCAPE);
            Mockito.when(queriedMetadata.queryAppState("queried.app"))
                    .thenReturn(ApplicationState.RUNNING_IN_FOREGROUND);
            Mockito.when(queriedMetadata.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(queriedMetadata.getPageSource()).thenReturn("<node/>");
            Mockito.when(queriedMetadata.getContext()).thenReturn("NATIVE_APP", "NATIVE_APP");
            Object queriedCapture = collect(queriedMetadata, 1024);
            Assert.assertEquals(((Map<?, ?>) value(queriedCapture, "applicationMetadata"))
                    .get("applicationState"), "RUNNING_IN_FOREGROUND");
            Assert.assertEquals(((Map<?, ?>) value(queriedCapture, "deviceMetadata"))
                    .get("orientation"), "LANDSCAPE");
            Assert.assertFalse(((Map<?, ?>) value(queriedCapture, "omissions"))
                    .containsKey("applicationState"));

            AppiumDriver webDriver = driver("evidence-web", Map.of());
            Mockito.when(webDriver.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(webDriver.getPageSource()).thenReturn("<html>web</html>");
            Mockito.when(((SupportsContextSwitching) webDriver).getContext())
                    .thenReturn("WEBVIEW_1", "WEBVIEW_1");
            Assert.assertEquals(value(collect(webDriver, 1024), "sourceKind"), "page-source");

            AppiumDriver unknownContext = Mockito.mock(AppiumDriver.class,
                    Mockito.withSettings().defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
            Mockito.when(unknownContext.getSessionId()).thenReturn(new SessionId("evidence-unknown-context"));
            Mockito.when(unknownContext.getCapabilities()).thenReturn(new ImmutableCapabilities());
            Mockito.when(unknownContext.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(unknownContext.getPageSource()).thenReturn("<node/>");
            Object unknownCapture = collect(unknownContext, 1024);
            Assert.assertEquals(value(unknownCapture, "context"), "unavailable");
            Assert.assertEquals(value(unknownCapture, "sourceKind"), "current-context-source");

            AppiumDriver changed = driver("evidence-context-change", Map.of());
            Mockito.when(changed.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(changed.getPageSource()).thenReturn("<node/>");
            Mockito.when(((SupportsContextSwitching) changed).getContext())
                    .thenReturn("NATIVE_APP", "WEBVIEW_1");
            Object changedCapture = collect(changed, 1024);
            Assert.assertNull(value(changedCapture, "screenshot"));
            Assert.assertNull(value(changedCapture, "source"));
            Assert.assertEquals(value(changedCapture, "omissions"), Map.of(
                    "applicationState", "unsupported",
                    "screenshot", "changed-during-capture",
                    "source", "changed-during-capture"));

            AppiumDriver oversized = driver("evidence-oversized", Map.of());
            Mockito.when(oversized.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
            Mockito.when(oversized.getPageSource()).thenReturn("€€€");
            Mockito.when(((SupportsContextSwitching) oversized).getContext())
                    .thenReturn("NATIVE_APP", "NATIVE_APP");
            Object oversizedCapture = collect(oversized, 8);
            Assert.assertNull(value(oversizedCapture, "screenshot"));
            Assert.assertNull(value(oversizedCapture, "source"));
            Map<String, String> oversizedOmissions = (Map<String, String>) value(oversizedCapture, "omissions");
            Assert.assertEquals(oversizedOmissions.get("screenshot"), "oversized");
            Assert.assertEquals(oversizedOmissions.get("source"), "oversized");

            RuntimeException providerFailure = new RuntimeException("provider-secret-message");
            AppiumDriver failed = driver("evidence-provider-failure", Map.of());
            Mockito.when(failed.getScreenshotAs(OutputType.BYTES)).thenThrow(providerFailure);
            Mockito.when(failed.getPageSource()).thenThrow(providerFailure);
            Mockito.when(((SupportsContextSwitching) failed).getContext())
                    .thenReturn("NATIVE_APP", "NATIVE_APP");
            Map<String, String> failedOmissions = (Map<String, String>) value(
                    collect(failed, 1024), "omissions");
            Assert.assertEquals(failedOmissions.get("screenshot"), "provider-failed");
            Assert.assertEquals(failedOmissions.get("source"), "provider-failed");
            Assert.assertFalse(FailureTraceReporter.redactInvocationText(
                    providerFailure, providerFailure.getMessage()).contains("provider-secret-message"));

            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false)
                    .traceIncludeDomSnapshots(false);
            AppiumDriver suppressed = driver("evidence-suppressed", Map.of());
            Mockito.when(((SupportsContextSwitching) suppressed).getContext())
                    .thenReturn("NATIVE_APP", "NATIVE_APP");
            Map<String, String> suppressedOmissions = (Map<String, String>) value(
                    collect(suppressed, 1024), "omissions");
            Assert.assertEquals(suppressedOmissions.get("screenshot"), "sensitive");
            Assert.assertEquals(suppressedOmissions.get("source"), "sensitive");
            Mockito.verify(suppressed, Mockito.never()).getScreenshotAs(OutputType.BYTES);
            Mockito.verify(suppressed, Mockito.never()).getPageSource();

            AppiumDriver invalidPayload = driver("evidence-invalid-payload", Map.of());
            Mockito.when(invalidPayload.getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[]{1, 2, 3});
            Mockito.when(invalidPayload.getPageSource()).thenReturn(" ");
            Mockito.when(((SupportsContextSwitching) invalidPayload).getContext())
                    .thenReturn("NATIVE_APP", "NATIVE_APP");
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(true)
                    .traceIncludeNativePageSource(true);
            Map<String, String> invalidOmissions = (Map<String, String>) value(
                    collect(invalidPayload, 1024), "omissions");
            Assert.assertEquals(invalidOmissions.get("screenshot"), "provider-failed");
            Assert.assertEquals(invalidOmissions.get("source"), "empty");

            AppiumDriver closed = driver("evidence-closed", Map.of());
            Mockito.when(closed.getSessionId()).thenReturn(null);
            InvocationTargetException closedFailure = Assert.expectThrows(InvocationTargetException.class,
                    () -> collect(closed, 1024));
            Assert.assertTrue(closedFailure.getCause() instanceof UnsupportedOperationException);
            Mockito.verify(closed, Mockito.never()).getCapabilities();
            Mockito.verify(closed, Mockito.never()).getScreenshotAs(OutputType.BYTES);
            Mockito.verify(closed, Mockito.never()).getPageSource();

            InvocationTargetException zeroBound = Assert.expectThrows(InvocationTargetException.class,
                    () -> collect(nativeDriver, 0));
            Assert.assertTrue(zeroBound.getCause() instanceof IllegalArgumentException);
            InvocationTargetException nullDriver = Assert.expectThrows(InvocationTargetException.class,
                    () -> collect(null, 1024));
            Assert.assertTrue(nullDriver.getCause() instanceof NullPointerException);
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource)
                    .traceIncludeDomSnapshots(domSource);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void sourceConsentShouldBeContextSpecificAndUnknownContextShouldFailClosed() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        boolean domSource = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        try {
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(true).traceIncludeDomSnapshots(false);
            AppiumDriver nativeAllowed = sourceDriver("native-allowed", "NATIVE_APP");
            AppiumDriver webDenied = sourceDriver("web-denied", "WEBVIEW_1");
            AppiumDriver unknownDenied = unknownSourceDriver("unknown-denied");
            Assert.assertNotNull(value(collect(nativeAllowed, 1024), "source"));
            Assert.assertEquals(((Map<String, String>) value(collect(webDenied, 1024), "omissions"))
                    .get("source"), "sensitive");
            Assert.assertEquals(((Map<String, String>) value(collect(unknownDenied, 1024), "omissions"))
                    .get("source"), "unsupported");
            Mockito.verify(webDenied, Mockito.never()).getPageSource();
            Mockito.verify(unknownDenied, Mockito.never()).getPageSource();

            SHAFT.Properties.reporting.set()
                    .traceIncludeNativePageSource(false).traceIncludeDomSnapshots(true);
            AppiumDriver nativeDenied = sourceDriver("native-denied", "NATIVE_APP");
            AppiumDriver webAllowed = sourceDriver("web-allowed", "WEBVIEW_1");
            AppiumDriver secondUnknown = unknownSourceDriver("second-unknown-denied");
            Assert.assertEquals(((Map<String, String>) value(collect(nativeDenied, 1024), "omissions"))
                    .get("source"), "sensitive");
            Assert.assertNotNull(value(collect(webAllowed, 1024), "source"));
            Assert.assertEquals(((Map<String, String>) value(collect(secondUnknown, 1024), "omissions"))
                    .get("source"), "unsupported");
            Mockito.verify(nativeDenied, Mockito.never()).getPageSource();
            Mockito.verify(secondUnknown, Mockito.never()).getPageSource();
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource).traceIncludeDomSnapshots(domSource);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void sensitiveBoundaryAggregateBudgetAndContinuityShouldFailClosed() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        try {
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(true).traceIncludeNativePageSource(true);
            FailureTraceReporter.suppressSensitiveBrowserArtifacts();
            AppiumDriver sensitive = sourceDriver("sensitive-pixels", "NATIVE_APP");
            Object sensitiveCapture = collect(sensitive, 1024);
            Map<String, String> sensitiveOmissions = (Map<String, String>) value(
                    sensitiveCapture, "omissions");
            Assert.assertEquals(sensitiveOmissions.get("screenshot"), "sensitive");
            Assert.assertEquals(sensitiveOmissions.get("source"), "sensitive");
            Assert.assertNull(value(sensitiveCapture, "screenshot"));
            Assert.assertNull(value(sensitiveCapture, "source"));
            Mockito.verify(sensitive, Mockito.never()).getScreenshotAs(OutputType.BYTES);
            Mockito.verify(sensitive, Mockito.never()).getPageSource();
            clearInvocationSensitivity();

            AppiumDriver aggregate = sourceDriver("aggregate-budget", "NATIVE_APP");
            Mockito.when(aggregate.getPageSource()).thenReturn("€€");
            Object aggregateCapture = collect(aggregate, PNG.length + 3L);
            Assert.assertNotNull(value(aggregateCapture, "screenshot"));
            Assert.assertNull(value(aggregateCapture, "source"));
            Assert.assertEquals(((Map<String, String>) value(aggregateCapture, "omissions"))
                    .get("source"), "oversized");

            AppiumDriver noBudget = sourceDriver("no-source-budget", "NATIVE_APP");
            Object noBudgetCapture = collect(noBudget, PNG.length);
            Assert.assertNotNull(value(noBudgetCapture, "screenshot"));
            Assert.assertNull(value(noBudgetCapture, "source"));
            Assert.assertEquals(((Map<String, String>) value(noBudgetCapture, "omissions"))
                    .get("source"), "oversized");
            Mockito.verify(noBudget, Mockito.never()).getPageSource();

            AppiumDriver finalContextMissing = sourceDriver("final-context-missing", "NATIVE_APP");
            Mockito.when(((SupportsContextSwitching) finalContextMissing).getContext())
                    .thenReturn("NATIVE_APP", null);
            Object missingCapture = collect(finalContextMissing, 1024);
            Assert.assertNull(value(missingCapture, "screenshot"));
            Assert.assertNull(value(missingCapture, "source"));
            Assert.assertEquals(((Map<String, String>) value(missingCapture, "omissions"))
                    .get("screenshot"), "changed-during-capture");

            AppiumDriver unknownThenWeb = sourceDriver("unknown-then-web", "NATIVE_APP");
            Mockito.when(((SupportsContextSwitching) unknownThenWeb).getContext())
                    .thenReturn(null, "WEBVIEW_1");
            Object unknownCapture = collect(unknownThenWeb, 1024);
            Assert.assertNull(value(unknownCapture, "screenshot"));
            Assert.assertEquals(((Map<String, String>) value(unknownCapture, "omissions"))
                    .get("screenshot"), "changed-during-capture");
            Assert.assertEquals(((Map<String, String>) value(unknownCapture, "omissions"))
                    .get("source"), "unsupported");

            AppiumDriver preservedReason = sourceDriver("preserved-reason", "NATIVE_APP");
            Mockito.when(((SupportsContextSwitching) preservedReason).getContext())
                    .thenReturn("NATIVE_APP", "WEBVIEW_1");
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            Map<String, String> reasons = (Map<String, String>) value(collect(preservedReason, 1024), "omissions");
            Assert.assertEquals(reasons.get("screenshot"), "sensitive");
            Assert.assertEquals(reasons.get("source"), "sensitive");

            SHAFT.Properties.reporting.set().traceIncludeScreenshots(true).traceIncludeNativePageSource(true);
            AppiumDriver closesLate = sourceDriver("closes-late", "NATIVE_APP");
            SessionId live = new SessionId("closes-late");
            Mockito.when(closesLate.getSessionId()).thenReturn(live, null);
            InvocationTargetException closed = Assert.expectThrows(InvocationTargetException.class,
                    () -> collect(closesLate, 1024));
            Assert.assertTrue(closed.getCause() instanceof UnsupportedOperationException);

            AppiumDriver changesSession = sourceDriver("changes-session", "NATIVE_APP");
            Mockito.when(changesSession.getSessionId()).thenReturn(
                    new SessionId("session-before"), new SessionId("session-after"));
            InvocationTargetException changedSession = Assert.expectThrows(InvocationTargetException.class,
                    () -> collect(changesSession, 1024));
            Assert.assertTrue(changedSession.getCause() instanceof UnsupportedOperationException);
        } finally {
            clearInvocationSensitivity();
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void exactProviderInterfacesAndMetadataFailuresShouldRemainOptional() throws Exception {
        ExplosiveBigInteger hostileNumber = new ExplosiveBigInteger();
        AppiumDriver rotating = Mockito.mock(AppiumDriver.class, Mockito.withSettings()
                .extraInterfaces(SupportsContextSwitching.class, SupportsRotation.class)
                .defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(rotating.getSessionId()).thenReturn(new SessionId("custom-rotation"));
        Mockito.when(rotating.getCapabilities()).thenReturn(new ImmutableCapabilities(Map.of(
                "platformName", "iOS", "appium:platformVersion", hostileNumber)));
        Mockito.when(((SupportsRotation) rotating).getOrientation()).thenReturn(ScreenOrientation.PORTRAIT);
        Mockito.when(((SupportsContextSwitching) rotating).getContext()).thenReturn("NATIVE_APP", "NATIVE_APP");
        Mockito.when(rotating.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        Mockito.when(rotating.getPageSource()).thenReturn("<node/>");
        Object rotatingCapture = collect(rotating, 1024);
        Assert.assertEquals(((Map<?, ?>) value(rotatingCapture, "deviceMetadata")).get("platformName"), "iOS");
        Assert.assertEquals(((Map<?, ?>) value(rotatingCapture, "deviceMetadata")).get("orientation"), "PORTRAIT");
        Assert.assertFalse(((Map<?, ?>) value(rotatingCapture, "deviceMetadata")).containsKey("platformVersion"));
        Assert.assertFalse(hostileNumber.stringified);

        List<Object> numericValues = List.of(
                Byte.valueOf((byte) 1), Short.valueOf((short) 2), Integer.valueOf(3), Long.valueOf(4),
                Float.valueOf(5.5F), Double.valueOf(6.5D), new BigInteger("7"), new BigDecimal("8.25"));
        for (int index = 0; index < numericValues.size(); index++) {
            Object numeric = numericValues.get(index);
            AppiumDriver numericDriver = sourceDriver("numeric-metadata-" + index, "NATIVE_APP");
            Mockito.when(numericDriver.getCapabilities()).thenReturn(new ImmutableCapabilities(
                    Map.of("appium:platformVersion", numeric)));
            Assert.assertEquals(((Map<?, ?>) value(collect(numericDriver, 1024), "deviceMetadata"))
                    .get("platformVersion"), numeric.toString());
        }

        Capabilities fallbackCapabilities = Mockito.mock(Capabilities.class);
        Mockito.when(fallbackCapabilities.getPlatformName()).thenReturn(Platform.ANDROID);
        AppiumDriver fallback = sourceDriver("platform-fallback", "NATIVE_APP");
        Mockito.when(fallback.getCapabilities()).thenReturn(fallbackCapabilities);
        Assert.assertEquals(((Map<?, ?>) value(collect(fallback, 1024), "deviceMetadata"))
                .get("platformName"), "Android");

        AppiumDriver uppercaseIos = sourceDriver("uppercase-ios", "NATIVE_APP");
        Mockito.when(uppercaseIos.getCapabilities()).thenReturn(
                new ImmutableCapabilities(Map.of("platformName", "IOS")));
        Assert.assertEquals(((Map<?, ?>) value(collect(uppercaseIos, 1024), "deviceMetadata"))
                .get("platformName"), "iOS");

        RuntimeException appStateFailure = new RuntimeException("app-state-secret");
        AndroidDriver appState = Mockito.mock(AndroidDriver.class,
                Mockito.withSettings().defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(appState.getSessionId()).thenReturn(new SessionId("app-state-failure"));
        Mockito.when(appState.getCapabilities()).thenReturn(new ImmutableCapabilities(Map.of(
                "platformName", "Android", "appPackage", "legacy.app")));
        Mockito.when(appState.queryAppState("legacy.app")).thenThrow(appStateFailure);
        Mockito.when(appState.getOrientation()).thenThrow(new RuntimeException("orientation-secret"));
        Mockito.when(appState.manage().window().getSize()).thenThrow(new RuntimeException("window-secret"));
        Mockito.when(appState.getContext()).thenReturn("NATIVE_APP", "NATIVE_APP");
        Mockito.when(appState.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        Mockito.when(appState.getPageSource()).thenReturn("<node/>");
        Object failureCapture = collect(appState, 1024);
        Assert.assertNotNull(value(failureCapture, "screenshot"));
        Assert.assertNotNull(value(failureCapture, "source"));
        Assert.assertEquals(((Map<String, String>) value(failureCapture, "applicationMetadata"))
                .get("appPackage"), "legacy.app");
        Assert.assertEquals(((Map<String, String>) value(failureCapture, "omissions"))
                .get("applicationState"), "provider-failed");
        Assert.assertFalse(FailureTraceReporter.redactInvocationText(
                appStateFailure, appStateFailure.getMessage()).contains("app-state-secret"));

        AppiumDriver capabilitiesFail = sourceDriver("capabilities-failure", "NATIVE_APP");
        Mockito.when(capabilitiesFail.getCapabilities()).thenThrow(new RuntimeException("capabilities-secret"));
        Object capabilitiesCapture = collect(capabilitiesFail, 1024);
        Assert.assertTrue(((Map<?, ?>) value(capabilitiesCapture, "applicationMetadata")).isEmpty());
        Assert.assertTrue(((Map<?, ?>) value(capabilitiesCapture, "deviceMetadata")).isEmpty());
        Assert.assertNotNull(value(capabilitiesCapture, "screenshot"));
    }

    @Test
    public void captureShouldOwnEveryMutableInputAndExposeOnlyCountsInText() {
        Map<String, String> application = new LinkedHashMap<>(Map.of("appPackage", "secret.app"));
        Map<String, String> device = new LinkedHashMap<>(Map.of("platformName", "Android"));
        Map<String, String> omissions = new LinkedHashMap<>(Map.of("recording", "active"));
        byte[] screenshot = Arrays.copyOf(PNG, PNG.length);
        byte[] source = "secret-source".getBytes(StandardCharsets.UTF_8);
        MobileEvidenceCollector.Capture capture = new MobileEvidenceCollector.Capture(
                "NATIVE_APP", application, device, screenshot, source,
                "native-accessibility-source", omissions);
        application.clear();
        device.clear();
        omissions.clear();
        screenshot[0] = 0;
        source[0] = 0;

        Assert.assertEquals(capture.applicationMetadata(), Map.of("appPackage", "secret.app"));
        Assert.assertEquals(capture.deviceMetadata(), Map.of("platformName", "Android"));
        Assert.assertEquals(capture.omissions(), Map.of("recording", "active"));
        Assert.assertEquals(capture.screenshot(), PNG);
        Assert.assertEquals(new String(capture.source(), StandardCharsets.UTF_8), "secret-source");
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> capture.applicationMetadata().put("bundleId", "changed"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> capture.omissions().put("source", "empty"));
        Assert.assertEquals(capture.toString(),
                "Capture[applicationMetadata=1, deviceMetadata=1, screenshotBytes=9, sourceBytes=13, omissions=1]");
    }

    private static Object collect(AppiumDriver driver, long maxBytes) throws Exception {
        Class<?> collector;
        try {
            collector = Class.forName("com.shaft.gui.mobile.MobileEvidenceCollector");
        } catch (ClassNotFoundException missing) {
            Assert.fail("Evidence needs a bounded current-context component collector.");
            return null;
        }
        Method method = collector.getDeclaredMethod("collect", AppiumDriver.class, long.class);
        method.setAccessible(true);
        return method.invoke(null, driver, maxBytes);
    }

    private static Object value(Object capture, String accessor) throws Exception {
        Method method = capture.getClass().getDeclaredMethod(accessor);
        method.setAccessible(true);
        return method.invoke(capture);
    }

    private static AppiumDriver driver(String id, Map<String, Object> capabilities) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class, Mockito.withSettings()
                .extraInterfaces(SupportsContextSwitching.class)
                .defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities(capabilities));
        return driver;
    }

    private static AppiumDriver sourceDriver(String id, String context) {
        AppiumDriver driver = driver(id, Map.of());
        Mockito.when(driver.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        Mockito.when(driver.getPageSource()).thenReturn("<node/>");
        Mockito.when(((SupportsContextSwitching) driver).getContext()).thenReturn(context, context);
        return driver;
    }

    private static AppiumDriver unknownSourceDriver(String id) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities());
        Mockito.when(driver.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        Mockito.when(driver.getPageSource()).thenReturn("<node/>");
        return driver;
    }

    private static void clearInvocationSensitivity() throws Exception {
        Method clear = FailureTraceReporter.class.getDeclaredMethod("clearInvocationSensitiveValues");
        clear.setAccessible(true);
        clear.invoke(null);
    }

    private static final class ExplosiveValue {
        private boolean stringified;

        @Override
        public String toString() {
            stringified = true;
            throw new IllegalStateException("Unallowlisted capability values must not be stringified.");
        }
    }

    private static final class ExplosiveBigInteger extends BigInteger {
        private boolean stringified;

        private ExplosiveBigInteger() {
            super("15");
        }

        @Override
        public String toString() {
            stringified = true;
            throw new IllegalStateException("Subclassed numeric capability must not be stringified.");
        }
    }
}
