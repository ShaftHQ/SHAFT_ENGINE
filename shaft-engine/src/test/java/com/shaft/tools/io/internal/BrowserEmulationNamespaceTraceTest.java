package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Tracing;
import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.EmulatedColorScheme;
import com.shaft.gui.driver.EmulatedMediaType;
import com.shaft.gui.driver.EmulatedReducedMotion;
import com.shaft.gui.driver.EmulationActionsContract;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.tms.XrayIntegrationHelper;
import org.mockito.Mockito;
import org.mockito.MockedStatic;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.SessionId;
import io.appium.java_client.AppiumDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.util.Optional;
import java.util.List;
import java.util.Comparator;
import java.util.Map;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipFile;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration") // Private trace-manager construction is an isolation fixture.
public class BrowserEmulationNamespaceTraceTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void playwrightGeolocationShouldEmitOnePrivacySafeBackendOwnedEvent() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);

        new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location()
                .geolocation(30.0444, 31.2357, 5);

        assertSingleEvent("emulation/location", "geolocation", "passed",
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().locator(), "<geolocation>");
    }

    @Test
    public void unsupportedSeleniumEmulationShouldEmitOneFailedBackendOwnedEvent() {
        WebDriver driver = Mockito.mock(WebDriver.class);

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true)
                        .emulation().location().clearGeolocation());

        assertSingleEvent("emulation/location", "clear-geolocation", "failed",
                AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void contextCreateOnlyPlaywrightEmulationShouldEmitOneFailedBackendOwnedEvent() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session)
                        .emulation().runtime().userAgent("custom-agent"));

        assertSingleEvent("emulation/runtime", "user agent", "failed",
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void appiumBiDiEmulationShouldEmitOneAppiumOwnedEvent() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class);
        BiDi bidi = Mockito.mock(BiDi.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-emulation"));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        Mockito.when(driver.maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(driver.getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("webview-context");

        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location().locale("ar-EG");

        assertSingleEvent("emulation/location", "locale", "passed", AutomationBackend.APPIUM);
        Mockito.verify(bidi).send(Mockito.argThat(command ->
                "emulation.setLocaleOverride".equals(command.getMethod())));
    }

    @Test
    @SuppressWarnings("rawtypes")
    public void seleniumGeolocationFailureShouldOmitProviderAndDomEvidenceWithoutReplacingTheError() {
        String latitude = "30.04441237";
        String longitude = "31.23576543";
        String domEvidence = "page rendered location " + latitude + "," + longitude;
        IllegalStateException providerFailure = new IllegalStateException(
                "Provider rejected coordinates " + latitude + "," + longitude);
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.doThrow(providerFailure).when(bidi).send(Mockito.any(org.openqa.selenium.bidi.Command.class));
        WebDriver driver = liveBiDiDriver(bidi, domEvidence);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)));

        Assert.assertSame(thrown, providerFailure);
        assertSensitiveEmulationFailure("geolocationFailure", providerFailure,
                latitude, longitude, domEvidence);
    }

    @Test
    @SuppressWarnings("rawtypes")
    public void seleniumUserAgentFailureShouldOmitProviderAndDomEvidenceWithoutReplacingTheError() {
        String userAgent = "opaque-user-agent-94731";
        String domEvidence = "page rendered navigator.userAgent=" + userAgent;
        IllegalStateException providerFailure = new IllegalStateException("Provider rejected " + userAgent);
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.doThrow(providerFailure).when(bidi).send(Mockito.any(org.openqa.selenium.bidi.Command.class));
        WebDriver driver = liveBiDiDriver(bidi, domEvidence);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().runtime()
                        .userAgent(userAgent));

        Assert.assertSame(thrown, providerFailure);
        assertSensitiveEmulationFailure("userAgentFailure", providerFailure, userAgent, domEvidence);
    }

    @Test
    @SuppressWarnings("rawtypes")
    public void appiumGeolocationFailureShouldRemainAppiumOwnedAndSecretSafe() {
        String latitude = "-33.85678421";
        String longitude = "151.21529637";
        String domEvidence = "native view rendered location " + latitude + "," + longitude;
        IllegalStateException providerFailure = new IllegalStateException(
                "Appium rejected coordinates " + latitude + "," + longitude);
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.doThrow(providerFailure).when(bidi).send(Mockito.any(org.openqa.selenium.bidi.Command.class));
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://bidi.example.test/appium-session");
        AppiumDriver driver = Mockito.mock(AppiumDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-sensitive-emulation"));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        Mockito.when(driver.maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(driver.getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("WEBVIEW_1");
        Mockito.when(driver.getPageSource()).thenReturn(domEvidence);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().backend(), AutomationBackend.APPIUM);
        assertSensitiveEmulationFailure("appiumGeolocationFailure", providerFailure,
                latitude, longitude, domEvidence);
    }

    @Test
    public void playwrightGeolocationFailureShouldOmitProviderEvidenceWithoutReplacingTheError() {
        String latitude = "48.85837091";
        String longitude = "2.29448132";
        IllegalStateException providerFailure = new IllegalStateException(
                "Playwright rejected coordinates " + latitude + "," + longitude);
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        Mockito.doThrow(providerFailure).when(context).setGeolocation(Mockito.any());

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().backend(),
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        assertSensitiveEmulationFailure("playwrightGeolocationFailure", providerFailure, latitude, longitude);
    }

    @Test
    public void successfulSeleniumGeolocationShouldNotLeakIntoALaterUnrelatedFailureArchive() throws Exception {
        String latitude = "35.65858129";
        String longitude = "139.74543291";
        String sensitiveValue = latitude + "," + longitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + sensitiveValue);

        assertSuccessfulSensitiveActionArchive("seleniumSensitiveSuccess", sensitiveValue,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)), null);
    }

    @Test
    public void successfulAppiumGeolocationShouldNotLeakIntoALaterUnrelatedFailureArchive() throws Exception {
        String latitude = "25.19721234";
        String longitude = "55.27437654";
        String sensitiveValue = latitude + "," + longitude;
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://bidi.example.test/appium-sensitive-success");
        BiDi bidi = Mockito.mock(BiDi.class);
        AppiumDriver driver = Mockito.mock(AppiumDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-sensitive-success"));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        Mockito.when(driver.maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(driver.getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("WEBVIEW_1");
        Mockito.when(driver.getPageSource()).thenReturn("application rendered " + sensitiveValue);

        assertSuccessfulSensitiveActionArchive("appiumSensitiveSuccess", sensitiveValue,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)), null);
    }

    @Test
    public void successfulPlaywrightGeolocationShouldOmitANativeTraceFromALaterUnrelatedFailureArchive()
            throws Exception {
        String latitude = "51.50072921";
        String longitude = "-0.12462543";
        String sensitiveValue = latitude + "," + longitude;
        Path nativeTrace = Files.createTempFile("shaft-sensitive-playwright-", ".zip");
        Files.writeString(nativeTrace, "native trace captured " + sensitiveValue, StandardCharsets.UTF_8);
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        try (MockedStatic<com.shaft.gui.playwright.internal.PlaywrightTraceManager> traceManager =
                     Mockito.mockStatic(com.shaft.gui.playwright.internal.PlaywrightTraceManager.class)) {
            traceManager.when(com.shaft.gui.playwright.internal.PlaywrightTraceManager::getLastTracePath)
                    .thenReturn(nativeTrace);
            assertSuccessfulSensitiveActionArchive("playwrightSensitiveSuccess", sensitiveValue,
                    () -> new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location()
                            .geolocation(coordinate(latitude), coordinate(longitude)),
                    nativeTrace.getFileName().toString());
        } finally {
            Files.deleteIfExists(nativeTrace);
        }
    }

    @Test
    public void successfulSeleniumGeolocationShouldRedactAnEchoedUnrelatedFailure() {
        String latitude = "27.17514483";
        String longitude = "78.04214268";
        String sensitiveValue = latitude + "," + longitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + sensitiveValue);

        assertSuccessfulSensitiveActionEchoFailure("seleniumEchoFailure", sensitiveValue,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)));
    }

    @Test
    public void defaultGeolocationAccuracyShouldNotEraseUnrelatedDecimalFailureEvidence() {
        String latitude = "27.17514483";
        String longitude = "78.04214268";
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                .geolocation(coordinate(latitude), coordinate(longitude));
        AssertionError unrelatedFailure = new AssertionError("Expected 100.0 but got 50.0");
        TestExecutionInfo execution = new TestExecutionInfo("emulation-short-decimal",
                getClass().getName(), "defaultGeolocationAccuracyShouldNotEraseUnrelatedDecimalFailureEvidence",
                "decimal failure", "decimal failure", null, unrelatedFailure, false);

        String trace = FailureTraceReporter.renderTraceJson(execution, "unrelated decimal failure", List.of());

        Assert.assertTrue(trace.contains("Expected 100.0 but got 50.0"), trace);
    }

    @Test
    public void geolocationNumbersShouldRemainSensitiveWhenRenderedWithDirectionAndUnitSuffixes() throws Exception {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                .geolocation(30.0444, -74.0445, 12.3456);
        AssertionError unrelatedFailure = new AssertionError(
                "Rendered +30.0444N, -74.0445W with accuracy=12.3456m");
        TestExecutionInfo execution = new TestExecutionInfo("emulation-numeric-units",
                getClass().getName(),
                "geolocationNumbersShouldRemainSensitiveWhenRenderedWithDirectionAndUnitSuffixes",
                "numeric units", "numeric units", null, unrelatedFailure, false);

        String trace = FailureTraceReporter.renderTraceJson(execution, "numeric unit failure", List.of());
        String exception = JSON.readTree(trace).path("exception").toString();

        Assert.assertFalse(exception.contains("30.0444"), trace);
        Assert.assertFalse(exception.contains("-74.0445W"), trace);
        Assert.assertFalse(exception.contains("12.3456m"), trace);
    }

    @Test
    public void negativeZeroGeolocationShouldProtectItsNormalizedPositiveZeroRepresentation() {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                .geolocation(-0.0, 12.345678, 7.0);
        AssertionError unrelatedFailure = new AssertionError("Rendered 0.0N after browser normalization");
        TestExecutionInfo execution = new TestExecutionInfo("emulation-negative-zero",
                getClass().getName(),
                "negativeZeroGeolocationShouldProtectItsNormalizedPositiveZeroRepresentation",
                "negative zero", "negative zero", null, unrelatedFailure, false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "negative zero normalization", List.of());

        Assert.assertFalse(trace.contains("Rendered 0.0N"), trace);
    }

    @Test
    public void seleniumGeolocationShouldProtectEquivalentIntegerFormatting() {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                .geolocation(30.0, 31.25, 12.3456);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-integer-format",
                getClass().getName(), "seleniumGeolocationShouldProtectEquivalentIntegerFormatting",
                "integer formatting", "integer formatting", null,
                new AssertionError("Rendered latitude=30N"), false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "integer formatting", List.of());

        Assert.assertFalse(trace.contains("latitude=30N"), trace);
    }

    @Test
    public void prefixedCoordinateFormattingShouldBeRedactedWithoutMatchingLargerNumbers() {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                .geolocation(30.0, 31.25, 12.3456);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-prefixed-format",
                getClass().getName(), "prefixedCoordinateFormattingShouldBeRedactedWithoutMatchingLargerNumbers",
                "prefixed formatting", "prefixed formatting", null,
                new AssertionError("Rendered N30, lat30, latitude_30; control 130 and 3.0E2"), false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "prefixed formatting", List.of());

        Assert.assertFalse(trace.contains("N30"), trace);
        Assert.assertFalse(trace.contains("lat30"), trace);
        Assert.assertFalse(trace.contains("latitude_30"), trace);
        Assert.assertTrue(trace.contains("control 130 and 3.0E2"), trace);
    }

    @Test
    public void excessiveSensitiveHistoryAndHostileNumericTokenShouldFailClosedWithinBounds() {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        var emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location();
        for (int index = 0; index < 120; index++) {
            emulation.geolocation(10.000001 + index / 1000.0,
                    20.000001 + index / 1000.0,
                    1.000001 + index / 1000.0);
        }
        TestExecutionInfo execution = new TestExecutionInfo("emulation-hostile-history",
                getClass().getName(),
                "excessiveSensitiveHistoryAndHostileNumericTokenShouldFailClosedWithinBounds",
                "hostile history", "hostile history", null,
                new AssertionError("Provider emitted " + "9".repeat(2048)), false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "hostile history", List.of());

        Assert.assertTrue(trace.contains("sensitive-value bounds were exceeded"), trace);
        Assert.assertTrue(trace.contains(AssertionError.class.getName()), trace);
    }

    @Test
    public void oversizedUserAgentShouldSuppressBrowserEvidenceAcrossTheNextInvocation() {
        String userAgent = "oversized-user-agent-" + "Z".repeat(600);
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + userAgent);
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().runtime().userAgent(userAgent);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-oversized-user-agent",
                getClass().getName(), "oversizedUserAgentShouldSuppressBrowserEvidenceAcrossTheNextInvocation",
                "oversized user agent", "oversized user agent", null,
                new AssertionError("unrelated failure"), false);
        boolean originalDomSnapshots = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        try {
            SHAFT.Properties.reporting.set().traceIncludeDomSnapshots(true);
            ReportContext.start(execution);
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "ordinary-action", "", driver);
            TraceEventRecorder.finish(event, "passed", "ordinary action", null, Map.of(), List.of());
            BrowserObservabilityRecorder.recordConsole("browser", "ERROR", userAgent, System.currentTimeMillis());

            String trace = FailureTraceReporter.renderTraceJson(execution, "", List.of());

            Assert.assertTrue(FailureTraceReporter.shouldOmitSensitiveBrowserEvidence());
            Assert.assertFalse(trace.contains(userAgent), trace);
            Assert.assertTrue(trace.contains("\"type\": \"omitted-sensitive\""), trace);
            Assert.assertFalse(trace.contains("domSnapshotBefore"), trace);
        } finally {
            SHAFT.Properties.reporting.set().traceIncludeDomSnapshots(originalDomSnapshots);
        }
    }

    @Test
    public void playwrightGeolocationShouldProtectEquivalentFixedDecimalFormatting() {
        PlaywrightSession session = livePlaywrightSession();
        new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location()
                .geolocation(0.0000001, 31.25, 12.3456);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-fixed-format",
                getClass().getName(), "playwrightGeolocationShouldProtectEquivalentFixedDecimalFormatting",
                "fixed formatting", "fixed formatting", null,
                new AssertionError("Rendered latitude=0.0000001N"), false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "fixed formatting", List.of());

        Assert.assertFalse(trace.contains("latitude=0.0000001N"), trace);
    }

    @Test
    public void shortUserAgentShouldNotEraseUnrelatedFailureEvidence() {
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "ordinary page evidence");
        new com.shaft.gui.browser.BrowserActions(driver, true).emulation().runtime().userAgent("a");
        AssertionError unrelatedFailure = new AssertionError("ordinary failure remains useful");
        TestExecutionInfo execution = new TestExecutionInfo("emulation-short-user-agent",
                getClass().getName(), "shortUserAgentShouldNotEraseUnrelatedFailureEvidence",
                "short user agent", "short user agent", null, unrelatedFailure, false);

        String trace = FailureTraceReporter.renderTraceJson(execution, "ordinary failure remains useful", List.of());

        Assert.assertTrue(trace.contains("\"message\": \"ordinary failure remains useful\""), trace);
    }

    @Test
    public void repeatedGeolocationChangesShouldKeepEveryHistoricalValueSensitiveUntilSessionClose() {
        String firstLatitude = "27.17514483";
        String firstLongitude = "78.04214268";
        String firstValue = firstLatitude + "," + firstLongitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application retained " + firstValue);
        var emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location();
        emulation.geolocation(coordinate(firstLatitude), coordinate(firstLongitude));
        emulation.clearGeolocation();
        emulation.geolocation(48.85837091, 2.29448132);
        AssertionError unrelatedFailure = new AssertionError("Application rendered " + firstValue);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-history", getClass().getName(),
                "repeatedGeolocationChangesShouldKeepEveryHistoricalValueSensitiveUntilSessionClose",
                "emulation history", "emulation history", null, unrelatedFailure, false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "historical emulation value", List.of());

        Assert.assertFalse(trace.contains(firstLatitude), trace);
        Assert.assertFalse(trace.contains(firstLongitude), trace);
    }

    @Test
    public void replacingSeleniumGeolocationShouldRetainThePreviousValueAsSensitiveEvidence() {
        String firstLatitude = "35.65858129";
        String firstLongitude = "139.74543291";
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application retained " + firstLatitude + "," + firstLongitude);
        var emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location();
        emulation.geolocation(coordinate(firstLatitude), coordinate(firstLongitude));
        emulation.geolocation(51.50072921, -0.12462543);

        assertHistoricalSensitiveValueIsRedacted("selenium-direct-replacement", firstLatitude, firstLongitude);
    }

    @Test
    public void replacingPlaywrightGeolocationShouldRetainThePreviousValueAsSensitiveEvidence() {
        String firstLatitude = "40.68924931";
        String firstLongitude = "-74.04450042";
        PlaywrightSession session = livePlaywrightSession();
        var emulation = new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location();
        emulation.geolocation(coordinate(firstLatitude), coordinate(firstLongitude));
        emulation.geolocation(48.85837091, 2.29448132);

        assertHistoricalSensitiveValueIsRedacted("playwright-direct-replacement", firstLatitude, firstLongitude);
    }

    @Test
    public void successfulPlaywrightGeolocationShouldRedactAnEchoedUnrelatedFailure() {
        String latitude = "41.40362991";
        String longitude = "2.17435582";
        String sensitiveValue = latitude + "," + longitude;
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);

        assertSuccessfulSensitiveActionEchoFailure("playwrightEchoFailure", sensitiveValue,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session).emulation().location()
                        .geolocation(coordinate(latitude), coordinate(longitude)));
    }

    @Test
    public void beforeClassSensitiveGeolocationShouldProtectEveryFollowingTestUntilExplicitlyCleared() {
        String latitude = "43.72301749";
        String longitude = "10.39663322";
        String sensitiveValue = latitude + "," + longitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + sensitiveValue);
        var emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location();
        emulation.geolocation(coordinate(latitude), coordinate(longitude));

        for (int invocation = 1; invocation <= 2; invocation++) {
            TestExecutionInfo execution = new TestExecutionInfo("before-class-sensitive-" + invocation,
                    getClass().getName(), "followingTest" + invocation, "following test", "following test",
                    null, new AssertionError("unrelated failure"), false);
            ReportContext.start(execution);
            var event = TraceEventRecorder.start("browser", "ordinary-action", "", driver);
            TraceEventRecorder.finish(event, "passed", "ordinary action completed", null, Map.of(), List.of());

            String rendered = FailureTraceReporter.renderTraceJson(execution, "", List.of(), 1);

            Assert.assertFalse(rendered.contains(sensitiveValue), rendered);
        }

        emulation.clearGeolocation();
    }

    @Test
    public void clearingGeolocationShouldNotPublishSensitiveDomCapturedByTheClearAction() {
        String latitude = "43.72301749";
        String longitude = "10.39663322";
        String sensitiveValue = latitude + "," + longitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application still renders " + sensitiveValue);
        var emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location();
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalDomSnapshots = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        try {
            SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(true).traceIncludeDomSnapshots(true);
            emulation.geolocation(coordinate(latitude), coordinate(longitude));
            TestExecutionInfo execution = new TestExecutionInfo("clear-sensitive-geolocation", getClass().getName(),
                    "clearingGeolocationShouldNotPublishSensitiveDomCapturedByTheClearAction",
                    "clear geolocation", "clear geolocation", null,
                    new AssertionError("unrelated failure"), false);
            ReportContext.start(execution);

            emulation.clearGeolocation();
            String rendered = FailureTraceReporter.renderTraceJson(execution, "", List.of());

            Assert.assertFalse(rendered.contains(sensitiveValue), rendered);
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeDomSnapshots(originalDomSnapshots);
        }
    }

    @Test
    public void playwrightSensitiveStateShouldRemainSessionScopedAcrossFollowingTestsAndOverlappingSessions() {
        PlaywrightSession first = livePlaywrightSession();
        PlaywrightSession second = livePlaywrightSession();
        var firstEmulation = new com.shaft.gui.playwright.browser.BrowserActions(first).emulation().location();
        var secondEmulation = new com.shaft.gui.playwright.browser.BrowserActions(second).emulation().location();
        firstEmulation.geolocation(37.81992861, -122.47825517);
        secondEmulation.geolocation(40.68924931, -74.04450042);

        new com.shaft.gui.playwright.browser.BrowserActions(first);
        Assert.assertTrue(FailureTraceReporter.shouldSuppressSensitiveBrowserArtifacts());
        FailureTraceReporter.clearPersistentSensitiveBrowserState(first);

        new com.shaft.gui.playwright.browser.BrowserActions(second);
        ReportContext.start(new TestExecutionInfo("playwright-following-test", getClass().getName(),
                "playwrightFollowingTest", "following test", "following test", null,
                new AssertionError("unrelated failure"), false));
        Assert.assertTrue(FailureTraceReporter.shouldSuppressSensitiveBrowserArtifacts(),
                "Clearing one live session must not erase another session's privacy boundary.");

        secondEmulation.clearGeolocation();
        Assert.assertFalse(FailureTraceReporter.shouldSuppressSensitiveBrowserArtifacts());
    }

    @Test
    public void sensitiveEmulationShouldPreventDirectPlaywrightTraceAttachment() throws Exception {
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Tracing tracing = Mockito.mock(Tracing.class);
        Mockito.when(context.tracing()).thenReturn(tracing);
        Path artifacts = Files.createTempDirectory("shaft-sensitive-playwright-manager-");
        var constructor = com.shaft.gui.playwright.internal.PlaywrightTraceManager.class
                .getDeclaredConstructor(BrowserContext.class, Path.class);
        constructor.setAccessible(true);
        var manager = constructor.newInstance(context, artifacts);
        try {
            manager.start();
            FailureTraceReporter.suppressSensitiveBrowserArtifacts();

            manager.stopAndAttach();

            Mockito.verify(tracing).stop();
            Mockito.verify(tracing, Mockito.never()).stop(Mockito.any(Tracing.StopOptions.class));
            Assert.assertNull(com.shaft.gui.playwright.internal.PlaywrightTraceManager.getLastTracePath());
            try (var files = Files.list(artifacts)) {
                Assert.assertEquals(files.count(), 0);
            }
        } finally {
            com.shaft.gui.playwright.internal.PlaywrightTraceManager.clearLastTracePath();
            TraceEventRecorder.clear();
            deleteDirectory(artifacts);
        }
    }

    @Test
    @SuppressWarnings("rawtypes")
    public void sensitiveProviderFailureShouldRemainRedactedAcrossTraceAndDiagnosticsReporters() throws Exception {
        String latitude = "40.68924987";
        String longitude = "-74.04450071";
        String sensitiveValue = latitude + "," + longitude;
        IllegalStateException providerFailure = new IllegalStateException("Provider rejected " + sensitiveValue);
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.doThrow(providerFailure).when(bidi).send(Mockito.any(org.openqa.selenium.bidi.Command.class));
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + sensitiveValue);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-diagnostics", getClass().getName(),
                "sensitiveProviderFailureShouldRemainRedactedAcrossTraceAndDiagnosticsReporters",
                "sensitive diagnostics", "sensitive diagnostics", null, providerFailure, false);
        Path traceDirectory = FailureTraceReporter.traceDirectory(execution);
        boolean originalTraceEnabled = SHAFT.Properties.reporting.traceEnabled();
        String originalTraceMode = SHAFT.Properties.reporting.traceMode();
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            Assert.expectThrows(RuntimeException.class,
                    () -> new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                            .geolocation(coordinate(latitude), coordinate(longitude)));

            FailureTraceReporter.attachOnFailure(execution, "diagnostic log", List.of());
            String diagnostics = FailureDiagnosticsReporter.renderDiagnosticsJson(execution, "diagnostic log",
                    List.of());
            String brief = FailureBriefReporter.renderBriefJson(execution, "diagnostic log", List.of());

            Assert.assertFalse(diagnostics.contains(latitude), diagnostics);
            Assert.assertFalse(diagnostics.contains(longitude), diagnostics);
            Assert.assertTrue(diagnostics.contains(IllegalStateException.class.getName()), diagnostics);
            Assert.assertFalse(brief.contains(latitude), brief);
            Assert.assertFalse(brief.contains(longitude), brief);
            Assert.assertTrue(brief.contains(IllegalStateException.class.getName()), brief);
        } finally {
            SHAFT.Properties.reporting.set().traceEnabled(originalTraceEnabled).traceMode(originalTraceMode);
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
        }
    }

    @Test
    public void sensitiveEmulationShouldRedactDownstreamJiraLogText() {
        String latitude = "37.81992861";
        String longitude = "-122.47825519";
        String sensitiveValue = latitude + "," + longitude;
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver driver = liveBiDiDriver(bidi, "application rendered " + sensitiveValue);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-jira", getClass().getName(),
                "sensitiveEmulationShouldRedactDownstreamJiraLogText", "sensitive jira", "sensitive jira",
                null, new AssertionError("unrelated terminal failure"), false);
        try {
            SHAFT.Properties.jira.set().jiraInteraction(true).reportBugs(true);
            new com.shaft.gui.browser.BrowserActions(driver, true).emulation().location()
                    .geolocation(coordinate(latitude), coordinate(longitude));
            try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
                JiraHelper.reportBugsToJIRA(List.of(), "provider log echoed " + sensitiveValue, execution);

                xray.verify(() -> XrayIntegrationHelper.createIssue(Mockito.eq(List.of()), Mockito.anyString(),
                        Mockito.argThat(text -> text != null && !text.contains(latitude) && !text.contains(longitude))));
            }
        } finally {
            Properties.clearForCurrentThread();
            TraceEventRecorder.clear();
        }
    }

    @Test
    public void sensitiveProviderFailureShouldRemainRedactedInPostArtifactFailureLogging() {
        String sensitiveValue = "opaque-post-artifact-value-5831";
        IllegalStateException providerFailure = new IllegalStateException("Provider echoed " + sensitiveValue);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-late-log", getClass().getName(),
                "sensitiveProviderFailureShouldRemainRedactedInPostArtifactFailureLogging", "late log",
                "late log", null, providerFailure, false);
        ReportContext.start(execution);
        try {
            FailureTraceReporter.registerSensitiveSourceValue(sensitiveValue);
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);

            ExecutionLifecycleHelper.logFinishedTestInformation(execution, "Failed", providerFailure);

            String output = String.join("\n", ReportContext.snapshotOutput());
            Assert.assertFalse(output.contains(sensitiveValue), output);
            Assert.assertTrue(output.contains("Finished test method"), output);
        } finally {
            ReportContext.clear();
        }
    }

    @Test
    public void sensitiveProviderFailureShouldRemainRedactedInExecutionSummary() {
        String sensitiveValue = "opaque-summary-value-6419";
        TestExecutionInfo execution = new TestExecutionInfo("emulation-summary", getClass().getName(),
                "sensitiveProviderFailureShouldRemainRedactedInExecutionSummary", "summary", "summary",
                null, new IllegalStateException("Provider echoed " + sensitiveValue), false);
        try {
            FailureTraceReporter.registerSensitiveSourceValue(sensitiveValue);
            try (MockedStatic<ExecutionSummaryReport> summary = Mockito.mockStatic(ExecutionSummaryReport.class)) {
                ExecutionLifecycleHelper.appendExecutionSummaryReport(execution,
                        "Provider echoed " + sensitiveValue, ExecutionSummaryReport.StatusIcon.FAILED,
                        ExecutionSummaryReport.Status.FAILED);

                summary.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(Mockito.anyString(),
                        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                        Mockito.argThat(message -> message != null && !message.contains(sensitiveValue)),
                        Mockito.anyString(), Mockito.anyString()));
            }
        } finally {
            TraceEventRecorder.clear();
        }
    }

    @Test
    public void everySeleniumEmulationOperationShouldOwnExactlyOneCategorizedEvent() {
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(HasBiDi.class, HasDevTools.class, HasCapabilities.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(((HasCapabilities) driver).getCapabilities()).thenReturn(capabilities);
        Mockito.when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) driver).getBiDi()).thenReturn(bidi);
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        Mockito.when(driver.getWindowHandle()).thenReturn("context-1");
        EmulationActionsContract emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation();

        List<Invocation> operations = List.of(
                new Invocation("emulation/screen", "viewport", () -> emulation.screen().viewport(800, 600)),
                new Invocation("emulation/screen", "clear-viewport", () -> emulation.screen().clearViewport()),
                new Invocation("emulation/screen", "screen-size", () -> emulation.screen().screenSize(1280, 720)),
                new Invocation("emulation/screen", "clear-screen-size", () -> emulation.screen().clearScreenSize()),
                new Invocation("emulation/location", "geolocation", () -> emulation.location().geolocation(30, 31)),
                new Invocation("emulation/location", "clear-geolocation", () -> emulation.location().clearGeolocation()),
                new Invocation("emulation/location", "timezone", () -> emulation.location().timezone("Africa/Cairo")),
                new Invocation("emulation/location", "clear-timezone", () -> emulation.location().clearTimezone()),
                new Invocation("emulation/location", "locale", () -> emulation.location().locale("ar-EG")),
                new Invocation("emulation/location", "clear-locale", () -> emulation.location().clearLocale()),
                new Invocation("emulation/media", "type", () -> emulation.media().type(EmulatedMediaType.PRINT)),
                new Invocation("emulation/media", "color-scheme", () -> emulation.media().colorScheme(EmulatedColorScheme.DARK)),
                new Invocation("emulation/media", "reduced-motion", () -> emulation.media().reducedMotion(EmulatedReducedMotion.REDUCE)),
                new Invocation("emulation/media", "reset", () -> emulation.media().reset()),
                new Invocation("emulation/runtime", "user-agent", () -> emulation.runtime().userAgent("agent")),
                new Invocation("emulation/runtime", "clear-user-agent", () -> emulation.runtime().clearUserAgent()),
                new Invocation("emulation/runtime", "disable-scripting", () -> emulation.runtime().disableScripting()),
                new Invocation("emulation/runtime", "clear-scripting", () -> emulation.runtime().clearScriptingOverride()));

        for (Invocation operation : operations) {
            TraceEventRecorder.clear();
            operation.action().run();
            assertSingleEvent(operation.category(), operation.name(), "passed", AutomationBackend.SELENIUM_WEBDRIVER);
        }
    }

    @Test
    public void everyPlaywrightEmulationBranchShouldOwnExactlyOneCategorizedEvent() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        Mockito.when(page.isClosed()).thenReturn(false);
        EmulationActionsContract emulation = new com.shaft.gui.playwright.browser.BrowserActions(session).emulation();

        List<Invocation> supported = List.of(
                new Invocation("emulation/screen", "viewport", () -> emulation.screen().viewport(800, 600)),
                new Invocation("emulation/screen", "clear-viewport", () -> emulation.screen().clearViewport()),
                new Invocation("emulation/location", "geolocation", () -> emulation.location().geolocation(30, 31)),
                new Invocation("emulation/location", "clear-geolocation", () -> emulation.location().clearGeolocation()),
                new Invocation("emulation/media", "type", () -> emulation.media().type(EmulatedMediaType.PRINT)),
                new Invocation("emulation/media", "color-scheme", () -> emulation.media().colorScheme(EmulatedColorScheme.DARK)),
                new Invocation("emulation/media", "reduced-motion", () -> emulation.media().reducedMotion(EmulatedReducedMotion.REDUCE)),
                new Invocation("emulation/media", "reset", () -> emulation.media().reset()));
        for (Invocation operation : supported) {
            TraceEventRecorder.clear();
            operation.action().run();
            assertSingleEvent(operation.category(), operation.name(), "passed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
        }

        List<Invocation> contextCreateOnly = List.of(
                new Invocation("emulation/screen", "screen size", () -> emulation.screen().screenSize(1280, 720)),
                new Invocation("emulation/screen", "clear screen size", () -> emulation.screen().clearScreenSize()),
                new Invocation("emulation/location", "timezone", () -> emulation.location().timezone("Africa/Cairo")),
                new Invocation("emulation/location", "clear timezone", () -> emulation.location().clearTimezone()),
                new Invocation("emulation/location", "locale", () -> emulation.location().locale("ar-EG")),
                new Invocation("emulation/location", "clear locale", () -> emulation.location().clearLocale()),
                new Invocation("emulation/runtime", "user agent", () -> emulation.runtime().userAgent("agent")),
                new Invocation("emulation/runtime", "clear user agent", () -> emulation.runtime().clearUserAgent()),
                new Invocation("emulation/runtime", "disable scripting", () -> emulation.runtime().disableScripting()),
                new Invocation("emulation/runtime", "clear scripting", () -> emulation.runtime().clearScriptingOverride()));
        for (Invocation operation : contextCreateOnly) {
            TraceEventRecorder.clear();
            Assert.expectThrows(UnsupportedOperationException.class, operation.action()::run);
            assertSingleEvent(operation.category(), operation.name(), "failed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
        }
    }

    private static void assertSingleEvent(String category, String name, String status, AutomationBackend backend) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), category);
        Assert.assertEquals(events.getFirst().name(), name);
        Assert.assertEquals(events.getFirst().status(), status);
        Assert.assertEquals(events.getFirst().backend(), backend);
    }

    private static WebDriver liveBiDiDriver(BiDi bidi, String pageSource) {
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HasBiDi.class, HasCapabilities.class));
        Mockito.when(((HasCapabilities) driver).getCapabilities()).thenReturn(capabilities);
        Mockito.when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) driver).getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("context-1");
        Mockito.when(driver.getPageSource()).thenReturn(pageSource);
        return driver;
    }

    private static void assertSensitiveEmulationFailure(String method, Throwable throwable, String... secrets) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().status(), "failed");
        Assert.assertTrue(events.getFirst().domSnapshotBefore().isEmpty());
        Assert.assertTrue(events.getFirst().domSnapshotAfter().isEmpty());
        String report = FailureTraceReporter.renderTraceJson(
                new TestExecutionInfo("emulation-" + method, BrowserEmulationNamespaceTraceTest.class.getName(),
                        method, method, "emulation trace", null, throwable, false), "", List.of());
        for (String secret : secrets) {
            Assert.assertFalse(report.contains(secret), report);
        }
        Assert.assertTrue(report.contains(IllegalStateException.class.getName()), report);
    }

    private static void assertSuccessfulSensitiveActionArchive(String method, String secret, Runnable action,
                                                               String forbiddenNativeEntry)
            throws Exception {
        AssertionError unrelatedFailure = new AssertionError("unrelated terminal failure");
        TestExecutionInfo execution = new TestExecutionInfo("emulation-" + method,
                BrowserEmulationNamespaceTraceTest.class.getName(), method, method,
                "sensitive emulation success", null, unrelatedFailure, false);
        Path traceDirectory = FailureTraceReporter.traceDirectory(execution);
        boolean originalTraceEnabled = SHAFT.Properties.reporting.traceEnabled();
        String originalTraceMode = SHAFT.Properties.reporting.traceMode();
        boolean originalCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        boolean originalFullPage = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        boolean originalNativePage = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        boolean originalDomSnapshots = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        boolean originalScreenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean originalNetwork = SHAFT.Properties.reporting.traceIncludeNetwork();
        boolean originalConsole = SHAFT.Properties.reporting.traceIncludeConsole();
        try {
            deleteDirectory(traceDirectory);
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceMode("failure")
                    .traceIncludeCodeContext(true)
                    .traceIncludeFullPageSnapshots(true)
                    .traceIncludeNativePageSource(true)
                    .traceIncludeDomSnapshots(true)
                    .traceIncludeScreenshots(true)
                    .traceIncludeNetwork(true)
                    .traceIncludeConsole(true);
            action.run();

            WebDriver laterDriver = Mockito.mock(WebDriver.class);
            Mockito.when(laterDriver.getPageSource()).thenReturn("later browser evidence " + secret);
            Mockito.when(laterDriver.getCurrentUrl()).thenReturn("https://example.test/after-sensitive-action");
            TraceEventRecorder.Event laterAction = TraceEventRecorder.start(
                    "element", "click", "button#after-sensitive", laterDriver);
            TraceEventRecorder.recordScreenshot(laterAction,
                    ("later screenshot evidence " + secret).getBytes(StandardCharsets.UTF_8));
            TraceEventRecorder.finish(laterAction, "passed", "ordinary action retained", null, Map.of(), List.of());
            BrowserObservabilityRecorder.recordConsole("browser", "ERROR",
                    "later console evidence " + secret, System.currentTimeMillis());
            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "GET", "https://example.test/" + secret, 200, Map.of(), Map.of(), 1, 0, 0, "",
                    "later network evidence " + secret));

            FailureTraceReporter.attachOnFailure(execution, "unrelated diagnostic log", List.of());

            try (ZipFile zip = new ZipFile(traceDirectory.resolve("shaft-trace.zip").toFile())) {
                var entries = zip.entries();
                while (entries.hasMoreElements()) {
                    var entry = entries.nextElement();
                    if (!entry.isDirectory()) {
                        String content = new String(zip.getInputStream(entry).readAllBytes(), StandardCharsets.UTF_8);
                        Assert.assertFalse(content.contains(secret), entry.getName() + ": " + content);
                    }
                }
                String json = new String(zip.getInputStream(zip.getEntry("shaft-trace.json")).readAllBytes(),
                        StandardCharsets.UTF_8);
                JsonNode trace = JSON.readTree(json);
                Assert.assertTrue(trace.path("exception").path("message").asText()
                        .contains("unrelated terminal failure"), json);
                Assert.assertEquals(trace.path("snapshot").path("type").asText(), "omitted-sensitive", json);
                JsonNode evidence = trace.path("evidence");
                Assert.assertTrue(evidence.path("actions").toString().contains("ordinary action retained"), json);
                for (JsonNode recordedAction : evidence.path("actions")) {
                    Assert.assertFalse(recordedAction.has("domSnapshotBefore"), json);
                }
                Assert.assertTrue(evidence.path("console").isEmpty(), json);
                Assert.assertTrue(evidence.path("network").isEmpty(), json);
                if (forbiddenNativeEntry != null) {
                    Assert.assertNull(zip.getEntry(forbiddenNativeEntry));
                }
                Assert.assertTrue(zip.stream().noneMatch(entry -> entry.getName().startsWith("screenshots/")));
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(originalTraceEnabled)
                    .traceMode(originalTraceMode)
                    .traceIncludeCodeContext(originalCodeContext)
                    .traceIncludeFullPageSnapshots(originalFullPage)
                    .traceIncludeNativePageSource(originalNativePage)
                    .traceIncludeDomSnapshots(originalDomSnapshots)
                    .traceIncludeScreenshots(originalScreenshots)
                    .traceIncludeNetwork(originalNetwork)
                    .traceIncludeConsole(originalConsole);
            TraceEventRecorder.clear();
            deleteDirectory(traceDirectory);
        }
    }

    private static void assertSuccessfulSensitiveActionEchoFailure(String method, String secret, Runnable action) {
        AssertionError unrelatedFailure = new AssertionError("Observed application value " + secret);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-" + method,
                BrowserEmulationNamespaceTraceTest.class.getName(), method, method,
                "sensitive emulation echo failure", null, unrelatedFailure, false);
        action.run();

        String trace = FailureTraceReporter.renderTraceJson(execution, "echo failure log", List.of());
        String diagnostics = FailureDiagnosticsReporter.renderDiagnosticsJson(execution, "echo failure log",
                List.of());
        String brief = FailureBriefReporter.renderBriefJson(execution, "echo failure log", List.of());
        ReportContext.start(execution);
        FailureTraceReporter.registerSensitiveSourceValue(secret);
        ReportManagerHelper.logFinishedTestInformation(getClassName(execution), method,
                "echo failure", "Failed", unrelatedFailure);
        String liveLog = String.join("\n", ReportContext.snapshotOutput());

        for (String output : List.of(trace, diagnostics, brief, liveLog)) {
            Assert.assertFalse(output.contains(secret), output);
            Assert.assertTrue(output.contains(AssertionError.class.getName())
                    || output.contains("Finished test method"), output);
        }
        ReportContext.clear();
    }

    private static void assertHistoricalSensitiveValueIsRedacted(String method, String latitude, String longitude) {
        AssertionError unrelatedFailure = new AssertionError("Application rendered " + latitude + "," + longitude);
        TestExecutionInfo execution = new TestExecutionInfo("emulation-" + method,
                BrowserEmulationNamespaceTraceTest.class.getName(), method, method,
                "historical emulation value", null, unrelatedFailure, false);
        ReportContext.start(execution);

        String trace = FailureTraceReporter.renderTraceJson(execution, "historical emulation value", List.of());

        Assert.assertFalse(trace.contains(latitude), trace);
        Assert.assertFalse(trace.contains(longitude), trace);
    }

    private static String getClassName(TestExecutionInfo info) {
        return info.className();
    }

    private static PlaywrightSession livePlaywrightSession() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        return session;
    }

    private static double coordinate(String value) {
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException exception) {
            throw new AssertionError("The test fixture must contain a valid coordinate.", exception);
        }
    }

    private static void deleteDirectory(Path directory) throws Exception {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (Exception exception) {
                    throw new IllegalStateException(exception);
                }
            });
        }
    }

    private record Invocation(String category, String name, Runnable action) { }
}
