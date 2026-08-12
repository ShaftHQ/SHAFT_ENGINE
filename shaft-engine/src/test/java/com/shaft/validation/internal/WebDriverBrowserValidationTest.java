package com.shaft.validation.internal;

import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BidiConsoleLogSource;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.StepResult;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.support.ui.FluentWait;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.function.Function;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.withSettings;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class WebDriverBrowserValidationTest {
    @BeforeMethod
    public void disableFailureArtifacts() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
    }

    @AfterMethod(alwaysRun = true)
    public void resetValidationState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void observableBrowserValuesShouldReadOnlyTheirAuthoritativeOwners() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(HasDownloads.class));
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.alert()).thenReturn(mock(org.openqa.selenium.Alert.class));
        HasDownloads downloads = (HasDownloads) driver;
        when(downloads.getDownloadedFiles()).thenReturn(java.util.List.of(
                mock(HasDownloads.DownloadedFile.class), mock(HasDownloads.DownloadedFile.class)));
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test")
                .nativeFeature(AutomationFeature.DOWNLOADS, "test")
                .adaptedFeature(AutomationFeature.NETWORK_OBSERVATION, "test")
                .adaptedFeature(AutomationFeature.CONSOLE_LOGS, "test")
                .build();
        var console = java.util.List.of(
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "info", "private-message", 1),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "debug", "private-debug", 2),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "warn", "private-warning", 3),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "error", "private-error", 4));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class);
             MockedStatic<BrowserNetworkInterceptor> network = Mockito.mockStatic(BrowserNetworkInterceptor.class);
             MockedStatic<BidiConsoleLogSource> bidi = Mockito.mockStatic(BidiConsoleLogSource.class);
             MockedStatic<Allure> allure = Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManager> report = Mockito.mockStatic(ReportManager.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            java.util.List<java.util.List<io.qameta.allure.model.Parameter>> parameterSnapshots =
                    captureAllStepParameterUpdates(lifecycle);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(driver)).thenReturn(capabilities);
            network.when(() -> BrowserNetworkInterceptor.observationCountIfPresent(driver))
                    .thenReturn(java.util.OptionalInt.of(3));
            bidi.when(() -> BidiConsoleLogSource.isHealthy(driver)).thenReturn(true);
            bidi.when(() -> BidiConsoleLogSource.snapshot(driver)).thenReturn(console);
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            assertions.dialogPresentValue().isEqualTo(true);
            assertions.downloadCountValue().isEqualTo(2);
            assertions.networkObservationCountValue().isEqualTo(3);
            assertions.consoleMessageCountValue().isEqualTo(4);
            assertions.consoleErrorCountValue().isEqualTo(1);
            assertions.featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);

            resolver.verify(() -> AutomationCapabilityResolver.forWebDriver(driver), atLeast(6));
            network.verify(() -> BrowserNetworkInterceptor.observationCountIfPresent(driver), times(1));
            bidi.verify(() -> BidiConsoleLogSource.snapshot(driver), times(2));
            Assert.assertTrue(parameterSnapshots.size() >= 6);
            Assert.assertTrue(parameterSnapshots.stream().flatMap(java.util.Collection::stream).noneMatch(parameter ->
                    String.valueOf(parameter.getValue()).contains("private-")));
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains("private-"))), never());
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains("private-")),
                    any(org.apache.logging.log4j.Level.class)), never());
        }

        verify(targetLocator).alert();
        verify(downloads).getDownloadedFiles();
    }

    @Test
    public void observableBrowserPlansShouldStayLazyFailClosedAndPreserveProviderTimeoutIdentity() {
        WebDriver driver = mock(WebDriver.class);
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test")
                .adaptedFeature(AutomationFeature.NETWORK_OBSERVATION, "test")
                .adaptedFeature(AutomationFeature.CONSOLE_LOGS, "test")
                .build();
        TimeoutException sentinel = new TimeoutException("provider-timeout-sentinel");
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.alert()).thenThrow(sentinel);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class);
             MockedStatic<BrowserNetworkInterceptor> network = Mockito.mockStatic(BrowserNetworkInterceptor.class);
             MockedStatic<BidiConsoleLogSource> bidi = Mockito.mockStatic(BidiConsoleLogSource.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(driver)).thenReturn(capabilities);
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            var dialogPlan = assertions.dialogPresentValue();
            assertions.downloadCountValue();
            var networkPlan = assertions.networkObservationCountValue();
            assertions.consoleMessageCountValue();
            assertions.consoleErrorCountValue();
            assertions.featureSupportedValue(AutomationFeature.MEDIA_EMULATION);
            resolver.verifyNoInteractions();
            network.verifyNoInteractions();
            bidi.verifyNoInteractions();

            Assert.assertSame(Assert.expectThrows(TimeoutException.class, () -> dialogPlan.isEqualTo(true)), sentinel);
            UnsupportedOperationException missingState = Assert.expectThrows(UnsupportedOperationException.class,
                    () -> networkPlan.isEqualTo(0));
            Assert.assertTrue(missingState.getMessage().contains("No retained network-observation state"));
            UnsupportedOperationException missingConsole = Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.consoleMessageCountValue().isEqualTo(0));
            Assert.assertTrue(missingConsole.getMessage().contains("No session-bound console-observation state"));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.downloadCountValue().isEqualTo(0));
            Assert.expectThrows(NullPointerException.class,
                    () -> assertions.featureSupportedValue(null));
        }
    }

    @Test
    public void observableBrowserValuesShouldRetryAndRejectClosedSessionsBeforeCapabilityResolution() {
        WebDriver driver = mock(WebDriver.class);
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.alert()).thenThrow(new org.openqa.selenium.NoAlertPresentException("not yet"))
                .thenReturn(mock(org.openqa.selenium.Alert.class));
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test").build();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 2);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(driver)).thenReturn(capabilities);
            new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser ")).dialogPresentValue().isEqualTo(true);
        }
        verify(targetLocator, times(2)).alert();

        RemoteWebDriver closed = mock(RemoteWebDriver.class);
        when(closed.getSessionId()).thenReturn(null);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(closed, 1);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                            closed, new StringBuilder("the browser "))
                            .featureSupportedValue(AutomationFeature.BROWSER_AUTOMATION).isEqualTo(true));
            resolver.verifyNoInteractions();
        }
    }

    @Test
    public void observableDialogAndConsoleShouldRejectMissingCapabilitiesBeforeTheirOwners() {
        WebDriver driver = mock(WebDriver.class);
        WebDriver.TargetLocator target = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(target);
        AutomationCapabilities unsupported = AutomationCapabilities
                .builder(AutomationBackend.SELENIUM_WEBDRIVER).build();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class);
             MockedStatic<BidiConsoleLogSource> bidi = Mockito.mockStatic(BidiConsoleLogSource.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(driver)).thenReturn(unsupported);
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.dialogPresentValue().isEqualTo(false));
            verify(target, never()).alert();
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.consoleMessageCountValue().isEqualTo(0));
            bidi.verifyNoInteractions();
        }
    }

    @Test
    public void everyPublicWebDriverBrowserRootShouldRouteObservableFeatureSupportWithHardAndSoftSemantics() {
        WebDriver driver = mock(WebDriver.class);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        var browser = new BrowserActions(helper);
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test").build();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(driver)).thenReturn(capabilities);
            new WizardHelpers.WebDriverAssertions(helper).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            new WizardHelpers.WebDriverVerifications(helper).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            browser.assertThat().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            browser.verifyThat().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);

            assertHardFailure(() -> browser.assertThat()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertSoftFailure(() -> browser.verifyThat()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertHardFailure(() -> new WizardHelpers.WebDriverAssertions(helper).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertSoftFailure(() -> new WizardHelpers.WebDriverVerifications(helper).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
        }
    }

    @Test
    public void retainedWizardBrowserRootsShouldStayBoundToTheirOriginalDriver() {
        WebDriver first = mock(WebDriver.class);
        WebDriver second = mock(WebDriver.class);
        DriverFactoryHelper firstHelper = mock(DriverFactoryHelper.class);
        DriverFactoryHelper secondHelper = mock(DriverFactoryHelper.class);
        when(firstHelper.getDriver()).thenReturn(first);
        when(secondHelper.getDriver()).thenReturn(second);
        var retainedAssertions = new WizardHelpers.WebDriverAssertions(firstHelper);
        var retainedVerifications = new WizardHelpers.WebDriverVerifications(firstHelper);
        new WizardHelpers.WebDriverAssertions(secondHelper);
        new WizardHelpers.WebDriverVerifications(secondHelper);
        AutomationCapabilities firstCapabilities = AutomationCapabilities
                .builder(AutomationBackend.SELENIUM_WEBDRIVER).build();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(first, 1);
             MockedStatic<AutomationCapabilityResolver> resolver = Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forWebDriver(first)).thenReturn(firstCapabilities);
            retainedAssertions.browser().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            retainedVerifications.browser().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            resolver.verify(() -> AutomationCapabilityResolver.forWebDriver(first), atLeast(2));
            resolver.verify(() -> AutomationCapabilityResolver.forWebDriver(second), never());
        }
    }

    @Test
    public void focusedBrowserCoreCategoriesShouldReadTheirProviderValues() {
        WebDriver driver = mock(WebDriver.class);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> {
                         when(actions.getPageSource()).thenReturn("<html>ready</html>");
                         when(actions.getWindowHandle()).thenReturn("window-2");
                         when(actions.getWindowPosition()).thenReturn("(10, 20)");
                         when(actions.getWindowSize()).thenReturn("(1200, 800)");
                     })) {
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            assertions.pageSourceValue().contains("ready");
            assertions.windowHandleValue().isEqualTo("window-2");
            assertions.windowPositionValue().isEqualTo("(10, 20)");
            assertions.windowSizeValue().isEqualTo("(1200, 800)");

            Assert.assertEquals(browserActions.constructed().size(), 4);
            verify(browserActions.constructed().get(0)).getPageSource();
            verify(browserActions.constructed().get(1)).getWindowHandle();
            verify(browserActions.constructed().get(2)).getWindowPosition();
            verify(browserActions.constructed().get(3)).getWindowSize();
        }
    }

    @Test
    public void focusedBrowserCoreAliasesShouldRouteToTheSameProviderValues() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getWindowHandles()).thenReturn(java.util.Set.of("one", "two"));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> {
                         when(actions.getPageSource()).thenReturn("source-value");
                         when(actions.getWindowHandle()).thenReturn("handle-value");
                         when(actions.getWindowPosition()).thenReturn("(10, 20)");
                         when(actions.getWindowSize()).thenReturn("(1200, 800)");
                     })) {
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            for (String alias : new String[]{"pagesource", "windowsource", "source"}) {
                assertions.attribute(alias).isEqualTo("source-value");
            }
            for (String alias : new String[]{"windowhandle", "pagehandle", "pagehndle", "handle"}) {
                assertions.attribute(alias).isEqualTo("handle-value");
            }
            for (String alias : new String[]{"windowposition", "pageposition", "position"}) {
                assertions.attribute(alias).isEqualTo("(10, 20)");
            }
            for (String alias : new String[]{"windowsize", "pagesize", "size"}) {
                assertions.attribute(alias).isEqualTo("(1200, 800)");
            }
            for (String alias : new String[]{"browsingcontextcount", "windowcount", "pagecount"}) {
                assertions.attribute(alias).isEqualTo(2);
            }
        }
    }

    @Test
    public void focusedBrowserCoreValuesShouldHonorEveryComparisonModeAndPolarity() {
        WebDriver driver = mock(WebDriver.class);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> when(actions.getPageSource()).thenReturn("<html>Ready</html>"))) {
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            assertions.pageSourceValue().matchesRegex(".*Ready.*");
            assertions.pageSourceValue().equalsIgnoringCaseSensitivity("<HTML>READY</HTML>");
            assertions.pageSourceValue().doesNotContain("missing");
            Assert.expectThrows(AssertionError.class,
                    () -> assertions.pageSourceValue().doesNotContain("Ready"));
        }
    }

    @Test
    public void focusedBrowserCoreCategoriesShouldUseTheExistingBoundedWait() {
        WebDriver driver = mock(WebDriver.class);
        AtomicInteger reads = new AtomicInteger();
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 2);
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> when(actions.getWindowPosition()).thenAnswer(invocation ->
                             reads.incrementAndGet() == 1 ? "(0, 0)" : "(10, 20)"))) {
            new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, driver,
                    new StringBuilder("the browser ")).windowPositionValue().isEqualTo("(10, 20)");

            Assert.assertEquals(browserActions.constructed().size(), 2);
            verify(browserActions.constructed().get(0)).getWindowPosition();
            verify(browserActions.constructed().get(1)).getWindowPosition();
        }
    }

    @Test
    public void browsingContextCountShouldRetryAgainstCurrentWindowHandles() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getWindowHandles()).thenReturn(java.util.Set.of("one"), java.util.Set.of("one", "two"));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 2)) {
            new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, driver,
                    new StringBuilder("the browser ")).browsingContextCountValue().isEqualTo(2);
        }

        verify(driver, times(2)).getWindowHandles();
    }

    @Test
    public void browsingContextCountShouldNormalizeNumericAndStringExpectations() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getWindowHandles()).thenReturn(java.util.Set.of("one", "two"));
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1)) {
            var assertions = new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    driver, new StringBuilder("the browser "));
            assertions.browsingContextCountValue().isEqualTo(2);
            assertions.browsingContextCountValue().isEqualTo("2");
            assertions.browsingContextCountValue().doesNotEqual("1");
        }

        verify(driver, times(3)).getWindowHandles();
    }

    @Test
    public void pageSourceValueShouldNeverPublishTheComparedPayload() {
        String secret = "private-dom-token-7831";
        WebDriver driver = mock(WebDriver.class);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> when(actions.getPageSource()).thenReturn("<html>" + secret + "</html>"));
             MockedStatic<Allure> allure = Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManager> report = Mockito.mockStatic(ReportManager.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            StepResult step = captureStepUpdates(lifecycle);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);

            for (String alias : new String[]{"pagesource", "windowsource", "source"}) {
                new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                        driver, new StringBuilder("the browser ")).attribute(alias)
                        .isEqualTo("<html>" + secret + "</html>");
            }

            Assert.assertTrue(step.getParameters().stream()
                    .noneMatch(parameter -> String.valueOf(parameter.getValue()).contains(secret)));
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains(secret))), never());
            report.verify(() -> ReportManager.logDiscrete(
                    argThat(message -> message.contains(secret)), any(org.apache.logging.log4j.Level.class)), never());
            report.verify(() -> ReportManager.logDiscrete(
                    "Assert that the browser page source payload matches the requested comparison."), times(3));
        }
    }

    @Test
    public void failedPageSourceValueShouldKeepPayloadOutOfHardAndSoftFailureMessages() {
        String secret = "failed-private-dom-token-6624";
        WebDriver driver = mock(WebDriver.class);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> when(actions.getPageSource())
                             .thenReturn("<html>actual-" + secret + "</html>"))) {
            AssertionError hard = Assert.expectThrows(AssertionError.class,
                    () -> new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                            driver, new StringBuilder("the browser ")).pageSourceValue()
                            .isEqualTo("<html>expected-" + secret + "</html>"));
            Assert.assertFalse(String.valueOf(hard.getMessage()).contains(secret));

            new WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                    driver, new StringBuilder("the browser ")).pageSourceValue()
                    .isEqualTo("<html>expected-" + secret + "</html>");
            AssertionError soft = ValidationsHelper.getVerificationErrorToForceFail();
            Assert.assertNotNull(soft);
            Assert.assertFalse(String.valueOf(soft.getMessage()).contains(secret));
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @Test
    public void everyPublicBrowserStarterShouldRouteFocusedCoreCategories() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getWindowHandles()).thenReturn(java.util.Set.of("one", "two"));
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        var browser = new BrowserActions(helper);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> {
                         if (!context.arguments().isEmpty() && context.arguments().getFirst() == driver) {
                             when(actions.getPageSource()).thenReturn("<html>ready</html>");
                         }
                     })) {
            new WizardHelpers.WebDriverAssertions(helper).browser().pageSourceValue().contains("ready");
            new WizardHelpers.WebDriverVerifications(helper).browser().pageSourceValue().contains("ready");
            browser.assertThat().pageSourceValue().contains("ready");
            browser.verifyThat().pageSourceValue().contains("ready");
            new WizardHelpers.WebDriverAssertions(helper).browser().browsingContextCountValue().isEqualTo(2);
            new WizardHelpers.WebDriverVerifications(helper).browser().browsingContextCountValue().isEqualTo(2);
            browser.assertThat().browsingContextCountValue().isEqualTo(2);
            browser.verifyThat().browsingContextCountValue().isEqualTo(2);

            Assert.assertEquals(browserActions.constructed().size(), 4);
            for (int index = 0; index < 4; index++) {
                verify(browserActions.constructed().get(index)).getPageSource();
            }
            verify(driver, times(4)).getWindowHandles();
        }
    }

    @Test
    public void everyPublicBrowserStarterShouldPreserveHardAndSoftFailureSemantics() {
        WebDriver driver = mock(WebDriver.class);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        var browser = new BrowserActions(helper);
        try (MockedConstruction<SynchronizationManager> ignored = waitApplying(driver, 1);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> {
                         if (!context.arguments().isEmpty() && context.arguments().getFirst() == driver) {
                             when(actions.getPageSource()).thenReturn("<html>actual</html>");
                         }
                     })) {
            assertHardFailure(() -> new WizardHelpers.WebDriverAssertions(helper).browser()
                    .pageSourceValue().isEqualTo("expected"));
            assertSoftFailure(() -> new WizardHelpers.WebDriverVerifications(helper).browser()
                    .pageSourceValue().isEqualTo("expected"));
            assertHardFailure(() -> browser.assertThat().pageSourceValue().isEqualTo("expected"));
            assertSoftFailure(() -> browser.verifyThat().pageSourceValue().isEqualTo("expected"));
        }
    }

    @Test
    public void exhaustedBrowserWaitShouldStillRouteThroughHardAndSoftFailureSemantics() {
        WebDriver driver = mock(WebDriver.class);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        try (MockedConstruction<SynchronizationManager> ignored = waitTimingOut(driver);
             MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                     (manager, context) -> when(manager.takeScreenshot(any(), any(), anyString(), anyBoolean()))
                             .thenReturn(java.util.List.of("screenshot", "image/png", new byte[]{1})));
             MockedConstruction<BrowserActions> browserActions = Mockito.mockConstruction(BrowserActions.class,
                     (actions, context) -> {
                         if (!context.arguments().isEmpty() && context.arguments().getFirst() == driver) {
                             when(actions.getPageSource()).thenReturn("<html>actual</html>");
                         }
                     })) {
            assertHardFailure(() -> new WizardHelpers.WebDriverAssertions(helper).browser()
                    .pageSourceValue().isEqualTo("expected"));
            assertSoftFailure(() -> new WizardHelpers.WebDriverVerifications(helper).browser()
                    .pageSourceValue().isEqualTo("expected"));
        }
    }

    private static void assertHardFailure(Runnable assertion) {
        Assert.expectThrows(AssertionError.class, assertion::run);
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());
    }

    private static void assertSoftFailure(Runnable verification) {
        verification.run();
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
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

    @SuppressWarnings("unchecked")
    private static MockedConstruction<SynchronizationManager> waitTimingOut(WebDriver driver) {
        return Mockito.mockConstruction(SynchronizationManager.class, (manager, context) -> {
            FluentWait<WebDriver> wait = mock(FluentWait.class);
            when(manager.fluentWait(anyBoolean())).thenReturn(wait);
            when(wait.until(any())).thenAnswer(invocation -> {
                Function<? super WebDriver, ?> condition = invocation.getArgument(0);
                condition.apply(driver);
                throw new TimeoutException("timed out");
            });
        });
    }

    @SuppressWarnings("unchecked")
    private static StepResult captureStepUpdates(AllureLifecycle lifecycle) {
        StepResult step = new StepResult();
        doAnswer(invocation -> {
            Consumer<StepResult> consumer = invocation.getArgument(0);
            consumer.accept(step);
            return null;
        }).when(lifecycle).updateStep(any(Consumer.class));
        return step;
    }

    @SuppressWarnings("unchecked")
    private static java.util.List<java.util.List<io.qameta.allure.model.Parameter>>
    captureAllStepParameterUpdates(AllureLifecycle lifecycle) {
        java.util.List<java.util.List<io.qameta.allure.model.Parameter>> snapshots = new java.util.ArrayList<>();
        StepResult step = new StepResult();
        doAnswer(invocation -> {
            Consumer<StepResult> consumer = invocation.getArgument(0);
            consumer.accept(step);
            snapshots.add(java.util.List.copyOf(step.getParameters()));
            return null;
        }).when(lifecycle).updateStep(any(Consumer.class));
        return snapshots;
    }
}
