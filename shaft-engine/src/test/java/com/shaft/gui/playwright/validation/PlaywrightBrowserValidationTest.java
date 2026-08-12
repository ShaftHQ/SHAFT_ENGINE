package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Page;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.TimeoutError;
import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.StepResult;
import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightBrowserValidationTest {
    private static final String WINDOW_POSITION_SCRIPT = "() => `(${window.screenX}, ${window.screenY})`";

    @AfterMethod(alwaysRun = true)
    public void resetValidationState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void observableBrowserValuesShouldReadOnlyTheirAuthoritativeOwners() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        when(session.isDialogSeen()).thenReturn(true);
        when(session.downloadSnapshot()).thenReturn(java.util.List.of(
                mock(com.microsoft.playwright.Download.class), mock(com.microsoft.playwright.Download.class)));
        when(session.consoleSnapshot()).thenReturn(java.util.List.of(
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "info", "private-message", 1),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "debug", "private-debug", 2),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "warn", "private-warning", 3),
                new BrowserObservabilityRecorder.ConsoleSnapshotEntry("console", "error", "private-error", 4)));
        PlaywrightNetworkInterceptor network = mock(PlaywrightNetworkInterceptor.class);
        when(session.networkInterceptor()).thenReturn(network);
        when(network.observationCount()).thenReturn(3);
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test")
                .nativeFeature(AutomationFeature.DOWNLOADS, "test")
                .nativeFeature(AutomationFeature.NETWORK_OBSERVATION, "test")
                .nativeFeature(AutomationFeature.CONSOLE_LOGS, "test")
                .build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class);
             MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManager> report = org.mockito.Mockito.mockStatic(ReportManager.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            java.util.List<java.util.List<io.qameta.allure.model.Parameter>> parameterSnapshots =
                    captureAllStepParameterUpdates(lifecycle);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(capabilities);
            var assertions = new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session);
            assertions.dialogPresentValue().isEqualTo(true);
            assertions.downloadCountValue().isEqualTo(2);
            assertions.networkObservationCountValue().isEqualTo(3);
            assertions.consoleMessageCountValue().isEqualTo(4);
            assertions.consoleErrorCountValue().isEqualTo(1);
            assertions.featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);

            resolver.verify(() -> AutomationCapabilityResolver.forPlaywright(session), times(6));
            Assert.assertTrue(parameterSnapshots.size() >= 6);
            Assert.assertTrue(parameterSnapshots.stream().flatMap(java.util.Collection::stream).noneMatch(parameter ->
                    String.valueOf(parameter.getValue()).contains("private-")));
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains("private-"))), never());
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains("private-")),
                    any(org.apache.logging.log4j.Level.class)), never());
        }

        verify(session).isDialogSeen();
        verify(session).downloadSnapshot();
        verify(session, times(2)).consoleSnapshot();
        verify(network).observationCount();
    }

    @Test
    public void observableBrowserPlansShouldStayLazyFailClosedAndPreserveProviderFailureIdentity() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        when(context.isClosed()).thenReturn(false);
        doAnswer(invocation -> {
            invocation.<BooleanSupplier>getArgument(0).getAsBoolean();
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        UnsupportedOperationException sentinel = new UnsupportedOperationException("provider-sentinel");
        when(session.isDialogSeen()).thenThrow(sentinel);
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test")
                .build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(capabilities);
            var assertions = new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session);
            var dialogPlan = assertions.dialogPresentValue();
            assertions.downloadCountValue();
            assertions.networkObservationCountValue();
            assertions.consoleMessageCountValue();
            assertions.consoleErrorCountValue();
            assertions.featureSupportedValue(AutomationFeature.MEDIA_EMULATION);
            resolver.verifyNoInteractions();

            Assert.assertSame(Assert.expectThrows(UnsupportedOperationException.class,
                    () -> dialogPlan.isEqualTo(true)), sentinel);
            Assert.expectThrows(NullPointerException.class,
                    () -> assertions.featureSupportedValue(null));

            IllegalStateException waitSentinel = new IllegalStateException("context-wait-sentinel");
            doThrow(waitSentinel).when(context).waitForCondition(any(BooleanSupplier.class));
            Assert.assertSame(Assert.expectThrows(IllegalStateException.class,
                    () -> assertions.featureSupportedValue(AutomationFeature.MEDIA_EMULATION)
                            .isEqualTo(false)), waitSentinel);
        }

        when(context.isClosed()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new PlaywrightBrowserValidationsBuilder(
                        com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session)
                        .downloadCountValue().isEqualTo(0));
    }

    @Test
    public void observableBrowserValuesShouldRetryAgainstTheCurrentSessionOwner() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        when(session.isDialogSeen()).thenReturn(false, true);
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            Assert.assertFalse(condition.getAsBoolean());
            Assert.assertTrue(condition.getAsBoolean());
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test").build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(capabilities);
            new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session)
                    .dialogPresentValue().isEqualTo(true);
        }
        verify(session, times(2)).isDialogSeen();
    }

    @Test
    public void observableBrowserValuesShouldRejectUnsupportedCapabilityAndMissingNetworkOwner() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        doAnswer(invocation -> invocation.<BooleanSupplier>getArgument(0).getAsBoolean())
                .when(context).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities unsupported = AutomationCapabilities
                .builder(AutomationBackend.MICROSOFT_PLAYWRIGHT).build();
        AutomationCapabilities networkOnly = AutomationCapabilities
                .builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .nativeFeature(AutomationFeature.NETWORK_OBSERVATION, "test").build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(unsupported);
            var assertions = new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session);
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.downloadCountValue().isEqualTo(0));
            verify(session, never()).downloadSnapshot();

            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(networkOnly);
            UnsupportedOperationException missing = Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.networkObservationCountValue().isEqualTo(0));
            Assert.assertTrue(missing.getMessage().contains("No retained network-observation state"));
        }
    }

    @Test
    public void observableDialogAndConsoleShouldRejectMissingCapabilitiesBeforeTheirOwners() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        doAnswer(invocation -> invocation.<BooleanSupplier>getArgument(0).getAsBoolean())
                .when(context).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities unsupported = AutomationCapabilities
                .builder(AutomationBackend.MICROSOFT_PLAYWRIGHT).build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(unsupported);
            var assertions = new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session);
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.dialogPresentValue().isEqualTo(false));
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> assertions.consoleMessageCountValue().isEqualTo(0));
            verify(session, never()).isDialogSeen();
            verify(session, never()).consoleSnapshot();
        }
    }

    @Test
    public void retainedPlaywrightBrowserRootsShouldStayBoundToTheirOriginalSession() {
        PlaywrightSession first = mock(PlaywrightSession.class);
        PlaywrightSession second = mock(PlaywrightSession.class);
        BrowserContext firstContext = mock(BrowserContext.class);
        when(first.browserContext()).thenReturn(firstContext);
        doAnswer(invocation -> invocation.<BooleanSupplier>getArgument(0).getAsBoolean())
                .when(firstContext).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities capabilities = AutomationCapabilities
                .builder(AutomationBackend.MICROSOFT_PLAYWRIGHT).build();
        var retainedAssertions = new PlaywrightDriverAssertions(first);
        var retainedVerifications = new PlaywrightDriverVerifications(first);
        new PlaywrightDriverAssertions(second);
        new PlaywrightDriverVerifications(second);
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(first)).thenReturn(capabilities);
            retainedAssertions.browser().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            retainedVerifications.browser().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            resolver.verify(() -> AutomationCapabilityResolver.forPlaywright(first), atLeast(2));
            resolver.verify(() -> AutomationCapabilityResolver.forPlaywright(second), never());
        }
    }

    @Test
    public void observableBrowserMismatchTimeoutShouldRemainAValidationOutcome() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        doAnswer(invocation -> {
            Assert.assertFalse(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            throw new TimeoutError("condition timed out");
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities capabilities = AutomationCapabilities
                .builder(AutomationBackend.MICROSOFT_PLAYWRIGHT).build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(capabilities);
            assertHardFailure(() -> new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT, session)
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertSoftFailure(() -> new PlaywrightBrowserValidationsBuilder(
                    com.shaft.validation.ValidationEnums.ValidationCategory.SOFT_ASSERT, session)
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
        }
    }

    @Test
    public void everyPublicPlaywrightBrowserRootShouldRouteObservableFeatureSupportWithHardAndSoftSemantics() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        doAnswer(invocation -> {
            invocation.<BooleanSupplier>getArgument(0).getAsBoolean();
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        AutomationCapabilities capabilities = AutomationCapabilities.builder(AutomationBackend.MICROSOFT_PLAYWRIGHT)
                .nativeFeature(AutomationFeature.BROWSER_AUTOMATION, "test").build();
        try (MockedStatic<AutomationCapabilityResolver> resolver = org.mockito.Mockito.mockStatic(AutomationCapabilityResolver.class)) {
            resolver.when(() -> AutomationCapabilityResolver.forPlaywright(session)).thenReturn(capabilities);
            var browser = new com.shaft.gui.playwright.browser.BrowserActions(session);
            new PlaywrightDriverAssertions(session).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            new PlaywrightDriverVerifications(session).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            browser.assertThat().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);
            browser.verifyThat().featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(false);

            assertHardFailure(() -> browser.assertThat()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertSoftFailure(() -> browser.verifyThat()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertHardFailure(() -> new PlaywrightDriverAssertions(session).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
            assertSoftFailure(() -> new PlaywrightDriverVerifications(session).browser()
                    .featureSupportedValue(AutomationFeature.MEDIA_EMULATION).isEqualTo(true));
        }
    }

    @Test
    public void focusedBrowserCoreCategoriesShouldReadTheirProviderValues() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>ready</html>");
        when(session.pageHandle(page)).thenReturn("page-2");
        when(page.evaluate(WINDOW_POSITION_SCRIPT)).thenReturn("(10, 20)");
        when(page.evaluate("() => `(${window.outerWidth}, ${window.outerHeight})`")).thenReturn("(1200, 800)");
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            Assert.assertTrue(condition.getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        var assertions = new PlaywrightDriverAssertions(session).browser();
        assertions.pageSourceValue().contains("ready");
        assertions.windowHandleValue().isEqualTo("page-2");
        assertions.windowPositionValue().isEqualTo("(10, 20)");
        assertions.windowSizeValue().isEqualTo("(1200, 800)");

        verify(page, times(4)).waitForCondition(any(BooleanSupplier.class));
    }

    @Test
    public void focusedBrowserCoreAliasesShouldRouteToTheSameProviderValues() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("source-value");
        when(session.pageHandle(page)).thenReturn("handle-value");
        BrowserContext context = mock(BrowserContext.class);
        when(session.browserContext()).thenReturn(context);
        when(context.pages()).thenReturn(java.util.List.of(page, mock(Page.class)));
        when(page.evaluate(WINDOW_POSITION_SCRIPT)).thenReturn("(10, 20)");
        when(page.evaluate("() => `(${window.outerWidth}, ${window.outerHeight})`")).thenReturn("(1200, 800)");
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));
        var assertions = new PlaywrightDriverAssertions(session).browser();

        for (String alias : new String[]{"pagesource", "windowsource", "source"}) {
            assertions.attribute(alias).isEqualTo("source-value");
        }
        for (String alias : new String[]{"windowhandle", "pagehandle", "handle"}) {
            assertions.attribute(alias).isEqualTo("handle-value");
        }
        for (String alias : new String[]{"windowposition", "pageposition", "position"}) {
            assertions.attribute(alias).isEqualTo("(10, 20)");
        }
        for (String alias : new String[]{"windowsize", "pagesize", "size"}) {
            assertions.attribute(alias).isEqualTo("(1200, 800)");
        }
        for (String alias : new String[]{"browsingcontextcount", "windowcount", "pagecount"}) {
            assertions.attribute(alias).isEqualTo("2");
        }
    }

    @Test
    public void focusedBrowserCoreValuesShouldRetryUntilTheComparisonMatches() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>loading</html>", "<html>ready</html>");
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            Assert.assertFalse(condition.getAsBoolean());
            Assert.assertTrue(condition.getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        new PlaywrightDriverAssertions(session).browser().pageSourceValue().contains("ready");

        verify(page, times(2)).content();
    }

    @Test
    public void browsingContextCountShouldRetryAgainstTheCurrentContext() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.page()).thenReturn(page);
        when(session.browserContext()).thenReturn(context);
        Page remainingPage = mock(Page.class);
        when(context.pages()).thenReturn(java.util.List.of(page, remainingPage), java.util.List.of(remainingPage),
                java.util.List.of(remainingPage));
        AtomicInteger waits = new AtomicInteger();
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            if (waits.getAndIncrement() == 0) {
                Assert.assertFalse(condition.getAsBoolean());
            }
            Assert.assertTrue(condition.getAsBoolean());
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));

        var assertions = new PlaywrightDriverAssertions(session).browser();
        assertions.browsingContextCountValue().isEqualTo(1);
        assertions.browsingContextCountValue().isEqualTo("1");
        assertions.browsingContextCountValue().doesNotEqual("2");

        verify(context, times(4)).pages();
        verify(page, never()).waitForCondition(any(BooleanSupplier.class));
    }

    @Test
    public void focusedBrowserCoreValuesShouldNeverCompareProviderExceptionMessagesAsValues() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenThrow(new IllegalStateException("ready"));
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            condition.getAsBoolean();
            throw new IllegalStateException("poll ended");
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        Assert.expectThrows(AssertionError.class,
                () -> new PlaywrightDriverAssertions(session).browser().pageSourceValue().isEqualTo("ready"));
    }

    @Test
    public void focusedBrowserCoreValuesShouldHonorEveryComparisonModeAndPolarity() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>Ready</html>");
        doAnswer(invocation -> {
            boolean matched = invocation.<BooleanSupplier>getArgument(0).getAsBoolean();
            if (!matched) {
                throw new IllegalStateException("poll ended");
            }
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        var assertions = new PlaywrightDriverAssertions(session).browser();

        assertions.pageSourceValue().matchesRegex(".*Ready.*");
        assertions.pageSourceValue().equalsIgnoringCaseSensitivity("<HTML>READY</HTML>");
        assertions.pageSourceValue().doesNotContain("missing");
        Assert.expectThrows(AssertionError.class,
                () -> assertions.pageSourceValue().doesNotContain("Ready"));
    }

    @Test
    public void focusedBrowserCoreFailureShouldThrowOnlyForHardStarters() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        doThrow(new IllegalStateException("closed")).when(page)
                .waitForCondition(any(BooleanSupplier.class));

        Assert.expectThrows(AssertionError.class,
                () -> new PlaywrightDriverAssertions(session).browser().pageSourceValue().isEqualTo("ready"));
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

        new PlaywrightDriverVerifications(session).browser().pageSourceValue().isEqualTo("ready");
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void pageSourceValueShouldNeverPublishTheComparedPayload() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        String secret = "private-dom-token-7831";
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>" + secret + "</html>");
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        try (MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManager> report = org.mockito.Mockito.mockStatic(ReportManager.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            StepResult step = captureStepUpdates(lifecycle);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);

            for (String alias : new String[]{"pagesource", "windowsource", "source"}) {
                new PlaywrightDriverAssertions(session).browser().attribute(alias)
                        .isEqualTo("<html>" + secret + "</html>");
            }

            Assert.assertTrue(step.getParameters().stream()
                    .noneMatch(parameter -> String.valueOf(parameter.getValue()).contains(secret)),
                    "Leaking parameters: " + step.getParameters().stream()
                            .map(parameter -> parameter.getName() + "=" + parameter.getValue()).toList());
            report.verify(() -> ReportManager.logDiscrete(argThat(message -> message.contains(secret))), never());
            report.verify(() -> ReportManager.logDiscrete(
                    argThat(message -> message.contains(secret)), any(org.apache.logging.log4j.Level.class)), never());
            report.verify(() -> ReportManager.logDiscrete(
                    "Assert that the browser page source payload matches the requested comparison."), times(3));
        }
    }

    @Test
    public void failedPageSourceValueShouldKeepPayloadOutOfHardAndSoftFailureMessages() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        String secret = "failed-private-dom-token-6624";
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>actual-" + secret + "</html>");
        doAnswer(invocation -> {
            Assert.assertFalse(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        AssertionError hard = Assert.expectThrows(AssertionError.class,
                () -> new PlaywrightDriverAssertions(session).browser().pageSourceValue()
                        .isEqualTo("<html>expected-" + secret + "</html>"));
        Assert.assertFalse(String.valueOf(hard.getMessage()).contains(secret));

        new PlaywrightDriverVerifications(session).browser().pageSourceValue()
                .isEqualTo("<html>expected-" + secret + "</html>");
        AssertionError soft = ValidationsHelper.getVerificationErrorToForceFail();
        Assert.assertNotNull(soft);
        Assert.assertFalse(String.valueOf(soft.getMessage()).contains(secret));
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void everyPublicBrowserStarterShouldRouteFocusedCoreCategories() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.page()).thenReturn(page);
        when(session.browserContext()).thenReturn(context);
        when(context.pages()).thenReturn(java.util.List.of(page, mock(Page.class)));
        when(page.content()).thenReturn("<html>ready</html>");
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(context).waitForCondition(any(BooleanSupplier.class));

        new PlaywrightDriverAssertions(session).browser().pageSourceValue().contains("ready");
        new PlaywrightDriverVerifications(session).browser().pageSourceValue().contains("ready");
        var browser = new com.shaft.gui.playwright.browser.BrowserActions(session);
        browser.assertThat().pageSourceValue().contains("ready");
        browser.verifyThat().pageSourceValue().contains("ready");
        new PlaywrightDriverAssertions(session).browser().browsingContextCountValue().isEqualTo(2);
        new PlaywrightDriverVerifications(session).browser().browsingContextCountValue().isEqualTo(2);
        browser.assertThat().browsingContextCountValue().isEqualTo(2);
        browser.verifyThat().browsingContextCountValue().isEqualTo(2);

        verify(page, times(4)).content();
        verify(context, times(4)).pages();
    }

    @Test
    public void everyPublicBrowserStarterShouldPreserveHardAndSoftFailureSemantics() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>actual</html>");
        doAnswer(invocation -> {
            invocation.<BooleanSupplier>getArgument(0).getAsBoolean();
            throw new IllegalStateException("poll ended");
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        var browser = new com.shaft.gui.playwright.browser.BrowserActions(session);
        assertHardFailure(() -> new PlaywrightDriverAssertions(session).browser()
                .pageSourceValue().isEqualTo("expected"));
        assertSoftFailure(() -> new PlaywrightDriverVerifications(session).browser()
                .pageSourceValue().isEqualTo("expected"));
        assertHardFailure(() -> browser.assertThat().pageSourceValue().isEqualTo("expected"));
        assertSoftFailure(() -> browser.verifyThat().pageSourceValue().isEqualTo("expected"));
    }

    @Test
    public void failedPageSourceValidationShouldHonorNeverSnapshotPolicyForHardAndSoftStarters() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        String secret = "failed-private-dom-token-9157";
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        when(session.page()).thenReturn(page);
        when(page.content()).thenReturn("<html>" + secret + "</html>");
        doAnswer(invocation -> {
            Assert.assertFalse(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));

        AtomicReference<java.util.List<java.util.List<Object>>> captured = new AtomicReference<>();
        try (MockedStatic<ValidationsHelper> reporting = mockReporting(captured)) {
            new PlaywrightDriverAssertions(session).browser().pageSourceValue().isEqualTo("expected");
            assertAttachmentsExclude(captured.get(), secret);
            new PlaywrightDriverVerifications(session).browser().pageSourceValue().isEqualTo("expected");
            assertAttachmentsExclude(captured.get(), secret);
        }
        verify(page, times(2)).content();
    }

    @Test
    public void focusedBrowserEvidenceShouldRemainBoundToTheEvaluatedPage() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Always");
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page evaluatedPage = mock(Page.class);
        Page laterPage = mock(Page.class);
        when(session.page()).thenReturn(evaluatedPage, laterPage);
        when(evaluatedPage.content()).thenReturn("<html>ready</html>", "evaluated-page-evidence");
        when(laterPage.content()).thenReturn("unrelated-page-evidence");
        doAnswer(invocation -> {
            Assert.assertTrue(invocation.<BooleanSupplier>getArgument(0).getAsBoolean());
            return null;
        }).when(evaluatedPage).waitForCondition(any(BooleanSupplier.class));

        AtomicReference<java.util.List<java.util.List<Object>>> captured = new AtomicReference<>();
        try (MockedStatic<ValidationsHelper> reporting = mockReporting(captured)) {
            new PlaywrightDriverAssertions(session).browser().pageSourceValue().contains("ready");
        }

        Assert.assertTrue(captured.get().stream().anyMatch(attachment -> attachment.contains("evaluated-page-evidence")));
        Assert.assertTrue(captured.get().stream().noneMatch(attachment -> attachment.contains("unrelated-page-evidence")));
        verify(laterPage, never()).content();
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

    private static MockedStatic<ValidationsHelper> mockReporting(
            AtomicReference<java.util.List<java.util.List<Object>>> captured) {
        MockedStatic<ValidationsHelper> reporting = org.mockito.Mockito.mockStatic(ValidationsHelper.class);
        reporting.when(() -> ValidationsHelper.reportValidationState(
                        any(com.shaft.validation.ValidationEnums.ValidationCategory.class), anyBoolean(), any(), any(),
                        anyList(), anyLong(), anyBoolean()))
                .thenAnswer(invocation -> {
                    captured.set(invocation.getArgument(4));
                    return null;
                });
        return reporting;
    }

    private static void assertAttachmentsExclude(java.util.List<java.util.List<Object>> attachments, String secret) {
        Assert.assertTrue(attachments.stream().flatMap(java.util.Collection::stream)
                .noneMatch(value -> String.valueOf(value).contains(secret)), "Leaking attachments: " + attachments);
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
