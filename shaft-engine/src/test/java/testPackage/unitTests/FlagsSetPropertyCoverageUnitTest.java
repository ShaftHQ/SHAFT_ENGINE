package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Flags;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test(singleThreaded = true)
public class FlagsSetPropertyCoverageUnitTest {
    private boolean originalAutomaticallyAddRecommendedChromeOptions;
    private int originalRetryMaximumNumberOfAttempts;
    private boolean originalForceCaptureSupportingEvidenceOnRetry;
    private boolean originalAutoMaximizeBrowserWindow;
    private boolean originalForceCheckForElementVisibility;
    private boolean originalForceCheckElementLocatorIsUnique;
    private boolean originalForceCheckTextWasTypedCorrectly;
    private String originalClearBeforeTypingMode;
    private String originalScrollingMode;
    private boolean originalForceCheckNavigationWasSuccessful;
    private boolean originalForceCheckStatusOfRemoteServer;
    private boolean originalRespectBuiltInWaitsInNativeMode;
    private boolean originalClickUsingJavascriptWhenWebDriverClickFails;
    private boolean originalAttemptToClickBeforeTyping;
    private boolean originalAutoCloseDriverInstance;
    private boolean originalAutomaticallyAssertResponseStatusCode;
    private int originalMaximumPerformanceMode;
    private boolean originalSkipTestsWithLinkedIssues;
    private boolean originalDisableCache;
    private boolean originalEnableTrueNativeMode;
    private boolean originalHandleNonSelectDropDown;
    private boolean originalValidateSwipeToElement;
    private boolean originalDisableSslCertificateCheck;
    private boolean originalTelemetryEnabled;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        originalAutomaticallyAddRecommendedChromeOptions = SHAFT.Properties.flags.automaticallyAddRecommendedChromeOptions();
        originalRetryMaximumNumberOfAttempts = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        originalForceCaptureSupportingEvidenceOnRetry = SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry();
        originalAutoMaximizeBrowserWindow = SHAFT.Properties.flags.autoMaximizeBrowserWindow();
        originalForceCheckForElementVisibility = SHAFT.Properties.flags.forceCheckForElementVisibility();
        originalForceCheckElementLocatorIsUnique = SHAFT.Properties.flags.forceCheckElementLocatorIsUnique();
        originalForceCheckTextWasTypedCorrectly = SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly();
        originalClearBeforeTypingMode = SHAFT.Properties.flags.clearBeforeTypingMode();
        originalScrollingMode = SHAFT.Properties.flags.scrollingMode();
        originalForceCheckNavigationWasSuccessful = SHAFT.Properties.flags.forceCheckNavigationWasSuccessful();
        originalForceCheckStatusOfRemoteServer = SHAFT.Properties.flags.forceCheckStatusOfRemoteServer();
        originalRespectBuiltInWaitsInNativeMode = SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode();
        originalClickUsingJavascriptWhenWebDriverClickFails = SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails();
        originalAttemptToClickBeforeTyping = SHAFT.Properties.flags.attemptToClickBeforeTyping();
        originalAutoCloseDriverInstance = SHAFT.Properties.flags.autoCloseDriverInstance();
        originalAutomaticallyAssertResponseStatusCode = SHAFT.Properties.flags.automaticallyAssertResponseStatusCode();
        originalMaximumPerformanceMode = SHAFT.Properties.flags.maximumPerformanceMode();
        originalSkipTestsWithLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();
        originalDisableCache = SHAFT.Properties.flags.disableCache();
        originalEnableTrueNativeMode = SHAFT.Properties.flags.enableTrueNativeMode();
        originalHandleNonSelectDropDown = SHAFT.Properties.flags.handleNonSelectDropDown();
        originalValidateSwipeToElement = SHAFT.Properties.flags.validateSwipeToElement();
        originalDisableSslCertificateCheck = SHAFT.Properties.flags.disableSslCertificateCheck();
        originalTelemetryEnabled = SHAFT.Properties.flags.telemetryEnabled();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.flags.set()
                .automaticallyAddRecommendedChromeOptions(originalAutomaticallyAddRecommendedChromeOptions)
                .retryMaximumNumberOfAttempts(originalRetryMaximumNumberOfAttempts)
                .forceCaptureSupportingEvidenceOnRetry(originalForceCaptureSupportingEvidenceOnRetry)
                .autoMaximizeBrowserWindow(originalAutoMaximizeBrowserWindow)
                .forceCheckForElementVisibility(originalForceCheckForElementVisibility)
                .forceCheckElementLocatorIsUnique(originalForceCheckElementLocatorIsUnique)
                .forceCheckTextWasTypedCorrectly(originalForceCheckTextWasTypedCorrectly)
                .clearBeforeTypingMode(originalClearBeforeTypingMode)
                .scrollingMode(originalScrollingMode)
                .forceCheckNavigationWasSuccessful(originalForceCheckNavigationWasSuccessful)
                .forceCheckStatusOfRemoteServer(originalForceCheckStatusOfRemoteServer)
                .respectBuiltInWaitsInNativeMode(originalRespectBuiltInWaitsInNativeMode)
                .clickUsingJavascriptWhenWebDriverClickFails(originalClickUsingJavascriptWhenWebDriverClickFails)
                .attemptToClickBeforeTyping(originalAttemptToClickBeforeTyping)
                .autoCloseDriverInstance(originalAutoCloseDriverInstance)
                .automaticallyAssertResponseStatusCode(originalAutomaticallyAssertResponseStatusCode)
                .maximumPerformanceMode(originalMaximumPerformanceMode)
                .skipTestsWithLinkedIssues(originalSkipTestsWithLinkedIssues)
                .disableCache(originalDisableCache)
                .enableTrueNativeMode(originalEnableTrueNativeMode)
                .handleNonSelectDropDown(originalHandleNonSelectDropDown)
                .validateSwipeToElement(originalValidateSwipeToElement)
                .disableSslCertificateCheck(originalDisableSslCertificateCheck)
                .telemetryEnabled(originalTelemetryEnabled);
    }

    @Test(description = "Coverage: exercise all Flags.SetProperty fluent setters and verify effective values")
    public void testFluentSettersModifyFlagsAndReturnSameInstance() {
        String targetClearBeforeTypingMode = "native".equalsIgnoreCase(originalClearBeforeTypingMode) ? "backspace" : "native";
        String targetScrollingMode = "javascript".equalsIgnoreCase(originalScrollingMode) ? "actions" : "javascript";

        Flags.SetProperty setProperty = SHAFT.Properties.flags.set();
        Assert.assertSame(setProperty.automaticallyAddRecommendedChromeOptions(!originalAutomaticallyAddRecommendedChromeOptions), setProperty);
        Assert.assertSame(setProperty.retryMaximumNumberOfAttempts(originalRetryMaximumNumberOfAttempts + 1), setProperty);
        Assert.assertSame(setProperty.forceCaptureSupportingEvidenceOnRetry(!originalForceCaptureSupportingEvidenceOnRetry), setProperty);
        Assert.assertSame(setProperty.autoMaximizeBrowserWindow(!originalAutoMaximizeBrowserWindow), setProperty);
        Assert.assertSame(setProperty.forceCheckForElementVisibility(!originalForceCheckForElementVisibility), setProperty);
        Assert.assertSame(setProperty.forceCheckElementLocatorIsUnique(!originalForceCheckElementLocatorIsUnique), setProperty);
        Assert.assertSame(setProperty.forceCheckTextWasTypedCorrectly(!originalForceCheckTextWasTypedCorrectly), setProperty);
        Assert.assertSame(setProperty.clearBeforeTypingMode(targetClearBeforeTypingMode), setProperty);
        Assert.assertSame(setProperty.scrollingMode(targetScrollingMode), setProperty);
        Assert.assertSame(setProperty.forceCheckNavigationWasSuccessful(!originalForceCheckNavigationWasSuccessful), setProperty);
        Assert.assertSame(setProperty.forceCheckStatusOfRemoteServer(!originalForceCheckStatusOfRemoteServer), setProperty);
        Assert.assertSame(setProperty.respectBuiltInWaitsInNativeMode(!originalRespectBuiltInWaitsInNativeMode), setProperty);
        Assert.assertSame(setProperty.clickUsingJavascriptWhenWebDriverClickFails(!originalClickUsingJavascriptWhenWebDriverClickFails), setProperty);
        Assert.assertSame(setProperty.attemptToClickBeforeTyping(!originalAttemptToClickBeforeTyping), setProperty);
        Assert.assertSame(setProperty.autoCloseDriverInstance(!originalAutoCloseDriverInstance), setProperty);
        Assert.assertSame(setProperty.automaticallyAssertResponseStatusCode(!originalAutomaticallyAssertResponseStatusCode), setProperty);
        Assert.assertSame(setProperty.maximumPerformanceMode(originalMaximumPerformanceMode == 0 ? 1 : 0), setProperty);
        Assert.assertSame(setProperty.skipTestsWithLinkedIssues(!originalSkipTestsWithLinkedIssues), setProperty);
        Assert.assertSame(setProperty.disableCache(!originalDisableCache), setProperty);
        Assert.assertSame(setProperty.enableTrueNativeMode(!originalEnableTrueNativeMode), setProperty);
        Assert.assertSame(setProperty.handleNonSelectDropDown(!originalHandleNonSelectDropDown), setProperty);
        Assert.assertSame(setProperty.validateSwipeToElement(!originalValidateSwipeToElement), setProperty);
        Assert.assertSame(setProperty.disableSslCertificateCheck(!originalDisableSslCertificateCheck), setProperty);
        Assert.assertSame(setProperty.telemetryEnabled(!originalTelemetryEnabled), setProperty);

        Assert.assertEquals(SHAFT.Properties.flags.automaticallyAddRecommendedChromeOptions(), !originalAutomaticallyAddRecommendedChromeOptions);
        Assert.assertEquals(SHAFT.Properties.flags.retryMaximumNumberOfAttempts(), originalRetryMaximumNumberOfAttempts + 1);
        Assert.assertEquals(SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry(), !originalForceCaptureSupportingEvidenceOnRetry);
        Assert.assertEquals(SHAFT.Properties.flags.autoMaximizeBrowserWindow(), !originalAutoMaximizeBrowserWindow);
        Assert.assertEquals(SHAFT.Properties.flags.forceCheckForElementVisibility(), !originalForceCheckForElementVisibility);
        Assert.assertEquals(SHAFT.Properties.flags.forceCheckElementLocatorIsUnique(), !originalForceCheckElementLocatorIsUnique);
        Assert.assertEquals(SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly(), !originalForceCheckTextWasTypedCorrectly);
        Assert.assertEquals(SHAFT.Properties.flags.clearBeforeTypingMode(), targetClearBeforeTypingMode);
        Assert.assertEquals(SHAFT.Properties.flags.scrollingMode(), targetScrollingMode);
        Assert.assertEquals(SHAFT.Properties.flags.forceCheckNavigationWasSuccessful(), !originalForceCheckNavigationWasSuccessful);
        Assert.assertEquals(SHAFT.Properties.flags.forceCheckStatusOfRemoteServer(), !originalForceCheckStatusOfRemoteServer);
        Assert.assertEquals(SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode(), !originalRespectBuiltInWaitsInNativeMode);
        Assert.assertEquals(SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails(), !originalClickUsingJavascriptWhenWebDriverClickFails);
        Assert.assertEquals(SHAFT.Properties.flags.attemptToClickBeforeTyping(), !originalAttemptToClickBeforeTyping);
        Assert.assertEquals(SHAFT.Properties.flags.autoCloseDriverInstance(), !originalAutoCloseDriverInstance);
        Assert.assertEquals(SHAFT.Properties.flags.automaticallyAssertResponseStatusCode(), !originalAutomaticallyAssertResponseStatusCode);
        Assert.assertEquals(SHAFT.Properties.flags.maximumPerformanceMode(), originalMaximumPerformanceMode == 0 ? 1 : 0);
        Assert.assertEquals(SHAFT.Properties.flags.skipTestsWithLinkedIssues(), !originalSkipTestsWithLinkedIssues);
        Assert.assertEquals(SHAFT.Properties.flags.disableCache(), !originalDisableCache);
        Assert.assertEquals(SHAFT.Properties.flags.enableTrueNativeMode(), !originalEnableTrueNativeMode);
        Assert.assertEquals(SHAFT.Properties.flags.handleNonSelectDropDown(), !originalHandleNonSelectDropDown);
        Assert.assertEquals(SHAFT.Properties.flags.validateSwipeToElement(), !originalValidateSwipeToElement);
        Assert.assertEquals(SHAFT.Properties.flags.disableSslCertificateCheck(), !originalDisableSslCertificateCheck);
        Assert.assertEquals(SHAFT.Properties.flags.telemetryEnabled(), !originalTelemetryEnabled);
    }
}
