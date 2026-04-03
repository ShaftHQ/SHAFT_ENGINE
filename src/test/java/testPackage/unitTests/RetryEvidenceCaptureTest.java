package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.properties.internal.Properties;
import org.aeonbits.owner.Config.DefaultValue;
import org.testng.Assert;
import org.testng.ITestResult;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the enhanced evidence capture on retry feature.
 * Verifies that the {@code forceCaptureSupportingEvidenceOnRetry} property
 * exists with the correct default, and that {@link RetryAnalyzer} enables
 * supporting evidence when a test is retried.
 */
@Test(singleThreaded = true)
public class RetryEvidenceCaptureTest {
    private boolean originalForceCaptureSupportingEvidenceOnRetry;
    private int originalRetryMaximumNumberOfAttempts;
    private boolean originalVideoParamsRecordVideo;
    private boolean originalCreateAnimatedGif;
    private boolean originalCaptureWebDriverLogs;
    private String originalScreenshotParamsWhenToTakeAScreenshot;
    private String originalWhenToTakePageSourceSnapshot;

    @BeforeClass(alwaysRun = true)
    public void beforeClass() {
        originalForceCaptureSupportingEvidenceOnRetry = SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry();
        originalRetryMaximumNumberOfAttempts = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        originalVideoParamsRecordVideo = SHAFT.Properties.visuals.videoParamsRecordVideo();
        originalCreateAnimatedGif = SHAFT.Properties.visuals.createAnimatedGif();
        originalCaptureWebDriverLogs = SHAFT.Properties.reporting.captureWebDriverLogs();
        originalScreenshotParamsWhenToTakeAScreenshot = SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot();
        originalWhenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(originalForceCaptureSupportingEvidenceOnRetry);
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetryMaximumNumberOfAttempts);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(originalVideoParamsRecordVideo);
        SHAFT.Properties.visuals.set().createAnimatedGif(originalCreateAnimatedGif);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(originalCaptureWebDriverLogs);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot(originalScreenshotParamsWhenToTakeAScreenshot);
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot(originalWhenToTakePageSourceSnapshot);
        Properties.clearForCurrentThread();
    }

    @Test(description = "forceCaptureSupportingEvidenceOnRetry can be enabled")
    public void testForceCaptureSupportingEvidenceOnRetryCanBeEnabled() {
        synchronized (Properties.class) {
            SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
            Assert.assertTrue(SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry(),
                    "forceCaptureSupportingEvidenceOnRetry should be true after setting it to true");
        }
    }

    @Test(description = "forceCaptureSupportingEvidenceOnRetry default contract should remain true")
    public void testForceCaptureSupportingEvidenceOnRetryDefaultContract() throws NoSuchMethodException {
        DefaultValue defaultValue = com.shaft.properties.internal.Flags.class
                .getMethod("forceCaptureSupportingEvidenceOnRetry")
                .getAnnotation(DefaultValue.class);
        Assert.assertNotNull(defaultValue, "forceCaptureSupportingEvidenceOnRetry should declare @DefaultValue");
        Assert.assertEquals(defaultValue.value(), "true",
                "forceCaptureSupportingEvidenceOnRetry should default to true by contract");
    }

    @Test(description = "forceCaptureSupportingEvidenceOnRetry setter should work correctly")
    public void testForceCaptureSupportingEvidenceOnRetrySetter() {
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(false);
        Assert.assertFalse(SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry(),
                "forceCaptureSupportingEvidenceOnRetry should be false after setting to false");
    }

    @Test(description = "RetryAnalyzer should enable video recording on retry when flag is true")
    public void testRetryEnablesVideoRecording() {
        // Ensure retries are enabled and evidence capture flag is on
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
        // Ensure video recording starts as disabled
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);

        ITestResult mockResult = mock(ITestResult.class);
        var mockMethod = mock(org.testng.ITestNGMethod.class);
        when(mockResult.getMethod()).thenReturn(mockMethod);
        when(mockMethod.getMethodName()).thenReturn("testMethod");

        RetryAnalyzer analyzer = new RetryAnalyzer();
        boolean shouldRetry = analyzer.retry(mockResult);

        Assert.assertTrue(shouldRetry, "RetryAnalyzer should allow retry");
        Assert.assertTrue(SHAFT.Properties.visuals.videoParamsRecordVideo(),
                "Video recording should be enabled after retry");
    }

    @Test(description = "RetryAnalyzer should enable animated GIF on retry when flag is true")
    public void testRetryEnablesAnimatedGif() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        ITestResult mockResult = mock(ITestResult.class);
        var mockMethod = mock(org.testng.ITestNGMethod.class);
        when(mockResult.getMethod()).thenReturn(mockMethod);
        when(mockMethod.getMethodName()).thenReturn("testMethod");

        RetryAnalyzer analyzer = new RetryAnalyzer();
        analyzer.retry(mockResult);

        Assert.assertTrue(SHAFT.Properties.visuals.createAnimatedGif(),
                "Animated GIF creation should be enabled after retry");
    }

    @Test(description = "RetryAnalyzer should enable WebDriver log capture on retry when flag is true")
    public void testRetryEnablesWebDriverLogs() {
        synchronized (Properties.class) {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
            SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
            SHAFT.Properties.reporting.set().captureWebDriverLogs(false);

            ITestResult mockResult = mock(ITestResult.class);
            var mockMethod = mock(org.testng.ITestNGMethod.class);
            when(mockResult.getMethod()).thenReturn(mockMethod);
            when(mockMethod.getMethodName()).thenReturn("testMethod");

            RetryAnalyzer analyzer = new RetryAnalyzer();
            analyzer.retry(mockResult);

            Assert.assertTrue(SHAFT.Properties.reporting.captureWebDriverLogs(),
                    "WebDriver log capture should be enabled after retry");
        }
    }

    @Test(description = "RetryAnalyzer should set screenshots to Always on retry when flag is true")
    public void testRetryEnablesAlwaysScreenshots() {
        synchronized (Properties.class) {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
            SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);

            ITestResult mockResult = mock(ITestResult.class);
            var mockMethod = mock(org.testng.ITestNGMethod.class);
            when(mockResult.getMethod()).thenReturn(mockMethod);
            when(mockMethod.getMethodName()).thenReturn("testMethod");

            RetryAnalyzer analyzer = new RetryAnalyzer();
            analyzer.retry(mockResult);

            Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot(), "Always",
                    "Screenshots should be set to Always after retry");
            Assert.assertEquals(SHAFT.Properties.visuals.whenToTakePageSourceSnapshot(), "FailuresOnly",
                    "Page source snapshot should be set to FailuresOnly after retry");
        }
    }

    @Test(description = "RetryAnalyzer should NOT enable evidence capture when flag is false")
    public void testRetryDoesNotEnableEvidenceWhenFlagDisabled() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(false);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        ITestResult mockResult = mock(ITestResult.class);
        var mockMethod = mock(org.testng.ITestNGMethod.class);
        when(mockResult.getMethod()).thenReturn(mockMethod);
        when(mockMethod.getMethodName()).thenReturn("testMethod");

        RetryAnalyzer analyzer = new RetryAnalyzer();
        analyzer.retry(mockResult);

        Assert.assertFalse(SHAFT.Properties.visuals.videoParamsRecordVideo(),
                "Video recording should remain disabled when evidence capture flag is off");
        Assert.assertFalse(SHAFT.Properties.visuals.createAnimatedGif(),
                "Animated GIF should remain disabled when evidence capture flag is off");
    }

    @Test(description = "RetryAnalyzer should not retry when max attempts exceeded")
    public void testRetryAnalyzerRespectsMaxAttempts() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        ITestResult mockResult = mock(ITestResult.class);
        var mockMethod = mock(org.testng.ITestNGMethod.class);
        when(mockResult.getMethod()).thenReturn(mockMethod);
        when(mockMethod.getMethodName()).thenReturn("testMethod");

        RetryAnalyzer analyzer = new RetryAnalyzer();
        boolean firstRetry = analyzer.retry(mockResult);

        // Reset evidence settings to verify second call doesn't change them
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        boolean secondRetry = analyzer.retry(mockResult);

        Assert.assertTrue(firstRetry, "First retry should be allowed");
        Assert.assertFalse(secondRetry, "Second retry should not be allowed when max is 1");
        Assert.assertFalse(SHAFT.Properties.visuals.videoParamsRecordVideo(),
                "Video recording should not be enabled when retry is rejected");
        Assert.assertFalse(SHAFT.Properties.visuals.createAnimatedGif(),
                "Animated GIF should not be enabled when retry is rejected");
    }
}
