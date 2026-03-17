package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

/**
 * TestNG retry analyzer that re-runs failed tests up to a configurable maximum
 * number of attempts.
 *
 * <p>The maximum retry count is read <b>lazily</b> on every {@link #retry} call from
 * {@link SHAFT.Properties#flags}{@code .retryMaximumNumberOfAttempts()} so that
 * property changes made at any point before the first failure are always honoured.
 *
 * <p>Each {@code RetryAnalyzer} instance maintains its own invocation counter,
 * which is correct because TestNG creates one instance per test method (or per
 * parameter combination for data-driven tests).
 */
public class RetryAnalyzer implements IRetryAnalyzer {
    private int counter = 0;

    @Override
    public boolean retry(ITestResult iTestResult) {
        int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        if (counter < maxRetryCount) {
            counter++;
            ReportManager.logDiscrete("Retry #" + counter + "/" + maxRetryCount
                    + " for test: " + iTestResult.getMethod().getMethodName()
                    + ", on thread: " + Thread.currentThread().getName());
            enableSupportingEvidenceCapture();
            return true;
        }
        return false;
    }

    /**
     * Enables enhanced evidence capture for the upcoming retry attempt.
     * When {@code forceCaptureSupportingEvidenceOnRetry} is {@code true}, this method
     * activates video recording, animated GIF creation, WebDriver log capture,
     * takes screenshots on every action, and captures page source on failures.
     * These thread-local property overrides ensure that the retried test run
     * produces richer diagnostic artifacts without affecting other threads.
     */
    private void enableSupportingEvidenceCapture() {
        if (SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry()) {
            ReportManager.logDiscrete("Enabling enhanced evidence capture for retry attempt...");
            SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
            SHAFT.Properties.visuals.set().createAnimatedGif(true);
            SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
            SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("FailuresOnly");
            SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
        }
    }
}