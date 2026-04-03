package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

/**
 * TestNG retry analyzer that re-runs failed tests up to a configurable maximum
 * number of attempts.
 *
 * <p>The maximum retry count is read from the engine-wide flags configuration on
 * every {@link #retry} call. Configure the retry count via
 * {@code SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(n)} or the
 * {@code retryMaximumNumberOfAttempts} JVM system property (e.g. {@code -DretryMaximumNumberOfAttempts=2}).
 * When set before the JVM starts, SHAFT's {@code Flags} config reads it automatically
 * via its {@code @Sources("system:properties")} source during initialization.
 *
 * <p>Each {@code RetryAnalyzer} instance maintains its own invocation counter,
 * which is correct because TestNG creates one instance per test method (or per
 * parameter combination for data-driven tests).
 */
public class RetryAnalyzer implements IRetryAnalyzer {
    private static final Logger logger = LogManager.getLogger(RetryAnalyzer.class);
    private int counter = 0;

    @Override
    public boolean retry(ITestResult iTestResult) {
        try {
            int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
            if (counter < maxRetryCount) {
                counter++;
                String message = "Retry #" + counter + "/" + maxRetryCount
                        + " for test: " + iTestResult.getMethod().getMethodName()
                        + ", on thread: " + Thread.currentThread().getName();
                // Log to both structured logger (always visible in CI) and SHAFT's discrete log
                logger.info("[SHAFT] {}", message);
                ReportManager.logDiscrete(message);
                enableSupportingEvidenceCapture();
                return true;
            }
        } catch (Exception e) {
            // Never let an exception from the retry decision prevent TestNG from proceeding.
            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
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
        try {
            if (SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry()) {
                ReportManager.logDiscrete("Enabling enhanced evidence capture for retry attempt...");
                SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
                SHAFT.Properties.visuals.set().createAnimatedGif(true);
                SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
                SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("FailuresOnly");
                SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
            }
        } catch (Exception e) {
            // Evidence capture is best-effort; don't let it prevent the retry.
            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
        }
    }
}
