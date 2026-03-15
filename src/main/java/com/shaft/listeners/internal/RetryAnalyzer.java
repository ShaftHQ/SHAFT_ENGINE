package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

/**
 * TestNG retry analyzer that re-runs failed tests up to a configurable maximum
 * number of attempts.
 *
 * <p>The maximum retry count is read <b>lazily</b> on every {@link #retry} call from
 * {@link SHAFT.Properties#flags}{@code .retryMaximumNumberOfAttempts()} so that
 * property changes made at any point before the first failure are always honoured.
 * As a safety net, if the SHAFT property system returns 0 the analyzer also checks
 * the raw {@code retryMaximumNumberOfAttempts} system property directly.
 *
 * <p>Each {@code RetryAnalyzer} instance maintains its own invocation counter,
 * which is correct because TestNG creates one instance per test method (or per
 * parameter combination for data-driven tests).
 */
public class RetryAnalyzer implements IRetryAnalyzer {
    private int counter = 0;

    @Override
    public boolean retry(ITestResult iTestResult) {
        try {
            int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
            if (maxRetryCount == 0) {
                // Double-check the raw system property in case the SHAFT property
                // system did not pick it up (e.g. timing/initialization issue).
                // Only override when no explicit thread-local value was set.
                int sysPropValue = readSystemProperty();
                if (sysPropValue > 0 && !hasExplicitOverride()) {
                    maxRetryCount = sysPropValue;
                }
            }
            if (counter < maxRetryCount) {
                counter++;
                String message = "Retry #" + counter + "/" + maxRetryCount
                        + " for test: " + iTestResult.getMethod().getMethodName()
                        + ", on thread: " + Thread.currentThread().getName();
                // Log to both console (always visible in CI) and SHAFT's discrete log
                System.out.println("[SHAFT] " + message);
                ReportManager.logDiscrete(message);
                enableSupportingEvidenceCapture();
                return true;
            }
        } catch (Exception e) {
            // Never let an exception from the retry decision prevent TestNG from proceeding.
            // Log full throwable for diagnostics, then fall back to system property.
            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
            int fallback = readSystemProperty();
            if (counter < fallback) {
                counter++;
                String message = "Retry #" + counter + "/" + fallback
                        + " for test: " + iTestResult.getMethod().getMethodName()
                        + ", on thread: " + Thread.currentThread().getName()
                        + " (fallback mode)";
                System.out.println("[SHAFT] " + message);
                ReportManager.logDiscrete(message);
                enableSupportingEvidenceCapture();
                return true;
            }
        }
        return false;
    }

    /**
     * Returns {@code true} when a thread-local override has been explicitly set for
     * the {@code retryMaximumNumberOfAttempts} property via
     * {@code SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(...)}.
     * In that case we must respect the overridden value even if it is 0.
     */
    private static boolean hasExplicitOverride() {
        try {
            String threadLocalValue = ThreadLocalPropertiesManager.getOverrides()
                    .getProperty("retryMaximumNumberOfAttempts");
            return threadLocalValue != null;
        } catch (Exception e) {
            // If we can't determine override status, assume no override so the
            // system property fallback can still take effect.
            return false;
        }
    }

    /**
     * Reads {@code retryMaximumNumberOfAttempts} directly from {@link System#getProperty}.
     *
     * @return the parsed value, or 0 if absent/unparsable
     */
    private static int readSystemProperty() {
        String value = System.getProperty("retryMaximumNumberOfAttempts");
        if (value != null && !value.isBlank()) {
            try {
                return Integer.parseInt(value.trim());
            } catch (NumberFormatException ignored) {
                // fall through
            }
        }
        return 0;
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