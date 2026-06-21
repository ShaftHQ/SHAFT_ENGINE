package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
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
    private static final ThreadLocal<SupportingEvidenceState> supportingEvidenceState = new ThreadLocal<>();
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
                ReportManagerHelper.enableDebugFileLogging();
                ReportManager.logDiscrete(message);
                enableSupportingEvidenceCaptureForRetryAttempt();
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
     * and captures page source on failures.
     * These thread-local property overrides ensure that the retried test run
     * produces richer diagnostic artifacts without affecting other threads.
     */
    public static void enableSupportingEvidenceCaptureForRetryAttempt() {
        try {
            if (SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry()) {
                ReportManager.logDiscrete("Enabling enhanced evidence capture for the retry attempt.");
                supportingEvidenceState.set(SupportingEvidenceState.current());
                SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
                SHAFT.Properties.visuals.set().createAnimatedGif(true);
                SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("FailuresOnly");
                SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
                if (SHAFT.Properties.playwright.tracingOnRetryOnly()) {
                    SHAFT.Properties.playwright.set().tracingEnabled(true);
                }
            }
        } catch (Exception e) {
            // Evidence capture is best-effort; don't let it prevent the retry.
            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
        }
    }

    /**
     * Marks retry evidence capture as active for the test invocation that is about to run.
     */
    public static void activateSupportingEvidenceCaptureForRetryAttempt() {
        SupportingEvidenceState state = supportingEvidenceState.get();
        if (state != null) {
            state.active = true;
        }
    }

    /**
     * Restores visual and reporting properties after a retry attempt finishes.
     */
    public static void restoreSupportingEvidenceCaptureForRetryAttempt() {
        SupportingEvidenceState state = supportingEvidenceState.get();
        if (state != null && state.active) {
            try {
                SHAFT.Properties.visuals.set().videoParamsRecordVideo(state.videoParamsRecordVideo);
                SHAFT.Properties.visuals.set().createAnimatedGif(state.createAnimatedGif);
                SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot(state.whenToTakePageSourceSnapshot);
                SHAFT.Properties.reporting.set().captureWebDriverLogs(state.captureWebDriverLogs);
                SHAFT.Properties.playwright.set().tracingEnabled(state.playwrightTracingEnabled);
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e, Level.DEBUG);
            } finally {
                supportingEvidenceState.remove();
            }
        }
    }

    private static class SupportingEvidenceState {
        private final boolean videoParamsRecordVideo;
        private final boolean createAnimatedGif;
        private final String whenToTakePageSourceSnapshot;
        private final boolean captureWebDriverLogs;
        private final boolean playwrightTracingEnabled;
        private boolean active;

        private SupportingEvidenceState(boolean videoParamsRecordVideo, boolean createAnimatedGif,
                                        String whenToTakePageSourceSnapshot, boolean captureWebDriverLogs,
                                        boolean playwrightTracingEnabled) {
            this.videoParamsRecordVideo = videoParamsRecordVideo;
            this.createAnimatedGif = createAnimatedGif;
            this.whenToTakePageSourceSnapshot = whenToTakePageSourceSnapshot;
            this.captureWebDriverLogs = captureWebDriverLogs;
            this.playwrightTracingEnabled = playwrightTracingEnabled;
        }

        private static SupportingEvidenceState current() {
            return new SupportingEvidenceState(
                    SHAFT.Properties.visuals.videoParamsRecordVideo(),
                    SHAFT.Properties.visuals.createAnimatedGif(),
                    SHAFT.Properties.visuals.whenToTakePageSourceSnapshot(),
                    SHAFT.Properties.reporting.captureWebDriverLogs(),
                    SHAFT.Properties.playwright.tracingEnabled());
        }
    }
}
