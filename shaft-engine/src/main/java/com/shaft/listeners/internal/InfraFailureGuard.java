package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.SessionNotCreatedException;

import java.net.ConnectException;
import java.net.UnknownHostException;
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Classifies infrastructure setup failures (no driver session, device offline, DNS, Appium down) so
 * they are not retried like product failures, and stops the run's remaining tests after
 * {@code infraSetupFailureLimit} (default 2) infrastructure configuration failures in a row (#7292).
 */
public final class InfraFailureGuard {
    /** System property holding the consecutive infrastructure setup failure limit; 0 disables the stop. */
    public static final String LIMIT_PROPERTY = "infraSetupFailureLimit";
    private static final AtomicInteger STREAK = new AtomicInteger();
    private static final String[] TRANSIENT_HINTS = {"timeout", "timed out", "queue", "capacity", "quota",
        "exhausted", "max session", "parallel", "try again", "temporarily", "rate limit", "too many"};

    private InfraFailureGuard() {
    }

    /**
     * Tells whether a failure comes from the test infrastructure rather than the product under test.
     *
     * @param throwable the failure
     * @return {@code true} for driver-session, DNS, connection, device-offline and preflight failures
     */
    public static boolean isInfra(Throwable throwable) {
        int hops = 0;
        for (Throwable t = throwable; t != null && hops < 20; t = t.getCause(), hops++) {
            String message = String.valueOf(t.getMessage()).toLowerCase(Locale.ROOT);
            if (t instanceof SessionNotCreatedException) {
                // a cloud grid's queue timeout or capacity limit is transient: keep retrying those
                return message.contains("preflight:") || !isTransient(message);
            }
            if (t instanceof UnknownHostException || t instanceof ConnectException) {
                return true;
            }
            if (message.contains("device offline") || message.startsWith("preflight:")
                    || message.contains(" preflight: ")) {
                return true;
            }
        }
        return false;
    }

    /**
     * Tells whether a session-creation message names a transient cloud-grid condition.
     *
     * @param message the lower-case failure message
     * @return {@code true} for timeouts, queue waits, capacity, quota and parallel-session limits
     */
    static boolean isTransient(String message) {
        for (String hint : TRANSIENT_HINTS) {
            if (message.contains(hint)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Records a failed configuration method.
     *
     * @param throwable its failure
     */
    public static void onConfigurationFailure(Throwable throwable) {
        if (isInfra(throwable)) {
            int streak = STREAK.incrementAndGet();
            ReportManagerHelper.logDiscrete("Infrastructure setup failure " + streak + " in a row: "
                    + (throwable == null ? "" : throwable.getClass().getSimpleName()), Level.WARN);
        }
    }

    /** Records a successful configuration method, which ends the streak. */
    public static void onConfigurationSuccess() {
        STREAK.set(0);
    }

    /**
     * Returns the reason to skip the next test, or empty when tests may run.
     *
     * @return the skip reason when the infrastructure failure limit is reached
     */
    public static Optional<String> stopReason() {
        int limit = limit();
        if (limit > 0 && STREAK.get() >= limit) {
            return Optional.of("Stopped: " + STREAK.get() + " infrastructure setup failures in a row (limit "
                    + LIMIT_PROPERTY + "=" + limit + "). Fix the device, Appium or network, then rerun.");
        }
        return Optional.empty();
    }

    static int limit() {
        return Math.max(0, Integer.getInteger(LIMIT_PROPERTY, 2));
    }

    /** Resets the streak; for tests. */
    public static void reset() {
        STREAK.set(0);
    }
}
