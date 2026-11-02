package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;

/**
 * Shares per-thread execution failure context between runner adapters and Allure lifecycle hooks.
 */
public final class ExecutionFailureContext {
    private static final ThreadLocal<PendingConfigFailure> pendingConfigFailure = new ThreadLocal<>();

    private ExecutionFailureContext() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Stores the current thread's setup/teardown failure.
     *
     * @param throwable failure to store, or {@code null} to clear
     */
    public static void setPendingConfigFailure(Throwable throwable) {
        if (throwable == null) {
            pendingConfigFailure.remove();
        } else {
            pendingConfigFailure.set(new PendingConfigFailure(throwable,
                    FailureTraceReporter.redactInvocationText(throwable, throwable.getMessage()),
                    FailureTraceReporter.redactInvocationText(throwable,
                            ReportManagerHelper.formatStackTraceToLogEntry(throwable))));
        }
    }

    /**
     * Returns and clears the current thread's pending setup/teardown failure.
     *
     * @return pending failure, or {@code null}
     */
    public static Throwable getAndClearPendingConfigFailure() {
        PendingConfigFailure failure = getAndClearPendingConfigFailureEvidence();
        return failure == null ? null : failure.throwable();
    }

    /** Returns and clears the original failure plus text sanitized at configuration-failure time. */
    public static PendingConfigFailure getAndClearPendingConfigFailureEvidence() {
        PendingConfigFailure failure = pendingConfigFailure.get();
        pendingConfigFailure.remove();
        return failure;
    }

    /** Immutable configuration failure evidence safe for later runner/report lifecycle phases. */
    public record PendingConfigFailure(Throwable throwable, String message, String trace) { }
}
