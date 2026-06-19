package com.shaft.listeners.internal;

/**
 * Shares per-thread execution failure context between runner adapters and Allure lifecycle hooks.
 */
public final class ExecutionFailureContext {
    private static final ThreadLocal<Throwable> pendingConfigFailure = new ThreadLocal<>();

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
            pendingConfigFailure.set(throwable);
        }
    }

    /**
     * Returns and clears the current thread's pending setup/teardown failure.
     *
     * @return pending failure, or {@code null}
     */
    public static Throwable getAndClearPendingConfigFailure() {
        Throwable throwable = pendingConfigFailure.get();
        pendingConfigFailure.remove();
        return throwable;
    }
}
