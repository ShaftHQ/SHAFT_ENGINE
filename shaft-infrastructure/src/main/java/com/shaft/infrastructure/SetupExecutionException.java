package com.shaft.infrastructure;

import java.util.Objects;

/** Failure with an immutable receipt for every action completed before the failed action. */
public final class SetupExecutionException extends RuntimeException {
    private final SetupAction failedAction;
    private final SetupReceipt partialReceipt;

    /**
     * Creates an exception for a failed setup action, keeping what was already installed.
     */
    public SetupExecutionException(SetupAction failedAction, SetupReceipt partialReceipt, RuntimeException cause) {
        super("Setup action failed: " + Objects.requireNonNull(failedAction, "failedAction"), cause);
        this.failedAction = failedAction;
        this.partialReceipt = Objects.requireNonNull(partialReceipt, "partialReceipt");
    }

    /**
     * Returns the action that failed.
     */
    public SetupAction failedAction() { return failedAction; }

    /**
     * Returns the receipt of what was installed before the failure.
     */
    public SetupReceipt partialReceipt() { return partialReceipt; }
}
