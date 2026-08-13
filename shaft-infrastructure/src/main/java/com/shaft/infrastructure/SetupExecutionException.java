package com.shaft.infrastructure;

import java.util.Objects;

/** Failure with an immutable receipt for every action completed before the failed action. */
public final class SetupExecutionException extends RuntimeException {
    private final SetupAction failedAction;
    private final SetupReceipt partialReceipt;

    public SetupExecutionException(SetupAction failedAction, SetupReceipt partialReceipt, RuntimeException cause) {
        super("Setup action failed: " + Objects.requireNonNull(failedAction, "failedAction"), cause);
        this.failedAction = failedAction;
        this.partialReceipt = Objects.requireNonNull(partialReceipt, "partialReceipt");
    }

    public SetupAction failedAction() { return failedAction; }

    public SetupReceipt partialReceipt() { return partialReceipt; }
}
