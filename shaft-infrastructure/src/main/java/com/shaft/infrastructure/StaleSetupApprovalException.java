package com.shaft.infrastructure;

/** Raised before mutation when approval does not match the exact current plan. */
public final class StaleSetupApprovalException extends IllegalArgumentException {
    /**
     * Creates an exception for an approval whose digest no longer matches the plan.
     */
    public StaleSetupApprovalException(String approvedDigest, String currentDigest) {
        super("Setup approval is stale: approved " + approvedDigest + " but current plan is " + currentDigest);
    }
}
