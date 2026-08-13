package com.shaft.infrastructure;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

/** Approval gate shared by CLI, Java, properties, and MCP execution adapters. */
public final class SetupExecutor {
    private SetupExecutor() { }

    public static SetupReceipt execute(SetupPlan plan, SetupApproval approval,
                                       Consumer<SetupAction> actionExecutor) {
        validate(plan, approval);
        Objects.requireNonNull(actionExecutor, "actionExecutor");
        List<SetupAction> completed = new ArrayList<>();
        for (SetupAction action : plan.actions()) {
            try {
                actionExecutor.accept(action);
                completed.add(action);
            } catch (RuntimeException failure) {
                throw new SetupExecutionException(action,
                        new SetupReceipt(plan.digest(), Instant.now(), completed), failure);
            }
        }
        return new SetupReceipt(plan.digest(), Instant.now(), completed);
    }

    /** Validates approval and licenses without creating files or invoking an action executor. */
    public static void validate(SetupPlan plan, SetupApproval approval) {
        Objects.requireNonNull(plan, "plan");
        Objects.requireNonNull(approval, "approval");
        if (!plan.digest().equals(approval.planDigest())) {
            throw new StaleSetupApprovalException(approval.planDigest(), plan.digest());
        }
        var requiredLicenses = plan.actions().stream().flatMap(action -> action.requiredLicenses().stream())
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        if (!approval.acceptedLicenses().containsAll(requiredLicenses)) {
            var missing = new java.util.TreeSet<>(requiredLicenses);
            missing.removeAll(approval.acceptedLicenses());
            throw new IllegalArgumentException("Missing required license acceptance: " + missing);
        }
    }
}
