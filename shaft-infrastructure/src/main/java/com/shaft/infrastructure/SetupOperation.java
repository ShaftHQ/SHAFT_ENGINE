package com.shaft.infrastructure;

/** Intent bound into a provider-generated setup plan. */
public enum SetupOperation {
    INSTALL,
    CLEAN,
    ROLLBACK;

    /** Recovers the single operation bound by an exact plan's action kinds. */
    public static SetupOperation fromPlan(SetupPlan plan) {
        java.util.Objects.requireNonNull(plan, "plan");
        boolean clean = plan.actions().stream().anyMatch(action -> action.kind() == SetupActionKind.CLEAN);
        boolean rollback = plan.actions().stream().anyMatch(action -> action.kind() == SetupActionKind.ROLLBACK);
        boolean other = plan.actions().stream().anyMatch(action -> action.kind() != SetupActionKind.CLEAN
                && action.kind() != SetupActionKind.ROLLBACK);
        if ((clean && rollback) || ((clean || rollback) && other)) {
            throw new IllegalArgumentException("Setup plan mixes incompatible operations.");
        }
        if (clean) return CLEAN;
        if (rollback) return ROLLBACK;
        return INSTALL;
    }
}
