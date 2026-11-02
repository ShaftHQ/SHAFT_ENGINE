package com.shaft.ai.agentic;

import java.util.Objects;

/**
 * Binds one phase to seed, tools/permissions, and budgets (FR-2).
 *
 * @param phase workflow phase
 * @param seed deterministic seed
 * @param permissions scoped permissions
 * @param budget phase budget
 */
public record PhaseBinding(
        AgenticPhase phase,
        AgenticSeed seed,
        PhasePermissions permissions,
        PhaseBudget budget) {
    /**
     * Creates a validated binding.
     */
    public PhaseBinding {
        Objects.requireNonNull(phase, "phase");
        Objects.requireNonNull(seed, "seed");
        Objects.requireNonNull(permissions, "permissions");
        Objects.requireNonNull(budget, "budget");
        if (!seed.isConsistent()) {
            throw new IllegalArgumentException("Inconsistent seed hash for phase " + phase);
        }
    }

    /**
     * Default binding for a phase using fixture budgets and phase-specific permissions.
     *
     * @param phase phase
     * @param seed seed
     * @return binding
     */
    public static PhaseBinding forPhase(AgenticPhase phase, AgenticSeed seed) {
        PhasePermissions permissions = switch (phase) {
            case PLANNER -> PhasePermissions.planner();
            case GENERATOR -> PhasePermissions.generator();
            case RUNNER -> PhasePermissions.runner();
            case DIAGNOSER -> PhasePermissions.diagnoser();
            case PROPOSAL -> PhasePermissions.proposal();
        };
        return new PhaseBinding(phase, seed, permissions, PhaseBudget.fixtureDefault());
    }
}
