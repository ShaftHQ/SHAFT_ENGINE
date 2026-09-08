package com.shaft.ai.agentic;

/**
 * Hard caps for one workflow phase (FR-2). Exhaustion stops the phase without mutating approved tests.
 *
 * @param maxSteps maximum planning or generation steps
 * @param maxToolCalls maximum scoped tool invocations
 * @param maxRetries maximum retry attempts for the phase
 */
public record PhaseBudget(int maxSteps, int maxToolCalls, int maxRetries) {
    /**
     * Validates non-negative budgets.
     */
    public PhaseBudget {
        if (maxSteps < 0 || maxToolCalls < 0 || maxRetries < 0) {
            throw new IllegalArgumentException("Phase budgets must not be negative.");
        }
    }

    /**
     * Default conservative budgets used by the fixture workflow.
     *
     * @return fixture budgets
     */
    public static PhaseBudget fixtureDefault() {
        return new PhaseBudget(8, 16, 1);
    }

    /**
     * Exhausted budget that immediately stops a phase.
     *
     * @return zeroed budget
     */
    public static PhaseBudget exhausted() {
        return new PhaseBudget(0, 0, 0);
    }
}
