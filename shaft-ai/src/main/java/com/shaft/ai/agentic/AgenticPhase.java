package com.shaft.ai.agentic;

/**
 * Separated responsibilities for the governed agentic test workflow (FR-1).
 */
public enum AgenticPhase {
    PLANNER,
    GENERATOR,
    RUNNER,
    DIAGNOSER,
    PROPOSAL
}
