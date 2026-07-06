package com.shaft.gui.internal.natural;

import java.util.List;
import java.util.Objects;

/**
 * Structured plan produced from a natural-language action request.
 *
 * @param plannerId planner identifier
 * @param intent original user intent
 * @param steps ordered engine-executable steps
 * @param trust aggregate plan trust score from 0.0 to 1.0
 * @param explanation safe human-readable explanation
 * @param matchConfidence deterministic engine confidence 0..1; null for unsupported plans
 * @param resolutionPath "deterministic" or "ai-fallback" or null for unsupported plans
 */
public record NaturalActionPlan(
        String plannerId,
        String intent,
        List<NaturalActionStep> steps,
        double trust,
        String explanation,
        Double matchConfidence,
        String resolutionPath) {
    /**
     * Creates an immutable plan.
     */
    public NaturalActionPlan {
        plannerId = Objects.requireNonNullElse(plannerId, "");
        intent = Objects.requireNonNullElse(intent, "");
        steps = steps == null ? List.of() : List.copyOf(steps);
        trust = Math.max(0, Math.min(1, trust));
        explanation = Objects.requireNonNullElse(explanation, "");
        if (matchConfidence != null) {
            matchConfidence = Math.max(0, Math.min(1, matchConfidence));
        }
        resolutionPath = Objects.requireNonNullElse(resolutionPath, "");
    }

    /**
     * Creates an unsupported zero-trust plan.
     *
     * @param plannerId planner identifier
     * @param intent original intent
     * @param explanation safe explanation
     * @return unsupported plan
     */
    public static NaturalActionPlan unsupported(String plannerId, String intent, String explanation) {
        return new NaturalActionPlan(plannerId, intent, List.of(), 0, explanation, null, null);
    }

    /**
     * Creates a plan with backward compatibility (without confidence/resolutionPath).
     *
     * @param plannerId planner identifier
     * @param intent original intent
     * @param steps executable steps
     * @param trust aggregate trust
     * @param explanation explanation
     * @return plan
     */
    public static NaturalActionPlan of(
            String plannerId,
            String intent,
            List<NaturalActionStep> steps,
            double trust,
            String explanation) {
        return new NaturalActionPlan(plannerId, intent, steps, trust, explanation, null, null);
    }

    /**
     * Creates a plan with all fields.
     *
     * @param plannerId planner identifier
     * @param intent original intent
     * @param steps executable steps
     * @param trust aggregate trust
     * @param explanation explanation
     * @param matchConfidence deterministic confidence
     * @param resolutionPath deterministic or ai-fallback
     * @return plan
     */
    public static NaturalActionPlan of(
            String plannerId,
            String intent,
            List<NaturalActionStep> steps,
            double trust,
            String explanation,
            Double matchConfidence,
            String resolutionPath) {
        return new NaturalActionPlan(plannerId, intent, steps, trust, explanation, matchConfidence, resolutionPath);
    }
}
