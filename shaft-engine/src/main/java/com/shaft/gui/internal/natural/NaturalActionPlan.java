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
 */
public record NaturalActionPlan(
        String plannerId,
        String intent,
        List<NaturalActionStep> steps,
        double trust,
        String explanation) {
    /**
     * Creates an immutable plan.
     */
    public NaturalActionPlan {
        plannerId = Objects.requireNonNullElse(plannerId, "");
        intent = Objects.requireNonNullElse(intent, "");
        steps = steps == null ? List.of() : List.copyOf(steps);
        trust = Math.max(0, Math.min(1, trust));
        explanation = Objects.requireNonNullElse(explanation, "");
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
        return new NaturalActionPlan(plannerId, intent, List.of(), 0, explanation);
    }
}
