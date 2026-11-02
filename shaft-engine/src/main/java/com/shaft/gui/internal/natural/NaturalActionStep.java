package com.shaft.gui.internal.natural;

import org.openqa.selenium.By;

import java.util.Objects;

/**
 * One validated operation in a natural-language action plan.
 *
 * @param kind action kind
 * @param locator target locator when the action targets an element
 * @param data optional action data, such as typed text or target URL
 * @param trust confidence score from 0.0 to 1.0
 * @param description safe human-readable explanation
 */
public record NaturalActionStep(
        NaturalActionKind kind,
        By locator,
        Object data,
        double trust,
        String description) {
    /**
     * Creates a bounded immutable action step.
     */
    public NaturalActionStep {
        kind = Objects.requireNonNull(kind, "kind");
        trust = Math.max(0, Math.min(1, trust));
        description = Objects.requireNonNullElse(description, "");
    }
}
