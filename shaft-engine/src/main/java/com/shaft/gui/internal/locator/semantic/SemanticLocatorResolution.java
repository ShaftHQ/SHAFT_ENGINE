package com.shaft.gui.internal.locator.semantic;

import org.openqa.selenium.By;

import java.util.Objects;

/**
 * Chosen locator plus retained inspection evidence and confidence (issue #5457 FR-3).
 */
public record SemanticLocatorResolution(
        By locator,
        SemanticLocatorStrategy strategy,
        String expression,
        double confidence,
        int matchCount,
        boolean usedExplicitScope,
        boolean usedSemanticFallback,
        SemanticElementEvidence evidence) {

    public SemanticLocatorResolution {
        locator = Objects.requireNonNull(locator, "locator");
        strategy = Objects.requireNonNull(strategy, "strategy");
        expression = Objects.requireNonNullElse(expression, "");
        evidence = Objects.requireNonNull(evidence, "evidence");
        if (confidence < 0.0 || confidence > 1.0) {
            throw new IllegalArgumentException("confidence must be in [0,1]");
        }
    }
}
