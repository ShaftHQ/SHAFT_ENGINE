package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;

import java.util.List;
import java.util.Objects;

/**
 * Provider-neutral explanation for an accepted healing decision.
 *
 * @param attemptId provider-owned safe correlation identifier
 * @param originalLocator original failed locator
 * @param healedLocator selected replacement locator
 * @param confidence confidence score from 0.0 to 1.0
 * @param threshold configured minimum score from 0.0 to 1.0
 * @param evidence safe evidence summary
 * @param providerStatus optional provider status
 * @param reason safe decision explanation
 */
public record HealingExplanation(
        String attemptId,
        By originalLocator,
        By healedLocator,
        double confidence,
        double threshold,
        List<String> evidence,
        String providerStatus,
        String reason) {
    /**
     * Creates a bounded immutable explanation.
     */
    public HealingExplanation {
        attemptId = Objects.requireNonNullElse(attemptId, "");
        originalLocator = Objects.requireNonNull(originalLocator, "originalLocator");
        healedLocator = Objects.requireNonNull(healedLocator, "healedLocator");
        confidence = Math.max(0, Math.min(1, confidence));
        threshold = Math.max(0, Math.min(1, threshold));
        evidence = evidence == null ? List.of() : List.copyOf(evidence);
        providerStatus = Objects.requireNonNullElse(providerStatus, "");
        reason = Objects.requireNonNullElse(reason, "");
    }
}
