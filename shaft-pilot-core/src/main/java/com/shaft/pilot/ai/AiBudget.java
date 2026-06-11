package com.shaft.pilot.ai;

import java.math.BigDecimal;

/**
 * Per-request limits for model usage.
 *
 * @param maxInputTokens maximum estimated input tokens
 * @param maxOutputTokens maximum requested output tokens
 * @param maxCostUsd maximum accepted provider-reported cost in USD
 */
public record AiBudget(long maxInputTokens, long maxOutputTokens, BigDecimal maxCostUsd) {
    /**
     * Creates and validates a request budget.
     */
    public AiBudget {
        if (maxInputTokens < 0 || maxOutputTokens < 0) {
            throw new IllegalArgumentException("Token budgets must not be negative.");
        }
        maxCostUsd = maxCostUsd == null ? BigDecimal.ZERO : maxCostUsd;
        if (maxCostUsd.signum() < 0) {
            throw new IllegalArgumentException("Cost budget must not be negative.");
        }
    }
}
