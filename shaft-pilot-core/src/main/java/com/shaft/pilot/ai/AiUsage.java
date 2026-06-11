package com.shaft.pilot.ai;

import java.math.BigDecimal;

/**
 * Provider-reported usage metadata.
 *
 * @param inputTokens input tokens, or zero when unavailable
 * @param outputTokens output tokens, or zero when unavailable
 * @param costUsd cost in USD, or {@code null} when unavailable
 */
public record AiUsage(long inputTokens, long outputTokens, BigDecimal costUsd) {
    /**
     * Returns empty usage metadata.
     *
     * @return empty usage
     */
    public static AiUsage empty() {
        return new AiUsage(0, 0, null);
    }
}
