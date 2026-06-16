package com.shaft.mcp;

/**
 * Result metadata returned after a trust-gated natural action.
 *
 * @param intentLength natural-language intent length
 * @param argumentCount number of user arguments supplied
 * @param minimumTrustPercentage effective trust threshold percentage
 * @param planner effective planner identifier
 * @param aiFallbackEnabled whether optional provider fallback was enabled for this call
 * @param allowedActions effective allowed action target categories
 */
public record McpNaturalActionResult(
        int intentLength,
        int argumentCount,
        int minimumTrustPercentage,
        String planner,
        boolean aiFallbackEnabled,
        String allowedActions) {
    /**
     * Creates a bounded immutable natural-action result.
     */
    public McpNaturalActionResult {
        intentLength = Math.max(0, intentLength);
        argumentCount = Math.max(0, argumentCount);
        minimumTrustPercentage = Math.max(0, Math.min(100, minimumTrustPercentage));
        planner = planner == null ? "" : planner.trim();
        allowedActions = allowedActions == null ? "" : allowedActions.trim();
    }
}
