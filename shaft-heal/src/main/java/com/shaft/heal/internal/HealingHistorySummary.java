package com.shaft.heal.internal;

/**
 * Summary of healing history statistics.
 *
 * @param consecutiveHeals count of consecutive successful heals for the most recent run
 * @param acceptanceRate percentage of accepted outcomes
 * @param isPromotionCandidate true if candidate meets promotion criteria
 */
public record HealingHistorySummary(int consecutiveHeals, double acceptanceRate, boolean isPromotionCandidate) {
}
