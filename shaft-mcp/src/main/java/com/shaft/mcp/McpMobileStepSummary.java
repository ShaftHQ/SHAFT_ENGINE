package com.shaft.mcp;

import java.util.List;

/**
 * Summary of one recorded mobile step, surfaced in {@link McpMobileRecordingStatus} so an agent
 * can target {@link McpMobileRecordingService#deleteStep(String)} or
 * {@link McpMobileRecordingService#reorderStep(String, String)} by stable {@code stepId} without
 * re-reading the persisted recording file.
 *
 * @param stepId stable identity assigned at record time; never changes or is reused
 * @param sequence current display position (1..N), renumbered after every delete/reorder
 * @param action recorded action name
 * @param locatorStrategy recorded locator strategy name, blank for coordinate-based actions
 * @param locatorValue recorded locator value
 * @param risky whether this step carries replay warnings
 * @param warnings step-level replay warnings
 */
public record McpMobileStepSummary(
        String stepId,
        long sequence,
        String action,
        String locatorStrategy,
        String locatorValue,
        boolean risky,
        List<String> warnings) {
    /**
     * Creates an immutable step summary.
     */
    public McpMobileStepSummary {
        stepId = stepId == null ? "" : stepId.trim();
        action = action == null ? "" : action.trim();
        locatorStrategy = locatorStrategy == null ? "" : locatorStrategy.trim();
        locatorValue = locatorValue == null ? "" : locatorValue.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
