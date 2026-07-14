package com.shaft.mcp;

import java.util.List;
import java.util.Map;

/**
 * Persisted MCP mobile action for deterministic replay generation.
 *
 * @param stepId stable identity assigned once at record time; never changes and never reused,
 *         even after {@link McpMobileRecordingService#deleteStep(String)}. Recordings persisted
 *         before schema 1.1 lack this field; it is backfilled to {@code "m" + sequence} on read
 *         (see {@link McpMobileRecordingService})
 */
public record McpMobileRecordedAction(
        String stepId,
        long sequence,
        String timestamp,
        String action,
        String locatorStrategy,
        String locatorValue,
        Map<String, String> parameters,
        String javaCode,
        boolean sensitiveValueStored,
        List<String> warnings) {
    /**
     * Creates an immutable recorded action.
     */
    public McpMobileRecordedAction {
        stepId = stepId == null ? "" : stepId.trim();
        timestamp = timestamp == null ? "" : timestamp.trim();
        action = action == null ? "" : action.trim();
        locatorStrategy = locatorStrategy == null ? "" : locatorStrategy.trim();
        locatorValue = locatorValue == null ? "" : locatorValue.trim();
        parameters = parameters == null ? Map.of() : Map.copyOf(parameters);
        javaCode = javaCode == null ? "" : javaCode.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
