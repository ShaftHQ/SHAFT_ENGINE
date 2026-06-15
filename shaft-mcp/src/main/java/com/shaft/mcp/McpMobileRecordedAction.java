package com.shaft.mcp;

import java.util.List;
import java.util.Map;

/**
 * Persisted MCP mobile action for deterministic replay generation.
 */
public record McpMobileRecordedAction(
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
        timestamp = timestamp == null ? "" : timestamp.trim();
        action = action == null ? "" : action.trim();
        locatorStrategy = locatorStrategy == null ? "" : locatorStrategy.trim();
        locatorValue = locatorValue == null ? "" : locatorValue.trim();
        parameters = parameters == null ? Map.of() : Map.copyOf(parameters);
        javaCode = javaCode == null ? "" : javaCode.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
