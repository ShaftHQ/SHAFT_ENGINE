package com.shaft.mcp;

import java.util.List;

/**
 * Mobile natural action result returned by mobile_natural_act tool.
 */
public record McpMobileNaturalActionResult(
        String intent,
        String locatorStrategy,
        String locatorValue,
        boolean success,
        List<String> warnings) {
    /**
     * Creates an immutable mobile natural action result.
     */
    public McpMobileNaturalActionResult {
        intent = intent == null ? "" : intent.trim();
        locatorStrategy = locatorStrategy == null ? "" : locatorStrategy.trim();
        locatorValue = locatorValue == null ? "" : locatorValue.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
