package com.shaft.mcp;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Structured follow-up MCP action for a preview-only coding partner plan.
 *
 * @param label concise UI label
 * @param toolName MCP tool to prefill
 * @param arguments safe typed arguments the client can prefill
 * @param requiresConfirmation whether the user must supply or confirm runtime context first
 * @param rationale why this action comes next
 */
public record McpCodingPartnerNextAction(
        String label,
        String toolName,
        Map<String, Object> arguments,
        boolean requiresConfirmation,
        List<String> rationale) {
    /**
     * Creates an immutable next action.
     */
    public McpCodingPartnerNextAction {
        label = label == null ? "" : label.trim();
        toolName = toolName == null ? "" : toolName.trim();
        arguments = arguments == null ? Map.of()
                : Collections.unmodifiableMap(new LinkedHashMap<>(arguments));
        rationale = rationale == null ? List.of() : List.copyOf(rationale);
    }
}
