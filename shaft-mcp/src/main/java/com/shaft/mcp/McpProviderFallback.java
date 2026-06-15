package com.shaft.mcp;

import com.shaft.pilot.ai.AiResponseStatus;

import java.util.List;

/**
 * Safe provider metadata for optional AI fallback.
 *
 * @param requested whether provider assistance was requested
 * @param used whether provider output was accepted
 * @param status normalized provider status
 * @param provider provider identifier
 * @param model model identifier
 * @param fallbackReason safe fallback reason
 * @param warnings safe warnings
 */
public record McpProviderFallback(
        boolean requested,
        boolean used,
        AiResponseStatus status,
        String provider,
        String model,
        String fallbackReason,
        List<String> warnings) {
    /**
     * Creates safe immutable provider metadata.
     */
    public McpProviderFallback {
        status = status == null ? AiResponseStatus.DISABLED : status;
        provider = provider == null ? "" : provider.trim();
        model = model == null ? "" : model.trim();
        fallbackReason = fallbackReason == null ? "" : fallbackReason.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    /**
     * Returns disabled provider metadata.
     *
     * @return disabled fallback
     */
    public static McpProviderFallback disabled() {
        return new McpProviderFallback(false, false, AiResponseStatus.DISABLED,
                "none", "", "AI fallback was not requested.", List.of());
    }
}
