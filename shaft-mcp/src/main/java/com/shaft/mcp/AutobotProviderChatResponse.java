package com.shaft.mcp;

import java.time.Duration;
import java.util.List;

/**
 * Safe cloud provider chat response returned by SHAFT Autobot MCP.
 *
 * @param status normalized provider status
 * @param provider provider identifier
 * @param model provider model
 * @param mode assistant mode
 * @param answer provider answer, or fallback text
 * @param warnings safe warnings
 * @param duration provider duration
 * @param fallbackReason safe failure reason
 */
public record AutobotProviderChatResponse(
        String status,
        String provider,
        String model,
        String mode,
        String answer,
        List<String> warnings,
        Duration duration,
        String fallbackReason) {
    /**
     * Creates an immutable response.
     */
    public AutobotProviderChatResponse {
        status = status == null ? "" : status;
        provider = provider == null ? "" : provider;
        model = model == null ? "" : model;
        mode = mode == null ? "" : mode;
        answer = answer == null ? "" : answer;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        duration = duration == null ? Duration.ZERO : duration;
        fallbackReason = fallbackReason == null ? "" : fallbackReason;
    }
}
