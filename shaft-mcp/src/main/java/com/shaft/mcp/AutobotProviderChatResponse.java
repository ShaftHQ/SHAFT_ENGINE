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
 * @param summary short summary of the structured codegen response
 * @param codeBlocks structured code blocks returned by the provider
 * @param citedGuideUrls official SHAFT guide URLs the provider cited
 * @param locatorAssumptions locator assumptions the provider flagged as unverified
 * @param guardrailStatus result of running SHAFT guardrails on the returned code
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
        String summary,
        List<AutobotCodeBlock> codeBlocks,
        List<String> citedGuideUrls,
        List<String> locatorAssumptions,
        String guardrailStatus,
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
        summary = summary == null ? "" : summary;
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        citedGuideUrls = citedGuideUrls == null ? List.of() : List.copyOf(citedGuideUrls);
        locatorAssumptions = locatorAssumptions == null ? List.of() : List.copyOf(locatorAssumptions);
        guardrailStatus = guardrailStatus == null ? "" : guardrailStatus;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        duration = duration == null ? Duration.ZERO : duration;
        fallbackReason = fallbackReason == null ? "" : fallbackReason;
    }

    /**
     * Compatibility constructor for callers compiled against the answer-only response shape.
     */
    public AutobotProviderChatResponse(
            String status,
            String provider,
            String model,
            String mode,
            String answer,
            List<String> warnings,
            Duration duration,
            String fallbackReason) {
        this(status, provider, model, mode, answer, "", List.of(), List.of(), List.of(), "", warnings, duration,
                fallbackReason);
    }
}
