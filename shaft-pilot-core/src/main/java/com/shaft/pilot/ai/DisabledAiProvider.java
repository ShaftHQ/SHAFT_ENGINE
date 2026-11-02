package com.shaft.pilot.ai;

import java.time.Duration;

/**
 * Deterministic no-op provider used when AI is disabled or unavailable.
 */
public final class DisabledAiProvider implements AiProvider {
    /**
     * Returns the disabled provider identifier.
     *
     * @return {@code none}
     */
    @Override
    public String id() {
        return "none";
    }

    /**
     * Returns disabled capabilities.
     *
     * @return disabled capabilities
     */
    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(false, false, false, 0, ProcessingLocation.NONE);
    }

    /**
     * Returns unavailable without performing any network call.
     *
     * @return unavailable result
     */
    @Override
    public AiProviderAvailability availability() {
        return AiProviderAvailability.unavailable("AI is disabled.");
    }

    /**
     * Returns the caller's deterministic fallback without performing any network call.
     *
     * @param request request
     * @return disabled response
     */
    @Override
    public AiResponse execute(AiRequest request) {
        return AiResponse.failure(AiResponseStatus.DISABLED, id(), "", "AI is disabled.",
                Duration.ZERO, request.deterministicFallback());
    }
}
