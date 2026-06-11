package com.shaft.pilot.ai;

/**
 * SHAFT-owned contract implemented by optional AI providers.
 */
public interface AiProvider {
    /**
     * Returns the stable provider identifier used by {@code pilot.ai.provider}.
     *
     * @return provider identifier
     */
    String id();

    /**
     * Returns provider capabilities.
     *
     * @return provider capabilities
     */
    AiCapabilities capabilities();

    /**
     * Checks local configuration without transmitting evidence.
     *
     * @return availability result
     */
    AiProviderAvailability availability();

    /**
     * Executes an already approved and redacted request.
     *
     * <p>Implementations must honor {@link AiRequest#timeout()} and must not log
     * request content, credentials, or raw provider responses.</p>
     *
     * @param request approved and redacted request
     * @return normalized response
     */
    AiResponse execute(AiRequest request);
}
