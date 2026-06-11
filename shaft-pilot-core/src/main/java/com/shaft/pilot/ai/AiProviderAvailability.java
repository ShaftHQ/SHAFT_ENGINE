package com.shaft.pilot.ai;

/**
 * Safe provider availability result.
 *
 * @param available whether the provider has enough configuration to execute
 * @param reason safe reason that never contains credential values
 */
public record AiProviderAvailability(boolean available, String reason) {
    /**
     * Creates an available result.
     *
     * @return available result
     */
    public static AiProviderAvailability ready() {
        return new AiProviderAvailability(true, "");
    }

    /**
     * Creates an unavailable result.
     *
     * @param reason safe unavailability reason
     * @return unavailable result
     */
    public static AiProviderAvailability unavailable(String reason) {
        return new AiProviderAvailability(false, reason == null ? "Provider unavailable" : reason);
    }
}
