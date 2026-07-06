package com.shaft.capture.model;

/**
 * Safe browser-facing summary of one persisted capture event, used to rehydrate the
 * recorder UI step list across navigations (including cross-origin ones) from the
 * server-side session store instead of page-scoped storage.
 *
 * @param clientActionId stable browser-assigned action identifier
 * @param sequence event sequence within the session
 * @param description human-readable step description
 */
public record CaptureStep(String clientActionId, long sequence, String description) {
    /**
     * Creates an immutable step summary.
     */
    public CaptureStep {
        clientActionId = ModelSupport.requireText(clientActionId, "Client action ID");
        if (sequence < 1) {
            throw new IllegalArgumentException("Step sequence must be positive.");
        }
        description = ModelSupport.text(description);
    }
}
