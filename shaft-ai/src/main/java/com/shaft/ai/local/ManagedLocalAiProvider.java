package com.shaft.ai.local;

import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ProcessingLocation;

import java.time.Duration;

/** SHAFT-owned provider entrypoint for managed local inference. */
public final class ManagedLocalAiProvider implements AiProvider {
    /** Creates the service-loadable managed provider. */
    public ManagedLocalAiProvider() {
    }

    @Override
    public String id() {
        return "managed-local";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, false, false, 0, ProcessingLocation.LOCAL);
    }

    @Override
    public AiProviderAvailability availability() {
        return AiProviderAvailability.unavailable("Managed local inference is not ready.");
    }

    @Override
    public AiResponse execute(AiRequest request) {
        return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, id(), "",
                "Managed local inference is not ready.", Duration.ZERO, request.deterministicFallback());
    }
}
