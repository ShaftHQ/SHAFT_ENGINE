package com.shaft.ai.local;

import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ProcessingLocation;

import java.time.Duration;
import java.util.Objects;

/** SHAFT-owned provider entrypoint for managed local inference. */
public final class ManagedLocalAiProvider implements AiProvider {
    private final Lifecycle lifecycle;

    /** Creates the service-loadable managed provider. */
    public ManagedLocalAiProvider() {
        this(new ServiceLifecycle(new ManagedLocalAiService()));
    }

    ManagedLocalAiProvider(Lifecycle lifecycle) {
        this.lifecycle = Objects.requireNonNull(lifecycle, "lifecycle");
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
        try {
            ManagedLocalAiSnapshot snapshot = lifecycle.inspect();
            if (lifecycle.executable() && (snapshot.state() == ManagedLocalAiSnapshot.State.READY
                    || snapshot.state() == ManagedLocalAiSnapshot.State.NOT_PROVISIONED
                    && snapshot.transparentProvisioning())) {
                return AiProviderAvailability.ready();
            }
            return AiProviderAvailability.unavailable(snapshot.action());
        } catch (RuntimeException failure) {
            return AiProviderAvailability.unavailable("Managed local inference status is unavailable.");
        }
    }

    @Override
    public AiResponse execute(AiRequest request) {
        Objects.requireNonNull(request, "request");
        try {
            return lifecycle.execute(request);
        } catch (RuntimeException failure) {
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, id(), "",
                    "Managed local inference is unavailable.", Duration.ZERO, request.deterministicFallback());
        }
    }

    interface Lifecycle {
        boolean executable();
        ManagedLocalAiSnapshot inspect();
        AiResponse execute(AiRequest request);
    }

    private record ServiceLifecycle(ManagedLocalAiService service) implements Lifecycle {
        @Override
        public boolean executable() {
            return false;
        }

        @Override
        public ManagedLocalAiSnapshot inspect() {
            return service.inspect();
        }

        @Override
        public AiResponse execute(AiRequest request) {
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "managed-local", "",
                    "Managed local inference is not ready.", Duration.ZERO, request.deterministicFallback());
        }
    }
}
