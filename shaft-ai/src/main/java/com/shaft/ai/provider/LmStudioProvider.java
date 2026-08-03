package com.shaft.ai.provider;

import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiModelDiscovery;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;

import java.net.URI;
import java.net.InetAddress;
import java.net.http.HttpClient;
import java.time.Duration;
import java.util.Map;
import java.util.function.Function;

/** OpenAI-compatible local LM Studio adapter. */
public final class LmStudioProvider extends OpenAiProvider {
    private static final HttpClient LOOPBACK_CLIENT = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();
    public LmStudioProvider() { super(); }
    LmStudioProvider(HttpClient client, Function<String, String> environment) { super(client, environment); }

    @Override public String id() { return "lmstudio"; }

    @Override public AiCapabilities capabilities() {
        return new AiCapabilities(true, true, false, 0, processingLocation(ProcessingLocation.LOCAL));
    }

    @Override public AiProviderAvailability availability() {
        return loopback(PilotConfiguration.current().provider(id()).endpoint()) ? super.availability()
                : AiProviderAvailability.unavailable("LM Studio endpoint must use loopback.");
    }

    @Override public AiModelDiscovery discoverModels() {
        return loopback(PilotConfiguration.current().provider(id()).endpoint()) ? super.discoverModels()
                : AiModelDiscovery.unavailable();
    }

    @Override public AiResponse execute(AiRequest request) {
        if (loopback(PilotConfiguration.current().provider(id()).endpoint())) return super.execute(request);
        return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, id(), "",
                "LM Studio endpoint must use loopback.", Duration.ZERO, request.deterministicFallback());
    }

    @Override protected Map<String, String> headers(com.shaft.pilot.config.ProviderConfiguration configuration) {
        return requiresCredential(configuration) ? super.headers(configuration) : Map.of();
    }

    @Override protected HttpClient client() { return LOOPBACK_CLIENT; }


    private static boolean loopback(URI endpoint) {
        String host = endpoint.getHost();
        if (host == null) return false;
        try {
            return InetAddress.ofLiteral(host).isLoopbackAddress();
        } catch (IllegalArgumentException exception) {
            return false;
        }
    }
}
