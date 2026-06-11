package com.shaft.pilot.ai;

import com.shaft.driver.SHAFT;
import com.shaft.pilot.config.PilotConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

class AiProviderRegistryTest {
    @AfterEach
    void clearProperties() {
        SHAFT.Properties.clearForCurrentThread();
    }

    @Test
    void disabledConfigurationResolvesNoOpProvider() {
        assertInstanceOf(DisabledAiProvider.class, new AiProviderRegistry().resolve(PilotTestConfiguration.disabled()));
    }

    @Test
    void pilotConfigurationDefaultsAreDisabledAndDenyConsent() {
        PilotConfiguration configuration = PilotConfiguration.current();

        assertEquals(false, configuration.enabled());
        assertEquals("none", configuration.provider());
        assertEquals(ApprovalPolicy.denyAll(), configuration.approvalPolicy());
    }

    @Test
    void explicitProviderTakesPrecedence() {
        AiProviderRegistry registry = new AiProviderRegistry();
        AiProvider provider = new MinimalProvider("explicit");
        registry.registerForCurrentThread(provider);
        try {
            assertEquals(provider, registry.resolve(PilotTestConfiguration.enabled("openai",
                    ProcessingLocation.REMOTE)));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void pilotPropertyOverridesAreIsolatedPerThread() {
        CompletableFuture<String> openAi = CompletableFuture.supplyAsync(() -> {
            try {
                SHAFT.Properties.pilot.set().enabled(true).provider("openai");
                return PilotConfiguration.current().provider();
            } finally {
                SHAFT.Properties.clearForCurrentThread();
            }
        });
        CompletableFuture<String> ollama = CompletableFuture.supplyAsync(() -> {
            try {
                SHAFT.Properties.pilot.set().enabled(true).provider("ollama");
                return PilotConfiguration.current().provider();
            } finally {
                SHAFT.Properties.clearForCurrentThread();
            }
        });

        assertEquals("openai", openAi.join());
        assertEquals("ollama", ollama.join());
        assertEquals("none", PilotConfiguration.current().provider());
    }

    private record MinimalProvider(String id) implements AiProvider {
        @Override
        public AiCapabilities capabilities() {
            return new AiCapabilities(true, false, false, 0, ProcessingLocation.REMOTE);
        }

        @Override
        public AiProviderAvailability availability() {
            return AiProviderAvailability.ready();
        }

        @Override
        public AiResponse execute(AiRequest request) {
            return AiResponse.failure(AiResponseStatus.ERROR, id, "", "not used", Duration.ZERO,
                    request.deterministicFallback());
        }
    }
}
