package com.shaft.pilot.ai;

import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.config.ProviderConfiguration;
import com.shaft.pilot.security.RedactionPolicy;

import java.math.BigDecimal;
import java.net.URI;
import java.time.Duration;
import java.util.EnumSet;
import java.util.Map;

final class PilotTestConfiguration {
    private PilotTestConfiguration() {
        throw new IllegalStateException("Utility class");
    }

    static PilotConfiguration enabled(String provider, ProcessingLocation location) {
        boolean local = location == ProcessingLocation.LOCAL;
        boolean remote = location == ProcessingLocation.REMOTE;
        return new PilotConfiguration(
                true,
                provider,
                new ApprovalPolicy(local, remote, EnumSet.allOf(EvidenceCategory.class)),
                false,
                Duration.ofSeconds(1),
                1024 * 1024,
                10_000,
                1_000,
                BigDecimal.TEN,
                2,
                2,
                2,
                Duration.ofSeconds(30),
                new RedactionPolicy(
                        java.util.Set.of("password", "authorization", "cookie"),
                        java.util.List.of("input[type=password]"),
                        java.util.List.of()),
                Map.of(provider, new ProviderConfiguration(provider, URI.create("http://127.0.0.1"),
                        "test-model", provider.equals("ollama") ? "" : "TEST_API_KEY", Map.of())));
    }

    static PilotConfiguration disabled() {
        PilotConfiguration enabled = enabled("openai", ProcessingLocation.REMOTE);
        return new PilotConfiguration(false, "none", ApprovalPolicy.denyAll(), false, enabled.timeout(),
                enabled.maxRequestBytes(), enabled.maxInputTokens(), enabled.maxOutputTokens(), enabled.maxCostUsd(),
                enabled.retryMaxAttempts(), enabled.maxConcurrency(), enabled.circuitBreakerFailureThreshold(),
                enabled.circuitBreakerCooldown(), enabled.redactionPolicy(), enabled.providers());
    }
}
