package com.shaft.pilot.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.pilot.config.PilotConfiguration;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.EnumSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiExecutionServiceTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void disabledConfigurationStillRequiresRequestApprovalBeforeExplicitProviderCall() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiExecutionService service = service(registry, PilotTestConfiguration.disabled());

            AiResponse response = service.execute(request(ApprovalPolicy.denyAll()));

            assertEquals(AiResponseStatus.CONSENT_REQUIRED, response.status());
            assertEquals(0, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void explicitProviderCanRunWhenUserConfigurationIsDisabled() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiResponse response = service(registry, PilotTestConfiguration.disabled())
                    .execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertEquals(1, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void invalidConfigurationReturnsFallbackWithoutProviderDiscovery() {
        AiProviderRegistry registry = new AiProviderRegistry();
        AiExecutionService service = new AiExecutionService(registry, () -> {
            throw new IllegalArgumentException("invalid endpoint");
        }, event -> {
        });

        AiResponse response = service.execute(request(ApprovalPolicy.denyAll()));

        assertEquals(AiResponseStatus.DISABLED, response.status());
        assertEquals("deterministic", response.structuredPayload().path("answer").asText());
        assertEquals("AI configuration is invalid.", response.fallbackReason());
    }

    @Test
    void remoteEvidenceRequiresConsentFromConfigurationAndRequest() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        PilotConfiguration configured = PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE);
        PilotConfiguration denied = new PilotConfiguration(
                true, configured.provider(), ApprovalPolicy.denyAll(), false, configured.timeout(),
                configured.maxRequestBytes(), configured.maxInputTokens(), configured.maxOutputTokens(),
                configured.maxCostUsd(), configured.retryMaxAttempts(), configured.maxConcurrency(),
                configured.circuitBreakerFailureThreshold(), configured.circuitBreakerCooldown(),
                configured.redactionPolicy(), configured.providers());
        try {
            AiResponse response = service(registry, denied).execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.CONSENT_REQUIRED, response.status());
            assertEquals(0, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void onPremEvidenceRequiresIndependentConsent() {
        StubProvider provider = new StubProvider(ProcessingLocation.ON_PREM);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        PilotConfiguration configured = PilotTestConfiguration.enabled("stub", ProcessingLocation.ON_PREM);
        PilotConfiguration denied = new PilotConfiguration(
                true,
                configured.provider(),
                new ApprovalPolicy(true, false, true, EnumSet.allOf(EvidenceCategory.class)),
                false,
                configured.timeout(),
                configured.maxRequestBytes(),
                configured.maxInputTokens(),
                configured.maxOutputTokens(),
                configured.maxCostUsd(),
                configured.retryMaxAttempts(),
                configured.maxConcurrency(),
                configured.circuitBreakerFailureThreshold(),
                configured.circuitBreakerCooldown(),
                configured.redactionPolicy(),
                configured.providers());
        try {
            AiResponse response = service(registry, denied)
                    .execute(request(approved(ProcessingLocation.ON_PREM)));

            assertEquals(AiResponseStatus.CONSENT_REQUIRED, response.status());
            assertEquals(0, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void requestIsRedactedBeforeProviderExecution() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiResponse response = service(registry,
                    PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE))
                    .execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertFalse(provider.lastRequest.get().text().contains("top-secret"));
            assertTrue(provider.lastRequest.get().text().contains("[REDACTED]"));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void schemaViolationReturnsDeterministicFallback() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        provider.payload = JSON.createObjectNode().put("wrong", true);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiResponse response = service(registry,
                    PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE))
                    .execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.INVALID_RESPONSE, response.status());
            assertEquals("deterministic", response.structuredPayload().path("answer").asText());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void retryableFailureIsRetried() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        provider.failuresRemaining.set(1);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiResponse response = service(registry,
                    PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE))
                    .execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertEquals(2, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void circuitOpensAfterConfiguredFailureThreshold() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        provider.alwaysFail = true;
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        PilotConfiguration base = PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE);
        PilotConfiguration thresholdOne = new PilotConfiguration(
                base.enabled(), base.provider(), base.approvalPolicy(), base.telemetryEnabled(), base.timeout(),
                base.maxRequestBytes(), base.maxInputTokens(), base.maxOutputTokens(), base.maxCostUsd(), 1,
                base.maxConcurrency(), 1, base.circuitBreakerCooldown(), base.redactionPolicy(), base.providers());
        try {
            AiExecutionService service = service(registry, thresholdOne);
            assertEquals(AiResponseStatus.RATE_LIMITED,
                    service.execute(request(approved(ProcessingLocation.REMOTE))).status());
            assertEquals(AiResponseStatus.CIRCUIT_OPEN,
                    service.execute(request(approved(ProcessingLocation.REMOTE))).status());
            assertEquals(1, provider.calls.get());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void auditSinkFailureDoesNotReplaceProviderOrFallbackResult() {
        StubProvider provider = new StubProvider(ProcessingLocation.REMOTE);
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider);
        try {
            AiExecutionService service = new AiExecutionService(registry,
                    () -> PilotTestConfiguration.enabled("stub", ProcessingLocation.REMOTE),
                    event -> {
                        throw new IllegalStateException("audit unavailable");
                    });

            AiResponse response = service.execute(request(approved(ProcessingLocation.REMOTE)));

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertTrue(response.warnings().contains("Safe AI audit metadata could not be recorded."));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    private static AiExecutionService service(AiProviderRegistry registry, PilotConfiguration configuration) {
        return new AiExecutionService(registry, () -> configuration, event -> {
        });
    }

    private static AiRequest request(ApprovalPolicy approvalPolicy) {
        JsonNode schema = JSON.createObjectNode()
                .put("type", "object")
                .set("properties", JSON.createObjectNode()
                        .set("answer", JSON.createObjectNode().put("type", "string")));
        ((com.fasterxml.jackson.databind.node.ObjectNode) schema)
                .putArray("required").add("answer");
        return AiRequest.builder("unit-test", schema)
                .text("password=top-secret")
                .budget(new AiBudget(1_000, 100, BigDecimal.ONE))
                .approvalPolicy(approvalPolicy)
                .deterministicFallback(JSON.createObjectNode().put("answer", "deterministic"))
                .timeout(Duration.ofSeconds(1))
                .build();
    }

    private static ApprovalPolicy approved(ProcessingLocation location) {
        return new ApprovalPolicy(
                location == ProcessingLocation.LOCAL,
                location == ProcessingLocation.ON_PREM,
                location == ProcessingLocation.REMOTE,
                EnumSet.allOf(EvidenceCategory.class));
    }

    private static final class StubProvider implements AiProvider {
        private final ProcessingLocation location;
        private final AtomicInteger calls = new AtomicInteger();
        private final AtomicInteger failuresRemaining = new AtomicInteger();
        private final AtomicReference<AiRequest> lastRequest = new AtomicReference<>();
        private JsonNode payload = JSON.createObjectNode().put("answer", "provider");
        private boolean alwaysFail;

        private StubProvider(ProcessingLocation location) {
            this.location = location;
        }

        @Override
        public String id() {
            return "stub";
        }

        @Override
        public AiCapabilities capabilities() {
            return new AiCapabilities(true, true, false, 10_000, location);
        }

        @Override
        public AiProviderAvailability availability() {
            return AiProviderAvailability.ready();
        }

        @Override
        public AiResponse execute(AiRequest request) {
            calls.incrementAndGet();
            lastRequest.set(request);
            if (alwaysFail || failuresRemaining.getAndUpdate(value -> Math.max(0, value - 1)) > 0) {
                return AiResponse.failure(AiResponseStatus.RATE_LIMITED, id(), "test-model",
                        "Rate limited.", Duration.ZERO, request.deterministicFallback());
            }
            return AiResponse.success(id(), "test-model", payload, Duration.ZERO, AiUsage.empty(),
                    request.deterministicFallback());
        }
    }
}
