package com.shaft.doctor.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.doctor.DoctorAiAnalysisRequest;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RedactionSummary;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.config.ProviderConfiguration;
import com.shaft.pilot.security.RedactionPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorAiAnalysisServiceTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final ApprovalPolicy APPROVED = new ApprovalPolicy(
            false,
            true,
            EnumSet.allOf(EvidenceCategory.class));

    @Test
    void successfulAnalysisUsesOnlyCitedMinimizedEvidenceAndRedactsProviderText(
            @TempDir Path temp) throws Exception {
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        JsonNode payload = fixture("high-quality.json").deepCopy();
        ((ObjectNode) payload.path("observations").path(0))
                .put("statement", "Authorization: Bearer provider-secret-canary");
        DoctorAiAnalysisService service = service(request -> {
            captured.set(request);
            return success(request, payload);
        });

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(),
                DoctorAiAnalysisRequest.defaults(APPROVED), temp);

        assertEquals(DoctorAdvisory.Status.SUCCESS, advisory.status());
        assertEquals(List.of("evidence-1"),
                captured.get().evidence().stream().map(value -> value.id()).toList());
        assertFalse(captured.get().text().contains("provider-secret-canary"));
        assertFalse(captured.get().evidence().stream()
                .anyMatch(value -> value.content().contains("browser=chrome")));
        assertFalse(advisory.analysis().observations().getFirst().statement()
                .contains("provider-secret-canary"));
        assertTrue(advisory.analysis().observations().getFirst().statement().contains("[REDACTED]"));
        assertTrue(advisory.metadata().warnings().stream()
                .anyMatch(value -> value.contains("redacted")));
    }

    @Test
    void categoryConsentIsEnforcedBeforeRequestCreation(@TempDir Path temp) {
        AtomicInteger calls = new AtomicInteger();
        ApprovalPolicy textOnly = new ApprovalPolicy(false, true, Set.of(EvidenceCategory.TEXT));
        DoctorAiAnalysisService service = service(request -> {
            calls.incrementAndGet();
            return success(request, fixtureUnchecked("high-quality.json"));
        });

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(),
                DoctorAiAnalysisRequest.defaults(textOnly), temp);

        assertEquals(DoctorAdvisory.Status.FALLBACK, advisory.status());
        assertEquals(AiResponseStatus.CONSENT_REQUIRED, advisory.metadata().providerStatus());
        assertEquals(0, calls.get());
    }

    @Test
    void inventedEvidenceReferencesAreRejected(@TempDir Path temp) throws Exception {
        DoctorAiAnalysisService service =
                service(request -> success(request, fixtureUnchecked("hallucinated-evidence.json")));

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(),
                DoctorAiAnalysisRequest.defaults(APPROVED), temp);

        assertEquals(DoctorAdvisory.Status.FALLBACK, advisory.status());
        assertEquals(AiResponseStatus.INVALID_RESPONSE, advisory.metadata().providerStatus());
        assertTrue(advisory.metadata().fallbackReason().contains("validation"));
    }

    @Test
    void contradictoryAndUncitedClaimsAreMarkedWithoutChangingBaseline(@TempDir Path temp) throws Exception {
        ObjectNode payload = (ObjectNode) fixture("contradictory.json").deepCopy();
        payload.withArray("observations").addObject()
                .put("statement", "The test may also have stale data.")
                .putArray("evidenceIds");
        DoctorAiAnalysisService service = service(request -> success(request, payload));

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(),
                DoctorAiAnalysisRequest.defaults(APPROVED), temp);

        assertEquals(DoctorAdvisory.Status.SUCCESS, advisory.status());
        assertTrue(advisory.analysis().hypotheses().getFirst().contradictsDeterministic());
        assertFalse(advisory.analysis().observations().getLast().cited());
        assertTrue(advisory.metadata().warnings().stream()
                .anyMatch(value -> value.contains("contradicts")));
        assertTrue(advisory.metadata().warnings().stream()
                .anyMatch(value -> value.contains("uncited")));
        assertEquals(CauseCategory.LOCATOR, diagnosis().primaryCause());
    }

    @ParameterizedTest
    @EnumSource(value = AiResponseStatus.class, names = {
            "TIMEOUT",
            "RATE_LIMITED",
            "AUTHENTICATION_FAILED",
            "INVALID_RESPONSE",
            "BUDGET_EXCEEDED",
            "PROVIDER_UNAVAILABLE"
    })
    void providerFailuresReturnExplicitDeterministicFallback(
            AiResponseStatus status,
            @TempDir Path temp) {
        DoctorAiAnalysisService service = service(request ->
                AiResponse.failure(status, "mock", "mock-model", "Safe provider fallback.",
                        Duration.ofMillis(5), request.deterministicFallback()));

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(),
                DoctorAiAnalysisRequest.defaults(APPROVED), temp);

        assertEquals(DoctorAdvisory.Status.FALLBACK, advisory.status());
        assertEquals(status, advisory.metadata().providerStatus());
        assertEquals("Safe provider fallback.", advisory.metadata().fallbackReason());
        assertEquals(CauseCategory.LOCATOR, diagnosis().primaryCause());
    }

    @Test
    void oversizedOutputReturnsFallback(@TempDir Path temp) throws Exception {
        ObjectNode payload = (ObjectNode) fixture("high-quality.json").deepCopy();
        payload.withArray("limitations").add("x".repeat(4_000));
        DoctorAiAnalysisRequest defaults = DoctorAiAnalysisRequest.defaults(APPROVED);
        DoctorAiAnalysisRequest bounded = new DoctorAiAnalysisRequest(
                true,
                defaults.approvalPolicy(),
                defaults.timeout(),
                defaults.budget(),
                defaults.maxEvidenceItems(),
                defaults.maxEvidenceBytes(),
                512,
                false);
        DoctorAiAnalysisService service = service(request -> success(request, payload));

        DoctorAdvisory advisory = service.analyze(bundle(), diagnosis(), bounded, temp);

        assertEquals(DoctorAdvisory.Status.FALLBACK, advisory.status());
        assertTrue(advisory.metadata().fallbackReason().contains("size limit"));
    }

    @Test
    void explicitSafeCacheReusesOnlyValidatedStructuredAdvisory(@TempDir Path temp) throws Exception {
        AtomicInteger calls = new AtomicInteger();
        DoctorAiAnalysisService service = service(request -> {
            calls.incrementAndGet();
            return success(request, fixtureUnchecked("high-quality.json"));
        });
        DoctorAiAnalysisRequest defaults = DoctorAiAnalysisRequest.defaults(APPROVED);
        DoctorAiAnalysisRequest cached = new DoctorAiAnalysisRequest(
                true,
                defaults.approvalPolicy(),
                defaults.timeout(),
                defaults.budget(),
                defaults.maxEvidenceItems(),
                defaults.maxEvidenceBytes(),
                defaults.maxResponseBytes(),
                true);

        DoctorAdvisory first = service.analyze(bundle(), diagnosis(), cached, temp);
        DoctorAdvisory second = service.analyze(bundle(), diagnosis(), cached, temp);

        assertEquals(DoctorAdvisory.Status.SUCCESS, first.status());
        assertEquals(DoctorAdvisory.Status.SUCCESS, second.status());
        assertFalse(first.metadata().cacheHit());
        assertTrue(second.metadata().cacheHit());
        assertEquals(1, calls.get());
        try (var entries = java.nio.file.Files.list(temp.resolve(".doctor-ai-cache"))) {
            assertTrue(entries.findAny().isPresent());
        }
    }

    @Test
    void safeCacheIsNotReusedAfterEffectiveConsentChanges(@TempDir Path temp) {
        AtomicInteger calls = new AtomicInteger();
        AtomicReference<PilotConfiguration> configuration = new AtomicReference<>(configuration());
        DoctorAiAnalysisService service = new DoctorAiAnalysisService(request -> {
            calls.incrementAndGet();
            return success(request, fixtureUnchecked("high-quality.json"));
        }, configuration::get);
        DoctorAiAnalysisRequest defaults = DoctorAiAnalysisRequest.defaults(APPROVED);
        DoctorAiAnalysisRequest cached = new DoctorAiAnalysisRequest(
                true,
                defaults.approvalPolicy(),
                defaults.timeout(),
                defaults.budget(),
                defaults.maxEvidenceItems(),
                defaults.maxEvidenceBytes(),
                defaults.maxResponseBytes(),
                true);

        service.analyze(bundle(), diagnosis(), cached, temp);
        PilotConfiguration approved = configuration();
        configuration.set(new PilotConfiguration(
                approved.enabled(),
                approved.provider(),
                ApprovalPolicy.denyAll(),
                approved.telemetryEnabled(),
                approved.timeout(),
                approved.maxRequestBytes(),
                approved.maxInputTokens(),
                approved.maxOutputTokens(),
                approved.maxCostUsd(),
                approved.retryMaxAttempts(),
                approved.maxConcurrency(),
                approved.circuitBreakerFailureThreshold(),
                approved.circuitBreakerCooldown(),
                approved.redactionPolicy(),
                approved.providers()));
        DoctorAdvisory second = service.analyze(bundle(), diagnosis(), cached, temp);

        assertFalse(second.metadata().cacheHit());
        assertEquals(2, calls.get());
    }

    private static DoctorAiAnalysisService service(
            java.util.function.Function<AiRequest, AiResponse> executor) {
        return new DoctorAiAnalysisService(executor, DoctorAiAnalysisServiceTest::configuration);
    }

    private static AiResponse success(AiRequest request, JsonNode payload) {
        return AiResponse.success("mock", "mock-model", payload, Duration.ofMillis(5),
                new AiUsage(100, 50, null), request.deterministicFallback());
    }

    private static PilotConfiguration configuration() {
        return new PilotConfiguration(
                true,
                "mock",
                APPROVED,
                false,
                Duration.ofSeconds(1),
                1_048_576,
                16_000,
                2_000,
                BigDecimal.ZERO,
                1,
                1,
                3,
                Duration.ofSeconds(60),
                new RedactionPolicy(Set.of(), List.of(), List.of()),
                Map.of("mock", new ProviderConfiguration(
                        "mock",
                        URI.create("https://provider.invalid"),
                        "mock-model",
                        "MOCK_API_KEY",
                        Map.of())));
    }

    private static EvidenceBundle bundle() {
        EvidenceItem cited = new EvidenceItem(
                "evidence-1",
                com.shaft.doctor.model.EvidenceCategory.EXCEPTION_CHAIN,
                "text/plain",
                "",
                "sha-1",
                50,
                "NoSuchElementException: unable to locate element",
                true,
                false,
                Map.of(),
                new EvidenceProvenance("fixture", "fixture/result.json", "sha-1"));
        EvidenceItem uncited = new EvidenceItem(
                "evidence-2",
                com.shaft.doctor.model.EvidenceCategory.CONFIGURATION,
                "text/plain",
                "",
                "sha-2",
                20,
                "browser=chrome",
                true,
                false,
                Map.of(),
                new EvidenceProvenance("fixture", "fixture/config.txt", "sha-2"));
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-1",
                List.of(cited, uncited),
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of());
    }

    private static Diagnosis diagnosis() {
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(),
                Confidence.HIGH,
                "Locator did not resolve an element.",
                "The deterministic locator rule matched submitted evidence.",
                List.of(new Finding(
                        "finding-1",
                        Finding.Kind.INFERENCE,
                        CauseCategory.LOCATOR,
                        Finding.Severity.ERROR,
                        "Locator not found",
                        "The locator did not resolve an element.",
                        "locator-not-found",
                        List.of("evidence-1"))),
                List.of(),
                List.of("Current DOM snapshot"));
    }

    private static JsonNode fixture(String name) throws IOException {
        try (var input = DoctorAiAnalysisServiceTest.class
                .getResourceAsStream("/fixtures/ai/" + name)) {
            if (input == null) {
                throw new IOException("Missing fixture: " + name);
            }
            return JSON.readTree(input);
        }
    }

    private static JsonNode fixtureUnchecked(String name) {
        try {
            return fixture(name);
        } catch (IOException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
