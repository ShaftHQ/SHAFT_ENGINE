package com.shaft.pilot.ai;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.pilot.audit.AiAuditEvent;
import com.shaft.pilot.audit.AiAuditSink;
import com.shaft.pilot.audit.ShaftAiAuditSink;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.json.JsonSchemaValidator;
import com.shaft.pilot.security.RedactionResult;
import com.shaft.pilot.security.Redactor;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * Enforces approval, redaction, budgets, concurrency, retries, schema validation,
 * and circuit breaking before accepting provider output.
 */
public final class AiExecutionService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final AiProviderRegistry registry;
    private final Supplier<PilotConfiguration> configurationSupplier;
    private final AiAuditSink auditSink;
    private final Map<String, Semaphore> concurrencyLimits = new ConcurrentHashMap<>();
    private final Map<String, CircuitState> circuits = new ConcurrentHashMap<>();

    /**
     * Creates a service using current SHAFT properties, service discovery, and safe SHAFT audit logging.
     */
    public AiExecutionService() {
        this(new AiProviderRegistry(), PilotConfiguration::current, new ShaftAiAuditSink());
    }

    /**
     * Creates a service with injectable collaborators.
     *
     * @param registry provider registry
     * @param configurationSupplier current-thread configuration supplier
     * @param auditSink safe audit sink
     */
    public AiExecutionService(AiProviderRegistry registry, Supplier<PilotConfiguration> configurationSupplier,
                              AiAuditSink auditSink) {
        this.registry = Objects.requireNonNull(registry, "registry");
        this.configurationSupplier = Objects.requireNonNull(configurationSupplier, "configurationSupplier");
        this.auditSink = Objects.requireNonNull(auditSink, "auditSink");
    }

    /**
     * Executes a request or returns its deterministic fallback.
     *
     * @param request request
     * @return normalized response
     */
    public AiResponse execute(AiRequest request) {
        Objects.requireNonNull(request, "request");
        Instant started = Instant.now();
        PilotConfiguration configuration;
        try {
            configuration = configurationSupplier.get();
        } catch (RuntimeException exception) {
            return finish(request, "none", "", "", started,
                    AiResponse.failure(AiResponseStatus.DISABLED, "none", "",
                            "AI configuration is invalid.", elapsed(started), request.deterministicFallback()));
        }
        AiProvider provider;
        try {
            provider = registry.resolve(configuration);
        } catch (RuntimeException exception) {
            return finish(request, "none", "", "", started,
                    AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "none", "",
                            "Provider discovery failed.", elapsed(started), request.deterministicFallback()));
        }
        if (provider instanceof DisabledAiProvider) {
            return finish(request, provider.id(), "", "", started, provider.execute(request));
        }

        String providerId = provider.id();
        String model = safeModel(configuration, providerId);
        AiResponse preflightFailure = preflight(request, configuration, provider, providerId, model, started);
        if (preflightFailure != null) {
            return finish(request, providerId, model, "", started, preflightFailure);
        }

        CircuitState circuit = circuits.computeIfAbsent(providerId, ignored -> new CircuitState());
        if (circuit.isOpen(configuration.circuitBreakerCooldown())) {
            return finish(request, providerId, model, "", started,
                    failure(request, AiResponseStatus.CIRCUIT_OPEN, providerId, model,
                            "Provider circuit is open.", started));
        }

        RedactionResult redaction = Redactor.redact(request, configuration.redactionPolicy());
        AiRequest sanitized = request.withSanitizedContent(redaction.redactedText(), redaction.redactedEvidence())
                .withTimeout(effectiveTimeout(request, configuration));
        Semaphore semaphore = concurrencyLimits.computeIfAbsent(
                providerId + ":" + configuration.maxConcurrency(),
                ignored -> new Semaphore(configuration.maxConcurrency()));
        boolean acquired = false;
        AiResponse result;
        try {
            acquired = semaphore.tryAcquire(effectiveTimeout(request, configuration).toMillis(), TimeUnit.MILLISECONDS);
            if (!acquired) {
                result = failure(request, AiResponseStatus.TIMEOUT, providerId, model,
                        "Timed out waiting for provider capacity.", started);
            } else {
                result = executeWithRetries(sanitized, configuration, provider, providerId, model, started);
            }
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            result = failure(request, AiResponseStatus.ERROR, providerId, model,
                    "Provider execution was interrupted.", started);
        } finally {
            if (acquired) {
                semaphore.release();
            }
        }

        if (result.successful()) {
            circuit.success();
        } else if (isCircuitFailure(result.status())) {
            circuit.failure(configuration.circuitBreakerFailureThreshold());
        }
        return finish(request, providerId, model, redaction.safeSummary(), started, result);
    }

    private AiResponse preflight(AiRequest request, PilotConfiguration configuration, AiProvider provider,
                                 String providerId, String model, Instant started) {
        AiCapabilities capabilities = provider.capabilities();
        if (!configuration.approvalPolicy().allows(capabilities.processingLocation(), request.evidenceCategories())
                || !request.approvalPolicy().allows(capabilities.processingLocation(), request.evidenceCategories())) {
            return failure(request, AiResponseStatus.CONSENT_REQUIRED, providerId, model,
                    "Explicit consent is required for the processing location and evidence categories.", started);
        }
        if (!request.images().isEmpty() && !capabilities.vision()) {
            return failure(request, AiResponseStatus.PROVIDER_UNAVAILABLE, providerId, model,
                    "The selected provider does not support image evidence.", started);
        }
        if (!capabilities.structuredOutput()) {
            return failure(request, AiResponseStatus.PROVIDER_UNAVAILABLE, providerId, model,
                    "The selected provider does not support structured output.", started);
        }
        long requestBytes = requestSize(request);
        if (requestBytes > configuration.maxRequestBytes()) {
            return failure(request, AiResponseStatus.REQUEST_TOO_LARGE, providerId, model,
                    "The request exceeds the configured size limit.", started);
        }
        long estimatedInputTokens = Math.max(1, (requestBytes + 3) / 4);
        long inputLimit = inheritedLimit(request.budget().maxInputTokens(), configuration.maxInputTokens());
        long outputLimit = inheritedLimit(request.budget().maxOutputTokens(), configuration.maxOutputTokens());
        if (estimatedInputTokens > inputLimit || outputLimit > configuration.maxOutputTokens()) {
            return failure(request, AiResponseStatus.BUDGET_EXCEEDED, providerId, model,
                    "The request exceeds the configured token budget.", started);
        }
        AiProviderAvailability availability = provider.availability();
        if (!availability.available()) {
            return failure(request, AiResponseStatus.PROVIDER_UNAVAILABLE, providerId, model,
                    availability.reason(), started);
        }
        return null;
    }

    private AiResponse executeWithRetries(AiRequest request, PilotConfiguration configuration, AiProvider provider,
                                          String providerId, String model, Instant started) {
        AiResponse last = null;
        for (int attempt = 1; attempt <= configuration.retryMaxAttempts(); attempt++) {
            try {
                last = provider.execute(request);
            } catch (RuntimeException exception) {
                last = failure(request, AiResponseStatus.ERROR, providerId, model,
                        "Provider execution failed.", started);
            }
            if (last.successful()) {
                List<String> schemaErrors = JsonSchemaValidator.validate(request.desiredResponseSchema(),
                        last.structuredPayload());
                if (!schemaErrors.isEmpty()) {
                    return failure(request, AiResponseStatus.INVALID_RESPONSE, providerId, model,
                            "Provider output did not match the requested schema.", started);
                }
                BigDecimal cost = last.usage().costUsd();
                BigDecimal costLimit = effectiveCostLimit(request, configuration);
                if (cost != null && costLimit.signum() > 0 && cost.compareTo(costLimit) > 0) {
                    return failure(request, AiResponseStatus.BUDGET_EXCEEDED, providerId, model,
                            "Provider-reported cost exceeded the configured budget.", started);
                }
                return last;
            }
            if (!isRetryable(last.status())) {
                return last;
            }
        }
        return last == null
                ? failure(request, AiResponseStatus.ERROR, providerId, model, "Provider did not return a result.", started)
                : last;
    }

    private AiResponse finish(AiRequest request, String provider, String model, String redactionSummary,
                              Instant started, AiResponse response) {
        Duration duration = elapsed(started);
        AiResponse normalized = new AiResponse(response.schemaVersion(), response.status(), provider,
                response.model().isBlank() ? model : response.model(), response.structuredPayload(),
                response.warnings(), duration, response.usage(), response.fallbackReason(),
                request.deterministicFallback());
        try {
            auditSink.record(new AiAuditEvent(Instant.now(), request.requestId(), request.purpose(), provider,
                    normalized.model(), redactionSummary, duration, normalized.status()));
            return normalized;
        } catch (RuntimeException auditFailure) {
            java.util.ArrayList<String> warnings = new java.util.ArrayList<>(normalized.warnings());
            warnings.add("Safe AI audit metadata could not be recorded.");
            return new AiResponse(normalized.schemaVersion(), normalized.status(), normalized.provider(),
                    normalized.model(), normalized.structuredPayload(), warnings, normalized.duration(),
                    normalized.usage(), normalized.fallbackReason(), normalized.deterministicFallback());
        }
    }

    private static AiResponse failure(AiRequest request, AiResponseStatus status, String provider, String model,
                                      String reason, Instant started) {
        return AiResponse.failure(status, provider, model, reason, elapsed(started), request.deterministicFallback());
    }

    private static Duration elapsed(Instant started) {
        return Duration.between(started, Instant.now());
    }

    private static Duration effectiveTimeout(AiRequest request, PilotConfiguration configuration) {
        return request.timeout().compareTo(configuration.timeout()) < 0 ? request.timeout() : configuration.timeout();
    }

    private static long inheritedLimit(long requested, long configured) {
        return requested <= 0 ? configured : Math.min(requested, configured);
    }

    private static BigDecimal effectiveCostLimit(AiRequest request, PilotConfiguration configuration) {
        BigDecimal requested = request.budget().maxCostUsd();
        BigDecimal configured = configuration.maxCostUsd();
        if (requested.signum() <= 0) {
            return configured;
        }
        if (configured.signum() <= 0) {
            return requested;
        }
        return requested.min(configured);
    }

    private static long requestSize(AiRequest request) {
        long size = request.text().getBytes(StandardCharsets.UTF_8).length;
        for (EvidenceReference evidence : request.evidence()) {
            size += evidence.content().getBytes(StandardCharsets.UTF_8).length;
        }
        for (AiImage image : request.images()) {
            size += image.data().length;
        }
        try {
            size += JSON.writeValueAsBytes(request.desiredResponseSchema()).length;
        } catch (JsonProcessingException ignored) {
            size += request.desiredResponseSchema().toString().getBytes(StandardCharsets.UTF_8).length;
        }
        return size;
    }

    private static String safeModel(PilotConfiguration configuration, String providerId) {
        try {
            return configuration.provider(providerId).model();
        } catch (IllegalArgumentException ignored) {
            return "";
        }
    }

    private static boolean isRetryable(AiResponseStatus status) {
        return status == AiResponseStatus.RATE_LIMITED
                || status == AiResponseStatus.TIMEOUT
                || status == AiResponseStatus.PROVIDER_UNAVAILABLE
                || status == AiResponseStatus.ERROR;
    }

    private static boolean isCircuitFailure(AiResponseStatus status) {
        return isRetryable(status) || status == AiResponseStatus.INVALID_RESPONSE;
    }

    private static final class CircuitState {
        private int consecutiveFailures;
        private Instant openedAt;

        synchronized boolean isOpen(Duration cooldown) {
            if (openedAt == null) {
                return false;
            }
            if (Duration.between(openedAt, Instant.now()).compareTo(cooldown) >= 0) {
                consecutiveFailures = 0;
                openedAt = null;
                return false;
            }
            return true;
        }

        synchronized void success() {
            consecutiveFailures = 0;
            openedAt = null;
        }

        synchronized void failure(int threshold) {
            consecutiveFailures++;
            if (consecutiveFailures >= threshold) {
                openedAt = Instant.now();
            }
        }
    }
}
