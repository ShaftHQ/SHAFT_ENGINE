package com.shaft.pilot.config;

import com.shaft.driver.SHAFT;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.security.RedactionPolicy;

import java.math.BigDecimal;
import java.net.URI;
import java.time.Duration;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Immutable snapshot of effective current-thread Pilot configuration.
 *
 * @param enabled whether AI execution is enabled
 * @param provider selected provider identifier
 * @param approvalPolicy global approval policy
 * @param telemetryEnabled whether optional external telemetry is enabled
 * @param timeout maximum provider timeout
 * @param maxRequestBytes maximum serialized request size
 * @param maxInputTokens maximum estimated input tokens
 * @param maxOutputTokens maximum output tokens
 * @param maxCostUsd maximum accepted provider-reported cost
 * @param retryMaxAttempts maximum execution attempts
 * @param maxConcurrency maximum concurrent calls per provider
 * @param circuitBreakerFailureThreshold failures before opening the circuit
 * @param circuitBreakerCooldown circuit-open cooldown
 * @param redactionPolicy effective redaction policy
 * @param providers provider-specific endpoint, model, and credential-source configuration
 */
public record PilotConfiguration(
        boolean enabled,
        String provider,
        ApprovalPolicy approvalPolicy,
        boolean telemetryEnabled,
        Duration timeout,
        int maxRequestBytes,
        long maxInputTokens,
        long maxOutputTokens,
        BigDecimal maxCostUsd,
        int retryMaxAttempts,
        int maxConcurrency,
        int circuitBreakerFailureThreshold,
        Duration circuitBreakerCooldown,
        RedactionPolicy redactionPolicy,
        Map<String, ProviderConfiguration> providers) {
    /**
     * Reads a current-thread configuration snapshot through {@link SHAFT.Properties}.
     *
     * @return effective configuration
     */
    public static PilotConfiguration current() {
        var properties = SHAFT.Properties.pilot;
        Set<EvidenceCategory> categories = parseCategories(properties.allowedEvidenceCategories());
        RedactionPolicy redactionPolicy = new RedactionPolicy(
                split(properties.redactionAttributes(), ",").stream().collect(Collectors.toUnmodifiableSet()),
                split(properties.redactionSelectors(), ","),
                split(properties.redactionPatterns(), ";;"));
        Map<String, ProviderConfiguration> providers = Map.of(
                "openai", provider("openai", properties.openAiEndpoint(), properties.openAiModel(),
                        properties.openAiApiKeyEnvironmentVariable(),
                        parseLocation(properties.openAiProcessingLocation(), ProcessingLocation.REMOTE)),
                "anthropic", provider("anthropic", properties.anthropicEndpoint(), properties.anthropicModel(),
                        properties.anthropicApiKeyEnvironmentVariable(),
                        parseLocation(properties.anthropicProcessingLocation(), ProcessingLocation.REMOTE),
                        Map.of("api-version", properties.anthropicVersion())),
                "gemini", provider("gemini", properties.geminiEndpoint(), properties.geminiModel(),
                        properties.geminiApiKeyEnvironmentVariable(),
                        parseLocation(properties.geminiProcessingLocation(), ProcessingLocation.REMOTE)),
                "ollama", provider("ollama", properties.ollamaEndpoint(), properties.ollamaModel(),
                        properties.ollamaApiKeyEnvironmentVariable(),
                        parseLocation(properties.ollamaProcessingLocation(), ProcessingLocation.LOCAL),
                        Map.of(
                                "api-key-header", properties.ollamaApiKeyHeader(),
                                "api-key-prefix", properties.ollamaApiKeyPrefix())));
        return new PilotConfiguration(
                properties.enabled(),
                normalize(properties.provider()),
                new ApprovalPolicy(
                        properties.localConsent(),
                        properties.onPremConsent(),
                        properties.remoteConsent(),
                        categories),
                properties.telemetryEnabled(),
                Duration.ofSeconds(positive(properties.timeoutSeconds(), 30)),
                positive(properties.maxRequestBytes(), 1_048_576),
                positive(properties.maxInputTokens(), 16_000),
                positive(properties.maxOutputTokens(), 2_000),
                decimal(properties.maxCostUsd()),
                positive(properties.retryMaxAttempts(), 1),
                positive(properties.maxConcurrency(), 1),
                positive(properties.circuitBreakerFailureThreshold(), 1),
                Duration.ofSeconds(positive(properties.circuitBreakerCooldownSeconds(), 60)),
                redactionPolicy,
                providers);
    }

    /**
     * Returns configuration for a provider.
     *
     * @param providerId provider identifier
     * @return provider configuration
     * @throws IllegalArgumentException when the provider is unknown
     */
    public ProviderConfiguration provider(String providerId) {
        ProviderConfiguration configuration = providers.get(normalize(providerId));
        if (configuration == null) {
            throw new IllegalArgumentException("Unknown AI provider: " + providerId);
        }
        return configuration;
    }

    private static ProviderConfiguration provider(
            String id,
            String endpoint,
            String model,
            String environmentVariable,
            ProcessingLocation processingLocation) {
        return provider(id, endpoint, model, environmentVariable, processingLocation, Map.of());
    }

    private static ProviderConfiguration provider(
            String id,
            String endpoint,
            String model,
            String environmentVariable,
            ProcessingLocation processingLocation,
            Map<String, String> options) {
        return new ProviderConfiguration(
                id, URI.create(endpoint), model, environmentVariable, processingLocation, options);
    }

    private static Set<EvidenceCategory> parseCategories(String value) {
        EnumSet<EvidenceCategory> categories = EnumSet.noneOf(EvidenceCategory.class);
        for (String item : split(value, ",")) {
            try {
                categories.add(EvidenceCategory.valueOf(item.toUpperCase(Locale.ROOT)));
            } catch (IllegalArgumentException ignored) {
                // Unknown categories are denied rather than becoming implicitly approved.
            }
        }
        return Set.copyOf(categories);
    }

    private static List<String> split(String value, String delimiter) {
        if (value == null || value.isBlank()) {
            return List.of();
        }
        return Arrays.stream(value.split(java.util.regex.Pattern.quote(delimiter)))
                .map(String::trim)
                .filter(item -> !item.isEmpty())
                .toList();
    }

    private static int positive(int value, int fallback) {
        return value > 0 ? value : fallback;
    }

    private static BigDecimal decimal(String value) {
        try {
            BigDecimal parsed = new BigDecimal(value);
            return parsed.signum() < 0 ? BigDecimal.ZERO : parsed;
        } catch (NumberFormatException ignored) {
            return BigDecimal.ZERO;
        }
    }

    private static String normalize(String value) {
        return value == null ? "none" : value.trim().toLowerCase(Locale.ROOT);
    }

    private static ProcessingLocation parseLocation(
            String value,
            ProcessingLocation fallback) {
        try {
            return ProcessingLocation.valueOf(
                    normalize(value).replace('-', '_').toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            return fallback;
        }
    }
}
