package com.shaft.ai.provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.config.ProviderConfiguration;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * Shared safe HTTP execution for direct providers.
 */
public abstract class AbstractHttpAiProvider implements AiProvider {
    protected static final ObjectMapper JSON = new ObjectMapper();
    private final HttpClient httpClient;
    private final Function<String, String> environment;

    /**
     * Creates a provider using the JDK HTTP client and process environment.
     */
    protected AbstractHttpAiProvider() {
        this(HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build(), System::getenv);
    }

    /**
     * Creates a provider with injectable HTTP and credential resolution.
     *
     * @param httpClient HTTP client
     * @param environment environment variable resolver
     */
    protected AbstractHttpAiProvider(HttpClient httpClient, Function<String, String> environment) {
        this.httpClient = Objects.requireNonNull(httpClient, "httpClient");
        this.environment = Objects.requireNonNull(environment, "environment");
    }

    /**
     * Checks model and credential configuration without contacting the provider.
     *
     * @return availability
     */
    @Override
    public AiProviderAvailability availability() {
        ProviderConfiguration configuration;
        try {
            configuration = PilotConfiguration.current().provider(id());
        } catch (RuntimeException exception) {
            return AiProviderAvailability.unavailable("Provider configuration is invalid.");
        }
        if (configuration.model().isBlank()) {
            return AiProviderAvailability.unavailable("Provider model is not configured.");
        }
        if (requiresCredential(configuration)) {
            String key = environment.apply(configuration.apiKeyEnvironmentVariable());
            if (key == null || key.isBlank()) {
                return AiProviderAvailability.unavailable("Provider credential environment variable is not set.");
            }
        }
        return AiProviderAvailability.ready();
    }

    /**
     * Executes an approved and redacted HTTP request.
     *
     * @param request approved and redacted request
     * @return normalized response
     */
    @Override
    public AiResponse execute(AiRequest request) {
        Instant started = Instant.now();
        ProviderConfiguration configuration = PilotConfiguration.current().provider(id());
        try {
            ObjectNode payload = buildPayload(request, configuration);
            HttpRequest.Builder builder = HttpRequest.newBuilder(endpoint(configuration))
                    .timeout(request.timeout())
                    .header("Accept", "application/json")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString(JSON.writeValueAsString(payload)));
            headers(configuration).forEach(builder::header);
            HttpResponse<String> response = httpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString());
            Duration duration = Duration.between(started, Instant.now());
            AiResponseStatus failureStatus = status(response.statusCode());
            if (failureStatus != null) {
                return AiResponse.failure(failureStatus, id(), configuration.model(),
                        safeHttpReason(failureStatus), duration, request.deterministicFallback());
            }
            JsonNode responseBody = JSON.readTree(response.body());
            JsonNode structuredPayload = parseStructuredPayload(responseBody);
            return AiResponse.success(id(), responseModel(responseBody, configuration), structuredPayload,
                    duration, usage(responseBody), request.deterministicFallback());
        } catch (java.net.http.HttpTimeoutException exception) {
            return AiResponse.failure(AiResponseStatus.TIMEOUT, id(), configuration.model(),
                    "Provider request timed out.", Duration.between(started, Instant.now()),
                    request.deterministicFallback());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return AiResponse.failure(AiResponseStatus.ERROR, id(), configuration.model(),
                    "Provider request was interrupted.", Duration.between(started, Instant.now()),
                    request.deterministicFallback());
        } catch (JsonProcessingException exception) {
            return AiResponse.failure(AiResponseStatus.INVALID_RESPONSE, id(), configuration.model(),
                    "Provider returned malformed JSON.", Duration.between(started, Instant.now()),
                    request.deterministicFallback());
        } catch (IOException exception) {
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, id(), configuration.model(),
                    "Provider could not be reached.", Duration.between(started, Instant.now()),
                    request.deterministicFallback());
        } catch (RuntimeException exception) {
            return AiResponse.failure(AiResponseStatus.INVALID_RESPONSE, id(), configuration.model(),
                    "Provider response could not be normalized.", Duration.between(started, Instant.now()),
                    request.deterministicFallback());
        }
    }

    /**
     * Returns provider capabilities.
     *
     * @return capabilities
     */
    @Override
    public abstract AiCapabilities capabilities();

    protected abstract ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration);

    protected abstract JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException;

    protected URI endpoint(ProviderConfiguration configuration) {
        return configuration.endpoint();
    }

    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of();
    }

    protected AiUsage usage(JsonNode response) {
        return AiUsage.empty();
    }

    protected String responseModel(JsonNode response, ProviderConfiguration configuration) {
        return response.path("model").asText(configuration.model());
    }

    protected boolean requiresCredential(ProviderConfiguration configuration) {
        return !configuration.apiKeyEnvironmentVariable().isBlank();
    }

    protected String credential(ProviderConfiguration configuration) {
        return environment.apply(configuration.apiKeyEnvironmentVariable());
    }

    protected ProcessingLocation processingLocation(ProcessingLocation fallback) {
        try {
            ProcessingLocation configured = PilotConfiguration.current().provider(id()).processingLocation();
            return configured == ProcessingLocation.NONE ? fallback : configured;
        } catch (RuntimeException exception) {
            return fallback;
        }
    }

    protected static long outputTokenLimit(AiRequest request, PilotConfiguration configuration) {
        long requested = request.budget().maxOutputTokens();
        return requested <= 0 ? configuration.maxOutputTokens() : Math.min(requested, configuration.maxOutputTokens());
    }

    protected static String prompt(AiRequest request) {
        StringBuilder prompt = new StringBuilder();
        prompt.append("Purpose: ").append(request.purpose()).append('\n');
        if (!request.text().isBlank()) {
            prompt.append(request.text());
        }
        request.evidence().forEach(evidence -> prompt.append("\n\nEvidence ")
                .append(evidence.id()).append(" [").append(evidence.category()).append("]:\n")
                .append(evidence.content()));
        return prompt.toString();
    }

    private static AiResponseStatus status(int statusCode) {
        if (statusCode >= 200 && statusCode < 300) {
            return null;
        }
        if (statusCode == 401 || statusCode == 403) {
            return AiResponseStatus.AUTHENTICATION_FAILED;
        }
        if (statusCode == 408 || statusCode == 504) {
            return AiResponseStatus.TIMEOUT;
        }
        if (statusCode == 429) {
            return AiResponseStatus.RATE_LIMITED;
        }
        if (statusCode >= 500) {
            return AiResponseStatus.PROVIDER_UNAVAILABLE;
        }
        return AiResponseStatus.ERROR;
    }

    private static String safeHttpReason(AiResponseStatus status) {
        return switch (status) {
            case AUTHENTICATION_FAILED -> "Provider authentication failed.";
            case RATE_LIMITED -> "Provider rate limit was reached.";
            case TIMEOUT -> "Provider request timed out.";
            case PROVIDER_UNAVAILABLE -> "Provider is unavailable.";
            default -> "Provider rejected the request.";
        };
    }
}
