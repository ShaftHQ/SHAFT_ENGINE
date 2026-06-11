package com.shaft.pilot.config;

import java.net.URI;
import java.util.Map;
import java.util.Objects;

/**
 * Configuration for one direct provider.
 *
 * @param id provider identifier
 * @param endpoint provider endpoint
 * @param model model identifier
 * @param apiKeyEnvironmentVariable environment variable containing credentials, or blank for local providers
 * @param options non-secret provider protocol options
 */
public record ProviderConfiguration(
        String id,
        URI endpoint,
        String model,
        String apiKeyEnvironmentVariable,
        Map<String, String> options) {
    /**
     * Creates validated provider configuration.
     */
    public ProviderConfiguration {
        id = Objects.requireNonNull(id, "id");
        endpoint = Objects.requireNonNull(endpoint, "endpoint");
        model = Objects.requireNonNullElse(model, "").trim();
        apiKeyEnvironmentVariable = Objects.requireNonNullElse(apiKeyEnvironmentVariable, "").trim();
        options = options == null ? Map.of() : Map.copyOf(options);
    }

    /**
     * Returns a non-secret provider option.
     *
     * @param name option name
     * @return option value, or an empty string
     */
    public String option(String name) {
        return options.getOrDefault(name, "");
    }
}
