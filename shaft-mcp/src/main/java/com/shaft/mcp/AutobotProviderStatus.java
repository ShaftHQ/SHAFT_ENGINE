package com.shaft.mcp;

import java.util.List;

/**
 * Read-only readiness snapshot for the configured SHAFT cloud provider.
 *
 * <p>Never exposes the API key value; only reports whether a key is present in the process environment so the
 * IntelliJ readiness view can show an at-a-glance status.</p>
 *
 * @param schemaVersion response schema version
 * @param provider normalized provider identifier
 * @param model configured model, or blank when none is set
 * @param apiKeyPresent whether the provider API key is present in the environment
 * @param apiKeyEnvironmentVariable the environment variable inspected for the key
 * @param structuredOutputSupported whether the provider supports JSON-schema structured output
 * @param supportedModes assistant modes the provider can serve
 * @param warnings safe readiness reminders
 */
public record AutobotProviderStatus(
        String schemaVersion,
        String provider,
        String model,
        boolean apiKeyPresent,
        String apiKeyEnvironmentVariable,
        boolean structuredOutputSupported,
        String supportedModes,
        List<String> warnings) {
    /**
     * Creates an immutable provider status.
     */
    public AutobotProviderStatus {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
        provider = provider == null ? "" : provider.trim();
        model = model == null ? "" : model.trim();
        apiKeyEnvironmentVariable = apiKeyEnvironmentVariable == null ? "" : apiKeyEnvironmentVariable.trim();
        supportedModes = supportedModes == null || supportedModes.isBlank() ? "ASK, PLAN" : supportedModes.trim();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
