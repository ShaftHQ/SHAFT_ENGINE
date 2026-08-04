package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;

final class ShaftMcpEnvironment {
    private static final List<String> PROVIDER_CREDENTIALS = List.of(
            "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GEMINI_API_KEY", "GITHUB_TOKEN",
            "GITLAB_TOKEN", "HF_TOKEN", "HUGGINGFACE_HUB_TOKEN", "NPM_TOKEN", "OLLAMA_API_KEY", "LMSTUDIO_API_KEY");
    private ShaftMcpEnvironment() {
        throw new IllegalStateException("Utility class");
    }

    static Map<String, String> forSettings(ShaftSettingsState.Settings settings) {
        return forSettings(settings, key -> ShaftCredentialService.getInstance().apiKey(key));
    }

    static Map<String, String> forSettings(ShaftSettingsState.Settings settings, Function<String, String> credentialLookup) {
        String provider = selectedProvider(settings);
        if (isLocalProvider(provider) && !selectedEndpoint(settings).isBlank()
                && !ShaftSettingsState.validLocalEndpoint(provider, selectedEndpoint(settings))) {
            return Map.of();
        }
        Map<String, String> environment = providerKeys(settings != null && settings.passProviderApiKeysToMcp, provider,
                credentialLookup);
        addPilotOptions(environment, settings);
        return environment.isEmpty() ? Map.of() : Map.copyOf(environment);
    }

    static Map<String, String> providerKeys(boolean passProviderKeys) {
        return providerKeys(passProviderKeys, "");
    }

    static Map<String, String> providerKeys(boolean passProviderKeys, String provider) {
        return providerKeys(passProviderKeys, provider, key -> ShaftCredentialService.getInstance().apiKey(key));
    }

    static Map<String, String> childEnvironment(Map<String, String> inherited, Map<String, String> configured) {
        Map<String, String> environment = new LinkedHashMap<>(inherited == null ? Map.of() : inherited);
        PROVIDER_CREDENTIALS.forEach(environment::remove);
        if (configured != null) {
            environment.putAll(configured);
        }
        return environment;
    }

    private static Map<String, String> providerKeys(boolean passProviderKeys, String provider,
                                                     Function<String, String> credentialLookup) {
        if (!passProviderKeys) {
            return new LinkedHashMap<>();
        }
        String keyName = providerKeyName(provider);
        if (!keyName.isBlank()) {
            Map<String, String> environment = new LinkedHashMap<>();
            putIfPresent(environment, keyName, credentialLookup.apply(keyName));
            return environment;
        }
        Map<String, String> environment = new LinkedHashMap<>();
        putIfPresent(environment, "OPENAI_API_KEY", credentialLookup.apply("OPENAI_API_KEY"));
        putIfPresent(environment, "ANTHROPIC_API_KEY", credentialLookup.apply("ANTHROPIC_API_KEY"));
        putIfPresent(environment, "GEMINI_API_KEY", credentialLookup.apply("GEMINI_API_KEY"));
        putIfPresent(environment, "GITHUB_TOKEN", credentialLookup.apply("GITHUB_TOKEN"));
        return environment;
    }

    static String providerKeyName(String provider) {
        return switch (normalize(provider)) {
            case "openai" -> "OPENAI_API_KEY";
            case "anthropic" -> "ANTHROPIC_API_KEY";
            case "gemini" -> "GEMINI_API_KEY";
            case "github" -> "GITHUB_TOKEN";
            case "lmstudio" -> "LMSTUDIO_API_KEY";
            case "ollama" -> "OLLAMA_API_KEY";
            default -> "";
        };
    }

    private static void addPilotOptions(Map<String, String> environment, ShaftSettingsState.Settings settings) {
        String provider = selectedProvider(settings);
        if (provider.isBlank() || "none".equals(provider)) {
            return;
        }
        List<String> options = new ArrayList<>();
        options.add("-Dpilot.ai.enabled=true");
        options.add("-Dpilot.ai.provider=" + provider);
        if (isLocalProvider(provider)) {
            options.add("-Dpilot.ai.consent.local=true");
        } else {
            options.add("-Dpilot.ai.consent.remote=true");
        }
        String model = selectedModel(settings);
        if (!model.isBlank()) {
            options.add("-Dpilot.ai." + provider + ".model=" + model);
        }
        if (isLocalProvider(provider)) {
            String endpoint = selectedEndpoint(settings);
            if (!endpoint.isBlank()) {
                options.add("-Dpilot.ai." + provider + ".endpoint=" + endpoint);
            }
            options.add("-Dpilot.ai." + provider + ".apiKeyEnvironmentVariable=" + providerKeyName(provider));
        }
        String existing = environment.getOrDefault("JAVA_TOOL_OPTIONS", System.getenv("JAVA_TOOL_OPTIONS"));
        environment.put("JAVA_TOOL_OPTIONS", (existing == null || existing.isBlank())
                ? String.join(" ", options)
                : existing + " " + String.join(" ", options));
    }

    private static void putIfPresent(Map<String, String> environment, String key, String value) {
        if (value != null && !value.isBlank()) {
            environment.put(key, value);
        }
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private static String selectedProvider(ShaftSettingsState.Settings settings) {
        if (settings == null) {
            return "";
        }
        if ("CLOUD".equalsIgnoreCase(settings.assistantProviderType)) {
            return normalize(settings.cloudProvider);
        }
        return normalize(settings.pilotAiProvider);
    }

    private static String selectedModel(ShaftSettingsState.Settings settings) {
        if (settings == null) {
            return "";
        }
        String model = "CLOUD".equalsIgnoreCase(settings.assistantProviderType) ? settings.cloudModel : settings.pilotAiModel;
        return model == null ? "" : model.trim();
    }

    private static String selectedEndpoint(ShaftSettingsState.Settings settings) {
        return settings == null ? "" : settings.localEndpointFor(selectedProvider(settings));
    }

    private static boolean isLocalProvider(String provider) {
        return "ollama".equals(provider) || "lmstudio".equals(provider);
    }
}
