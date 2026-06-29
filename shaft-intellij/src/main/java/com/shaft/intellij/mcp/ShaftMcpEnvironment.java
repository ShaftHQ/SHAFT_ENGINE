package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

final class ShaftMcpEnvironment {
    private ShaftMcpEnvironment() {
        throw new IllegalStateException("Utility class");
    }

    static Map<String, String> forSettings(ShaftSettingsState.Settings settings) {
        String provider = selectedProvider(settings);
        Map<String, String> environment = providerKeys(settings != null && settings.passProviderApiKeysToMcp, provider);
        addPilotOptions(environment, settings);
        return environment.isEmpty() ? Map.of() : Map.copyOf(environment);
    }

    static Map<String, String> providerKeys(boolean passProviderKeys) {
        return providerKeys(passProviderKeys, "");
    }

    static Map<String, String> providerKeys(boolean passProviderKeys, String provider) {
        if (!passProviderKeys) {
            return new LinkedHashMap<>();
        }
        String keyName = providerKeyName(provider);
        if (!keyName.isBlank()) {
            ShaftCredentialService credentials = ShaftCredentialService.getInstance();
            Map<String, String> environment = new LinkedHashMap<>();
            putIfPresent(environment, keyName, credentials.apiKey(keyName));
            return environment;
        }
        ShaftCredentialService credentials = ShaftCredentialService.getInstance();
        Map<String, String> environment = new LinkedHashMap<>();
        putIfPresent(environment, "OPENAI_API_KEY", credentials.apiKey("OPENAI_API_KEY"));
        putIfPresent(environment, "ANTHROPIC_API_KEY", credentials.apiKey("ANTHROPIC_API_KEY"));
        putIfPresent(environment, "GEMINI_API_KEY", credentials.apiKey("GEMINI_API_KEY"));
        putIfPresent(environment, "GITHUB_TOKEN", credentials.apiKey("GITHUB_TOKEN"));
        return environment;
    }

    static String providerKeyName(String provider) {
        return switch (normalize(provider)) {
            case "openai" -> "OPENAI_API_KEY";
            case "anthropic" -> "ANTHROPIC_API_KEY";
            case "gemini" -> "GEMINI_API_KEY";
            case "github" -> "GITHUB_TOKEN";
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
        if ("ollama".equals(provider)) {
            options.add("-Dpilot.ai.consent.local=true");
        } else {
            options.add("-Dpilot.ai.consent.remote=true");
        }
        String model = selectedModel(settings);
        if (!model.isBlank()) {
            options.add("-Dpilot.ai." + provider + ".model=" + model);
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
}
