package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Arrays;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpEnvironmentTest {
    @Test
    void javaToolOptionsEnableSelectedCloudProvider() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "github";
        settings.cloudModel = "openai/gpt-4.1";
        settings.passProviderApiKeysToMcp = false;

        Map<String, String> environment = ShaftMcpEnvironment.forSettings(settings);

        String options = environment.get("JAVA_TOOL_OPTIONS");
        assertTrue(options.contains("-Dpilot.ai.enabled=true"));
        assertTrue(options.contains("-Dpilot.ai.provider=github"));
        assertTrue(options.contains("-Dpilot.ai.consent.remote=true"));
        assertTrue(options.contains("-Dpilot.ai.github.model=openai/gpt-4.1"));
        assertFalse(environment.containsKey("OPENAI_API_KEY"));
    }

    @Test
    void javaToolOptionsUseGeminiCloudDefaults() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "CLOUD";
        settings.passProviderApiKeysToMcp = false;

        Map<String, String> environment = ShaftMcpEnvironment.forSettings(settings);

        String options = environment.get("JAVA_TOOL_OPTIONS");
        assertTrue(options.contains("-Dpilot.ai.enabled=true"));
        assertTrue(options.contains("-Dpilot.ai.provider=gemini"));
        assertTrue(options.contains("-Dpilot.ai.consent.remote=true"));
        assertTrue(options.contains("-Dpilot.ai.gemini.model=gemini-3.5-flash"));
        assertFalse(environment.containsKey("GEMINI_API_KEY"));
    }

    @Test
    void selectedProviderKeyNameIsStable() {
        assertEquals("GEMINI_API_KEY", ShaftMcpEnvironment.providerKeyName("gemini"));
        assertEquals("GITHUB_TOKEN", ShaftMcpEnvironment.providerKeyName("github"));
        assertEquals("ANTHROPIC_API_KEY", ShaftMcpEnvironment.providerKeyName("anthropic"));
        assertEquals("", ShaftMcpEnvironment.providerKeyName("none"));
    }

    @Test
    void selectedEnvironmentVariableIsForwardedWithoutPasswordSafeFallback() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        settings.passProviderApiKeysToMcp = true;
        settings.setProviderApiKeyEnvironmentVariable("gemini", "GOOGLE_API_KEY");

        Map<String, String> environment = ShaftMcpEnvironment.forSettings(settings, ignored -> "stored-secret",
                key -> "GOOGLE_API_KEY".equals(key) ? "environment-secret" : "");

        assertEquals("environment-secret", environment.get("GOOGLE_API_KEY"));
        assertFalse(environment.containsKey("GEMINI_API_KEY"));
        assertTrue(environment.get("JAVA_TOOL_OPTIONS")
                .contains("-Dpilot.ai.gemini.apiKeyEnvironmentVariable=GOOGLE_API_KEY"));
    }

    @Test
    void localRoutePropagatesOnlyItsEndpointModelAndPasswordSafeToken() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "LOCAL";
        settings.pilotAiProvider = "lmstudio";
        assertTrue(Arrays.stream(ShaftSettingsState.Settings.class.getFields())
                .anyMatch(field -> "pilotAiEndpoint".equals(field.getName())), "local endpoint must be persisted separately");
        set(settings, "lmStudioEndpoint", "http://127.0.0.1:1234/v1/responses");
        settings.pilotAiModel = "qwen3";
        settings.passProviderApiKeysToMcp = true;

        Map<String, String> environment = environment(settings,
                key -> "LMSTUDIO_API_KEY".equals(key) ? "local-secret" : "other-secret");

        String options = environment.get("JAVA_TOOL_OPTIONS");
        assertTrue(options.contains("-Dpilot.ai.provider=lmstudio"));
        assertTrue(options.contains("-Dpilot.ai.consent.local=true"));
        assertTrue(options.contains("-Dpilot.ai.lmstudio.endpoint=http://127.0.0.1:1234/v1/responses"));
        assertTrue(options.contains("-Dpilot.ai.lmstudio.model=qwen3"));
        assertTrue(options.contains("-Dpilot.ai.lmstudio.apiKeyEnvironmentVariable=LMSTUDIO_API_KEY"));
        assertFalse(options.contains("local-secret"));
        assertEquals("local-secret", environment.get("LMSTUDIO_API_KEY"));
        assertFalse(environment.containsKey("OPENAI_API_KEY"));
        assertFalse(environment.containsKey("GEMINI_API_KEY"));
    }

    @Test
    void unsafeLocalEndpointDoesNotProduceAnEnvironment() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.pilotAiProvider = "ollama";
        set(settings, "pilotAiEndpoint", "https://example.test/api/chat");

        assertEquals(Map.of(), ShaftMcpEnvironment.forSettings(settings, ignored -> "secret"));
    }

    @Test
    void localDefaultsForwardProviderChatEndpoints() {
        ShaftSettingsState.Settings ollama = new ShaftSettingsState.Settings();
        ollama.pilotAiProvider = "ollama";
        String ollamaOptions = ShaftMcpEnvironment.forSettings(ollama, ignored -> "").get("JAVA_TOOL_OPTIONS");

        ShaftSettingsState.Settings lmStudio = new ShaftSettingsState.Settings();
        lmStudio.pilotAiProvider = "lmstudio";
        set(lmStudio, "pilotAiEndpoint", ShaftSettingsState.defaultLocalEndpoint("lmstudio"));
        String lmStudioOptions = ShaftMcpEnvironment.forSettings(lmStudio, ignored -> "").get("JAVA_TOOL_OPTIONS");

        assertTrue(ollamaOptions.contains("-Dpilot.ai.ollama.endpoint=http://127.0.0.1:11434/api/chat"));
        assertTrue(lmStudioOptions.contains("-Dpilot.ai.lmstudio.endpoint=http://127.0.0.1:1234/v1/responses"));
    }

    @Test
    void freshLmStudioSelectionUsesItsOwnEndpointInsteadOfTheOllamaEndpoint() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.pilotAiProvider = "lmstudio";

        String options = ShaftMcpEnvironment.forSettings(settings, ignored -> "").get("JAVA_TOOL_OPTIONS");

        assertTrue(options.contains("-Dpilot.ai.lmstudio.endpoint=http://127.0.0.1:1234/v1/responses"));
        assertFalse(options.contains("/api/chat"));
    }

    @Test
    void childEnvironmentStripsInheritedProviderCredentialsUnlessConfigured() {
        Map<String, String> inherited = Map.of("OPENAI_API_KEY", "host-openai", "Google_Api_Key", "host-google",
                "GITLAB_TOKEN", "host-gitlab", "HF_TOKEN", "host-hf", "NPM_TOKEN", "host-npm", "PATH", "safe-path");

        Map<String, String> withoutOptIn = ShaftMcpEnvironment.childEnvironment(inherited, Map.of());
        Map<String, String> optedIn = ShaftMcpEnvironment.childEnvironment(inherited,
                Map.of("OPENAI_API_KEY", "selected-key"));

        assertEquals(Map.of("PATH", "safe-path"), withoutOptIn);
        assertEquals("selected-key", optedIn.get("OPENAI_API_KEY"));
        assertFalse(optedIn.containsKey("GITLAB_TOKEN"));
        assertFalse(optedIn.containsKey("HF_TOKEN"));
        assertFalse(optedIn.containsKey("NPM_TOKEN"));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> environment(ShaftSettingsState.Settings settings, Function<String, String> lookup) {
        try {
            Method method = ShaftMcpEnvironment.class.getDeclaredMethod("forSettings",
                    ShaftSettingsState.Settings.class, Function.class);
            return (Map<String, String>) method.invoke(null, settings, lookup);
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError("settings transport must accept a Password Safe credential lookup", exception);
        }
    }

    private static void set(ShaftSettingsState.Settings settings, String fieldName, String value) {
        try {
            Field field = ShaftSettingsState.Settings.class.getField(fieldName);
            field.set(settings, value);
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError("missing persisted route metadata", exception);
        }
    }
}
