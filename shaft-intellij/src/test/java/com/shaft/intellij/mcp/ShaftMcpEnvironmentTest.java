package com.shaft.intellij.mcp;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.util.Map;

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
}
