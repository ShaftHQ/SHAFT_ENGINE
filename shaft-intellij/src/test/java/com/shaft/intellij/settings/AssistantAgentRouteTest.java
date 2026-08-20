package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class AssistantAgentRouteTest {

    @Test
    void routesAreAlphabeticalAndOwnTheirInstallerDestinations() throws Exception {
        Class<?> type = Class.forName("com.shaft.intellij.settings.AssistantAgentRoute");
        Object[] routes = type.getEnumConstants();
        Method displayName = type.getMethod("displayName");
        Method installerTarget = type.getMethod("installerTarget");

        assertEquals(List.of(
                        "Claude Code", "Claude Desktop", "Codex CLI", "Gemini in IntelliJ",
                        "Grok CLI", "GitHub Copilot CLI", "GitHub Copilot in IntelliJ"),
                Arrays.stream(routes).map(route -> String.valueOf(invoke(displayName, route))).toList());
        assertEquals(List.of(
                        "CLAUDE_CODE", "CLAUDE_DESKTOP", "CODEX", "INTELLIJ_PLUGIN",
                        "GROK", "COPILOT_CLI", "COPILOT_INTELLIJ"),
                Arrays.stream(routes).map(route -> String.valueOf(invoke(installerTarget, route))).toList());
    }

    @Test
    void legacySettingsMigrateToTheExactAgentRoute() throws Exception {
        Class<?> type = Class.forName("com.shaft.intellij.settings.AssistantAgentRoute");
        Method fromSettings = type.getMethod("fromSettings", ShaftSettingsState.Settings.class);

        assertEquals("CLAUDE_DESKTOP", routeName(fromSettings, settings("LOCAL", "CLAUDE", "DESKTOP_APP")));
        assertEquals("COPILOT_INTELLIJ", routeName(fromSettings, settings("LOCAL", "COPILOT", "IDE_PLUGIN")));
        assertEquals("GEMINI_INTELLIJ", routeName(fromSettings, settings("CLOUD", "CODEX", "CLI")));
        assertEquals("CODEX_CLI", routeName(fromSettings, settings("LOCAL", "CODEX", "CLI")));
        assertEquals("CLAUDE_CODE", routeName(fromSettings, settings("LOCAL", "CLAUDE", "CLI")));
        assertEquals("COPILOT_CLI", routeName(fromSettings, settings("LOCAL", "COPILOT", "CLI")));
        assertEquals("GROK", routeName(fromSettings, settings("LOCAL", "GROK", "CLI")));
    }

    @Test
    void emptyFamilyAndClientDoNotCoerceToCodex() throws Exception {
        Class<?> type = Class.forName("com.shaft.intellij.settings.AssistantAgentRoute");
        Method fromSettings = type.getMethod("fromSettings", ShaftSettingsState.Settings.class);
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = "";
        settings.defaultAutobotClient = "";
        assertNull(invoke(fromSettings, null, settings));
    }

    @Test
    void everyRouteRoundTripsThroughLegacyPersistence() throws Exception {
        Class<?> type = Class.forName("com.shaft.intellij.settings.AssistantAgentRoute");
        Method applyTo = type.getMethod("applyTo", ShaftSettingsState.Settings.class);
        Method fromSettings = type.getMethod("fromSettings", ShaftSettingsState.Settings.class);

        for (Object route : type.getEnumConstants()) {
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            invoke(applyTo, route, settings);
            assertEquals(route, invoke(fromSettings, null, settings), String.valueOf(route));
        }
    }

    private static ShaftSettingsState.Settings settings(String providerType, String family, String runtime) {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.assistantProviderType = providerType;
        settings.assistantFamily = family;
        settings.assistantRuntime = runtime;
        settings.cloudProvider = "gemini";
        return settings;
    }

    private static String routeName(Method fromSettings, ShaftSettingsState.Settings settings) {
        return ((Enum<?>) invoke(fromSettings, null, settings)).name();
    }

    private static Object invoke(Method method, Object target, Object... arguments) {
        try {
            return method.invoke(target, arguments);
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
    }
}
