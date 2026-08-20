package com.shaft.intellij.settings;

import java.util.Locale;

/**
 * One user-facing agent choice and its complete execution/MCP-install route.
 *
 * <p>Legacy family/runtime settings remain the persisted compatibility format, but UI callers
 * select this value so the configured assistant and installer destination cannot diverge.</p>
 */
public enum AssistantAgentRoute {
    CLAUDE_CODE("Claude Code", "LOCAL", "CLAUDE", "CLI", "CLAUDE_CODE", "CLAUDE_CODE"),
    CLAUDE_DESKTOP("Claude Desktop", "LOCAL", "CLAUDE", "DESKTOP_APP", "CLAUDE_CODE", "CLAUDE_DESKTOP"),
    CODEX_CLI("Codex CLI", "LOCAL", "CODEX", "CLI", "CODEX", "CODEX"),
    GEMINI_INTELLIJ("Gemini in IntelliJ", "CLOUD", "GEMINI", "IDE_PLUGIN", "CODEX", "INTELLIJ_PLUGIN"),
    GROK("Grok CLI", "LOCAL", "GROK", "CLI", "GROK", "GROK"),
    COPILOT_CLI("GitHub Copilot CLI", "LOCAL", "COPILOT", "CLI", "COPILOT_CLI", "COPILOT_CLI"),
    COPILOT_INTELLIJ("GitHub Copilot in IntelliJ", "LOCAL", "COPILOT", "IDE_PLUGIN", "COPILOT_CLI", "COPILOT_INTELLIJ");

    private final String displayName;
    private final String providerType;
    private final String family;
    private final String runtime;
    private final String client;
    private final String installerTarget;

    AssistantAgentRoute(String displayName, String providerType, String family, String runtime,
                        String client, String installerTarget) {
        this.displayName = displayName;
        this.providerType = providerType;
        this.family = family;
        this.runtime = runtime;
        this.client = client;
        this.installerTarget = installerTarget;
    }

    public String displayName() {
        return displayName;
    }

    public String providerType() {
        return providerType;
    }

    public String family() {
        return family;
    }

    public String runtime() {
        return runtime;
    }

    public String client() {
        return client;
    }

    public String installerTarget() {
        return installerTarget;
    }

    public boolean cli() {
        return "CLI".equals(runtime);
    }

    public boolean gemini() {
        return this == GEMINI_INTELLIJ;
    }

    public void applyTo(ShaftSettingsState.Settings settings) {
        settings.assistantProviderType = providerType;
        if (gemini()) {
            settings.cloudProvider = "gemini";
            settings.passProviderApiKeysToMcp = true;
            return;
        }
        settings.assistantFamily = family;
        settings.assistantRuntime = runtime;
        settings.defaultAutobotClient = client;
    }

    public static AssistantAgentRoute fromSettings(ShaftSettingsState.Settings settings) {
        String provider = normalize(settings.assistantProviderType, "LOCAL");
        String cloud = normalize(settings.cloudProvider, "");
        if ("CLOUD".equals(provider) && "GEMINI".equals(cloud)) {
            return GEMINI_INTELLIJ;
        }
        String family = normalize(settings.assistantFamily, "");
        String client = settings.defaultAutobotClient == null ? "" : settings.defaultAutobotClient.trim();
        if (family.isBlank()) {
            family = switch (client.toUpperCase(Locale.ROOT)) {
                case "CLAUDE_CODE" -> "CLAUDE";
                case "COPILOT_CLI" -> "COPILOT";
                case "GROK" -> "GROK";
                case "CODEX" -> "CODEX";
                default -> "";
            };
        }
        if (family.isBlank()) {
            return null;
        }
        String runtime = normalize(settings.assistantRuntime, "CLI");
        return switch (family) {
            case "CLAUDE" -> "DESKTOP_APP".equals(runtime) ? CLAUDE_DESKTOP : CLAUDE_CODE;
            case "COPILOT" -> "IDE_PLUGIN".equals(runtime) ? COPILOT_INTELLIJ : COPILOT_CLI;
            case "GROK" -> GROK;
            case "CODEX" -> CODEX_CLI;
            default -> null;
        };
    }

    private static String normalize(String value, String fallback) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
        return normalized.isBlank() ? fallback : normalized;
    }

    @Override
    public String toString() {
        return displayName;
    }
}
