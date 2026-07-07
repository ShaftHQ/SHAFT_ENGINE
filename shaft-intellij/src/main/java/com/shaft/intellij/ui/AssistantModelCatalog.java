package com.shaft.intellij.ui;

import java.util.List;
import java.util.Locale;

/**
 * Curated model and effort-level choices for the Assistant chat view.
 *
 * <p>Live CLI listings win when a local agent can report its own models; these lists keep the
 * model selector useful when a cloud provider or CLI cannot list models. Model combos stay
 * editable so newer model names can always be typed in.</p>
 */
final class AssistantModelCatalog {
    static final String DEFAULT_EFFORT = "DEFAULT";
    private static final List<String> EFFORT_LEVELS = List.of(DEFAULT_EFFORT, "LOW", "MEDIUM", "HIGH");
    private static final List<String> CLAUDE_MODELS =
            List.of("claude-fable-5", "claude-opus-4-8", "claude-sonnet-5", "claude-haiku-4-5");

    private AssistantModelCatalog() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the suggested models for a cloud provider selected in the Assistant chat view.
     *
     * @param provider cloud provider identifier: gemini, openai, anthropic, or github
     * @return non-empty model list, most capable defaults first
     */
    static List<String> cloudModels(String provider) {
        return switch (normalizeLower(provider)) {
            case "openai" -> List.of("gpt-5.2", "gpt-5.2-codex", "gpt-5.1", "gpt-5.1-mini");
            case "anthropic" -> CLAUDE_MODELS;
            case "github" -> List.of("openai/gpt-4.1", "openai/gpt-4.1-mini", "openai/gpt-4o");
            default -> List.of("gemini-3.5-flash", "gemini-2.5-flash", "gemini-2.5-pro", "gemini-flash-latest");
        };
    }

    /**
     * Returns the fallback models for a local agent family when its CLI cannot report a model
     * list of its own.
     *
     * @param family local assistant family: CODEX, CLAUDE, or COPILOT
     * @return non-empty model list, most capable defaults first
     */
    static List<String> localModels(String family) {
        return switch (normalizeUpper(family)) {
            case "CLAUDE" -> CLAUDE_MODELS;
            case "COPILOT" -> List.of("gpt-5.2", "claude-sonnet-5", "gemini-3.5-pro");
            default -> List.of("gpt-5.2-codex", "gpt-5.2", "gpt-5.1-codex-mini");
        };
    }

    /**
     * Returns the default model preselected for a cloud provider.
     *
     * @param provider cloud provider identifier
     * @return first catalog model for the provider
     */
    static String defaultCloudModel(String provider) {
        return cloudModels(provider).get(0);
    }

    /**
     * Returns the selectable reasoning-effort levels, with {@link #DEFAULT_EFFORT} first.
     *
     * @return effort-level tokens
     */
    static List<String> effortLevels() {
        return EFFORT_LEVELS;
    }

    /**
     * Indicates whether an effort token is an explicit (non-default) level that should be
     * forwarded to the selected model.
     *
     * @param effort effort token
     * @return true for LOW, MEDIUM, or HIGH
     */
    static boolean isExplicitEffort(String effort) {
        String normalized = normalizeUpper(effort);
        return !normalized.isBlank() && !DEFAULT_EFFORT.equals(normalized) && EFFORT_LEVELS.contains(normalized);
    }

    private static String normalizeLower(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private static String normalizeUpper(String value) {
        return value == null ? "" : value.trim().toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }
}
