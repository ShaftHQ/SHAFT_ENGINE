package com.shaft.intellij.ui;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import java.awt.Component;
import java.util.Locale;

/**
 * Human-friendly labels for persisted Assistant option tokens.
 */
public final class ShaftUiLabels {
    private static final String UNSELECTED_OPTION = "Select an option";

    private ShaftUiLabels() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Keeps combo box item values unchanged while rendering readable labels.
     *
     * @param combo combo box containing persisted string tokens
     */
    public static void applyFriendlyRenderer(JComboBox<String> combo) {
        combo.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list,
                                                          Object value,
                                                          int index,
                                                          boolean isSelected,
                                                          boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                label.setText(friendly(value));
                return label;
            }
        });
    }

    /**
     * Compact local-agent route, or setup's unselected wording when no family is chosen.
     *
     * @param family persisted family token or combo selection
     * @param runtime persisted runtime token or combo selection
     * @return {@code Codex CLI}-style label, or {@code Select an option}
     */
    public static String localAgentRoute(Object family, Object runtime) {
        if (unselected(family)) {
            return UNSELECTED_OPTION;
        }
        String runtimeLabel = friendly(runtime);
        return runtimeLabel.isBlank() ? friendly(family) : friendly(family) + " " + runtimeLabel;
    }

    /**
     * Settings/tooltip current-agent row, or setup's unselected wording when no family is chosen.
     *
     * @param family persisted family token or combo selection
     * @param runtime persisted runtime token or combo selection
     * @return {@code Agent: Local / Codex / CLI}, or {@code Agent: Select an option}
     */
    public static String localAgentSummary(Object family, Object runtime) {
        if (unselected(family)) {
            return "Agent: " + UNSELECTED_OPTION;
        }
        return "Agent: Local / " + friendly(family) + " / " + friendly(runtime);
    }

    /**
     * Returns display text for known Assistant option tokens.
     *
     * @param value token
     * @return readable label
     */
    public static String friendly(Object value) {
        if (unselected(value)) {
            return "";
        }
        String text = value.toString();
        return switch (normalize(text)) {
            case "ASK" -> "Ask";
            case "PLAN" -> "Plan";
            case "AGENT" -> "Agent";
            case "LOCAL" -> "Local";
            case "CLOUD" -> "Cloud";
            case "CODEX" -> "Codex";
            case "CLAUDE" -> "Claude";
            case "COPILOT" -> "GitHub Copilot";
            case "GROK" -> "Grok";
            case "CLAUDE_CODE" -> "Claude Code CLI";
            case "CLAUDE_DESKTOP" -> "Claude Desktop";
            case "COPILOT_CLI" -> "GitHub Copilot CLI";
            case "COPILOT_INTELLIJ" -> "GitHub Copilot in IntelliJ";
            case "INTELLIJ_PLUGIN" -> "SHAFT IntelliJ plugin (this plugin only - no external agent)";
            case "CLI" -> "CLI";
            case "IDE_PLUGIN" -> "IDE plugin";
            case "DESKTOP_APP" -> "Desktop app";
            case "OPENAI" -> "OpenAI";
            case "ANTHROPIC" -> "Anthropic";
            case "GEMINI" -> "Gemini";
            case "GITHUB" -> "GitHub";
            case "OLLAMA" -> "Ollama (local)";
            case "LMSTUDIO" -> "LM Studio (local)";
            case "NONE" -> "None";
            case "DEFAULT" -> "Default effort";
            case "LOW" -> "Low effort";
            case "MEDIUM" -> "Medium effort";
            case "HIGH" -> "High effort";
            default -> text;
        };
    }

    private static boolean unselected(Object value) {
        return value == null || value.toString().isBlank();
    }

    private static String normalize(String value) {
        return value.trim().toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }
}
