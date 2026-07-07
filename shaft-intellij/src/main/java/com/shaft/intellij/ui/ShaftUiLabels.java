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
     * Returns display text for known Assistant option tokens.
     *
     * @param value token
     * @return readable label
     */
    public static String friendly(Object value) {
        if (value == null) {
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
            case "OLLAMA" -> "Ollama";
            case "NONE" -> "None";
            default -> text;
        };
    }

    private static String normalize(String value) {
        return value.trim().toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }
}
