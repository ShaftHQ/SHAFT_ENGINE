package com.shaft.intellij.ui.presenter;

import com.shaft.intellij.ui.ShaftStatusPresentation;
import com.shaft.intellij.ui.theme.ShaftUiMetrics;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JLabel;
import java.awt.Font;

/**
 * Behaviour-preserving chrome helpers for Assistant panel (W1.8 + W1.4).
 */
public final class ShaftAssistantPresenter {
    private ShaftAssistantPresenter() {
        throw new IllegalStateException("Utility class");
    }

    public static void applyHeaderFont(@NotNull JComponent component) {
        component.setFont(ShaftUiMetrics.headerFont());
    }

    public static void applySectionFont(@NotNull JLabel label) {
        label.setFont(ShaftUiMetrics.sectionFont());
    }

    public static @NotNull Font bodyFont() {
        return ShaftUiMetrics.bodyFont();
    }

    public static @NotNull String plainLanguageAgentFailure(@NotNull String exitReason) {
        String trimmed = exitReason.trim();
        if (trimmed.isEmpty()) {
            return ShaftStatusPresentation.ERROR_ICON + " The assistant stopped. Reattach or restart MCP, then try again.";
        }
        return ShaftStatusPresentation.ERROR_ICON + " The assistant stopped: " + trimmed
                + ". Reattach or restart MCP, then try again.";
    }
}
