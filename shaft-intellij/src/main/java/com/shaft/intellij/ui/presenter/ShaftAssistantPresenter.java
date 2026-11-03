package com.shaft.intellij.ui.presenter;

import com.shaft.intellij.ui.ShaftStatusPresentation;
import com.shaft.intellij.ui.theme.ShaftUiMetrics;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JLabel;
import java.awt.Font;
import java.util.List;

/**
 * Behaviour-preserving chrome helpers for Assistant panel (W1.8 + W1.4 + W2.6).
 */
public final class ShaftAssistantPresenter {
    private static final List<String> SHORT_SLASH_LIST = List.of(
            "/help — what Assistant can do",
            "/record — capture a browser flow",
            "/heal — fix a failed locator",
            "/doctor — optional deep diagnosis",
            "/mcp — run a verified MCP tool"
    );

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

    /**
     * Assistant chat is optional and only after MCP is verified (W2.6). Recorder/codegen/doctor
     * remain available from Setup without opening Assistant.
     */
    public static boolean assistantChatAllowed(boolean mcpVerified) {
        return mcpVerified;
    }

    public static @NotNull String assistantGateMessage(boolean mcpVerified) {
        if (mcpVerified) {
            return "MCP verified. Assistant is optional — use slash commands or keep working from Setup/Tests.";
        }
        return "Verify SHAFT MCP in Setup before opening Assistant chat. Recorder, codegen, and Doctor do not need Assistant.";
    }

    public static @NotNull List<String> shortSlashList() {
        return SHORT_SLASH_LIST;
    }

    public static @NotNull String reattachPrompt(@NotNull String exitReason) {
        return plainLanguageAgentFailure(exitReason) + " Tip: use Reattach on the Assistant toolbar after MCP is healthy.";
    }
}
