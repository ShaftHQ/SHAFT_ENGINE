package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;

import java.awt.Color;

/**
 * Central kind-&gt;style registry for {@link AssistantTranscriptView}'s {@code fallbackMessage}
 * bubble seam (issue #3921). Before this class, a bubble's color was decided by one inline
 * user/non-user branch inside {@code fallbackMessage} -- every {@link ShaftAssistantChatState.Message#kind}
 * value now resolves to its own {@link MessageStyle} entry here, so a later ticket can give a kind
 * (e.g. {@code error}, {@code tool-event}) a distinct look by editing one case in this class instead
 * of hunting for markdown text-prefix/glyph branches scattered through the rendering code. Scope for
 * #3921 is the model + this addressable seam, not a visual redesign: every non-user kind currently
 * resolves to the exact same colors the single "assistant" bubble style already used, so switching
 * to this registry is a zero-pixel-diff refactor.
 *
 * <p>Every color is resolved through {@link JBColor#namedColor}, the same theme-aware pattern
 * {@code AssistantTranscriptView#resolvedColor} already uses, so light/dark IDE themes are honored
 * automatically.
 */
final class MessageStyleRegistry {
    private MessageStyleRegistry() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves the bubble style for {@code kind}. Unrecognized/blank kinds fall back to the same
     * style as {@link ShaftAssistantChatState#KIND_ASSISTANT_TEXT} -- callers normally pass an
     * already-{@link ShaftAssistantChatState#resolveKind}d value, but this stays defensive so a
     * stale/corrupt persisted kind can never throw mid-render.
     */
    static MessageStyle styleFor(String kind) {
        return switch (kind == null ? "" : kind) {
            case ShaftAssistantChatState.KIND_USER -> new MessageStyle(
                    color("Panel.selectionBackground", new Color(0x2563EB)),
                    color("Panel.selectionForeground", Color.WHITE),
                    null);
            case ShaftAssistantChatState.KIND_TOOL_EVENT,
                    ShaftAssistantChatState.KIND_ERROR,
                    ShaftAssistantChatState.KIND_RAW_VERBOSE,
                    ShaftAssistantChatState.KIND_MILESTONE,
                    ShaftAssistantChatState.KIND_ASSISTANT_TEXT -> defaultAssistantStyle();
            default -> defaultAssistantStyle();
        };
    }

    private static MessageStyle defaultAssistantStyle() {
        return new MessageStyle(
                color("Panel.background", new Color(0xF6F8FA)),
                color("TextArea.foreground", new Color(0x202020)),
                color("Component.borderColor", new Color(0xD0D7DE)));
    }

    private static Color color(String uiKey, Color fallback) {
        Color resolved = JBColor.namedColor(uiKey, fallback);
        return resolved == null ? fallback : resolved;
    }

    /**
     * One kind's resolved bubble colors. {@code stroke} is {@code null} for kinds that render
     * without a border (matches today's borderless user-bubble treatment).
     */
    record MessageStyle(Color background, Color foreground, Color stroke) {
    }
}
