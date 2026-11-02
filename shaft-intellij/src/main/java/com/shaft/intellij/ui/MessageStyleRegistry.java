package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;

import java.awt.Color;

/**
 * Central kind-&gt;style registry for {@link AssistantTranscriptView}'s {@code fallbackMessage}
 * bubble seam (issue #3921). Before this class, a bubble's color was decided by one inline
 * user/non-user branch inside {@code fallbackMessage} -- every {@link ShaftAssistantChatState.Message#kind}
 * value now resolves to its own {@link MessageStyle} entry here, so a kind can get a distinct look
 * by editing one case in this class instead of hunting for markdown text-prefix/glyph branches
 * scattered through the rendering code. #3921 itself shipped this as a zero-pixel-diff refactor
 * (model + addressable seam only); #3968 is the follow-up that actually differentiates kinds:
 *
 * <ul>
 *   <li>{@code error} gets a genuinely distinct background/foreground/stroke -- the stated original
 *       motivation for the whole kind model (owner directive: "ensure that we can dynamically style
 *       and parse the agent messages") -- so a failed tool call or local-agent run reads as an error
 *       at a glance, not just from its text.</li>
 *   <li>{@code milestone} gets a dimmer foreground only: it is the highest-volume kind in a run
 *       (every compact non-verbose progress line), so a restrained "ambient status" distinction was
 *       chosen over a loud recolor that would compete with the assistant's actual answers.</li>
 *   <li>{@code tool-event} and {@code raw-verbose} stay uniform with {@code assistant-text} on
 *       purpose: tool-event lines are already short, self-explanatory confirmations (e.g. "Approved
 *       `x`."), and raw-verbose content already visually sets itself apart via its own fenced-code-
 *       block markdown rendering, so an additional bubble-level color would be redundant chrome
 *       rather than a genuine readability aid.</li>
 * </ul>
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
            case ShaftAssistantChatState.KIND_ERROR -> errorStyle();
            case ShaftAssistantChatState.KIND_MILESTONE -> milestoneStyle();
            case ShaftAssistantChatState.KIND_TOOL_EVENT,
                    ShaftAssistantChatState.KIND_RAW_VERBOSE,
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

    /**
     * A failed tool call or local-agent run must read as an error at a glance (issue #3968's
     * mandatory minimum): a red-tinted background/foreground plus a stronger red border, all three
     * distinct from {@link #defaultAssistantStyle()}. {@code Notification.error*} are IntelliJ's own
     * theme-aware error UI keys, matching this file's established {@link #color} pattern.
     */
    private static MessageStyle errorStyle() {
        return new MessageStyle(
                color("Notification.errorBackground", new Color(0xFDECEA)),
                color("Notification.errorForeground", new Color(0x8B1D1D)),
                color("Notification.errorBorderColor", new Color(0xE5484D)));
    }

    /**
     * Milestone bubbles keep {@link #defaultAssistantStyle()}'s background/border and only dim the
     * foreground text, signaling "ambient status" without competing for attention with the
     * assistant's real answers -- see the class doc for the full reasoning.
     */
    private static MessageStyle milestoneStyle() {
        return new MessageStyle(
                color("Panel.background", new Color(0xF6F8FA)),
                color("Label.disabledForeground", new Color(0x6B7280)),
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
