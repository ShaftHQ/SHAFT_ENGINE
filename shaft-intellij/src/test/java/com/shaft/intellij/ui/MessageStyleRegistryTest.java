package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Issue #3921: {@link MessageStyleRegistry} is the single kind-&gt;style seam {@link
 * AssistantTranscriptView#fallbackMessage} resolves bubble colors from, replacing the old inline
 * user/non-user branch. Every {@link ShaftAssistantChatState} kind constant must resolve to its
 * own addressable {@link MessageStyleRegistry.MessageStyle} entry, theme-aware via {@link
 * com.intellij.ui.JBColor#namedColor} exactly like the rest of this file's color lookups.
 */
class MessageStyleRegistryTest {
    @Test
    void everyMessageKindResolvesToANonNullStyle() {
        assertAll(
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_USER)),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT)),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_TOOL_EVENT)),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ERROR)),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_RAW_VERBOSE)),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_MILESTONE)));
    }

    @Test
    void everyStyleCarriesNonNullBackgroundAndForeground() {
        for (String kind : new String[] {
                ShaftAssistantChatState.KIND_USER, ShaftAssistantChatState.KIND_ASSISTANT_TEXT,
                ShaftAssistantChatState.KIND_TOOL_EVENT, ShaftAssistantChatState.KIND_ERROR,
                ShaftAssistantChatState.KIND_RAW_VERBOSE, ShaftAssistantChatState.KIND_MILESTONE
        }) {
            MessageStyleRegistry.MessageStyle style = MessageStyleRegistry.styleFor(kind);
            assertNotNull(style.background(), "background must be resolved for kind " + kind);
            assertNotNull(style.foreground(), "foreground must be resolved for kind " + kind);
        }
    }

    @Test
    void userKindHasNoBubbleStrokeMatchingTodaysBorderlessUserBubble() {
        MessageStyleRegistry.MessageStyle style = MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_USER);

        assertNull(style.stroke(), "User bubbles render without a border, same as before this registry existed");
    }

    @Test
    void nonUserKindsRenderWithABubbleStroke() {
        assertAll(
                () -> assertNotNull(
                        MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT).stroke()),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_TOOL_EVENT).stroke()),
                () -> assertNotNull(MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ERROR).stroke()),
                () -> assertNotNull(
                        MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_RAW_VERBOSE).stroke()),
                () -> assertNotNull(
                        MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_MILESTONE).stroke()));
    }

    @Test
    void userStyleDiffersFromAssistantTextStyleMatchingTodaysTwoBubbleLook() {
        MessageStyleRegistry.MessageStyle user = MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_USER);
        MessageStyleRegistry.MessageStyle assistantText =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT);

        assertAll(
                () -> assertNotEquals(user.background(), assistantText.background()),
                () -> assertNotEquals(user.foreground(), assistantText.foreground()));
    }

    @Test
    void unrecognizedOrBlankKindFallsBackToAssistantTextStyleDefensively() {
        MessageStyleRegistry.MessageStyle assistantText =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT);

        assertAll(
                () -> assertEquals(assistantText, MessageStyleRegistry.styleFor("")),
                () -> assertEquals(assistantText, MessageStyleRegistry.styleFor(null)),
                () -> assertEquals(assistantText, MessageStyleRegistry.styleFor("bogus-kind")));
    }
}
