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

    /**
     * Issue #3968: the original motivation for the whole kind model (owner directive: "ensure that
     * we can dynamically style and parse the agent messages") -- {@code error} must be visually
     * distinguishable from plain assistant text at a glance, on every channel {@link MessageStyle}
     * exposes, not just one.
     */
    @Test
    void errorStyleDiffersFromAssistantTextStyleOnBackgroundForegroundAndStroke() {
        MessageStyleRegistry.MessageStyle error = MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ERROR);
        MessageStyleRegistry.MessageStyle assistantText =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT);

        assertAll(
                () -> assertNotEquals(error.background(), assistantText.background(),
                        "error must not share assistant-text's background"),
                () -> assertNotEquals(error.foreground(), assistantText.foreground(),
                        "error must not share assistant-text's foreground"),
                () -> assertNotEquals(error.stroke(), assistantText.stroke(),
                        "error must not share assistant-text's border color"));
    }

    /**
     * Issue #3968 design decision: milestone bubbles are the highest-volume message kind in a run
     * (every compact non-verbose progress line), so they get a restrained distinction -- a dimmer
     * foreground signaling "ambient status" -- rather than a loud recolor that would compete with the
     * assistant's actual answers for attention. Background and border stay shared with assistant-text
     * on purpose (see {@link #nonUserKindsRenderWithABubbleStroke} for the still-bordered contract).
     */
    @Test
    void milestoneStyleUsesADimmerForegroundThanAssistantTextButSharesItsBackgroundAndBorder() {
        MessageStyleRegistry.MessageStyle milestone =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_MILESTONE);
        MessageStyleRegistry.MessageStyle assistantText =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT);

        assertAll(
                () -> assertNotEquals(milestone.foreground(), assistantText.foreground(),
                        "milestone must read as visually distinct (dimmer) from a real assistant answer"),
                () -> assertEquals(assistantText.background(), milestone.background()),
                () -> assertEquals(assistantText.stroke(), milestone.stroke()));
    }

    /**
     * Issue #3968 design decision: tool-event outcomes (e.g. "Approved `x`.") and raw-verbose dumps
     * are intentionally left uniform with assistant-text -- tool-event lines are already short,
     * self-explanatory confirmations, and raw-verbose content already visually sets itself apart via
     * its own fenced-code-block markdown rendering, so an additional bubble-level color would be
     * redundant chrome rather than a genuine readability aid.
     */
    @Test
    void toolEventAndRawVerboseStylesAreUniformWithAssistantTextByDesign() {
        MessageStyleRegistry.MessageStyle assistantText =
                MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_ASSISTANT_TEXT);

        assertAll(
                () -> assertEquals(assistantText, MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_TOOL_EVENT)),
                () -> assertEquals(assistantText,
                        MessageStyleRegistry.styleFor(ShaftAssistantChatState.KIND_RAW_VERBOSE)));
    }
}
