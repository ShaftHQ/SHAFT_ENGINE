package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #3921: {@link ShaftAssistantChatState.Message} gained an explicit {@code kind} field
 * (assistant-text/tool-event/error/raw-verbose/milestone/user) instead of the kind being encoded
 * only in markdown text and glyph prefixes. {@link ShaftAssistantChatState#resolveKind} is the
 * single place that normalizes an explicit kind or falls back to a role-inferred default, so
 * pre-existing serialized sessions (saved before this field existed, so {@code kind} deserializes
 * as the field's default {@code ""}) still load and render with a sensible kind.
 */
class ShaftAssistantChatStateTest {
    @Test
    void newSessionReturnsExistingEmptySessionWithoutCreating() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.newSession();
        int initialCount = state.sessions().size();
        String initialActiveId = state.activeSession().id;

        ShaftAssistantChatState.Session result = state.newSession();

        assertAll(
                () -> assertEquals(initialCount, state.sessions().size(),
                        "Should not create a new session when active session is empty"),
                () -> assertEquals(initialActiveId, result.id,
                        "Should return the existing empty session"),
                () -> assertTrue(result.messages.isEmpty(),
                        "Returned session should have no messages"));
    }

    @Test
    void newSessionCreatesNewWhenActiveSessionHasMessages() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.append("user", "first message", "{}");
        int initialCount = state.sessions().size();
        String initialActiveId = state.activeSession().id;

        ShaftAssistantChatState.Session result = state.newSession();

        assertAll(
                () -> assertEquals(initialCount + 1, state.sessions().size(),
                        "Should create a new session when active session has messages"),
                () -> assertNotEquals(initialActiveId, result.id,
                        "New session should have a different ID"),
                () -> assertTrue(result.messages.isEmpty(),
                        "New session should start with no messages"),
                () -> assertEquals(result.id, state.activeSession().id,
                        "New session should become the active session"));
    }

    @Test
    void newSessionInMultiSessionStatePreservesPreviousSessions() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.append("user", "session 1 message", "{}");
        String session1Id = state.activeSession().id;

        state.newSession();
        state.append("user", "session 2 message", "{}");
        String session2Id = state.activeSession().id;

        int beforeCount = state.sessions().size();
        ShaftAssistantChatState.Session result = state.newSession();

        assertAll(
                () -> assertEquals(beforeCount + 1, state.sessions().size(),
                        "Should create new session when activating on non-empty session"),
                () -> assertTrue(state.sessions().stream()
                        .anyMatch(s -> s.id.equals(session1Id)),
                        "Previous session 1 should still exist"),
                () -> assertTrue(state.sessions().stream()
                        .anyMatch(s -> s.id.equals(session2Id)),
                        "Previous session 2 should still exist"),
                () -> assertEquals(result.id, state.activeSession().id,
                        "Newly created session should be active"));
    }

    @Test
    void appendedUserMessageDefaultsKindToUser() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();

        state.append("user", "hello there", "");

        ShaftAssistantChatState.Message message = state.activeMessages().get(0);
        assertEquals(ShaftAssistantChatState.KIND_USER, message.kind);
    }

    @Test
    void appendedNonUserMessageDefaultsKindToAssistantText() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();

        state.append("assistant", "hi, how can I help?", "");

        ShaftAssistantChatState.Message message = state.activeMessages().get(0);
        assertEquals(ShaftAssistantChatState.KIND_ASSISTANT_TEXT, message.kind);
    }

    /**
     * A session saved before this field existed deserializes {@code kind} as the field's declared
     * default {@code ""} (IntelliJ's XmlSerializer leaves fields with no matching XML element at
     * their class-declared default). Loading such a session must still resolve a sensible kind
     * per-message instead of leaving it blank -- role-inferred, matching {@link
     * ShaftAssistantChatState#resolveKind}.
     */
    @Test
    void loadingPreExistingSessionsWithoutAKindFieldDefaultsEachMessagesKindByRole() {
        ShaftAssistantChatState.StateData legacyState = new ShaftAssistantChatState.StateData();
        ShaftAssistantChatState.Session legacySession = new ShaftAssistantChatState.Session();
        legacySession.id = "legacy-session";

        ShaftAssistantChatState.Message legacyUserMessage = new ShaftAssistantChatState.Message();
        legacyUserMessage.role = "user";
        legacyUserMessage.markdown = "a message saved before the kind field existed";
        // legacyUserMessage.kind intentionally left at its declared default ("") to simulate an
        // older serialized <message> element with no <kind> child.

        ShaftAssistantChatState.Message legacyAssistantMessage = new ShaftAssistantChatState.Message();
        legacyAssistantMessage.role = "assistant";
        legacyAssistantMessage.markdown = "an older assistant reply";

        legacySession.messages = List.of(legacyUserMessage, legacyAssistantMessage);
        legacyState.sessions = List.of(legacySession);
        legacyState.activeSessionId = "legacy-session";

        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.loadState(legacyState);

        List<ShaftAssistantChatState.Message> loaded = state.activeMessages();
        assertAll(
                () -> assertEquals(2, loaded.size()),
                () -> assertEquals(ShaftAssistantChatState.KIND_USER, loaded.get(0).kind,
                        "A legacy user-role message with no persisted kind must default to KIND_USER"),
                () -> assertEquals(ShaftAssistantChatState.KIND_ASSISTANT_TEXT, loaded.get(1).kind,
                        "A legacy non-user-role message with no persisted kind must default to "
                                + "KIND_ASSISTANT_TEXT"));
    }

    /**
     * An explicit, already-persisted kind (e.g. {@code error}) must round-trip through {@link
     * ShaftAssistantChatState#getState()}/{@link ShaftAssistantChatState#loadState} unchanged
     * (normalized to lowercase/trimmed), not be silently overwritten by the role-inferred default.
     */
    @Test
    void explicitKindRoundTripsThroughGetStateAndLoadStateUnchanged() {
        ShaftAssistantChatState.StateData originalState = new ShaftAssistantChatState.StateData();
        ShaftAssistantChatState.Session session = new ShaftAssistantChatState.Session();
        session.id = "session-with-explicit-kind";

        ShaftAssistantChatState.Message errorMessage = new ShaftAssistantChatState.Message();
        errorMessage.role = "assistant";
        errorMessage.markdown = "Tool call failed: timeout";
        errorMessage.kind = "  " + ShaftAssistantChatState.KIND_ERROR.toUpperCase() + "  ";

        session.messages = List.of(errorMessage);
        originalState.sessions = List.of(session);
        originalState.activeSessionId = "session-with-explicit-kind";

        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.loadState(originalState);

        ShaftAssistantChatState.StateData roundTripped = state.getState();
        ShaftAssistantChatState.Message roundTrippedMessage = roundTripped.sessions.get(0).messages.get(0);

        assertEquals(ShaftAssistantChatState.KIND_ERROR, roundTrippedMessage.kind,
                "An explicit persisted kind must survive a getState()/loadState() round trip, "
                        + "normalized to lowercase/trimmed rather than replaced by the role default");
    }

    /**
     * Issue #3968: {@code append} gained a 4-arg overload so {@code ShaftAssistantPanel} call sites
     * that already know they are producing a non-default kind (a tool-event, an error, a raw-verbose
     * dump, a milestone) can say so explicitly instead of always falling back to the role-inferred
     * default the 3-arg {@link ShaftAssistantChatState#append(String, String, String)} still uses.
     */
    @Test
    void fourArgAppendWithExplicitKindOverridesTheRoleInferredDefault() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();

        state.append("assistant", "Tool call failed: timeout", "", ShaftAssistantChatState.KIND_ERROR);

        ShaftAssistantChatState.Message message = state.activeMessages().get(0);
        assertEquals(ShaftAssistantChatState.KIND_ERROR, message.kind);
    }

    @Test
    void fourArgAppendWithBlankKindFallsBackToTheRoleInferredDefaultLikeTheThreeArgOverload() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();

        state.append("assistant", "hi, how can I help?", "", "");

        ShaftAssistantChatState.Message message = state.activeMessages().get(0);
        assertEquals(ShaftAssistantChatState.KIND_ASSISTANT_TEXT, message.kind);
    }
}
