package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}
