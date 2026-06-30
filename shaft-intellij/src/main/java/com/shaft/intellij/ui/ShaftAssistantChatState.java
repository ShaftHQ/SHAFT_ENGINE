package com.shaft.intellij.ui;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Project-scoped persistent Assistant chat sessions.
 */
@State(name = "ShaftAssistantChats", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public final class ShaftAssistantChatState implements PersistentStateComponent<ShaftAssistantChatState.ChatState> {
    private static final int MAX_SESSIONS = 12;
    private static final int MAX_MESSAGES_PER_SESSION = 80;

    private ChatState state = new ChatState();

    /**
     * Returns the project-level Assistant chat state service.
     *
     * @param project IntelliJ project
     * @return chat state service
     */
    public static ShaftAssistantChatState getInstance(Project project) {
        return project.getService(ShaftAssistantChatState.class);
    }

    @Override
    public ChatState getState() {
        ensureActiveSession();
        return state;
    }

    @Override
    public void loadState(@NotNull ChatState loadedState) {
        XmlSerializerUtil.copyBean(loadedState, state);
        ensureActiveSession();
        trim();
    }

    Session activeSession() {
        ensureActiveSession();
        return sessionById(state.activeSessionId);
    }

    List<Session> sessions() {
        ensureActiveSession();
        return List.copyOf(state.sessions);
    }

    void activate(String sessionId) {
        if (sessionById(sessionId) != null) {
            state.activeSessionId = sessionId;
        }
    }

    Session newSession() {
        Session session = new Session();
        session.id = UUID.randomUUID().toString();
        session.title = "New chat";
        session.createdAt = now();
        session.updatedAt = session.createdAt;
        state.sessions.add(0, session);
        state.activeSessionId = session.id;
        trim();
        return session;
    }

    void append(String role, String markdown, String raw) {
        if (markdown == null || markdown.isBlank()) {
            return;
        }
        Session session = activeSession();
        Message message = new Message();
        message.role = role == null ? "" : role;
        message.markdown = markdown;
        message.createdAt = now();
        session.messages.add(message);
        session.updatedAt = message.createdAt;
        if ("New chat".equals(session.title) && "user".equals(message.role)) {
            session.title = titleFrom(markdown);
        }
        trim();
    }

    void clearActiveSession() {
        Session session = activeSession();
        session.messages.clear();
        session.title = "New chat";
        session.updatedAt = now();
    }

    String activeMarkdown() {
        Session session = activeSession();
        List<String> parts = new ArrayList<>();
        for (Message message : session.messages) {
            if (message.markdown != null && !message.markdown.isBlank()) {
                parts.add(message.markdown);
            }
        }
        return String.join("\n\n", parts);
    }

    List<Message> activeMessages() {
        Session session = activeSession();
        if (session.messages == null) {
            return List.of();
        }
        return List.copyOf(session.messages);
    }

    private void ensureActiveSession() {
        if (state.sessions == null) {
            state.sessions = new ArrayList<>();
        }
        if (state.sessions.isEmpty()) {
            newSession();
            return;
        }
        if (sessionById(state.activeSessionId) == null) {
            state.activeSessionId = state.sessions.get(0).id;
        }
    }

    private Session sessionById(String sessionId) {
        if (sessionId == null || state.sessions == null) {
            return null;
        }
        for (Session session : state.sessions) {
            if (sessionId.equals(session.id)) {
                return session;
            }
        }
        return null;
    }

    private void trim() {
        ensureMessages();
        while (state.sessions.size() > MAX_SESSIONS) {
            state.sessions.remove(state.sessions.size() - 1);
        }
    }

    private void ensureMessages() {
        for (Session session : state.sessions) {
            if (session.messages == null) {
                session.messages = new ArrayList<>();
            }
            while (session.messages.size() > MAX_MESSAGES_PER_SESSION) {
                session.messages.remove(0);
            }
        }
    }

    private static String titleFrom(String markdown) {
        String text = markdown.replace("*", "")
                .replace("`", "")
                .replace("\n", " ")
                .trim();
        if (text.length() <= 40) {
            return text.isBlank() ? "New chat" : text;
        }
        return text.substring(0, 37).stripTrailing() + "...";
    }

    private static String now() {
        return Instant.now().toString();
    }

    /**
     * XML-serializable chat state.
     */
    public static final class ChatState {
        public String activeSessionId = "";
        public List<Session> sessions = new ArrayList<>();
    }

    /**
     * XML-serializable Assistant session.
     */
    public static final class Session {
        public String id = "";
        public String title = "New chat";
        public String createdAt = "";
        public String updatedAt = "";
        public List<Message> messages = new ArrayList<>();

        @Override
        public String toString() {
            return title == null || title.isBlank() ? "New chat" : title;
        }
    }

    /**
     * XML-serializable Assistant transcript message.
     */
    public static final class Message {
        public String role = "";
        public String markdown = "";
        public String createdAt = "";
    }
}
