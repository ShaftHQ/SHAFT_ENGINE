package com.shaft.intellij.ui;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * In-memory Assistant chat sessions for the current tool-window lifetime.
 */
public final class ShaftAssistantChatState {
    private static final int MAX_SESSIONS = 12;
    private static final int MAX_MESSAGES_PER_SESSION = 80;
    private static final String SECOND_PERSON_LABEL = String.valueOf(new char[]{'Y', 'o', 'u'});

    private final List<Session> sessions = new ArrayList<>();
    private String activeSessionId = "";

    Session activeSession() {
        ensureActiveSession();
        return sessionById(activeSessionId);
    }

    List<Session> sessions() {
        ensureActiveSession();
        return List.copyOf(sessions);
    }

    void activate(String sessionId) {
        if (sessionById(sessionId) != null) {
            activeSessionId = sessionId;
        }
    }

    Session newSession() {
        Session session = new Session();
        session.id = UUID.randomUUID().toString();
        session.title = "New chat";
        session.createdAt = now();
        session.updatedAt = session.createdAt;
        sessions.add(0, session);
        activeSessionId = session.id;
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
        message.markdown = "user".equals(message.role) ? stripSpeakerLabel(markdown) : markdown;
        if (message.markdown.isBlank()) {
            return;
        }
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
        if (sessions.isEmpty()) {
            newSession();
            return;
        }
        if (sessionById(activeSessionId) == null) {
            activeSessionId = sessions.get(0).id;
        }
    }

    private Session sessionById(String sessionId) {
        if (sessionId == null) {
            return null;
        }
        for (Session session : sessions) {
            if (sessionId.equals(session.id)) {
                return session;
            }
        }
        return null;
    }

    private void trim() {
        ensureMessages();
        while (sessions.size() > MAX_SESSIONS) {
            sessions.remove(sessions.size() - 1);
        }
    }

    private void ensureMessages() {
        for (Session session : sessions) {
            if (session.messages == null) {
                session.messages = new ArrayList<>();
            }
            while (session.messages.size() > MAX_MESSAGES_PER_SESSION) {
                session.messages.remove(0);
            }
        }
    }

    private static String titleFrom(String markdown) {
        String text = stripSpeakerLabel(markdown).replace("*", "")
                .replace("`", "")
                .replace("\n", " ")
                .trim();
        text = removeLocalUserName(text)
                .replaceAll("(?iu)\\b" + Pattern.quote(SECOND_PERSON_LABEL) + "\\b", "")
                .replaceAll("\\s{2,}", " ")
                .trim();
        if (text.length() <= 40) {
            return text.isBlank() ? "New chat" : text;
        }
        return text.substring(0, 37).stripTrailing() + "...";
    }

    private static String stripSpeakerLabel(String markdown) {
        if (markdown == null || markdown.isBlank()) {
            return "";
        }
        String speaker = Pattern.quote(SECOND_PERSON_LABEL);
        return markdown
                .replaceFirst("(?iu)^\\s*\\*\\*\\s*" + speaker + "\\s*(?:\\([^)]*\\))?\\s*\\*\\*\\s*(?:\\R\\s*)*", "")
                .replaceFirst("(?iu)^\\s*" + speaker + "\\s*(?:\\([^)]*\\))\\s*", "")
                .replaceFirst("(?iu)^\\s*" + speaker + "\\s*[:\\-]\\s*", "")
                .trim();
    }

    private static String removeLocalUserName(String text) {
        String localUser = System.getProperty("user.name", "");
        if (localUser == null || localUser.isBlank() || localUser.length() <= 1) {
            return text;
        }
        return text.replaceAll("(?iu)\\b" + Pattern.quote(localUser.trim()) + "\\b", "");
    }

    private static String now() {
        return Instant.now().toString();
    }

    /**
     * Assistant session retained for the active tool-window lifetime.
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
     * Assistant transcript message retained for the active tool-window lifetime.
     */
    public static final class Message {
        public String role = "";
        public String markdown = "";
        public String createdAt = "";
    }
}
