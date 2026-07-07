package com.shaft.intellij.ui;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Project-level Assistant chat sessions persisted without raw MCP payloads.
 */
@State(name = "ShaftAssistantChatState", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public final class ShaftAssistantChatState implements PersistentStateComponent<ShaftAssistantChatState.StateData> {
    private static final int MAX_SESSIONS = 12;
    private static final int MAX_MESSAGES_PER_SESSION = 80;
    private static final String SECOND_PERSON_LABEL = String.valueOf(new char[]{'Y', 'o', 'u'});
    private static final Pattern KEY_VALUE_SECRET = Pattern.compile(
            "(?iu)([\"']?\\b(?:api[_-]?key|access[_-]?token|refresh[_-]?token|token|secret|password|authorization|cookie|set-cookie)\\b[\"']?\\s*[:=]\\s*)([\"']?)[^\\s`'\",}]+([\"']?)");
    private static final Pattern ENV_SECRET = Pattern.compile(
            "(?iu)\\b([A-Z][A-Z0-9_]*(?:API_KEY|TOKEN|SECRET|PASSWORD))\\b\\s*=\\s*([^\\s`'\\\"]+)");
    private static final Pattern BEARER_SECRET = Pattern.compile(
            "(?iu)\\bBearer\\s+[A-Za-z0-9._~+/=-]+");

    private final List<Session> sessions = new ArrayList<>();
    private String activeSessionId = "";

    public static ShaftAssistantChatState getInstance(Project project) {
        if (project == null) {
            return new ShaftAssistantChatState();
        }
        ShaftAssistantChatState state = project.getService(ShaftAssistantChatState.class);
        return state == null ? new ShaftAssistantChatState() : state;
    }

    @Override
    public StateData getState() {
        StateData state = new StateData();
        state.activeSessionId = activeSessionId;
        for (Session session : sessions) {
            state.sessions.add(copySession(session));
        }
        return state;
    }

    @Override
    public void loadState(@NotNull StateData state) {
        sessions.clear();
        if (state.sessions != null) {
            for (Session session : state.sessions) {
                Session normalized = normalizeSession(session);
                if (normalized != null) {
                    sessions.add(normalized);
                }
            }
        }
        activeSessionId = state.activeSessionId == null ? "" : state.activeSessionId;
        trim();
        if (!sessions.isEmpty() && sessionById(activeSessionId) == null) {
            activeSessionId = sessions.get(0).id;
        }
    }

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
        // Dedup guard: if sessions is non-empty and the current active session
        // (resolved directly via sessionById, not activeSession() to avoid recursion)
        // has zero messages, return/activate that existing empty session instead of creating a new one.
        if (!sessions.isEmpty()) {
            Session activeSession = sessionById(activeSessionId);
            if (activeSession != null && activeSession.messages != null && activeSession.messages.isEmpty()) {
                // Current session is already empty; return it without creating a new one
                return activeSession;
            }
        }

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
        String renderedMarkdown = "user".equals(message.role) ? stripSpeakerLabel(markdown) : markdown;
        message.markdown = redactSecrets(renderedMarkdown);
        if (message.markdown.isBlank()) {
            return;
        }
        message.createdAt = now();
        session.messages.add(message);
        session.updatedAt = message.createdAt;
        if ("New chat".equals(session.title) && "user".equals(message.role)) {
            session.title = titleFrom(message.markdown);
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

    private static Session normalizeSession(Session source) {
        if (source == null) {
            return null;
        }
        Session copy = new Session();
        copy.id = normalizeSessionId(source.id);
        copy.title = normalizeSessionTitle(source.title);
        copy.createdAt = blankIfNull(source.createdAt);
        copy.updatedAt = blankIfNull(source.updatedAt);
        copy.messages = normalizeMessages(source.messages);
        return copy;
    }

    private static String normalizeSessionId(String id) {
        return id == null || id.isBlank() ? UUID.randomUUID().toString() : id;
    }

    private static String normalizeSessionTitle(String title) {
        return title == null || title.isBlank() ? "New chat" : redactSecrets(title);
    }

    private static String blankIfNull(String value) {
        return value == null ? "" : value;
    }

    private static List<Message> normalizeMessages(List<Message> messages) {
        List<Message> normalizedMessages = new ArrayList<>();
        if (messages == null) {
            return normalizedMessages;
        }
        for (Message message : messages) {
            Message normalized = normalizeMessage(message);
            if (normalized != null) {
                normalizedMessages.add(normalized);
            }
        }
        return normalizedMessages;
    }

    private static Session copySession(Session source) {
        Session copy = normalizeSession(source);
        return copy == null ? new Session() : copy;
    }

    private static Message normalizeMessage(Message source) {
        if (source == null || source.markdown == null || source.markdown.isBlank()) {
            return null;
        }
        Message copy = new Message();
        copy.role = source.role == null ? "" : source.role;
        copy.markdown = redactSecrets(source.markdown);
        copy.createdAt = source.createdAt == null ? "" : source.createdAt;
        return copy;
    }

    private static String redactSecrets(String value) {
        if (value == null || value.isBlank()) {
            return "";
        }
        String redacted = BEARER_SECRET.matcher(value).replaceAll("Bearer <redacted>");
        redacted = ENV_SECRET.matcher(redacted).replaceAll("$1=<redacted>");
        redacted = KEY_VALUE_SECRET.matcher(redacted).replaceAll("$1$2<redacted>$3");
        return redacted;
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

    public static final class StateData {
        public List<Session> sessions = new ArrayList<>();
        public String activeSessionId = "";
    }

    /**
     * Assistant session retained for the active project.
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
     * Assistant transcript message retained for the active project.
     */
    public static final class Message {
        public String role = "";
        public String markdown = "";
        public String createdAt = "";
    }
}
