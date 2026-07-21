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
import java.util.Locale;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Project-level Assistant chat sessions persisted without raw MCP payloads.
 */
@State(name = "ShaftAssistantChatState", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public final class ShaftAssistantChatState implements PersistentStateComponent<ShaftAssistantChatState.StateData> {
    private static final int MAX_SESSIONS = 12;
    // Package-private (not private): AssistantTranscriptView reuses this exact constant, rather than
    // duplicating the number, to bound its own rendered messages/bubbles to the same trim policy
    // (issue #3751 part 2, MEDIUM finding 4). Trimming past this cap deletes the oldest messages
    // outright -- a real agent-response content loss, not just a rendering choice (issue #3920). Raised
    // from 80 to make genuine mid-session data loss practically unreachable while still bounding
    // memory/render cost against a runaway pathological run; a disclosure-collapse ("N earlier
    // messages") alternative was considered and rejected as disproportionate to this ticket -- the trim
    // logic in AssistantTranscriptView is tightly coupled to precise index bookkeeping across three
    // parallel structures with existing fragile invariants, and this same cap is enforced independently
    // on the persisted session below (ensureMessages), so a real "no loss ever" guarantee would need
    // both layers redesigned together regardless of a collapse bubble here.
    static final int MAX_MESSAGES_PER_SESSION = 500;
    // Message-kind model (issue #3921): before this, a message's visual/semantic "kind" (a plain
    // assistant reply vs. a tool-call result vs. an error vs. verbose raw output vs. a compact
    // progress milestone) existed only implicitly, encoded in markdown text and glyph prefixes
    // (ShaftStatusPresentation icons) that AssistantTranscriptView's fallbackMessage() never
    // actually branched on -- it only ever distinguished USER_ROLE from everything else. These
    // constants are the full, explicit kind vocabulary; MessageStyleRegistry is the single place
    // that maps each one to a rendered style.
    public static final String KIND_ASSISTANT_TEXT = "assistant-text";
    public static final String KIND_TOOL_EVENT = "tool-event";
    public static final String KIND_ERROR = "error";
    public static final String KIND_RAW_VERBOSE = "raw-verbose";
    public static final String KIND_MILESTONE = "milestone";
    public static final String KIND_USER = "user";
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
        append(role, markdown, raw, null);
    }

    /**
     * Same as {@link #append(String, String, String)}, plus an explicit {@code kind} (issue #3968)
     * for call sites that already know they are producing a non-default kind -- a tool-event, an
     * error, a raw-verbose dump, a milestone -- instead of always falling back to the role-inferred
     * default. A blank/{@code null} kind behaves exactly like the 3-arg overload.
     */
    void append(String role, String markdown, String raw, String kind) {
        if (markdown == null || markdown.isBlank()) {
            return;
        }
        Session session = activeSession();
        Message message = new Message();
        message.role = role == null ? "" : role;
        message.kind = resolveKind(kind, message.role);
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

    /**
     * Clears every session (chat and prompt/search history) and the active session pointer,
     * returning this state to a fresh-install baseline. Unlike {@link #sessions()}, this does not
     * lazily recreate a session afterward.
     */
    public void clearAll() {
        sessions.clear();
        activeSessionId = "";
    }

    /**
     * Indicates whether this state is at its fresh, pre-use baseline: no sessions and no active
     * session id. Does not lazily create a session, unlike {@link #sessions()}.
     *
     * @return true if there are no sessions and the active session id is blank
     */
    public boolean isCleared() {
        return sessions.isEmpty() && (activeSessionId == null || activeSessionId.isBlank());
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
        copy.kind = resolveKind(source.kind, copy.role);
        copy.markdown = redactSecrets(source.markdown);
        copy.createdAt = source.createdAt == null ? "" : source.createdAt;
        return copy;
    }

    /**
     * Resolves the effective kind for a message: an explicit, non-blank {@code kind} is normalized
     * (trimmed, lower-cased) and returned as-is; otherwise the kind is inferred from {@code role} --
     * {@code "user"} resolves to {@link #KIND_USER}, everything else (including a blank/null role)
     * resolves to {@link #KIND_ASSISTANT_TEXT}. Used both at message-construction time ({@link
     * #append}) and at persistence-load time ({@link #normalizeMessage}) so a session saved before
     * the {@code kind} field existed -- where every message deserializes {@code kind} as the field's
     * declared default {@code ""} -- still resolves a sensible, role-inferred kind instead of staying
     * blank.
     */
    public static String resolveKind(String kind, String role) {
        if (kind != null && !kind.isBlank()) {
            return kind.trim().toLowerCase(Locale.ROOT);
        }
        String normalizedRole = role == null ? "" : role.trim().toLowerCase(Locale.ROOT);
        return KIND_USER.equals(normalizedRole) ? KIND_USER : KIND_ASSISTANT_TEXT;
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
        // Keep the full text here -- no fixed character cap. The dropdown renderer
        // (ShaftAssistantPanel.trimChatTitleForWidth) ellipsizes at render time based on the
        // component's actual available width, so the stored title must stay untruncated.
        String text = stripSpeakerLabel(markdown).replace("*", "")
                .replace("`", "")
                .replace("\n", " ")
                .trim();
        text = removeLocalUserName(text)
                .replaceAll("(?iu)\\b" + Pattern.quote(SECOND_PERSON_LABEL) + "\\b", "")
                .replaceAll("\\s{2,}", " ")
                .trim();
        return text.isBlank() ? "New chat" : text;
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
        // Left blank by default rather than eagerly resolved here: a session serialized before this
        // field existed deserializes it as exactly this default ("", the XmlSerializer's behavior for
        // an XML element with no matching child), which is how #resolveKind/normalizeMessage detect
        // "no persisted kind" and fall back to a role-inferred default instead of trusting a blank
        // value as if it were meaningful.
        public String kind = "";
    }
}
