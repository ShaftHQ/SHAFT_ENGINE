package com.shaft.intellij.cloud;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Cloud session awareness helpers (W2.8 / #7592): warn before a second session on a 1-slot license,
 * and offer stop-orphan / open-logs verbs without rewriting shaft-mcp.
 */
public final class ShaftCloudSessionAwareness {
    public enum SessionState {
        NONE,
        QUEUED,
        RUNNING,
        UNKNOWN
    }

    public record Snapshot(
            @NotNull SessionState state,
            int runningCount,
            int licenseSlots,
            @Nullable String sessionId,
            @Nullable String logsUrl,
            @Nullable String videoUrl
    ) {
        public Snapshot {
            if (runningCount < 0) {
                throw new IllegalArgumentException("runningCount");
            }
            if (licenseSlots < 1) {
                throw new IllegalArgumentException("licenseSlots");
            }
        }
    }

    private ShaftCloudSessionAwareness() {
        throw new IllegalStateException("Utility class");
    }

    public static boolean wouldExceedLicense(@NotNull Snapshot snapshot) {
        return snapshot.runningCount() >= snapshot.licenseSlots();
    }

    public static @NotNull String preLaunchWarning(@NotNull Snapshot snapshot) {
        if (!wouldExceedLicense(snapshot)) {
            return "";
        }
        String id = snapshot.sessionId() == null || snapshot.sessionId().isBlank()
                ? "an existing session"
                : "session " + snapshot.sessionId();
        return "Cloud license looks full (" + snapshot.runningCount() + "/" + snapshot.licenseSlots()
                + "). Starting another run may queue or fail. Stop " + id
                + " first, or open its logs/video before launching.";
    }

    public static @NotNull String recoveryHint(@NotNull Snapshot snapshot) {
        StringBuilder sb = new StringBuilder("Next: ");
        if (snapshot.sessionId() != null && !snapshot.sessionId().isBlank()) {
            sb.append("stop orphan ").append(snapshot.sessionId());
        } else {
            sb.append("stop the orphan cloud session");
        }
        if (snapshot.logsUrl() != null && !snapshot.logsUrl().isBlank()) {
            sb.append("; logs: ").append(snapshot.logsUrl());
        }
        if (snapshot.videoUrl() != null && !snapshot.videoUrl().isBlank()) {
            sb.append("; video: ").append(snapshot.videoUrl());
        }
        return sb.toString();
    }

    /**
     * Best-effort parse of a provider status JSON fragment (no network). Missing fields → UNKNOWN/0.
     */
    public static @NotNull Snapshot fromProviderJson(@Nullable String json, int licenseSlots) {
        int slots = Math.max(1, licenseSlots);
        if (json == null || json.isBlank()) {
            return new Snapshot(SessionState.UNKNOWN, 0, slots, null, null, null);
        }
        String lower = json.toLowerCase();
        SessionState state = SessionState.UNKNOWN;
        if (lower.contains("\"status\":\"running\"") || lower.contains("\"state\":\"running\"")) {
            state = SessionState.RUNNING;
        } else if (lower.contains("\"status\":\"queued\"") || lower.contains("\"state\":\"queued\"")) {
            state = SessionState.QUEUED;
        } else if (lower.contains("\"status\":\"none\"") || lower.contains("\"active\":0")) {
            state = SessionState.NONE;
        }
        int running = countToken(lower, "\"status\":\"running\"") + countToken(lower, "\"state\":\"running\"");
        if (state == SessionState.RUNNING && running == 0) {
            running = 1;
        }
        String sessionId = extract(json, "sessionId");
        if (sessionId == null) {
            sessionId = extract(json, "id");
        }
        return new Snapshot(state, running, slots, sessionId, extract(json, "logsUrl"), extract(json, "videoUrl"));
    }

    private static int countToken(String haystack, String needle) {
        int count = 0;
        int from = 0;
        while (true) {
            int at = haystack.indexOf(needle, from);
            if (at < 0) {
                return count;
            }
            count++;
            from = at + needle.length();
        }
    }

    private static @Nullable String extract(String json, String field) {
        String key = "\"" + field + "\"";
        int at = json.indexOf(key);
        if (at < 0) {
            return null;
        }
        int colon = json.indexOf(':', at + key.length());
        if (colon < 0) {
            return null;
        }
        int startQuote = json.indexOf('"', colon + 1);
        if (startQuote < 0) {
            return null;
        }
        int endQuote = json.indexOf('"', startQuote + 1);
        if (endQuote < 0) {
            return null;
        }
        return json.substring(startQuote + 1, endQuote);
    }
}
