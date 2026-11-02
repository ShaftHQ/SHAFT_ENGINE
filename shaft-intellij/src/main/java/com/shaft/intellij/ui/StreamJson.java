package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;

import java.util.List;

/**
 * Low-level JSON field readers shared by {@link StructuredStreamParser} and its per-protocol
 * {@link StreamEventMapper} implementations ({@link ClaudeStreamEventMapper}, {@link
 * CodexStreamEventMapper}) -- both Claude's stream-json and Codex's {@code --json} events are
 * plain {@link JsonObject}s read defensively (missing/wrong-typed fields resolve to {@code null}
 * rather than throwing), since both CLIs' schemas are experimental and subject to drift.
 */
final class StreamJson {
    private StreamJson() {
        throw new IllegalStateException("Utility class");
    }

    static JsonObject parseObject(String candidate) {
        if (!candidate.startsWith("{")) {
            return null;
        }
        try {
            JsonElement parsed = JsonParser.parseString(candidate);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (JsonParseException exception) {
            return null;
        }
    }

    static String stringField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()
                ? value.getAsString()
                : null;
    }

    static JsonObject objectField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonObject() ? value.getAsJsonObject() : null;
    }

    static boolean booleanField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isBoolean()
                && value.getAsBoolean();
    }

    static Integer intField(JsonObject object, String key) {
        JsonElement value = object == null ? null : object.get(key);
        if (value == null || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isNumber()) {
            return null;
        }
        try {
            return value.getAsInt();
        } catch (RuntimeException exception) {
            return null;
        }
    }

    static int orZero(Integer value) {
        return value == null ? 0 : value;
    }

    static String firstNonBlank(String... candidates) {
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate;
            }
        }
        return null;
    }

    static Integer firstNonNull(Integer preferred, Integer fallback) {
        return preferred != null ? preferred : fallback;
    }

    /**
     * Picks the first present, non-blank value among the input keys most likely to tell the user
     * what a tool call is actually doing (the command run, the file touched, the pattern searched
     * for, ...), collapsed to a single line (but not otherwise truncated -- the user asked to see
     * full messages). Returns {@code null} when none of the known keys are present rather than
     * guessing at unfamiliar tool schemas.
     */
    static String toolInputSummary(JsonObject input) {
        if (input == null) {
            return null;
        }
        for (String key : List.of(
                "command", "file_path", "path", "pattern", "url", "query", "description", "prompt")) {
            String value = stringField(input, key);
            if (value != null && !value.isBlank()) {
                return value.strip().replaceAll("\\s+", " ");
            }
        }
        return null;
    }
}
