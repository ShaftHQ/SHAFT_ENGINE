package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A clarifying question with selectable suggested answers, detected in an assistant turn's final
 * markdown (issue #3674). Two detection protocols exist, tried in priority order by callers:
 *
 * <ol>
 *   <li>{@link #detectStructuredLine} (issue #3719, preferred): a single trailing line that is a
 *       complete JSON object {@code {"shaft-question": "...", "shaft-options": [...]}}, carrying
 *       the question text and options atomically instead of splitting them across free prose and a
 *       separately-fenced array. {@code AssistantLocalAgentRunner}'s {@code StructuredStreamParser}
 *       recognizes this at the terminal-event boundary for local Claude Code/Codex runs (neither
 *       CLI's own stream-json/{@code --json} protocol carries a native "question with options"
 *       event -- verified against {@code StructuredStreamParser}: the terminal event only ever
 *       yields a plain answer string -- so this is still text the model writes, just a stricter,
 *       single-value shape than the fence below); this class's own {@code detectStructuredLine} is
 *       also called directly on displayed markdown for paths with no runner envelope (cloud chat).
 *   <li>{@link #detect} (issue #3674, documented fallback): a trailing fenced {@code
 *       ```shaft-options} block containing a JSON array of option labels. Kept unchanged so every
 *       existing CLI (Copilot, custom commands, or a model that can't produce a clean single JSON
 *       line) keeps working exactly as before.
 * </ol>
 *
 * The local/cloud system prompt is instructed to prefer the structured line and fall back to the
 * fence (see {@code AssistantCommand}'s {@code SHAFT_OPTIONS_HINT}). Both protocols keep detection
 * an explicit, deliberate signal rather than a prose-pattern-matching guess at "does this text
 * sound like a question".
 *
 * @param promptMarkdown the turn's markdown with the detected marker removed, for normal
 *         persisted/rendered display
 * @param options the offered answer labels, in order, 2-6 non-blank entries
 */
record AssistantQuestion(String promptMarkdown, List<String> options) {
    private static final int MIN_OPTIONS = 2;
    private static final int MAX_OPTIONS = 6;
    private static final Pattern OPTIONS_FENCE = Pattern.compile(
            "```\\s*shaft-options\\s*\\R(.*?)```", Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
    private static final String QUESTION_KEY = "shaft-question";
    private static final String OPTIONS_KEY = "shaft-options";

    /**
     * Detects a {@code shaft-options} fence in {@code markdown} and parses it into an {@link
     * AssistantQuestion}. Returns {@code null} -- never a partially-populated instance -- when no
     * fence is present, its body is not a JSON array of strings, or fewer than {@value
     * #MIN_OPTIONS} / more than {@value #MAX_OPTIONS} non-blank options remain after trimming, so
     * callers can fall back to today's plain-text rendering with a single null check.
     */
    static AssistantQuestion detect(String markdown) {
        if (markdown == null || markdown.isBlank()) {
            return null;
        }
        Matcher matcher = OPTIONS_FENCE.matcher(markdown);
        if (!matcher.find()) {
            return null;
        }
        List<String> options = parseOptionsArray(matcher.group(1));
        if (options.size() < MIN_OPTIONS || options.size() > MAX_OPTIONS) {
            return null;
        }
        String stripped = (markdown.substring(0, matcher.start()) + markdown.substring(matcher.end())).strip();
        return new AssistantQuestion(stripped, options);
    }

    /**
     * Detects the structured single-line protocol (issue #3719) at the end of {@code text}: its
     * last non-blank line must be a complete JSON object {@code {"shaft-question": "<text>",
     * "shaft-options": [...]}}. Returns {@code null} -- never a partially-populated instance --
     * when {@code text} is blank, the last line is not that exact shape (missing/blank question
     * text, missing/non-array options), or the option count falls outside {@value #MIN_OPTIONS}-
     * {@value #MAX_OPTIONS} after trimming, so callers can fall back to {@link #detect} with a
     * single null check, exactly as they do for a run that never attempted this protocol.
     */
    static AssistantQuestion detectStructuredLine(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        String stripped = text.stripTrailing();
        int lastNewline = stripped.lastIndexOf('\n');
        String lastLine = (lastNewline < 0 ? stripped : stripped.substring(lastNewline + 1)).strip();
        if (lastLine.length() < 2 || !lastLine.startsWith("{") || !lastLine.endsWith("}")) {
            return null;
        }
        JsonElement parsed;
        try {
            parsed = JsonParser.parseString(lastLine);
        } catch (RuntimeException exception) {
            return null;
        }
        if (parsed == null || !parsed.isJsonObject()) {
            return null;
        }
        JsonObject object = parsed.getAsJsonObject();
        JsonElement questionElement = object.get(QUESTION_KEY);
        JsonElement optionsElement = object.get(OPTIONS_KEY);
        if (questionElement == null || !questionElement.isJsonPrimitive()
                || !questionElement.getAsJsonPrimitive().isString()
                || optionsElement == null || !optionsElement.isJsonArray()) {
            return null;
        }
        String questionText = questionElement.getAsString().strip();
        if (questionText.isEmpty()) {
            return null;
        }
        List<String> options = parseOptionsArray(optionsElement.getAsJsonArray());
        if (options.size() < MIN_OPTIONS || options.size() > MAX_OPTIONS) {
            return null;
        }
        String remainder = (lastNewline < 0 ? "" : stripped.substring(0, lastNewline)).strip();
        return new AssistantQuestion(remainder, options);
    }

    private static List<String> parseOptionsArray(String fenceBody) {
        JsonElement parsed;
        try {
            parsed = JsonParser.parseString(fenceBody);
        } catch (RuntimeException exception) {
            return List.of();
        }
        if (parsed == null || !parsed.isJsonArray()) {
            return List.of();
        }
        return parseOptionsArray(parsed.getAsJsonArray());
    }

    private static List<String> parseOptionsArray(JsonArray array) {
        List<String> options = new ArrayList<>();
        for (JsonElement element : array) {
            if (element != null && element.isJsonPrimitive()) {
                String option = element.getAsString().strip();
                if (!option.isEmpty()) {
                    options.add(option);
                }
            }
        }
        return options;
    }
}
