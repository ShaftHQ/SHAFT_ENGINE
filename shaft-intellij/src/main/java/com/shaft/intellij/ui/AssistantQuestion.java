package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A clarifying question with selectable suggested answers, detected in an assistant turn's final
 * markdown (issue #3674). Neither Claude's nor Codex's stream-json protocol carries a structured
 * "question with options" shape (verified against {@code AssistantLocalAgentRunner}'s
 * {@code StructuredStreamParser}: the terminal event only ever yields a plain answer string), so
 * the signal is a trailing fenced {@code ```shaft-options} block containing a JSON array of short
 * option labels -- the local/cloud system prompt is instructed to emit one only for a genuine
 * multiple-choice clarifying question (see {@code AssistantCommand}'s usage hint). This keeps
 * detection an explicit, deliberate protocol addition rather than a prose-pattern-matching guess
 * at "does this text sound like a question".
 *
 * @param promptMarkdown the turn's markdown with the {@code shaft-options} fence removed, for
 *         normal persisted/rendered display
 * @param options the offered answer labels, in order, 2-6 non-blank entries
 */
record AssistantQuestion(String promptMarkdown, List<String> options) {
    private static final int MIN_OPTIONS = 2;
    private static final int MAX_OPTIONS = 6;
    private static final Pattern OPTIONS_FENCE = Pattern.compile(
            "```\\s*shaft-options\\s*\\R(.*?)```", Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

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
        List<String> options = parseOptions(matcher.group(1));
        if (options.size() < MIN_OPTIONS || options.size() > MAX_OPTIONS) {
            return null;
        }
        String stripped = (markdown.substring(0, matcher.start()) + markdown.substring(matcher.end())).strip();
        return new AssistantQuestion(stripped, options);
    }

    private static List<String> parseOptions(String fenceBody) {
        JsonElement parsed;
        try {
            parsed = JsonParser.parseString(fenceBody);
        } catch (RuntimeException exception) {
            return List.of();
        }
        if (parsed == null || !parsed.isJsonArray()) {
            return List.of();
        }
        List<String> options = new ArrayList<>();
        for (JsonElement element : parsed.getAsJsonArray()) {
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
