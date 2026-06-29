package com.shaft.intellij.ui;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Converts Assistant and MCP payloads into Markdown suitable for display.
 */
final class AssistantMarkdown {
    private static final Gson PRETTY = new GsonBuilder().setPrettyPrinting().create();

    private AssistantMarkdown() {
        throw new IllegalStateException("Utility class");
    }

    static String fromMcpOutput(String output) {
        return normalizeMarkdown(unwrapMcpText(output));
    }

    static String normalizeMarkdown(String text) {
        if (text == null || text.isBlank()) {
            return "_No response returned._";
        }
        String trimmed = text.trim();
        if (containsCodeFence(trimmed)) {
            return trimmed;
        }
        String prettyJson = prettyJson(trimmed);
        if (!prettyJson.isBlank()) {
            return fence("json", prettyJson);
        }
        if (looksLikeJava(trimmed)) {
            return fence("java", trimmed);
        }
        return trimmed;
    }

    private static String unwrapMcpText(String output) {
        String candidate = output == null ? "" : output.trim();
        for (int depth = 0; depth < 5; depth++) {
            JsonElement parsed = parse(candidate);
            if (parsed == null) {
                return candidate;
            }
            if (parsed.isJsonPrimitive() && parsed.getAsJsonPrimitive().isString()) {
                candidate = parsed.getAsString().trim();
                continue;
            }
            if (parsed.isJsonArray()) {
                String contentText = contentText(parsed.getAsJsonArray());
                if (contentText.isBlank()) {
                    return PRETTY.toJson(parsed);
                }
                if (parse(contentText) != null) {
                    candidate = contentText;
                    continue;
                }
                return contentText;
            }
            if (!parsed.isJsonObject()) {
                return PRETTY.toJson(parsed);
            }
            JsonObject object = parsed.getAsJsonObject();
            String contentText = contentText(object);
            if (!contentText.isBlank()) {
                if (parse(contentText) != null) {
                    candidate = contentText;
                    continue;
                }
                return contentText;
            }
            String nested = firstTextProperty(object, "markdown", "text", "message", "response", "output");
            if (!nested.isBlank() && !nested.equals(candidate)) {
                candidate = nested.trim();
                continue;
            }
            JsonElement result = object.get("result");
            if (result != null && !result.isJsonNull()) {
                candidate = result.isJsonPrimitive() && result.getAsJsonPrimitive().isString()
                        ? result.getAsString().trim()
                        : PRETTY.toJson(result);
                continue;
            }
            return PRETTY.toJson(object);
        }
        return candidate;
    }

    private static String contentText(JsonObject object) {
        JsonElement content = object.get("content");
        return content != null && content.isJsonArray() ? contentText(content.getAsJsonArray()) : "";
    }

    private static String contentText(JsonArray array) {
        List<String> parts = new ArrayList<>();
        for (JsonElement item : array) {
            if (item == null || !item.isJsonObject()) {
                continue;
            }
            JsonObject object = item.getAsJsonObject();
            JsonElement text = object.get("text");
            if (text != null && text.isJsonPrimitive() && text.getAsJsonPrimitive().isString()) {
                parts.add(text.getAsString().trim());
            }
        }
        return String.join("\n\n", parts).trim();
    }

    private static String firstTextProperty(JsonObject object, String... names) {
        for (String name : names) {
            JsonElement value = object.get(name);
            if (value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()) {
                return value.getAsString();
            }
        }
        return "";
    }

    private static JsonElement parse(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        try {
            return JsonParser.parseString(text);
        } catch (RuntimeException exception) {
            return null;
        }
    }

    private static String prettyJson(String text) {
        JsonElement parsed = parse(text);
        return parsed == null ? "" : PRETTY.toJson(parsed);
    }

    private static boolean containsCodeFence(String text) {
        return text.contains("```");
    }

    private static boolean looksLikeJava(String text) {
        String lower = text.toLowerCase(Locale.ROOT);
        return lower.contains("public class ")
                || lower.contains("class ")
                && (lower.contains("void ") || lower.contains("extends ") || lower.contains("implements "))
                || lower.startsWith("package ")
                || lower.startsWith("import ")
                || lower.contains("@test")
                || lower.contains("webDriver.element()");
    }

    private static String fence(String language, String text) {
        return "```" + language + "\n" + text.stripTrailing() + "\n```";
    }
}
