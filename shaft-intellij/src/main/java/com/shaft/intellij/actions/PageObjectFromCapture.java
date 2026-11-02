package com.shaft.intellij.actions;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Turns the {@code capture_code_blocks} Page Object draft into a Java compilation unit (issue #6422).
 * Codegen stays in shaft-mcp; this class only adds the package and imports.
 */
final class PageObjectFromCapture {
    private static final Pattern CLASS_NAME = Pattern.compile("\\bclass\\s+(\\w+)");

    /** A ready-to-write Page Object. {@code relativePath} is under the source root. */
    record Draft(String className, String relativePath, String source) {
    }

    private PageObjectFromCapture() {
    }

    /** The Page Object draft in a tool result, or null when the session produced none. */
    static Draft draft(String output, String packageName) {
        JsonObject block = draftBlock(parse(output));
        if (block == null) {
            return null;
        }
        String code = block.get("code").getAsString();
        Matcher name = CLASS_NAME.matcher(code);
        if (!name.find()) {
            return null;
        }
        StringBuilder source = new StringBuilder("package ").append(packageName).append(";\n\n");
        if (block.has("imports") && block.get("imports").isJsonArray()) {
            block.getAsJsonArray("imports").forEach(i -> source.append("import ").append(i.getAsString()).append(";\n"));
        }
        source.append('\n').append(code);
        return new Draft(name.group(1), packageName.replace('.', '/') + "/" + name.group(1) + ".java", source.toString());
    }

    private static JsonObject draftBlock(JsonObject payload) {
        if (payload == null) {
            return null;
        }
        if (payload.has("codeBlocks") && payload.get("codeBlocks").isJsonArray()) {
            for (JsonElement block : payload.getAsJsonArray("codeBlocks")) {
                if (block.isJsonObject() && block.getAsJsonObject().has("code")
                        && block.getAsJsonObject().has("id")
                        && block.getAsJsonObject().get("id").getAsString().endsWith("page-object-draft")) {
                    return block.getAsJsonObject();
                }
            }
        }
        if (payload.has("content") && payload.get("content").isJsonArray()) {
            for (JsonElement entry : payload.getAsJsonArray("content")) {
                if (entry.isJsonObject() && entry.getAsJsonObject().has("text")) {
                    JsonObject nested = draftBlock(parse(entry.getAsJsonObject().get("text").getAsString()));
                    if (nested != null) {
                        return nested;
                    }
                }
            }
        }
        return null;
    }

    private static JsonObject parse(String text) {
        try {
            JsonElement parsed = text == null ? null : JsonParser.parseString(text);
            return parsed != null && parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (RuntimeException malformed) {
            return null;
        }
    }
}
